#!/usr/bin/env node

/**
 * CLI entry point for the RESO Web API Add/Edit compliance testing tool.
 *
 * Parses command-line arguments (--url, --resource, --payloads, etc.),
 * optionally starts a local mock OData server with --mock, runs all 8 certification
 * scenarios, and outputs results to the console or as JSON.
 *
 * Authentication: provide either --auth-token for a pre-fetched bearer token,
 * or --client-id / --client-secret / --token-url for OAuth2 Client Credentials.
 *
 * Exit codes: 0 = all scenarios passed, 1 = one or more failed, 2 = runtime error.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { Command } from 'commander';
import { generateComplianceReport } from '../add-edit/compliance-report.js';
import { startMockServer, stopMockServer } from '../add-edit/mock/server.js';
import { runAllScenarios } from '../add-edit/test-runner.js';
import { fetchMetadata, getEntityType, loadMetadataFromFile, parseMetadataXml } from '../test-runner/metadata.js';
import { formatConsoleReport, formatJsonReport } from '../test-runner/reporter.js';
import type { AuthConfig, TestConfig } from '../test-runner/types.js';

/** Default port for the mock OData server when started via --mock. */
const DEFAULT_MOCK_PORT = 8800;

const program = new Command();

program
  .name('reso-cert-add-edit')
  .description('RESO Web API Add/Edit Endorsement (RCP-010) compliance testing tool')
  .requiredOption('--url <url>', 'Server base URL (e.g., https://api.reso.org)')
  .requiredOption('--resource <name>', 'OData resource name (e.g., Property)')
  .requiredOption('--payloads <dir>', 'Path to directory containing payload JSON files')
  .option('--auth-token <token>', 'Pre-fetched bearer token for authorization')
  .option('--client-id <id>', 'OAuth2 client ID for Client Credentials grant')
  .option('--client-secret <secret>', 'OAuth2 client secret')
  .option('--token-url <url>', 'OAuth2 token endpoint URL')
  .option('--metadata <path>', 'Path to local XML metadata file')
  .option('--mock', 'Start a mock OData server instead of testing a real server')
  .option('--output <format>', 'Output format: console or json', 'console')
  .option('--compliance-report <path>', 'Write compliance report JSON to file')
  .option('--spec-version <version>', 'Specification version for compliance report', '2.0.0')
  .action(
    async (opts: {
      url: string;
      resource: string;
      payloads: string;
      authToken?: string;
      clientId?: string;
      clientSecret?: string;
      tokenUrl?: string;
      metadata?: string;
      mock?: boolean;
      output: string;
      complianceReport?: string;
      specVersion: string;
    }) => {
      let serverUrl = opts.url;
      let mockServer: Awaited<ReturnType<typeof startMockServer>> | null = null;

      try {
        if (opts.mock) {
          const metadataXml = opts.metadata ? await readFile(resolve(opts.metadata), 'utf-8') : await loadDefaultMetadata();

          const mock = await startMockServer({
            metadataXml,
            resource: opts.resource,
            port: DEFAULT_MOCK_PORT
          });
          mockServer = mock;
          serverUrl = mock.url;
          console.log(`Mock server started at ${mock.url}`);
        }

        const auth = buildAuthConfig(opts, serverUrl);

        const config: TestConfig = {
          serverUrl,
          resource: opts.resource,
          payloadsDir: resolve(opts.payloads),
          auth,
          metadataPath: opts.metadata ? resolve(opts.metadata) : undefined,
          useMock: opts.mock
        };

        const report = await runAllScenarios(config);

        if (opts.output === 'json') {
          console.log(formatJsonReport(report));
        } else {
          console.log(formatConsoleReport(report));
        }

        // Generate compliance report if requested
        if (opts.complianceReport) {
          const metadataXml = opts.metadata
            ? await loadMetadataFromFile(resolve(opts.metadata))
            : await fetchMetadata(serverUrl, opts.authToken ?? '');
          const metadata = parseMetadataXml(metadataXml);
          const entityType = getEntityType(metadata, opts.resource);

          if (entityType) {
            const payloads = await loadPayloadMap(resolve(opts.payloads));
            const complianceReport = generateComplianceReport(report, {
              version: opts.specVersion,
              payloads,
              entityType
            });
            await writeFile(resolve(opts.complianceReport), JSON.stringify(complianceReport, null, 2));
            console.log(`\nCompliance report written to: ${opts.complianceReport}`);
          }
        }

        process.exitCode = report.summary.failed > 0 ? 1 : 0;
      } catch (error) {
        console.error('Error:', error instanceof Error ? error.message : String(error));
        process.exitCode = 2;
      } finally {
        if (mockServer) {
          await stopMockServer(mockServer.server);
        }
      }
    }
  );

/**
 * Builds an AuthConfig from CLI options.
 * Validates that either --auth-token OR all three of --client-id/--client-secret/--token-url are provided.
 * When --mock is used with client credentials, the token URL is automatically pointed to the mock server.
 */
const buildAuthConfig = (
  opts: {
    authToken?: string;
    clientId?: string;
    clientSecret?: string;
    tokenUrl?: string;
    mock?: boolean;
  },
  serverUrl: string
): AuthConfig => {
  const hasToken = Boolean(opts.authToken);
  const hasClientCreds = Boolean(opts.clientId) || Boolean(opts.clientSecret) || Boolean(opts.tokenUrl);

  if (hasToken && hasClientCreds) {
    throw new Error('Cannot use --auth-token together with --client-id/--client-secret/--token-url. Choose one authentication method.');
  }

  if (!hasToken && !hasClientCreds) {
    throw new Error('Authentication required. Provide either --auth-token or --client-id/--client-secret/--token-url.');
  }

  if (hasToken) {
    return { mode: 'token', authToken: opts.authToken! };
  }

  if (!opts.clientId || !opts.clientSecret) {
    throw new Error('OAuth2 Client Credentials requires --client-id, --client-secret, and --token-url.');
  }

  const tokenUrl = opts.mock ? `${serverUrl}/oauth/token` : opts.tokenUrl;

  if (!tokenUrl) {
    throw new Error('OAuth2 Client Credentials requires --token-url (or use --mock for automatic mock token endpoint).');
  }

  return {
    mode: 'client_credentials',
    clientId: opts.clientId,
    clientSecret: opts.clientSecret,
    tokenUrl
  };
};

/** Loads all payload JSON files from a directory into a keyed map for the compliance report. */
const loadPayloadMap = async (payloadsDir: string): Promise<Record<string, Record<string, unknown>>> => {
  const loadJson = async (filename: string): Promise<Record<string, unknown>> => {
    const content = await readFile(resolve(payloadsDir, filename), 'utf-8');
    return JSON.parse(content) as Record<string, unknown>;
  };

  return {
    createSucceeds: await loadJson('create-succeeds.json'),
    createFails: await loadJson('create-fails.json'),
    updateSucceeds: await loadJson('update-succeeds.json'),
    updateFails: await loadJson('update-fails.json'),
    deleteSucceeds: await loadJson('delete-succeeds.json'),
    deleteFails: await loadJson('delete-fails.json')
  };
};

/** Loads the bundled sample-metadata.xml shipped with this package as a fallback for --mock without --metadata. */
const loadDefaultMetadata = async (): Promise<string> => {
  const defaultPath = resolve(import.meta.dirname, '../../sample-metadata.xml');
  return readFile(defaultPath, 'utf-8');
};

program.parse();
