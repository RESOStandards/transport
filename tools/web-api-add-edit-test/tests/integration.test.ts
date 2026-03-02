import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import type { Server } from "node:http";
import { startMockServer, stopMockServer } from "../src/mock/server.js";
import { seedStore } from "../src/mock/handlers.js";
import { runAllScenarios } from "../src/lib/test-runner.js";

const metadataXml = readFileSync(
  resolve(import.meta.dirname, "../sample-metadata.xml"),
  "utf-8",
);

const payloadsDir = resolve(import.meta.dirname, "../sample-payloads");

let server: Server;
let serverUrl: string;

beforeAll(async () => {
  const mock = await startMockServer({
    metadataXml,
    resource: "Property",
    port: 0,
  });
  server = mock.server;
  serverUrl = mock.url;

  // Seed the store with a record so update and delete-succeeds can target it
  seedStore("12345", {
    ListingKey: "12345",
    ListPrice: 123456.0,
    BedroomsTotal: 3,
    BathroomsTotalInteger: 3,
    StandardStatus: "Coming Soon",
    ModificationTimestamp: "2022-12-05T18:33:20Z",
  });
});

afterAll(async () => {
  await stopMockServer(server);
});

describe("Full scenario run against mock server", () => {
  it("all 8 scenarios pass against a compliant mock", async () => {
    const report = await runAllScenarios({
      serverUrl,
      resource: "Property",
      payloadsDir,
      auth: { mode: "token", authToken: "test-token" },
    });

    expect(report.scenarios).toHaveLength(8);

    for (const scenario of report.scenarios) {
      const failedAssertions = scenario.assertions.filter(
        (a) => a.status === "fail",
      );
      if (failedAssertions.length > 0) {
        const details = failedAssertions
          .map(
            (a) =>
              `  - ${a.description}: expected=${a.expected}, actual=${a.actual}`,
          )
          .join("\n");
        expect.fail(
          `Scenario "${scenario.scenario}" failed:\n${details}`,
        );
      }
    }

    expect(report.summary.failed).toBe(0);
    expect(report.summary.passed).toBe(8);
  }, 30_000);

  it("all 8 scenarios pass with OAuth2 Client Credentials auth", async () => {
    // Re-seed the store since previous tests may have deleted the record
    seedStore("12345", {
      ListingKey: "12345",
      ListPrice: 123456.0,
      BedroomsTotal: 3,
      BathroomsTotalInteger: 3,
      StandardStatus: "Coming Soon",
      ModificationTimestamp: "2022-12-05T18:33:20Z",
    });

    const report = await runAllScenarios({
      serverUrl,
      resource: "Property",
      payloadsDir,
      auth: {
        mode: "client_credentials",
        clientId: "test-client",
        clientSecret: "test-secret",
        tokenUrl: `${serverUrl}/oauth/token`,
      },
    });

    expect(report.scenarios).toHaveLength(8);
    expect(report.summary.failed).toBe(0);
    expect(report.summary.passed).toBe(8);
  }, 30_000);

  it("produces a valid report structure", async () => {
    const report = await runAllScenarios({
      serverUrl,
      resource: "Property",
      payloadsDir,
      auth: { mode: "token", authToken: "test-token" },
    });

    expect(report.serverUrl).toBe(serverUrl);
    expect(report.resource).toBe("Property");
    expect(report.timestamp).toBeTruthy();
    expect(report.summary.total).toBe(8);

    const scenarioNames = report.scenarios.map((s) => s.scenario);
    expect(scenarioNames).toContain("create-succeeds-representation");
    expect(scenarioNames).toContain("create-succeeds-minimal");
    expect(scenarioNames).toContain("create-fails");
    expect(scenarioNames).toContain("update-succeeds-representation");
    expect(scenarioNames).toContain("update-succeeds-minimal");
    expect(scenarioNames).toContain("update-fails");
    expect(scenarioNames).toContain("delete-succeeds");
    expect(scenarioNames).toContain("delete-fails");
  }, 30_000);
});
