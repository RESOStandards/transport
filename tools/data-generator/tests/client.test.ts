import { readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, describe, expect, it } from 'vitest';
import { generateCurlScript, writeRecordsAsJson } from '../src/client.js';

const TEST_DIR = join(tmpdir(), 'reso-data-generator-test');

afterEach(async () => {
  try {
    await rm(TEST_DIR, { recursive: true, force: true });
  } catch {
    // Ignore cleanup errors
  }
});

describe('writeRecordsAsJson', () => {
  it('creates directory and writes JSON files', async () => {
    const records = [
      { ListPrice: 500000, City: 'Springfield' },
      { ListPrice: 300000, City: 'Fairview' }
    ];
    const result = await writeRecordsAsJson(TEST_DIR, 'Property', records);

    expect(result.created).toBe(2);
    expect(result.failed).toBe(0);
    expect(result.keys).toHaveLength(2);

    // Verify file content
    const content = await readFile(join(TEST_DIR, 'property', '0001.json'), 'utf-8');
    const parsed = JSON.parse(content);
    expect(parsed.ListPrice).toBe(500000);
    expect(parsed.City).toBe('Springfield');
  });

  it('names files with zero-padded indices', async () => {
    const records = Array.from({ length: 3 }, (_, i) => ({ index: i }));
    const result = await writeRecordsAsJson(TEST_DIR, 'Media', records);

    expect(result.created).toBe(3);
    expect(result.keys).toEqual(['0001.json', '0002.json', '0003.json']);
  });

  it('calls progress callback', async () => {
    const records = [{ a: 1 }, { a: 2 }];
    const progress: Array<[number, number]> = [];
    await writeRecordsAsJson(TEST_DIR, 'Test', records, (completed, total) => {
      progress.push([completed, total]);
    });
    expect(progress).toEqual([
      [1, 2],
      [2, 2]
    ]);
  });
});

describe('generateCurlScript', () => {
  it('generates a valid bash script', async () => {
    const outputFile = join(TEST_DIR, 'seed.sh');
    const records = [
      { ListPrice: 500000, City: 'Springfield' },
      { ListPrice: 300000, City: 'Fairview' }
    ];

    await generateCurlScript(outputFile, 'http://localhost:8080', { mode: 'token', authToken: 'test-token' }, [
      { resource: 'Property', records }
    ]);

    const content = await readFile(outputFile, 'utf-8');

    // Verify script structure
    expect(content).toContain('#!/usr/bin/env bash');
    expect(content).toContain('http://localhost:8080');
    expect(content).toContain('test-token');
    expect(content).toContain('/Property');
    expect(content).toContain('curl -sf -X POST');
    expect(content).toContain('"ListPrice":500000');
    expect(content).toContain('"City":"Springfield"');
    expect(content).toContain('Seed complete.');
  });

  it('includes all resources in the script', async () => {
    const outputFile = join(TEST_DIR, 'multi-seed.sh');
    await generateCurlScript(outputFile, 'http://localhost:8080', { mode: 'token', authToken: 'admin' }, [
      { resource: 'Property', records: [{ ListPrice: 100000 }] },
      { resource: 'Media', records: [{ MediaURL: 'https://example.com/img.jpg' }] }
    ]);

    const content = await readFile(outputFile, 'utf-8');
    expect(content).toContain('Property (1 records)');
    expect(content).toContain('Media (1 records)');
    expect(content).toContain('/Property');
    expect(content).toContain('/Media');
  });

  it('includes wait-for-server logic', async () => {
    const outputFile = join(TEST_DIR, 'wait-seed.sh');
    await generateCurlScript(outputFile, 'http://localhost:8080', { mode: 'token', authToken: 'test' }, [
      { resource: 'Property', records: [{ ListPrice: 100000 }] }
    ]);

    const content = await readFile(outputFile, 'utf-8');
    expect(content).toContain('health');
    expect(content).toContain('until curl');
    expect(content).toContain('sleep 2');
  });
});
