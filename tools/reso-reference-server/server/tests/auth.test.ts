import { describe, expect, it } from 'vitest';
import type { AuthTokenConfig } from '../src/auth/config.js';
import { hasRole, registerDynamicToken, resolveRole } from '../src/auth/config.js';

const TEST_CONFIG: AuthTokenConfig = {
  adminToken: 'admin-token',
  writeToken: 'write-token',
  readToken: 'read-token',
  authRequired: true
};

describe('hasRole', () => {
  it('admin has all roles', () => {
    expect(hasRole('admin', 'admin')).toBe(true);
    expect(hasRole('admin', 'write')).toBe(true);
    expect(hasRole('admin', 'read')).toBe(true);
  });

  it('write has write and read roles', () => {
    expect(hasRole('write', 'write')).toBe(true);
    expect(hasRole('write', 'read')).toBe(true);
    expect(hasRole('write', 'admin')).toBe(false);
  });

  it('read has only read role', () => {
    expect(hasRole('read', 'read')).toBe(true);
    expect(hasRole('read', 'write')).toBe(false);
    expect(hasRole('read', 'admin')).toBe(false);
  });
});

describe('resolveRole', () => {
  it('resolves admin token', () => {
    expect(resolveRole('admin-token', TEST_CONFIG)).toBe('admin');
  });

  it('resolves write token', () => {
    expect(resolveRole('write-token', TEST_CONFIG)).toBe('write');
  });

  it('resolves read token', () => {
    expect(resolveRole('read-token', TEST_CONFIG)).toBe('read');
  });

  it('returns null for unknown tokens', () => {
    expect(resolveRole('unknown-token', TEST_CONFIG)).toBeNull();
  });

  it('resolves dynamic tokens from mock OAuth', () => {
    registerDynamicToken('dynamic-admin-123', 'admin');
    expect(resolveRole('dynamic-admin-123', TEST_CONFIG)).toBe('admin');
  });

  it('resolves dynamic tokens with write role', () => {
    registerDynamicToken('dynamic-write-456', 'write');
    expect(resolveRole('dynamic-write-456', TEST_CONFIG)).toBe('write');
  });
});
