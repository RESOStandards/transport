import type { Server } from 'node:http';
import express from 'express';
import { afterAll, beforeAll, describe, expect, it } from 'vitest';
import { fetchAccessToken, resolveAuthToken } from '../src/lib/auth.js';

let server: Server;
let tokenUrl: string;

beforeAll(async () => {
  const app = express();
  app.use(express.urlencoded({ extended: false }));

  app.post('/oauth/token', (req, res) => {
    const { grant_type, client_id, client_secret } = req.body;

    if (grant_type !== 'client_credentials') {
      res.status(400).json({ error: 'unsupported_grant_type' });
      return;
    }

    if (client_id === 'bad-client') {
      res.status(401).json({ error: 'invalid_client' });
      return;
    }

    if (!client_id || !client_secret) {
      res.status(400).json({ error: 'invalid_request' });
      return;
    }

    res.json({
      access_token: `token-for-${client_id}`,
      token_type: 'Bearer',
      expires_in: 3600
    });
  });

  await new Promise<void>(resolve => {
    server = app.listen(0, () => {
      const address = server.address();
      const port = typeof address === 'object' && address ? address.port : 0;
      tokenUrl = `http://localhost:${port}/oauth/token`;
      resolve();
    });
  });
});

afterAll(async () => {
  await new Promise<void>((resolve, reject) => {
    server.close(err => (err ? reject(err) : resolve()));
  });
});

describe('fetchAccessToken', () => {
  it('returns an access token for valid credentials', async () => {
    const token = await fetchAccessToken('my-client', 'my-secret', tokenUrl);
    expect(token).toBe('token-for-my-client');
  });

  it('throws on invalid client', async () => {
    await expect(fetchAccessToken('bad-client', 'secret', tokenUrl)).rejects.toThrow('OAuth2 token request failed: 401');
  });

  it('throws on unreachable URL', async () => {
    await expect(fetchAccessToken('id', 'secret', 'http://localhost:1/oauth/token')).rejects.toThrow();
  });
});

describe('resolveAuthToken', () => {
  it('returns token directly for token mode', async () => {
    const token = await resolveAuthToken({
      mode: 'token',
      authToken: 'my-bearer-token'
    });
    expect(token).toBe('my-bearer-token');
  });

  it('fetches token for client_credentials mode', async () => {
    const token = await resolveAuthToken({
      mode: 'client_credentials',
      clientId: 'sdk-client',
      clientSecret: 'sdk-secret',
      tokenUrl
    });
    expect(token).toBe('token-for-sdk-client');
  });
});
