// Minimal passthrough service worker — enables PWA installability on Android.
// Does not cache anything; the app requires a live connection.
const CACHE = 'catrack-v1';

self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', e => e.waitUntil(clients.claim()));
self.addEventListener('fetch', e =>
  e.respondWith(fetch(e.request).catch(() => caches.match(e.request)))
);
