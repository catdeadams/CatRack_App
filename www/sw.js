// CatRack service worker.
//
// What we cache:
//   * the PWA manifest + icons (so the home-screen install survives offline)
//   * static JS/CSS shipped by Shiny under /static and /shared
//
// What we DON'T cache: any Supabase calls or Shiny websocket traffic.
// The app needs a live R server to render workouts/log sets, so going
// offline mid-session still degrades — but the SHELL won't blank out
// and the user sees a clear banner.

const CACHE = 'catrack-v5';

// No static PRECACHE list: under Posit Connect the app is served from a
// content-scoped path (not '/'), so absolute paths like '/www/...' 404 and
// addAll() rejects atomically — precaching nothing anyway, while risking a
// wrong-shell cache on a root deploy. The runtime fetch handler below already
// mirrors successful /www, /static and /shared responses into the cache, which
// is scope-correct. So install just activates immediately.
self.addEventListener('install', e => {
  e.waitUntil(self.skipWaiting());
});

self.addEventListener('activate', e => {
  e.waitUntil(
    caches.keys().then(keys =>
      Promise.all(keys.filter(k => k !== CACHE).map(k => caches.delete(k)))
    ).then(() => clients.claim())
  );
});

// Network-first for everything, falling back to cache, falling back to
// a friendly offline string for navigation requests. Skip non-GET and
// non-http(s) to avoid breaking websockets.
self.addEventListener('fetch', e => {
  const req = e.request;
  if (req.method !== 'GET') return;
  if (!req.url.startsWith('http')) return;

  e.respondWith(
    fetch(req)
      .then(resp => {
        // Mirror successful static responses into the cache
        if (resp && resp.status === 200 &&
            (req.url.includes('/www/') ||
             req.url.includes('/static/') ||
             req.url.includes('/shared/'))) {
          const clone = resp.clone();
          caches.open(CACHE).then(c => c.put(req, clone)).catch(() => null);
        }
        return resp;
      })
      .catch(() =>
        caches.match(req).then(hit => hit ||
          (req.mode === 'navigate'
            ? new Response('CatRack is offline. Reconnect to continue logging.',
                {headers: {'Content-Type': 'text/plain'}})
            : Response.error()))
      )
  );
});
