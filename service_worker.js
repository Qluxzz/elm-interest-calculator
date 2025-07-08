const CACHE_NAME = "interest-app"
const PRE_CACHED_RESOURCES = [
  "/elm-interest-calculator/",
  "manifest.json",
  "elm.js",
  "style.css",
]

self.addEventListener("install", (e) => {
  e.waitUntil(
    caches.open(CACHE_NAME).then((cache) => cache.addAll(PRE_CACHED_RESOURCES))
  )
})

/**
 * @param {Request} request
 */
async function cacheFirst(request) {
  const url = new URL(request.url)

  // The query is only read by the app so we want to ignore the query when caching
  // so we don't cache the same thing multiple times
  const urlWithoutQuery = `${url.origin}${url.pathname}`

  const cachedResponse = await caches.match(urlWithoutQuery)
  if (cachedResponse) {
    return cachedResponse
  }
  try {
    const networkResponse = await fetch(request)
    if (networkResponse.ok) {
      const cache = await caches.open(CACHE_NAME)
      cache.put(urlWithoutQuery, networkResponse.clone())
    }
    return networkResponse
  } catch (error) {
    return Response.error()
  }
}

self.addEventListener("fetch", (event) => {
  const { pathname } = new URL(event.request.url)
  if (PRE_CACHED_RESOURCES.some((resource) => pathname.endsWith(resource))) {
    event.respondWith(cacheFirst(event.request))
  }
})
