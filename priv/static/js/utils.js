async function doFetch(fetchFn) {
  try {
    const response = await fetchFn

    if (!response.ok) throw response

    location.reload()
  } catch (e) {
    console.error(e)
    alert("Something bad happened. Check the console for more info.")
  }
}

function encodeFormData(formElement) {
  const data = new URLSearchParams()
  for (const [key, value] of new FormData(formElement)) {
    data.append(key, value)
  }
  return data.toString()
}
