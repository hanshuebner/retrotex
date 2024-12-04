document.addEventListener('DOMContentLoaded', () => {
  const fileSelector = document.getElementById('file-selector')
  if (fileSelector) {
    fileSelector.addEventListener('change', async (ev) => {
      const filename = ev.target.value
      const result = await fetch(
        new Request(`/api/load-btl-file?filename=${filename}`, {}),
      )
      if (!result.ok) {
        window.location = window.location
      }
    })
  }

  const pageSelector = document.getElementById('page-selector')
  if (pageSelector) {
    pageSelector.addEventListener('change', async (ev) => {
      const page = ev.target.value
      const result = await fetch(
        new Request(`/api/goto-page?number=${page}`, {}),
      )
      if (!result.ok) {
        window.location = window.location
      }
    })
  }

  const closeButton = document.getElementById('close-btl-file')
  if (closeButton) {
    closeButton.addEventListener('click', async (ev) => {
      const result = await fetch(new Request(`/api/close-btl-file`, {}))
      if (!result.ok) {
        window.location = window.location
      }
    })
  }
})
