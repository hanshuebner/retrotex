const initKeyboard = (send: (code: number) => void) => {
  const modifiersPressed: Element[] = []
  // Function to handle clicks on SVG elements
  const handleSvgClick = (event: MouseEvent) => {
    const target = (event.target as Element).closest('[id]')
    if (!target) {
      return
    }
    const keyCode = parseInt(target?.getAttribute('data-keycode') as string)
    console.log('keycode', keyCode)
    send(keyCode)
    target.classList.toggle('black')
    if (target.getAttribute('data-modifier')) {
      if (modifiersPressed.includes(target)) {
        modifiersPressed.splice(modifiersPressed.indexOf(target), 1)
      } else {
        modifiersPressed.push(target)
      }
    } else {
      setTimeout(() => {
        target.classList.toggle('black')
        modifiersPressed.forEach((modifier) =>
          modifier.classList.toggle('black'),
        )
        modifiersPressed.length = 0
      }, 100)
      if (target.classList.contains('led-key')) {
        const dataGroup = target.getAttribute('data-group')

        if (dataGroup) {
          if (!target.querySelector('.led')?.classList.contains('active')) {
            const groupElements = target.ownerDocument.querySelectorAll(
              `.led-key[data-group="${dataGroup}"]`,
            )
            groupElements.forEach((element) => {
              element.querySelector('.led')?.classList.remove('active')
            })
            target.querySelector('.led')?.classList.toggle('active', true)
          }
        } else {
          target.querySelector('.led')?.classList.toggle('active')
        }
      }
    }
  }

  // Add event listener to the embedded SVG once it's loaded
  const container = document.getElementById('keyboard-svg') as HTMLObjectElement
  const svgDoc = container.contentDocument as Document

  // Function to set the active state of an element's LED child
  const setLed = (elementId: string, isActive: boolean) => {
    const element = svgDoc.getElementById(elementId)
    if (element) {
      const led = element.querySelector('.led')
      if (led) {
        led.classList.toggle('active', isActive)
      }
    }
  }

  const ensureIntegerAttribute = (element: Element, attributeName: string) => {
    const attribute = element.getAttribute(attributeName)
    if (!attribute) {
      throw new Error(`Attribute ${attributeName} not found on element`)
    }
    return parseInt(attribute)
  }

  const resizeKeyboard = () => {
    const svgElement = svgDoc.documentElement
    const keyboardContainer = document.getElementById('keyboard-container')
    const aspectRatio =
      ensureIntegerAttribute(svgElement, 'width') /
      ensureIntegerAttribute(svgElement, 'height')
    const containerWidth = keyboardContainer!.clientWidth
    const containerHeight = window.innerHeight * (1 / 3) // One-third of the window height

    let width, height
    if (containerWidth / containerHeight > aspectRatio) {
      // Window is too wide, adjust width to maintain aspect ratio
      height = containerHeight
      width = height * aspectRatio
    } else {
      // Window is not too wide, adjust height to maintain aspect ratio
      width = containerWidth
      height = width / aspectRatio
    }

    // Apply the new size to the SVG object
    svgElement.style.width = `${width}px`
    svgElement.style.height = `${height}px`
  }

  if (svgDoc) {
    // keyboard might not be displayed
    svgDoc.addEventListener('mousedown', handleSvgClick)
    window.addEventListener('resize', resizeKeyboard)
    resizeKeyboard()
  }
  container.addEventListener('keypress', (event: KeyboardEvent) => {
    console.log(event)
    send(event.key.charCodeAt(0))
    event.preventDefault()
  })

  return { setLed }
}

export default initKeyboard
