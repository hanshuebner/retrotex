const initKeyboard = (send: (ceptCodes: number[]) => void) => {
  const modifiersPressed: Element[] = []

  const keyToCept = {
    c14Star: [0x13],
    c14Hash: [0x1c],
    Enter: [0x0a],
    Space: [0x20],
    Delete: [0x7f],
  }

  // Flash a typed key
  const flashKey = (element: Element) => {
    element.classList.toggle('black')
    if (element.getAttribute('data-modifier')) {
      if (modifiersPressed.includes(element)) {
        modifiersPressed.splice(modifiersPressed.indexOf(element), 1)
      } else {
        modifiersPressed.push(element)
      }
    } else {
      setTimeout(() => {
        element.classList.toggle('black')
        modifiersPressed.forEach((modifier) =>
          modifier.classList.toggle('black'),
        )
        modifiersPressed.length = 0
      }, 100)
      if (element.classList.contains('led-key')) {
        const dataGroup = element.getAttribute('data-group')

        if (dataGroup) {
          if (!element.querySelector('.led')?.classList.contains('active')) {
            const groupElements = element.ownerDocument.querySelectorAll(
              `.led-key[data-group="${dataGroup}"]`,
            )
            groupElements.forEach((element) => {
              element.querySelector('.led')?.classList.remove('active')
            })
            element.querySelector('.led')?.classList.toggle('active', true)
          }
        } else {
          element.querySelector('.led')?.classList.toggle('active')
        }
      }
    }
  }

  // Handle clicks on SVG elements
  const handleSvgClick = (event: MouseEvent) => {
    const target = (event.target as Element).closest('[id]')
    if (!target) {
      return
    }
    const code = target?.getAttribute('data-code') as string
    console.log('key', code)
    const sequence = keyToCept[code as keyof typeof keyToCept]
    if (sequence) {
      send(sequence)
    }
    flashKey(target)
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
  const handleKeyDownEvent = (event: KeyboardEvent) => {
    console.log('event.code', event.code)
    let keyElement = svgDoc.getElementById(`key-${event.code}`)
    switch (event.key) {
      case 'F1': // '*'
        send([0x13])
        keyElement = svgDoc.getElementById(`key-c14Star`)
        break
      case 'F2': // '#'
        send([0x1c])
        keyElement = svgDoc.getElementById(`key-c14Hash`)
        break
      case 'Delete':
      case 'Backspace':
        send([0x7f])
        keyElement = svgDoc.getElementById(`key-Delete`)
        break
      case 'F12':
        // fixme what do we need to send?
        keyElement = svgDoc.getElementById('key-c14Reveal')
        break
      default:
        if (event.key.length === 1 && !event.altKey && !event.metaKey) {
          console.log(`sending ${event.key.charCodeAt(0)}`)
          send([event.key.charCodeAt(0)])
        } else {
          console.log(`ignored key ${event.key}`)
          keyElement = null
        }
    }
    if (keyElement) {
      event.preventDefault()
      flashKey(keyElement)
    }
  }
  document.addEventListener('keydown', handleKeyDownEvent)
  svgDoc.addEventListener('keydown', handleKeyDownEvent)

  document.addEventListener('keyup', (_event: KeyboardEvent) => {})

  return { setLed }
}

export default initKeyboard
