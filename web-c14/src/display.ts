export interface Display {
  fonts: Uint8Array[]
  fontDiacritical: Uint8Array
  drawString: (
    str: string,
    row: number,
    col: number,
    fontData: Uint8Array,
    fgColor: number,
    bgColor: number,
    doubleWidth?: boolean,
    doubleHeight?: boolean,
  ) => void
  drawGlyph: (
    glyphIndex: number,
    row: number,
    col: number,
    fontData: Uint8Array,
    fgColor: number,
    bgColor: number,
    doubleWidth?: boolean,
    doubleHeight?: boolean,
  ) => void
  defineDrcs: (glyphNumber: number, rows: number[][]) => void
  render: () => void
  setScreenColor: (color: number) => void
  setRowColor: (row: number, color: number) => void
}

export default async (canvas: HTMLCanvasElement): Promise<Display> => {
  const ctx = canvas.getContext('2d')
  if (!ctx) {
    throw new Error('Could not get 2d context')
  }

  const glyphWidth = 12
  const glyphHeight = 10
  const glyphsPerFontRow = 16 // 96 glyphs in a 16x6 grid
  const screenPixelWidth = 40 * glyphWidth
  const screenPixelHeight = 25 * glyphHeight
  const borderPixelWidth = Math.floor((canvas.width - screenPixelWidth) / 2)
  const borderPixelHeight = Math.floor((canvas.height - screenPixelHeight) / 2)
  const renderImageWidth = screenPixelWidth + 2 * borderPixelWidth
  const renderImageHeight = screenPixelHeight + 2 * borderPixelHeight

  const framebuffer = new Uint16Array(renderImageWidth * renderImageHeight)

  const drawString = (
    str: string,
    row: number,
    col: number,
    fontData: Uint8Array,
    fgColor: number,
    bgColor: number,
    doubleWidth: boolean = false,
    doubleHeight: boolean = false,
  ) => {
    console.assert(col + str.length < 40)
    for (let i = 0; i < str.length; i++) {
      drawGlyph(
        str.charCodeAt(i) - 32,
        row,
        col + i,
        fontData,
        fgColor,
        bgColor,
        doubleWidth,
        doubleHeight,
      )
    }
  }

  // Text rendering

  const drawGlyph = (
    glyphIndex: number,
    row: number,
    col: number,
    fontData: Uint8Array,
    fgColor: number,
    bgColor: number,
    doubleWidth: boolean = false,
    doubleHeight: boolean = false,
  ) => {
    console.assert(
      0 <= glyphIndex && glyphIndex <= 96,
      `invalid glyph index ${glyphIndex}`,
    )

    const glyphX = (glyphIndex % glyphsPerFontRow) * glyphWidth
    const glyphY = Math.floor(glyphIndex / glyphsPerFontRow) * glyphHeight

    for (let y = 0; y < glyphHeight; y++) {
      for (let x = 0; x < glyphWidth; x++) {
        const pixelIndex =
          (glyphY + y) * (glyphsPerFontRow * glyphWidth) + (glyphX + x)
        const pixel = fontData[pixelIndex]
        const screenX = col * glyphWidth + x * (doubleWidth ? 2 : 1)
        const screenY = row * glyphHeight + y * (doubleHeight ? 2 : 1)
        const color = pixel ? fgColor : bgColor
        setPixel(screenX, screenY, color)
        if (doubleWidth) {
          setPixel(screenX + 1, screenY, color)
        }
        if (doubleHeight) {
          setPixel(screenX, screenY + 1, color)
        }
        if (doubleWidth && doubleHeight) {
          setPixel(screenX + 1, screenY + 1, color)
        }
      }
    }
  }

  const drcsFont = new Uint8Array(96 * glyphHeight * glyphWidth)
  const defineDrcs = (glyphIndex: number, rows: number[][]) => {
    if (glyphIndex === 99) return
    const glyphX = (glyphIndex % glyphsPerFontRow) * glyphWidth
    const glyphY = Math.floor(glyphIndex / glyphsPerFontRow) * glyphHeight
    for (let y = 0; y < glyphHeight; y++) {
      for (let x = 0; x < glyphWidth; x++) {
        const pixelIndex =
          (glyphY + y) * (glyphsPerFontRow * glyphWidth) + (glyphX + x)
        drcsFont[pixelIndex] = rows[y][x]
      }
    }
  }

  const setPixel = (x: number, y: number, color: number) => {
    if (x < 0 || x >= screenPixelWidth || y < 0 || y >= screenPixelHeight)
      return

    const index =
      (y + borderPixelHeight) * (screenPixelWidth + 2 * borderPixelWidth) +
      x +
      borderPixelWidth
    framebuffer[index] = color
  }

  const setScreenPixel = (x: number, y: number, color: number) => {
    const index = y * renderImageWidth + x
    framebuffer[index] = color
  }

  const setScreenColor = (color: number) => {
    for (let y = 0; y < renderImageHeight; y++) {
      for (let x = 0; x < renderImageWidth; x++) {
        if (
          y < borderPixelHeight ||
          y >= borderPixelHeight + screenPixelHeight ||
          x < borderPixelWidth ||
          x >= borderPixelWidth + screenPixelWidth
        ) {
          setScreenPixel(x, y, color)
        }
      }
    }
  }

  const setRowColor = (row: number, color: number) => {
    for (let y = 0; y < glyphHeight; y++) {
      for (let x = 0; x < renderImageWidth; x++) {
        if (x < borderPixelWidth || x >= borderPixelWidth + screenPixelWidth) {
          setScreenPixel(x, y + row * glyphHeight + borderPixelHeight, color)
        }
      }
    }
  }

  const loadFontData = async (url: string) => {
    console.log(`loading font ${url}`)
    const response = await fetch(url)
    const blob = await response.blob()
    const img = await createImageBitmap(blob)

    const fontLoadCanvas = document.createElement('canvas')
    const fontCtx = fontLoadCanvas.getContext('2d')
    if (!fontCtx) {
      throw new Error('Could not get 2d context')
    }
    fontLoadCanvas.width = img.width
    fontLoadCanvas.height = img.height
    fontCtx.drawImage(img, 0, 0)

    const imageData = fontCtx.getImageData(0, 0, img.width, img.height)
    const bitmap = new Uint8Array(img.width * img.height)

    for (let y = 0; y < img.height; y++) {
      for (let x = 0; x < img.width; x++) {
        const index = (y * img.width + x) * 4
        const bitIndex = y * img.width + x

        bitmap[bitIndex] = imageData.data[index] > 128 ? 1 : 0
      }
    }

    return bitmap
  }

  // Create an ImageData object for rendering
  const imageData = ctx.createImageData(renderImageWidth, renderImageHeight)
  const data = imageData.data

  // Convert a 12-bit color to a 24-bit RGB color
  const color12to24 = (color: number) => {
    const r = ((color >> 8) & 0xf) * 17 // Scale from 0-15 to 0-255
    const g = ((color >> 4) & 0xf) * 17
    const b = (color & 0xf) * 17
    return [r, g, b]
  }

  // Render the framebuffer to the canvas
  const render = () => {
    for (let i = 0; i < framebuffer.length; i++) {
      const color = framebuffer[i]
      const [r, g, b] = color12to24(color)

      // Set the pixel color in the ImageData buffer
      const index = i * 4
      data[index] = r
      data[index + 1] = g
      data[index + 2] = b
      data[index + 3] = 255 // Alpha channel (opaque)
    }

    // Draw the ImageData onto the canvas
    ctx.putImageData(imageData, 0, 0)
  }

  const resizeCanvas = () => {
    const originalWidth = renderImageWidth
    const originalHeight = renderImageHeight

    // Desired pixel aspect ratio
    const pixelAspectRatio = 0.6

    // Calculate the height for the display container (two-thirds of the window height)
    const displayHeight = window.innerHeight * (2 / 3)
    const displayWidth = window.innerWidth

    // Adjust dimensions to maintain the original display aspect ratio with pixel scaling
    let width, height
    if (
      displayWidth / displayHeight >
      (originalWidth * pixelAspectRatio) / originalHeight
    ) {
      // Display container is wider than the desired display aspect ratio
      height = displayHeight
      width = height * ((originalWidth * pixelAspectRatio) / originalHeight)
    } else {
      // Display container is taller than the desired display aspect ratio
      width = displayWidth
      height = width / ((originalWidth * pixelAspectRatio) / originalHeight)
    }

    // Apply the new size to the canvas
    canvas.style.width = `${width}px`
    canvas.style.height = `${height}px`
  }

  // Handle window resizing
  window.addEventListener('resize', resizeCanvas)

  // Initialize the canvas size
  resizeCanvas()

  const fontG0 = await loadFontData('font-g0.png')

  const fonts = [
    fontG0,
    await loadFontData('font-g1.png'),
    await loadFontData('font-g2.png'),
    await loadFontData('font-g3.png'),
    new Uint8Array(fontG0.length).fill(0), // defaults for DRCS (?)
    drcsFont,
  ]
  const fontDiacritical = await loadFontData('font-diacritical.png')

  return {
    fonts,
    fontDiacritical,
    setScreenColor,
    setRowColor,
    drawString,
    drawGlyph,
    defineDrcs,
    render,
  }
}
