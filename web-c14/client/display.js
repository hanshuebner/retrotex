const canvas = document.getElementById('emulator')
const ctx = canvas.getContext('2d')

const screen_width = 40 * 12
const screen_height = 25 * 10

const framebuffer = new Uint16Array(screen_width * screen_height)

// Text rendering

const drawString = (str, row, col, fontData, fgColor, bgColor, doubleWidth = false, doubleHeight = false) => {
  const glyphWidth = 12
  const glyphHeight = 10
  const glyphsPerFontRow = 16 // 96 glyphs in a 16x6 grid

  for (let i = 0; i < str.length; i++) {
    const charCode = str.charCodeAt(i)
    if (charCode < 0x20 || charCode > 0x7f) continue // Skip non-ASCII characters

    const glyphIndex = charCode - 0x20
    const glyphX = (glyphIndex % glyphsPerFontRow) * glyphWidth
    const glyphY = Math.floor(glyphIndex / glyphsPerFontRow) * glyphHeight

    for (let y = 0; y < glyphHeight; y++) {
      for (let x = 0; x < glyphWidth; x++) {
        const pixelIndex = (glyphY + y) * (glyphsPerFontRow * glyphWidth) + (glyphX + x)
        const pixel = fontData[pixelIndex]

        const screenX = col * glyphWidth + (i * glyphWidth + x) * (doubleWidth ? 2 : 1)
        const screenY = row * glyphHeight + y * (doubleHeight ? 2 : 1)

        setPixel(screenX, screenY, pixel ? fgColor : bgColor, doubleWidth, doubleHeight)
      }
    }
  }
}

const setPixel = (x, y, color, doubleWidth, doubleHeight) => {
  if (x < 0 || x >= screen_width || y < 0 || y >= screen_height) return

  const [r, g, b] = color12to24(color)
  const index = y * screen_width + x
  framebuffer[index] = color

  if (doubleWidth) {
    setPixel(x + 1, y, color, false, doubleHeight)
  }
  if (doubleHeight) {
    setPixel(x, y + 1, color, doubleWidth, false)
  }
}

const loadFontData = async url => {
  const response = await fetch(url)
  const blob = await response.blob()
  const img = await createImageBitmap(blob)

  const canvas = document.createElement('canvas')
  canvas.width = img.width
  canvas.height = img.height
  const ctx = canvas.getContext('2d')
  ctx.drawImage(img, 0, 0)

  const imageData = ctx.getImageData(0, 0, img.width, img.height)
  const bitmap = new Uint8Array(img.width * img.height)

  for (let y = 0; y < img.height; y++) {
    for (let x = 0; x < img.width; x++) {
      const index = (y * img.width + x) * 4
      const bitIndex = y * img.width + x

      bitmap[bitIndex] = imageData.data[index] > 128
    }
  }

  return bitmap
}

// Create an ImageData object for rendering
const imageData = ctx.createImageData(screen_width, screen_height)
const data = imageData.data

// Convert a 12-bit color to a 24-bit RGB color
function color12to24(color) {
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
  const originalWidth = screen_width;
  const originalHeight = screen_height;

  // Desired pixel aspect ratio
  const pixelAspectRatio = 0.6;

  // Calculate the height for the display container (two-thirds of the window height)
  const displayHeight = window.innerHeight * (2 / 3);
  const displayWidth = window.innerWidth;

  // Adjust dimensions to maintain the original display aspect ratio with pixel scaling
  let width, height;
  if (displayWidth / displayHeight > (originalWidth * pixelAspectRatio) / originalHeight) {
    // Display container is wider than the desired display aspect ratio
    height = displayHeight;
    width = height * ((originalWidth * pixelAspectRatio) / originalHeight);
  } else {
    // Display container is taller than the desired display aspect ratio
    width = displayWidth;
    height = width / ((originalWidth * pixelAspectRatio) / originalHeight);
  }

  // Apply the new size to the canvas
  canvas.style.width = `${width}px`;
  canvas.style.height = `${height}px`;
};

// Handle window resizing
window.addEventListener('resize', resizeCanvas);

// Initialize the canvas size
resizeCanvas();

const fillFramebufferWithRandomColors = () => {
  for (let i = 0; i < framebuffer.length; i++) {
    framebuffer[i] = Math.floor(Math.random() * 4096)
  }
}

loadFontData('font-g0.png').then(fontData => {
  drawString('Hello world!', 0, 0, fontData, 0, 0x00f, true, true)
  drawString('0123456789012345678901234567890123456789', 2, 0, fontData, 0xf00, 0)
  drawString('Dank an: Computermuseum Hamburg, RAFI,', 8, 0, fontData, 0xfff, 0x00f)
})

setInterval(render, 100)
