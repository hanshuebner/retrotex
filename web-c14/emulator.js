const canvas = document.getElementById("emulator");
const ctx = canvas.getContext("2d");

const screen_width = 40 * 12
const screen_height = 25 * 10

const framebuffer = new Uint16Array(screen_width * screen_height);

// Create an ImageData object for rendering
const imageData = ctx.createImageData(screen_width, screen_height);
const data = imageData.data;

// Convert a 12-bit color to a 24-bit RGB color
function color12to24(color) {
    const r = ((color >> 8) & 0xF) * 17; // Scale from 0-15 to 0-255
    const g = ((color >> 4) & 0xF) * 17;
    const b = (color & 0xF) * 17;
    return [r, g, b];
}

// Render the framebuffer to the canvas
function render() {
    for (let i = 0; i < framebuffer.length; i++) {
        const color = framebuffer[i];
        const [r, g, b] = color12to24(color);

        // Set the pixel color in the ImageData buffer
        const index = i * 4;
        data[index] = r;
        data[index + 1] = g;
        data[index + 2] = b;
        data[index + 3] = 255; // Alpha channel (opaque)
    }

    // Draw the ImageData onto the canvas
    ctx.putImageData(imageData, 0, 0);
}

// Resize the canvas to fit the window while maintaining aspect ratio
function resizeCanvas() {
    const originalWidth = screen_width;
    const originalHeight = screen_height;

    // Desired pixel aspect ratio (2:1 for twice as wide as tall)
    const pixelAspectRatio = 0.7; // Horizontal stretch factor

    // Calculate new canvas dimensions based on window size
    let width = window.innerWidth;
    let height = window.innerHeight;

    // Adjust dimensions to maintain the original display aspect ratio with pixel scaling
    if ((width / height) > (originalWidth * pixelAspectRatio / originalHeight)) {
        // Window is wider than the desired display aspect ratio
        height = window.innerHeight;
        width = height * (originalWidth * pixelAspectRatio / originalHeight);
    } else {
        // Window is taller than the desired display aspect ratio
        width = window.innerWidth;
        height = width / (originalWidth * pixelAspectRatio / originalHeight);
    }

    // Apply the new size to the canvas
    canvas.style.width = `${width}px`;
    canvas.style.height = `${height}px`;
}

// Handle window resizing
window.addEventListener('resize', resizeCanvas);

// Initialize the canvas size
resizeCanvas();

// Handle window resizing
window.addEventListener('resize', resizeCanvas);

// Initialize the canvas size
resizeCanvas();

// Example: Fill the framebuffer with random colors
function fillFramebufferWithRandomColors() {
    for (let i = 0; i < framebuffer.length; i++) {
        framebuffer[i] = Math.floor(Math.random() * 4096);
    }
    render();
}

// Start emulation
fillFramebufferWithRandomColors();
