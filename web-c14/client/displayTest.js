import * as display from './display.mjs'

document.addEventListener('DOMContentLoaded',
    async () => {
        const {drawString, fontG0} = await display.init(document.getElementById('emulator'))
        drawString('Hello world!', 0, 0, fontG0, 0, 0x00f, true, true)
        drawString('0123456789012345678901234567890123456789', 2, 0, fontG0, 0xf00, 0)
        drawString('Dank an: Computermuseum Hamburg, RAFI,', 8, 0, fontG0, 0xfff, 0x00f)
    })
