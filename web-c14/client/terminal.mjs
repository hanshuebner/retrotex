import initDisplay from './display.mjs'
import ceptInterpreter from './ceptInterpreter.mjs'

document.addEventListener('DOMContentLoaded',
    async () => {
        const display = await initDisplay(document.getElementById('emulator'))
        display.drawString('Display initialized', 0, 0, display.fontG0, 0xfff, 0x00f)
        const interpreter = ceptInterpreter(console.log, display)
        interpreter.clearScreen()
        interpreter.putChar('h')
    })
