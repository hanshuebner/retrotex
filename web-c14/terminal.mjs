import initDisplay from './display.mjs'
import initWebsocket from './websocket.mjs'
import initKeyboard from './keyboard.mjs'
import ceptDecoder from './ceptDecoder.mjs'
import ceptInterpreter from './ceptInterpreter.mjs'

document.addEventListener('DOMContentLoaded',
    async () => {
        const display = await initDisplay(document.getElementById('emulator'))
        display.drawString('Display initialized', 0, 0, display.fonts[0], 0xfff, 0x000)
        const interpreter = ceptInterpreter(console.log, display)
        const websocket = initWebsocket()
        const keyPressed = (keyCode) => {
            websocket.send(new Uint8Array([keyCode]))
        }
        const keyboard = initKeyboard(keyPressed)
        while (true) {
            await ceptDecoder(
                interpreter,
                websocket.next,
                websocket.putback,
                e => console.log('Error', e));
        }
    })
