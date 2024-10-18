import initDisplay from './display'
import initWebsocket from './websocket'
import initKeyboard from './keyboard'
import ceptDecoder from './ceptDecoder'
import ceptInterpreter from './ceptInterpreter'

document.addEventListener('DOMContentLoaded',
    async () => {
        const display = await initDisplay(document.getElementById('emulator') as HTMLCanvasElement)
        display.drawString('Display initialized', 0, 0, display.fonts[0], 0xfff, 0x000)
        const interpreter = ceptInterpreter(console.log, display)
        const websocket = initWebsocket()
        const keyPressed = (keyCode: number) => {
            websocket.send(new Uint8Array([keyCode]))
        }
        initKeyboard(keyPressed)
        while (true) {
            await ceptDecoder(
                interpreter,
                websocket.next,
                websocket.putback,
                (e: any) => console.log('Error', e));
        }
    })
