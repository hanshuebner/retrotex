import initDisplay from './display.mjs'
import initWebsocket from './websocket.mjs'
import ceptDecoder from './ceptDecoder.mjs'
import ceptInterpreter from './ceptInterpreter.mjs'

document.addEventListener('DOMContentLoaded',
    async () => {
        const display = await initDisplay(document.getElementById('emulator'))
        display.drawString('Display initialized', 0, 0, display.fontG0, 0xfff, 0x000)
        const interpreter = ceptInterpreter(console.log, display)
        const {next, putback} = initWebsocket()
        while (true) {
            await ceptDecoder(
                interpreter,
                next,
                putback,
                e => console.log('Error', e));
        }
    })
