import initDisplay from './display'
import initWebsocket from './websocket'
import initKeyboard from './keyboard'
import ceptDecoder from './ceptDecoder'
import ceptInterpreter from './ceptInterpreter'

document.addEventListener('DOMContentLoaded', async () => {
  const display = await initDisplay(
    document.getElementById('emulator') as HTMLCanvasElement,
  )
  display.drawString(
    'Display initialized',
    0,
    0,
    display.fonts[0],
    0xfff,
    0x000,
  )
  let atStep = 0

  const interpreter = ceptInterpreter(
    (message: string, ...args: any[]) =>
      console.log.apply(console, [
        `${atStep.toString(10).padStart(4, ' ')} [${interpreter.attributeMode()}] ${message}`,
        ...args,
      ]),
    display,
  )
  const websocket = initWebsocket()
  const keyPressed = (keyCode: number) => {
    websocket.send(new Uint8Array([keyCode]))
  }
  initKeyboard(keyPressed)

  let runTo: number | undefined = undefined

  const updateRunTo = () => {
    if (runTo) {
      ;(document.getElementById('run-to') as HTMLInputElement).value =
        runTo.toString()
    }
  }

  document.location.search.replace(/\WrunTo=(\d+)/, (match, runToString) => {
    runTo = parseInt(runToString)
    updateRunTo()
    return match
  })

  document
    .getElementById('run-to')!
    .addEventListener('blur', (event: Event) => {
      runTo = parseInt((event.target as HTMLInputElement).value)
      if (runTo < atStep) {
        document.location.search = `?runTo=${runTo}`
      }
    })

  if (runTo) {
    document
      .getElementById('next')!
      .addEventListener('click', (event: Event) => {
        if (runTo) {
          runTo += 1
          updateRunTo()
        }
      })
  }
  while (true) {
    await ceptDecoder(
      interpreter,
      async () => {
        while (runTo && atStep >= runTo) {
          await new Promise((fulfill) => setTimeout(fulfill, 100))
        }
        return websocket.next()
      },
      websocket.putback,
      (e: any) => console.log('Error', e),
    )
    atStep += 1
    interpreter.updateDisplay()
  }
})
