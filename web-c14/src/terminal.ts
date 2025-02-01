import initDisplay from './display'
import initWebsocket from './websocket'
import initKeyboard from './keyboard'
import ceptDecoder from './ceptDecoder'
import ceptInterpreter from './ceptInterpreter'

document.addEventListener('DOMContentLoaded', async () => {
  const canvas = document.getElementById('emulator') as HTMLCanvasElement
  const display = await initDisplay(canvas)
  display.drawString(
    'Display initialized',
    0,
    0,
    display.fonts[0],
    0xfff,
    0x000,
  )
  let atStep = 0
  let debug = !!location.search.match(/debug/)

  const log = (message: string, ...args: any[]) => {
    if (debug) {
      console.log.apply(console, [
        `${atStep.toString(10).padStart(4, ' ')} [${interpreter.attributeMode()}] ${message}`,
        ...args,
      ])
    }
  }

  const interpreter = ceptInterpreter(log, display)
  const websocket = initWebsocket()
  const keyPressed = (ceptCodes: number[]) => {
    if (ceptCodes[0] === 0xff && ceptCodes[1] === 0xff) {
      switch (ceptCodes[2]) {
        case 0x01:
          debuggerStatus(true)
          break
        default:
          console.error(`unknown private cept code ${ceptCodes[2]}`)
      }
    } else {
      websocket.send(new Uint8Array(ceptCodes))
    }
  }
  initKeyboard(keyPressed)

  // Fixme DEMO
  canvas.addEventListener('keypress', (e) => {
    console.log('canvas key press', e.key)
    if (e.key === '*') {
      keyPressed([0x13])
    } else if (e.key === '#' || e.key === 'Enter') {
      keyPressed([0x1c])
    } else if (e.key.match('^[a-zA-Z0-9 ]$')) {
      keyPressed([e.key.charCodeAt(0)])
    }
  })
  canvas.tabIndex = 1
  canvas.focus()

  // debugger functionality
  let runTo: number | undefined = undefined

  const debuggerStatus = (enabled: boolean) => {
    document.getElementById('keyboard-container')!.style.display = enabled
      ? 'none'
      : 'flex'
    document.getElementById('cept-debugger')!.style.display = enabled
      ? 'block'
      : 'none'
  }

  // fixme demo
  // debuggerStatus(false)

  const updateRunTo = () => {
    if (runTo) {
      ;(document.getElementById('run-to') as HTMLInputElement).value =
        runTo.toString()
    }
  }

  if (document.getElementById('cept-debugger')) {
    document.location.search.replace(/\WrunTo=(\d+)/, (match, runToString) => {
      runTo = parseInt(runToString)
      debuggerStatus(true)
      updateRunTo()
      return match
    })

    document
      .getElementById('run-to')!
      ?.addEventListener('blur', (event: Event) => {
        runTo = parseInt((event.target as HTMLInputElement).value)
        if (runTo < atStep) {
          document.location.search = `?runTo=${runTo}`
        }
      })

    if (runTo) {
      document
        .getElementById('next-step')!
        .addEventListener('click', (event: Event) => {
          if (runTo) {
            runTo += 1
            updateRunTo()
          }
        })
    } else {
      document.getElementById('next-step')!.style.display = 'none'
    }

    document
      .getElementById('previous-page')!
      .addEventListener('click', () =>
        websocket.send(new Uint8Array(1).fill(8)),
      )
    document
      .getElementById('next-page')!
      .addEventListener('click', () =>
        websocket.send(new Uint8Array(1).fill(32)),
      )
    document
      .getElementById('close-debugger')!
      .addEventListener('click', () => debuggerStatus(false))
    document
      .getElementById('tia')!
      .addEventListener('change', (e) =>
        interpreter.setTia((e.target as HTMLInputElement).checked),
      )
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
