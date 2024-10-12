const decodeCept = async (interpreter, next, error) => {
  const esc = async () => {
    switch (await next()) {
      case 0x22:
        switch (await next()) {
          case 0x40:
            interpreter.serialMode()
            break
          case 0x41:
            interpreter.parallelMode()
            break
          default:
            error()
        }
        break
      case 0x28:
        switch (await next()) {
          case 0x40:
            interpreter.loadG0IntoG0()
            break
          case 0x62:
            interpreter.loadG2IntoG0()
            break
          case 0x63:
            interpreter.loadG1IntoG0()
            break
          case 0x64:
            interpreter.loadG3IntoG0()
            break
          default:
            error()
        }
        break
      case 0x29:
        switch (await next()) {
          case 0x40:
            interpreter.loadG0IntoG1()
            break
          case 0x62:
            interpreter.loadG2IntoG1()
            break
          case 0x63:
            interpreter.loadG1IntoG1()
            break
          case 0x64:
            interpreter.loadG3IntoG1()
            break
          case 0x20:
            if ((await next()) === 0x40) {
              interpreter.loadDRCsIntoG1()
            } else {
              error()
            }
            break
          default:
            error()
        }
        break
      case 0x2a:
        switch (await this.next()) {
          case 0x40:
            interpreter.loadG0IntoG2()
            break
          case 0x62:
            interpreter.loadG2IntoG2()
            break
          case 0x63:
            interpreter.loadG1IntoG2()
            break
          case 0x64:
            interpreter.loadG3IntoG2()
            break
          case 0x20:
            if ((await this.next()) === 0x40) {
              interpreter.loadDRCsIntoG2()
            } else {
              error()
            }
            break
          default:
            error()
        }
        break
      case 0x2b:
        switch (await next()) {
          case 0x40:
            interpreter.loadG0IntoG3()
            break
          case 0x62:
            interpreter.loadG2IntoG3()
            break
          case 0x63:
            interpreter.loadG1IntoG3()
            break
          case 0x64:
            interpreter.loadG3IntoG3()
            break
          case 0x20:
            if ((await next()) === 0x40) {
              interpreter.loadDRCsIntoG3()
            } else {
              error()
            }
            break
          default:
            error()
        }
        break
      case 0x23:
        switch (await next()) {
          case 0x20: {
            const color = await next()
            switch (color & 0xf0) {
              case 0x40:
                interpreter.setFgColorOfScreen(color & 0x0f)
                break
              case 0x50:
                interpreter.setBgColorOfScreen(color & 0x0f)
                break
              default:
                error()
            }
            break
          }
          case 0x21: {
            const color = await next()
            switch (color & 0xf0) {
              case 0x40:
                interpreter.setFgColorOfLine(color & 0x0f)
                break
              case 0x50:
                interpreter.setBgColorOfLine(color & 0x0f)
                break
              default:
                error()
            }
            break
          }
          default:
            error()
        }
        break
      case 0x6e:
        interpreter.g2IntoLeftCharset()
        break
      case 0x6f:
        interpreter.g3IntoLeftCharset()
        break
      case 0x7c:
        interpreter.g3IntoRightCharset()
        break
      case 0x7d:
        interpreter.g2IntoRightCharset()
        break
      case 0x7e:
        interpreter.g1IntoRightCharset()
      default:
        error()
    }
  }

  const us = async () => {
    switch (await next()) {
      case 0x23:
        await handleCharacterSet()
        break
      case 0x26:
        await handleColors()
        break
      case 0x2d:
        interpreter.setResolutionTo40x24()
        break
      case 0x2f:
        switch (await next()) {
          case 0x40:
            interpreter.serviceBreakToRow((await next()) - 0x40)
            break
          case 0x4f:
            interpreter.serviceBreakBack()
            break
          case 0x41:
            interpreter.serialMode()
            break
          case 0x42:
            interpreter.parallelMode()
            break
          case 0x43:
            interpreter.serialLimitedMode()
            break
          case 0x44:
            interpreter.parallelLimitedMode()
            break
          default:
            error()
        }
        break
      case 0x3d:
        await handleShortcuts()
        break
      default:
        await handleSetCursor()
    }
  }

  const csi = async () => {
    const first = await next()
    const second = await next()

    if (first === 0x30 && second === 0x40) {
      interpreter.selectPalette(0)
    } else if (first === 0x30 && second === 0x41) {
      interpreter.invertBlinking()
    } else if (first === 0x31 && second === 0x40) {
      interpreter.selectPalette(1)
    } else if (first === 0x31 && second === 0x41) {
      interpreter.blinkPalettes()
    } else if (first === 0x31 && second === 0x51) {
      interpreter.unprotectLine()
    } else if (first === 0x31 && second === 0x50) {
      interpreter.protectLine()
    } else if (first === 0x32 && second === 0x40) {
      interpreter.selectPalette(2)
    } else if (first === 0x32 && second === 0x41) {
      interpreter.fastBlinking(true, false, false)
    } else if (first === 0x32 && second === 0x53) {
      interpreter.startSelection()
    } else if (first === 0x32 && second === 0x54) {
      interpreter.endSelection()
    } else if (first === 0x33 && second === 0x40) {
      interpreter.selectPalette(3)
    } else if (first === 0x33 && second === 0x41) {
      interpreter.fastBlinking(false, true, false)
    } else if (first === 0x34 && second === 0x41) {
      interpreter.fastBlinking(false, false, true)
    } else if (first === 0x35 && second === 0x41) {
      interpreter.blinkingShiftRight()
    } else if (first === 0x36 && second === 0x41) {
      interpreter.blinkingShiftLeft()
    } else {
      error()
    }
  }

  const c = await next()
  switch (c) {
    case 0x08:
      interpreter.cursorLeft()
      break
    case 0x09:
      interpreter.cursorRight()
      break
    case 0x0a:
      interpreter.cursorDown()
      break
    case 0x0b:
      interpreter.cursorUp()
      break
    case 0x0c:
      interpreter.clearScreen()
      break
    case 0x0d:
      interpreter.cursorToBeginningOfLine()
      break
    case 0x0e:
      interpreter.g1IntoLeftCharset()
      break
    case 0x0f:
      interpreter.g0IntoLeftCharset()
      break
    case 0x11:
      interpreter.showCursor()
      break
    case 0x12:
      interpreter.repeatLastPrintedCharacter((await next()) - 0x40)
      break
    case 0x14:
      interpreter.hideCursor()
      break
    case 0x18:
      interpreter.clearLine()
      break
    case 0x19:
      interpreter.switchToG2ForOneCharacter()
      break
    case 0x1a:
      interpreter.endOfPage()
      break
    case 0x1d:
      interpreter.switchToG3ForOneCharacter()
      break
    case 0x1e:
      interpreter.cursorHome()
      break
    case 0x1b:
      interpreter.esc()
      await esc()
      break
    case 0x1f:
      interpreter.esc()
      await us()
      break
    case 0x9b:
      interpreter.csi()
      await csi()
      break
    case 0x88:
      interpreter.blinkOn()
      break
    case 0x89:
      interpreter.blinkOff()
      break
    case 0x8a:
      interpreter.transparencyOn()
      break
    case 0x8b:
      interpreter.transparencyOff()
      break
    case 0x8c:
      interpreter.normalSize()
      break
    case 0x8d:
      interpreter.doubleHeight()
      break
    case 0x8e:
      interpreter.doubleWidth()
      break
    case 0x8f:
      interpreter.doubleWidthAndHeight()
      break
    case 0x98:
      interpreter.hide()
      break
    case 0x99:
      interpreter.underlineOff()
      break
    case 0x9a:
      interpreter.underlineOn()
    case 0x9c:
      interpreter.normalPolarity()
      break
    case 0x9d:
      interpreter.inversePolarity()
      break
    case 0x9e:
      interpreter.mosaicOrTransparent()
      break
    default:
      if (0x80 <= c && c <= 0x87) {
        interpreter.setFgColor(c - 0x80)
      } else if (0x90 <= c && c <= 0x97) {
        interpreter.setBgColor(c - 0x90)
      } else {
        error()
      }
  }
}

const isPrintable = c => (0x20 <= c && c <= 0x7f) || c >= 0xa0

const decodeRes = c => {
  switch (c - 0x40) {
    case 6:
      return '12x12'
    case 7:
      return '12x10'
    case 0xa:
      return '6x12'
    case 0xb:
      return '6x10'
    case 0xc:
      return '6x5'
    case 0xf:
      return '6x6'
    default:
      return '???'
  }
}

const decodeCol = c => {
  switch (c - 0x40) {
    case 1:
      return 2
    case 2:
      return 4
    case 4:
      return 16
    default:
      return -1
  }
}

const printPalette = (s, p, c) => {
  let first = true
  let q = p
  for (let i = 0; i < c / 2; i++) {
    if (!first) s.push(', ')
    first = false
    const r3 = (q[0] >> 5) & 1
    const g3 = (q[0] >> 4) & 1
    const b3 = (q[0] >> 3) & 1
    const r2 = (q[0] >> 2) & 1
    const g2 = (q[0] >> 1) & 1
    const b2 = (q[0] >> 0) & 1
    const r1 = (q[1] >> 5) & 1
    const g1 = (q[1] >> 4) & 1
    const b1 = (q[1] >> 3) & 1
    const r0 = (q[1] >> 2) & 1
    const g0 = (q[1] >> 1) & 1
    const b0 = (q[1] >> 0) & 1
    const r = r0 | (r1 << 1) | (r2 << 2) | (r3 << 3)
    const g = g0 | (g1 << 1) | (g2 << 2) | (g3 << 3)
    const b = b0 | (b1 << 1) | (b2 << 2) | (b3 << 3)
    s.push(`"#${r.toString(16)}${g.toString(16)}${b.toString(16)}"`)
    q += 2
  }
}
