import {CeptInterpreter} from "./ceptInterpreter";

const decode = async (interpreter: CeptInterpreter, next: () => Promise<number>, putback: (code: number) => void, error: (...args: any) => void) => {
  const esc = async () => {
    const c = await next()
    switch (c) {
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
                interpreter.setFgColorOfRow(color & 0x0f)
                break
              case 0x50:
                interpreter.setBgColorOfRow(color & 0x0f)
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
      case 0x28:
      case 0x29:
      case 0x2a:
      case 0x2b:
        switch (await next()) {
          case 0x40:
            interpreter.loadCharset(c - 0x28, 0)
            break
          case 0x62:
            interpreter.loadCharset(c - 0x28, 2)
            break
          case 0x63:
            interpreter.loadCharset(c - 0x28, 1)
            break
          case 0x64:
            interpreter.loadCharset(c - 0x28, 3)
            break
          case 0x20:
            if (c !== 0x28 && (await next()) === 0x40) {
              interpreter.loadCharset(c - 0x28, 4)
            } else {
              error()
            }
            break
          default:
            error()
        }
        break
      case 0x6e:
        interpreter.intoLeftCharset(2)
        break
      case 0x6f:
        interpreter.intoLeftCharset(3)
        break
      case 0x7c:
        interpreter.intoRightCharset(3)
        break
      case 0x7d:
        interpreter.intoRightCharset(2)
        break
      case 0x7e:
        interpreter.intoRightCharset(1)
        break
      default:
        error()
    }
  }

  const handleCharacterSet = async () => {
    const startCharCode = await next()
    if (startCharCode === 0x20) {
      let resolutionCode = await next()
      let colorDepthCode = await next()
      if (resolutionCode === 0x28 && colorDepthCode === 0x20) {
        if ((await next()) !== 0x40) {
          error()
          return
        }
        resolutionCode = await next()
        colorDepthCode = await next()
        interpreter.clearDrcsSet(startCharCode, resolutionCode & 0x0f, colorDepthCode & 0x0f)
      } else {
        interpreter.startDrcsSet(startCharCode, resolutionCode, colorDepthCode)
      }
    } else if (startCharCode > 0x20 && startCharCode < 0x7f) {
      let drcsBlock: number[] = []
      const drcsBlocks = [drcsBlock]
      while (true) {
        const c = await next()
        if (0x30 <= c && c <= 0x3f) {
          if (drcsBlock.length) {
            drcsBlock = []
            drcsBlocks.push(drcsBlock)
          }
          drcsBlock.push(c)
        } else if ((0x20 <= c && c <= 0x2f) || (0x40 <= c && c <= 0x07f)) {
          drcsBlock.push(c)
        } else {
          putback(c)
          break
        }
      }
      interpreter.drcsDefinitionBlocks(drcsBlocks)
    } else {
      error()
    }
  }

  const readDefaulting = async (defaults: number[]) => {
    const result = [...defaults]
    for (let i = 0; i < defaults.length; i++) {
      const c = await next()
      if (c === 0x1f) {
        putback(c)
        break
      }
      result[i] = c
    }
    return result
  }

  const readColorDefinitionBytes = async () => {
    const result = []
    while (true) {
      const c = await next()
      if (0x40 <= c && c <= 0x7f) {
        result.push(c)
      } else {
        putback(c)
        return result
      }
    }
  }

  type ColorTableDefinitionHeader = {
    tableType: number
    tableNumber: number
    bitsPerUnit: number
    rgb: boolean
  }

  const decodeColorDefinitionHeader = (header: number[]): ColorTableDefinitionHeader => {
    const [tableType, tableNumber, unitResolution, codingMethod] = header
    return {
      tableType: tableType & 0x0f,
      tableNumber: tableNumber & 0x0f,
      bitsPerUnit: unitResolution & 0x0f,
      rgb: !!(codingMethod & 1)
    }
  }

  const handleColors = async () => {
    const c = await next()
    if (c === 0x21) {
      interpreter.resetColorDefinitions()
    } else if (c === 0x20) {
      const headerDefaults = [0x20, 0x20, 0x34, 0x41, 0x4e]
      const colorDefinitionHeader = decodeColorDefinitionHeader(await readDefaulting(headerDefaults))
      interpreter.setColorDefinitionHeader(colorDefinitionHeader)
    } else if ((c & 0xf0) === 0x30) {
      let colorNumber = c & 0x0f
      const low = await next()
      if ((low & 0xf0) === 0x30) {
        colorNumber = colorNumber * 10 + (low & 0x0f)
      } else {
        putback(low)
      }
      const colorBytes = await readColorDefinitionBytes()
      const [b1, b2] = colorBytes
      if ((b1 & 0xc0) !== 0x40) {
        error(`invalid first color byte ${b1.toString(2)}`)
      }
      if ((b2 & 0xc0) !== 0x40) {
        error(`invalid second color byte ${b2.toString(2)}`)
      }
      const r = ((b1 & 0b00100000) >> 2) + ((b1 & 0b00000100) << 0) + ((b2 & 0b00100000) >> 4) + ((b2 & 0b00000100) >> 2)
      const g = ((b1 & 0b00010000) >> 1) + ((b1 & 0b00000010) << 1) + ((b2 & 0b00010000) >> 3) + ((b2 & 0b00000010) >> 1)
      const b = ((b1 & 0b00001000) >> 0) + ((b1 & 0b00000001) << 2) + ((b2 & 0b00001000) >> 2) + ((b2 & 0b00000001) >> 0)
      interpreter.defineColor(colorNumber, r, g, b)
    } else {
      error()
    }
  }

  const handleShortcuts = async () => {
    const c = await next()
    if (c === 0x30) {
      interpreter.resetShortcuts()
    } else {
      const buf = []
      while (true) {
        const c = await next()
        if (c === 0x1f) {
          break
        }
        if (buf.length === 15) {
          error('definition not terminated')
          return
        }
        switch (c) {
          case 0x2a:
            buf.push(0x13)
            break
          case 0x23:
            buf.push(0x1c)
            break
          default:
            buf.push(c)
        }
      }
      interpreter.setShortcut(c, buf)
    }
  }

  const readFormatVpde = async () => {
    let rows
    let columns
    let accumulator = 0
    let wraparound = true
    let more = true
    while (more) {
      const c = await next()
      switch (c & 0xf0) {
        case 0x40:
          switch (c) {
            case 0x41: columns = 40; rows = 24; break
            case 0x42: columns = 40; rows = 20; break
            case 0x43: columns = 80; rows = 24; break
            case 0x44: columns = 80; rows = 20; break
            case 0x45: columns = 48; rows = 20; break
            case 0x46: columns = 40; rows = 25; break
            default:
              error('unknown screen format code in vpde')
              return
          }
          break
        case 0x30:
          if (c === 0x3B) {
            if (rows === undefined) {
              rows = accumulator
            } else {
              columns = accumulator
            }
            accumulator = 0
          } else {
            accumulator = accumulator * 10 + (c & 0x0f)
          }
          break
        case 0x70:
          wraparound = c === 0x70
          break
        default:
          putback(c)
          more = false
      }
    }
    interpreter.setScreenFormat(rows || 40, columns || 24, wraparound)
  }

  const us = async () => {
    const c = await next()
    switch (c) {
      case 0x23:
        await handleCharacterSet()
        break
      case 0x26:
        await handleColors()
        break
      case 0x2d:
        await readFormatVpde()
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
            interpreter.reset(false, false)
            break
          case 0x42:
            interpreter.reset(true, false)
            break
          case 0x43:
            interpreter.reset(false, true)
            break
          case 0x44:
            interpreter.reset(true, true)
            break
          default:
            error()
        }
        break
      case 0x3d:
        await handleShortcuts()
        break
      default:
        if (c > 0x40 && c < 0x40 + 26) {
          const col = await next()
          if (col > 0x40 && col < 0x40 + 41) {
            interpreter.setCursor(c - 0x41, col - 0x41)
          } else {
            error('col out of range in set cursor position command')
          }
        } else {
          error('US parameter unknown')
        }
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
    } else if ((first & 0xf0) === 0x20 && (second & 0xfe) === 0x72) {
      interpreter.sendShortcut(second & 0x0f, !(second & 1))
    } else if (first === 0x32 && second === 0x3b) {
      // end of RAFI C14 data dump
    } else {
      error()
    }
  }

  const c = await next()
  switch (c) {
    case 0x08:
      interpreter.cursorBack()
      break
    case 0x09:
      interpreter.cursorForward()
      break
    case 0x0a:
      interpreter.cursorDown()
      break
    case 0x0b:
      interpreter.cursorUp()
      break
    case 0x0c:
      interpreter.clearScreen(true)
      break
    case 0x0d:
      interpreter.cursorToBeginningOfLine()
      break
    case 0x0e:
      interpreter.intoLeftCharset(1)
      break
    case 0x0f:
      interpreter.intoLeftCharset(0)
      break
    case 0x11:
      interpreter.hideCursor(false)
      break
    case 0x12:
      interpreter.repeatLastPrintedCharacter((await next()) - 0x40)
      break
    case 0x14:
      interpreter.hideCursor(true)
      break
    case 0x18:
      interpreter.clearToEndOfLine()
      break
    case 0x19:
      interpreter.switchCharsetForOneCharacter(2)
      break
    case 0x1a:
      interpreter.endOfPage()
      break
    case 0x1d:
      interpreter.switchCharsetForOneCharacter(3)
      break
    case 0x1e:
      interpreter.cursorHome()
      break
    case 0x1b:
      await esc()
      break
    case 0x1f:
      await us()
      break
    case 0x9b:
      await csi()
      break
    case 0x88:
      interpreter.blink(true)
      break
    case 0x89:
      interpreter.blink(false)
      break
    case 0x8a:
      interpreter.transparency(true)
      break
    case 0x8b:
      interpreter.transparency(false)
      break
    case 0x8c:
      interpreter.doubleSize(false, false)
      break
    case 0x8d:
      interpreter.doubleSize(false, true)
      break
    case 0x8e:
      interpreter.doubleSize(true, false)
      break
    case 0x8f:
      interpreter.doubleSize(true, true)
      break
    case 0x98:
      interpreter.hide()
      break
    case 0x99:
      interpreter.underline(false)
      break
    case 0x9a:
      interpreter.underline(true)
      break
    case 0x9c:
      interpreter.polarity(false) // todo: serial mode
      break
    case 0x9d:
      interpreter.polarity(true) // todo: serial mode
      break
    case 0x9e:
      interpreter.mosaicOrTransparent()
      break
    default:
      if (0x80 <= c && c <= 0x87) {
        interpreter.setFgColor(c - 0x80)
      } else if (0x90 <= c && c <= 0x97) {
        interpreter.setBgColor(c - 0x90)
      } else if ((0x20 <= c && c <= 0x7f) || c >= 0xa0) {
        interpreter.putChar(c)
      } else {
        console.log('ignored', c)
      }
  }
}

const isPrintable = (c: number) => (0x20 <= c && c <= 0x7f) || c >= 0xa0

const decodeRes = (c: number) => {
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

const decodeCol = (c: number) => {
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

export default decode
