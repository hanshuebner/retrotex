import { Display } from './display'
import { renderDebugDisplay } from './ceptInterpreterDebug'

export type CeptInterpreter = {
  blink: (enabled: boolean) => void
  blinkPalettes: () => void
  blinkingShiftLeft: () => void
  blinkingShiftRight: () => void
  clearDrcsSet: (
    startCharCode: number,
    resolutionCode: number,
    colorDepthCode: number,
  ) => void
  clearToEndOfLine: () => void
  clearScreen: (clearAttributes: boolean) => void
  cursorDown: () => void
  cursorHome: () => void
  cursorBack: () => void
  cursorForward: () => void
  cursorToBeginningOfLine: () => void
  cursorUp: () => void
  defineColor: (index: number, r: number, g: number, b: number) => void
  doubleSize: (width: boolean, height: boolean) => void
  drcsDefinitionBlocks: (blocks: number[][]) => void
  endOfPage: () => void
  endSelection: () => void
  fastBlinking: (phase0: boolean, phase1: boolean, phase2: boolean) => void
  hide: () => void
  hideCursor: (hidden: boolean) => void
  intoLeftCharset: (charset: number) => void
  intoRightCharset: (charset: number) => void
  invertBlinking: () => void
  loadCharset: (intoCharset: number, loadCharset: number) => void
  mosaicOrTransparent: () => void
  reset: (parallel: boolean, limited: boolean) => void
  parallelMode: () => void
  polarity: (inverted: boolean) => void
  protectLine: () => void
  putChar: (charCode: number) => void
  repeatLastPrintedCharacter: (count: number) => void
  resetColorDefinitions: () => void
  resetShortcuts: () => void
  selectPalette: (palette: number) => void
  sendShortcut: (shortcut: number, suppressClearScreen: boolean) => void
  serialMode: () => void
  serviceBreakBack: () => void
  serviceBreakToRow: (row: number) => void
  setBgColorOfRow: (color: number) => void
  setBgColorOfScreen: (color: number) => void
  setColorDefinitionHeader: (colorDefinitionHeader: any) => void
  setCursor: (row: number, column: number) => void
  setFgColor: (color: number) => void
  setFgColorOfRow: (color: number) => void
  setFgColorOfScreen: (color: number) => void
  setScreenFormat: (columns: number, rows: number, wrapAround: boolean) => void
  setShortcut: (c: number, buf: number[]) => void
  startDrcsSet: (
    startCharCode: number,
    resolutionCode: number,
    colorDepthCode: number,
  ) => void
  startSelection: () => void
  switchCharsetForOneCharacter: (charset: number) => void
  transparency: (enabled: boolean) => void
  underline: (enabled: boolean) => void
  unprotectLine: () => void
  setBgColor: (color: number) => void
}

export interface Attributes {
  font?: Uint8Array
  backgroundColor?: number
  foregroundColor?: number
  doubleWidth?: boolean
  doubleHeight?: boolean
  boxed?: boolean
  concealed?: boolean
  blink?: boolean
  lined?: boolean
  inverted?: boolean
  protected?: boolean
  marked?: boolean
}

export default (
  log: (...data: any) => void,
  display: Display,
): CeptInterpreter => {
  let currentRow = 0
  let currentColumn = 0
  let screenRows: number
  let screenColumns: number

  let currentLeftFont = 0
  let currentRightFont = 1

  let glyphs: Uint8Array[]
  let attrs: Attributes[][]
  let rowColors: number[]
  let screenColor: number = 4

  const debug = () => renderDebugDisplay(glyphs, attrs, rowColors, screenColor)
  setInterval(debug, 500)

  const defaultAttributes: Attributes = {
    font: display.fonts[0],
    doubleWidth: false,
    doubleHeight: false,
    boxed: false,
    concealed: false,
    blink: undefined,
    lined: false,
    inverted: false,
    protected: false,
    marked: false,
    backgroundColor: 4,
    foregroundColor: 7,
  }

  const setScreenSize = (rows: number, columns: number) => {
    currentRow = 0
    currentColumn = 0
    screenRows = rows
    screenColumns = columns
    glyphs = new Array(screenRows)
      .fill(undefined)
      .map(() => new Uint8Array(screenColumns))
    attrs = new Array(screenRows).fill(undefined).map(() =>
      new Array(screenColumns).fill(undefined).map((_) => {
        return {}
      }),
    )
    rowColors = new Array(screenRows).fill(undefined)
    screenColor = 4
  }

  const colors = [
    0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff, 0x000, 0xf00, 0x0f0,
    0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff, 0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f,
    0x0ff, 0xfff, 0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff,
  ]

  setScreenSize(24, 40) // bla
  let currentWrapAround = true
  let currentMode: 'serial' | 'parallel' = 'serial'
  let currentAttributes = { ...defaultAttributes }

  const getFgColor = (row: number, column: number): number => {
    if (attrs[row][column].foregroundColor !== undefined) {
      return attrs[row][column].foregroundColor as number
    } else if (currentAttributes.foregroundColor !== undefined) {
      return currentAttributes.foregroundColor
    } else {
      return 7
    }
  }

  const getBgColor = (row: number, column: number): number => {
    if (attrs[row][column].backgroundColor !== undefined) {
      return attrs[row][column].backgroundColor as number
    } else if (rowColors[row] !== undefined) {
      return rowColors[row]
    } else {
      return screenColor
    }
  }

  const redraw = () => {
    for (let row = 0; row < screenRows; row++) {
      for (let column = 0; column < screenColumns; column++) {
        const glyphIndex = glyphs[row][column]
        const attributes = attrs[row][column]
        const fgColor = getFgColor(row, column)
        const bgColor = getBgColor(row, column)
        display.drawGlyph(
          glyphIndex,
          row,
          column,
          attributes.font || display.fonts[0],
          colors[attributes.inverted ? bgColor : fgColor],
          colors[attributes.inverted ? fgColor : bgColor],
        )
      }
    }
    display.render()
  }

  setInterval(redraw, 250)

  let lastCharCode = 0
  const putChar = (charCode: number) => {
    if (charCode < 0x80) {
      log(`putChar '${String.fromCharCode(charCode)}'`)
    } else {
      log(`putChar 0x${charCode.toString(16).padStart(2, '0')}`)
    }
    lastCharCode = charCode
    glyphs[currentRow][currentColumn] = (charCode & 0x7f) - 0x20
    attrs[currentRow][currentColumn] = { ...currentAttributes }
    attrs[currentRow][currentColumn].font =
      charCode >= 0x80
        ? display.fonts[currentRightFont]
        : display.fonts[currentLeftFont]
    if (charCode < 0xc0 || charCode > 0xcf) {
      // diacritical marks
      if (currentColumn + 1 < screenColumns) {
        currentColumn += 1
      } else if (currentWrapAround) {
        currentColumn = 0
        if (currentRow + 1 < screenRows) {
          currentRow += 1
        } else {
          currentRow = 0
        }
      }
    } else {
      console.log('skipping diacritical mark for now')
    }
  }

  const clearScreen = (clearAttributes: boolean) => {
    log('clearScreen')
    for (let row = 0; row < screenRows; row++) {
      for (let column = 0; column < screenColumns; column++) {
        glyphs[row][column] = 0
        if (clearAttributes) {
          attrs[row][column] = {}
        }
      }
    }
  }

  return {
    blink: (enabled: boolean) => {
      log('blink', { enabled })
    },
    blinkPalettes: () => {
      log('blinkPalettes')
    },
    blinkingShiftLeft: () => {
      log('blinkingShiftLeft')
    },
    blinkingShiftRight: () => {
      log('blinkingShiftRight')
    },
    clearDrcsSet: (
      startCharCode: number,
      resolutionCode: number,
      colorDepthCode: number,
    ) => {
      log('clearDrcsSet', { startCharCode, resolutionCode, colorDepthCode })
    },
    clearToEndOfLine: () => {
      log('clearToEndOfLine')
      for (let column = currentColumn; column < screenColumns; column++) {
        glyphs[currentRow][column] = 0
      }
    },
    clearScreen,
    cursorDown: () => {
      log('cursorDown')
      if (currentRow + 1 < screenRows) {
        currentRow += 1
      }
    },
    cursorHome: () => {
      log('cursorHome')
      currentRow = 0
      currentColumn = 0
    },
    cursorBack: () => {
      log('cursorLeft')
      if (currentColumn > 1) {
        currentColumn -= 1
      } else {
        currentColumn = screenColumns - 1
        if (currentRow > 0) {
          currentRow -= 1
        } else {
          currentRow = screenRows - 1
        }
      }
    },
    cursorForward: () => {
      log('cursorRight')
      if (currentColumn + 1 < screenColumns) {
        currentColumn += 1
      } else {
        currentColumn = 0
        if (currentRow + 1 < screenRows) {
          currentRow += 1
        } else {
          currentRow = 0
        }
      }
    },
    cursorToBeginningOfLine: () => {
      log('cursorToBeginningOfLine')
      currentColumn = 0
    },
    cursorUp: () => {
      log('cursorUp')
      if (currentRow > 1) {
        currentRow -= 1
      }
    },
    defineColor: (index: number, r: number, g: number, b: number) => {
      log('defineColor', { index, r, g, b })
      colors[index] = (b << 8) | (g << 4) | r
    },
    doubleSize: (width: boolean, height: boolean) => {
      log('doubleSize', { width, height })
    },
    drcsDefinitionBlocks: (blocks: number[][]) => {
      log(
        'drcsDefinitionBlocks',
        blocks.map((block) =>
          block.map((x) => x.toString(16).padStart(2, '0')).join(' '),
        ),
      )
    },
    endOfPage: () => {
      log('endOfPage')
    },
    endSelection: () => {
      log('endSelection')
    },
    fastBlinking: (phase0: boolean, phase1: boolean, phase2: boolean) => {
      log('fastBlinking', { phase0, phase1, phase2 })
    },
    hide: () => {
      log('hide')
    },
    hideCursor: (hidden: boolean) => {
      log('hideCursor', { hidden })
    },
    intoLeftCharset: (charset: number) => {
      log('intoLeftCharset', { charset })
      currentLeftFont = charset
    },
    intoRightCharset: (charset: number) => {
      log('intoRightCharset', { charset })
      currentRightFont = charset
    },
    invertBlinking: () => {
      log('invertBlinking')
    },
    loadCharset: (intoCharset: number, loadCharset: number) => {
      log('loadCharset', { intoCharset, loadCharset })
    },
    mosaicOrTransparent: () => {
      log('mosaicOrTransparent')
    },
    reset: (parallel: boolean, limited: boolean) => {
      log('reset', { parallel, limited })
      currentMode = parallel ? 'parallel' : 'serial'
    },
    parallelMode: () => {
      log('parallelMode')
      currentMode = 'parallel'
    },
    polarity: (inverted: boolean) => {
      log('polarity', { inverted })
      currentAttributes.inverted = inverted
    },
    protectLine: () => {
      log('protectLine')
    },
    putChar,
    repeatLastPrintedCharacter: (count: number) => {
      log('repeatLastPrintedCharacter', { count })
      for (let i = 0; i < count; i++) {
        putChar(lastCharCode)
      }
    },
    resetColorDefinitions: () => {
      // fixme missing
      log('resetColorDefinitions')
    },
    resetShortcuts: () => {
      log('resetShortcuts')
    },
    selectPalette: (palette: number) => {
      log('selectPalette', { palette })
    },
    sendShortcut: (shortcut: number, suppressClearScreen: boolean) => {
      log('sendShortcut', { shortcut }, suppressClearScreen)
    },
    serialMode: () => {
      log('serialMode')
      currentMode = 'serial'
    },
    serviceBreakBack: () => {
      log('serviceBreakBack')
    },
    serviceBreakToRow: (row: number) => {
      log('serviceBreakToRow', { row })
    },
    setBgColorOfRow: (color: number) => {
      log('setBgColorOfRow', { color })
      rowColors[currentRow] = color
    },
    setBgColorOfScreen: (color: number) => {
      log('setBgColorOfScreen', { color })
      screenColor = color
    },
    setColorDefinitionHeader: (colorDefinitionHeader: any) => {
      // fixme: any
      log('setColorDefinitionHeader', colorDefinitionHeader)
    },
    setCursor: (row: number, column: number) => {
      log('setCursor', { row, col: column })
      if (
        row >= 0 &&
        row < screenRows &&
        column >= 0 &&
        column < screenColumns
      ) {
        currentRow = row
        currentColumn = column
      } else {
        log(`new cursor position ${row}/${column} out of range`)
      }
    },
    setFgColor: (color: number) => {
      log('setFgColor', { color })
      currentAttributes.foregroundColor = color
    },
    setFgColorOfRow: (color: number) => {
      log('setFgColorOfRow', { color })
    },
    setFgColorOfScreen: (color: number) => {
      log('setFgColorOfScreen', { color })
    },
    setScreenFormat: (columns: number, rows: number, wrapAround: boolean) => {
      log('setScreenFormat', { columns, rows, wrapAround })
      setScreenSize(rows, columns)
      currentWrapAround = wrapAround
    },
    setShortcut: (c: number, buf: number[]) => {
      log('setShortcut', { c, buf })
    },
    startDrcsSet: (
      startCharCode: number,
      resolutionCode: number,
      colorDepthCode: number,
    ) => {
      // fixme: types
      log('startDrcsSet', { startCharCode, resolutionCode, colorDepthCode })
    },
    startSelection: () => {
      log('startSelection')
    },
    switchCharsetForOneCharacter: (charset: number) => {
      log('switchCharsetForOneCharacter', { charset })
    },
    transparency: (enabled: boolean) => {
      log('transparency', { enabled })
    },
    underline: (enabled: boolean) => {
      log('underline', { enabled })
    },
    unprotectLine: () => {
      log('unprotectLine')
    },
    setBgColor: (color: number) => {
      log('setBgColor', { color })
    },
  }
}
