import { Display } from './display'
import { renderDebugDisplay } from './ceptInterpreterDebug'

export type AttributeMode = 'serial' | 'parallel'

export type CeptInterpreter = {
  attributeMode: () => AttributeMode
  updateDebugDisplay: () => void
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
  clearScreen: (keepAttributes: boolean) => void
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
  assignFont: (intoCharset: number, loadCharset: number) => void
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
  notRendered?: boolean
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

  let currentDrcsGlyph = 0

  let glyphs: Uint8Array[]
  let attrs: Attributes[][]
  let rowColors: number[]
  let screenColor: number = 4

  let noAttributes: boolean = false

  const updateDebugDisplay = () =>
    renderDebugDisplay(
      glyphs,
      attrs,
      rowColors,
      screenColor,
      currentRow,
      currentColumn,
      parallelAttributes,
    )

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
  let currentMode: AttributeMode = 'serial'
  let parallelAttributes = { ...defaultAttributes }

  const getFgColor = (row: number, column: number): number => {
    if (attrs[row][column].foregroundColor !== undefined) {
      return attrs[row][column].foregroundColor as number
    } else if (parallelAttributes.foregroundColor !== undefined) {
      return parallelAttributes.foregroundColor
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
      let attributes = { ...defaultAttributes }
      for (let column = 0; column < screenColumns; column++) {
        const glyphIndex = glyphs[row][column]
        if (!noAttributes) {
          attributes = { ...attributes, ...attrs[row][column] }
        }
        if (attributes.notRendered) {
          continue
        }
        const fgColor = noAttributes
          ? (defaultAttributes.foregroundColor as number)
          : getFgColor(row, column)
        const bgColor = noAttributes
          ? (defaultAttributes.backgroundColor as number)
          : getBgColor(row, column)
        display.drawGlyph(
          glyphIndex,
          row,
          column,
          attributes.font || display.fonts[0],
          colors[attributes.inverted ? bgColor : fgColor],
          colors[attributes.inverted ? fgColor : bgColor],
          attributes.doubleWidth,
          attributes.doubleHeight,
        )
        if (attributes.doubleWidth) {
          column += 1
        }
      }
    }
    display.render()
  }

  setInterval(redraw, 250)

  const getCurrentRowAttributes = () => {
    if (currentMode == 'serial') {
      let attributes = { ...defaultAttributes }
      for (let column = 0; column <= currentColumn; column++) {
        attributes = { ...attributes, ...attrs[currentRow][column] }
      }
      return attributes
    } else {
      return parallelAttributes
    }
  }

  const advanceCursor = () => {
    const { doubleWidth, doubleHeight } = getCurrentRowAttributes()
    const columnIncrement = doubleWidth ? 2 : 1
    if (doubleWidth && currentColumn < screenColumns - 1) {
      attrs[currentRow][currentColumn + 1].notRendered = true
    }
    if (doubleHeight && currentRow < screenRows - 1) {
      attrs[currentRow + 1][currentColumn].notRendered = true
    }
    if (
      doubleHeight &&
      currentRow < screenRows - 1 &&
      doubleWidth &&
      currentColumn < screenColumns - 1
    ) {
      attrs[currentRow + 1][currentColumn + 1].notRendered = true
    }
    if (currentColumn + columnIncrement < screenColumns) {
      currentColumn += columnIncrement
    } else if (currentWrapAround) {
      currentColumn = 0
      if (currentRow + 1 < screenRows) {
        currentRow += 1
      } else {
        currentRow = 0
      }
    }
  }

  let lastCharCode = 0
  const putChar = (charCode: number) => {
    if (charCode < 0x80) {
      log(`putChar '${String.fromCharCode(charCode)}'`)
    } else {
      log(`putChar 0x${charCode.toString(16).padStart(2, '0')}`)
    }
    lastCharCode = charCode
    glyphs[currentRow][currentColumn] = (charCode & 0x7f) - 0x20
    if (currentMode == 'parallel') {
      attrs[currentRow][currentColumn] = { ...parallelAttributes }
    }
    attrs[currentRow][currentColumn].font =
      charCode >= 0x80
        ? display.fonts[currentRightFont]
        : display.fonts[currentLeftFont]

    if (currentLeftFont !== 0 || charCode < 0xc0 || charCode > 0xcf) {
      // diacritical marks don't move the cursor
      advanceCursor()
    } else {
      console.log('skipping diacritical mark for now')
    }
  }

  const changeAttribute = (change: Attributes) => {
    if (currentMode == 'serial') {
      attrs[currentRow][currentColumn] = {
        ...attrs[currentRow][currentColumn],
        ...change,
      }
      glyphs[currentRow][currentColumn]
      currentColumn += 1
    } else {
      parallelAttributes = { ...parallelAttributes, ...change }
    }
  }

  const clearScreen = (keepAttributes: boolean) => {
    log('clearScreen')
    for (let row = 0; row < screenRows; row++) {
      for (let column = 0; column < screenColumns; column++) {
        glyphs[row][column] = 0
        if (!keepAttributes) {
          attrs[row][column] = {}
        }
      }
    }
  }

  const defineDrcs = (glyphNumber: number, block: number[]) => {
    const rowOfOnes = new Array(12).fill(1)
    const rowOfZeros = new Array(12).fill(0)
    const rows = new Array(10).fill(undefined).map(() => new Array(12).fill(0))
    const iterateBlocks = () => {
      let currentRow = 0
      let rowAccumulator: number[] = []
      for (let code of block) {
        const lastCompleteRow = currentRow > 0 ? rows[currentRow - 1] : rows[0]
        if (code === 0x20) {
          return
        } else if (0x21 <= code && code <= 0x2a) {
          const count = code & 0x0f
          for (let i = 1; i <= count; i++) {
            rows[currentRow + i] = lastCompleteRow
          }
          currentRow += count
        } else if (code === 0x2c) {
          rows[currentRow++] = rowOfZeros
        } else if (code === 0x2c) {
          rows[currentRow++] = rowOfOnes
        } else if (code === 0x2e) {
          for (; currentRow < 10; currentRow++) {
            rows[currentRow] = lastCompleteRow
          }
        } else if (code === 0x2f) {
          for (; currentRow < 10; currentRow++) {
            rows[currentRow] = rowOfZeros
          }
          return
        } else if (0x30 <= code && code <= 0x33) {
        } else if ((code & 0xc0) !== 0x40) {
          console.log(
            `unrecognized byte 0x${code.toString(16).padStart(2, '0')} in DRCS definition block`,
          )
        } else {
          const bits = (code & 0x3f)
            .toString(2)
            .padStart(6, '0')
            .split('')
            .map((s) => parseInt(s, 2))
          rowAccumulator = [...rowAccumulator, ...bits]
          if (rowAccumulator.length == 12) {
            rows[currentRow++] = [...rowAccumulator]
            rowAccumulator = []
          }
        }
      }
    }
    iterateBlocks()
    console.log('glyph', glyphNumber, 'rows', rows)
  }

  return {
    // Internal handlers
    attributeMode: () => {
      return currentMode
    },
    updateDebugDisplay,

    // CEPT handlers
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
      currentDrcsGlyph = startCharCode - 0x20
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
      log('cursorBack')
      if (currentColumn > 0) {
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
    doubleSize: (doubleWidth: boolean, doubleHeight: boolean) => {
      log('doubleSize', { doubleWidth, doubleHeight })
      changeAttribute({ doubleWidth, doubleHeight })
    },
    drcsDefinitionBlocks: (blocks: number[][]) => {
      log(
        'drcsDefinitionBlocks',
        blocks.map((block) =>
          block.map((x) => x.toString(16).padStart(2, '0')).join(' '),
        ),
      )
      blocks.forEach((block) => {
        defineDrcs(currentDrcsGlyph, block)
        currentDrcsGlyph += 1
      })
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
    assignFont: (charset: number, fontNumber: number) => {
      log('loadCharset', { charset, fontNumber })
    },
    mosaicOrTransparent: () => {
      log('mosaicOrTransparent')
    },
    reset: (parallel: boolean, limited: boolean) => {
      log('reset', { parallel, limited })
      currentMode = parallel ? 'parallel' : 'serial'
      parallelAttributes = { ...defaultAttributes }
    },
    parallelMode: () => {
      log('parallelMode')
      currentMode = 'parallel'
      parallelAttributes = { ...defaultAttributes }
    },
    polarity: (inverted: boolean) => {
      log('polarity', { inverted })
      changeAttribute({ inverted })
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
      display.setRowColor(currentRow, colors[color])
    },
    setBgColorOfScreen: (color: number) => {
      log('setBgColorOfScreen', { color })
      screenColor = color
      display.setScreenColor(colors[color])
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
      changeAttribute({ foregroundColor: color })
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
      currentDrcsGlyph = 0
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
