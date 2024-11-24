import { Display } from './display'
import { renderDebugDisplay } from './ceptInterpreterDebug'

export type AttributeMode = 'serial' | 'parallel'

export type CeptInterpreter = {
  setTia: (state: boolean) => void
  attributeMode: () => AttributeMode
  updateDisplay: () => void
  blink: (enabled: boolean) => void
  blinkPalettes: () => void
  blinkingShiftLeft: () => void
  blinkingShiftRight: () => void
  clearDrcsSet: (resolutionCode: number, colorDepthCode: number) => void
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
  drcsDefinitionBlocks: (charCode: number, blocks: number[][]) => void
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
  startDrcsSet: (resolutionCode: number, colorDepthCode: number) => void
  startSelection: () => void
  switchCharsetForOneCharacter: (charset: number) => void
  transparency: (enabled: boolean) => void
  underline: (enabled: boolean) => void
  unprotectLine: () => void
  setBgColor: (color: number) => void
}

export interface Attributes {
  font?: Uint8Array
  diacritical?: number
  leftCharset?: number
  rightCharset?: number

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

  let charsetFont = [0, 1, 2, 3]
  let charsetForOneCharacter: number | undefined = undefined

  let glyphs: Uint8Array[]
  let attrs: Attributes[][]
  let rowColors: number[]
  let screenColor: number = 4

  let tia: boolean = false

  const defaultAttributes: Attributes = {
    leftCharset: 0,
    rightCharset: 2,
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

  const setCharacterSetDefaults = () => {
    charsetFont = [0, 1, 2, 3]
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

  setScreenSize(25, 40) // bla
  let currentWrapAround = true
  let currentMode: AttributeMode = 'serial'
  let currentPalette = 0
  let parallelAttributes: Attributes = {}

  let serviceBreakSave:
    | {
        mode: AttributeMode
        row: number
        column: number
        palette: number
        attributes: Attributes
      }
    | undefined

  const drcsResolutionMap = {
    6: { x: 12, y: 12 },
    7: { x: 12, y: 10 },
    10: { x: 6, y: 12 },
    11: { x: 6, y: 10 },
    12: { x: 6, y: 5 },
    15: { x: 6, y: 6 },
  }
  type DrcsColorDepth = 1 | 2 | 4

  let currentDrcsType: {
    resolution: { x: number; y: number }
    colorDepth: DrcsColorDepth
  } = {
    resolution: { x: 12, y: 12 },
    colorDepth: 1,
  }

  const redraw = () => {
    for (let row = 0; row < screenRows; row++) {
      for (let column = 0; column < screenColumns; column++) {
        let attributes = {
          ...defaultAttributes,
          ...(currentMode === 'serial' ? attrs[row][0] : {}),
        }
        const glyphIndex = glyphs[row][column]
        if (tia) {
          attributes = defaultAttributes
        } else {
          attributes = { ...attributes, ...attrs[row][column] }
          // skip over cells that are concealed by dh/db chars
          const dhBlindRowOffset = currentMode == 'serial' ? -1 : 1
          if (
            (row + dhBlindRowOffset >= 0 &&
              row + dhBlindRowOffset < screenRows &&
              attrs[row + dhBlindRowOffset][column].doubleHeight) ||
            (column > 0 && attrs[row][column - 1].doubleWidth) ||
            (row + dhBlindRowOffset >= 0 &&
              row + dhBlindRowOffset < screenRows &&
              column > 0 &&
              attrs[row + dhBlindRowOffset][column - 1].doubleWidth &&
              attrs[row + dhBlindRowOffset][column - 1].doubleHeight)
          ) {
            continue
          }
        }
        let foregroundColor = attributes.inverted
          ? attributes.backgroundColor
          : attributes.foregroundColor
        if (foregroundColor === undefined) {
          foregroundColor = defaultAttributes.foregroundColor as number
        }
        let backgroundColor = attributes.inverted
          ? attributes.foregroundColor
          : attributes.backgroundColor
        if (backgroundColor === undefined) {
          if (rowColors[row] !== undefined) {
            backgroundColor = rowColors[row]
          } else {
            backgroundColor = screenColor
          }
        }
        const dhDrawRowOffset =
          currentMode == 'parallel' && attributes.doubleHeight && row > 0
            ? -1
            : 0
        display.drawGlyph(
          glyphIndex,
          row + dhDrawRowOffset,
          column,
          attributes.font || display.fonts[0],
          colors[foregroundColor],
          colors[backgroundColor],
          attributes.doubleWidth,
          attributes.doubleHeight,
        )
        if (attributes.diacritical) {
          display.drawGlyph(
            attributes.diacritical,
            row + dhDrawRowOffset,
            column,
            display.fontDiacritical,
            colors[foregroundColor],
            colors[backgroundColor],
            attributes.doubleWidth,
            attributes.doubleHeight,
            true,
          )
        }
        if (attributes.doubleWidth && !tia) {
          column += 1
        }
      }
    }
    display.render()
  }

  let updateTimer: number | undefined
  const scheduleUpdate = () => {
    if (updateTimer) {
      clearTimeout(updateTimer)
    }
    updateTimer = setTimeout(() => {
      renderDebugDisplay(
        glyphs,
        attrs,
        rowColors,
        colors,
        screenColor,
        currentRow,
        currentColumn,
      )
      redraw()
      updateTimer = undefined
    }, 100)
  }

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

  let lastCharCode = 0
  let diacritical: number | undefined
  const putChar = (charCode: number) => {
    if (charCode < 0x80) {
      log(`putChar '${String.fromCharCode(charCode)}'`)
    } else {
      log(`putChar 0x${charCode.toString(16).padStart(2, '0')}`)
    }
    lastCharCode = charCode
    let { doubleWidth, doubleHeight, leftCharset, rightCharset } =
      getCurrentRowAttributes()
    let rowAdjust = 0
    if (currentMode == 'parallel' && doubleHeight) {
      if (currentRow > 0) {
        rowAdjust = 1
      }
      // fixme in parallel mode: need to delete double height attribute in second row (?)
    }
    glyphs[currentRow][currentColumn] = (charCode & 0x7f) - 0x20
    if (currentMode === 'parallel') {
      attrs[currentRow][currentColumn] = { ...parallelAttributes }
    }
    const attributes = attrs[currentRow][currentColumn] || {}
    const charset =
      charsetForOneCharacter ||
      (charCode >= 0x80
        ? attributes.rightCharset || (rightCharset as number)
        : attributes.leftCharset || (leftCharset as number))
    const font = display.fonts[charsetFont[charset]]
    attributes.font = font
    if (doubleWidth !== undefined) {
      attributes.doubleWidth = doubleWidth
    }
    if (doubleHeight !== undefined) {
      attributes.doubleHeight = doubleHeight
    }
    const isDiacritical =
      (charsetForOneCharacter === 2 || font === display.fonts[2]) &&
      (charCode & 0x70) === 0x40

    charsetForOneCharacter = undefined

    if (isDiacritical) {
      // diacritical marks don't move the cursor
      diacritical = charCode & 0x0f
      return
    }

    attrs[currentRow][currentColumn].diacritical = diacritical
    diacritical = undefined

    const columnIncrement = doubleWidth ? 2 : 1
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
    // fixme next row in serial mode?
    if (currentMode === 'serial') {
      // wrap around some attributes
      attrs[currentRow][currentColumn] = {
        leftCharset: leftCharset,
        rightCharset: rightCharset,
        ...attrs[currentRow][currentColumn],
      }
    }
  }

  const changeAttribute = (change: Attributes, advance: boolean = true) => {
    if (currentMode == 'serial') {
      if ('inverted' in change) {
        if (change.inverted) {
          change = {
            backgroundColor: getCurrentRowAttributes().foregroundColor,
          }
        } else {
          change = { backgroundColor: 0 }
        }
      }
      attrs[currentRow][currentColumn] = {
        ...attrs[currentRow][currentColumn],
        ...change,
      }
      if (advance) {
        currentColumn += 1
      }
    } else {
      if ('leftCharset' in change) {
        parallelAttributes.leftCharset = change.leftCharset
      }
      if ('rightCharset' in change) {
        parallelAttributes.rightCharset = change.rightCharset
      }
      if ('backgroundColor' in change) {
        parallelAttributes.backgroundColor = change.backgroundColor
      }
      if ('foregroundColor' in change) {
        parallelAttributes.foregroundColor = change.foregroundColor
      }
      if ('doubleWidth' in change) {
        if (!change.doubleWidth) {
          delete parallelAttributes.doubleWidth
        } else {
          parallelAttributes.doubleWidth = true
        }
      }
      if ('doubleHeight' in change) {
        if (!change.doubleHeight) {
          delete parallelAttributes.doubleHeight
        } else {
          parallelAttributes.doubleHeight = true
        }
      }
      if ('boxed' in change) {
        if (!change.boxed) {
          delete parallelAttributes.boxed
        } else {
          parallelAttributes.boxed = true
        }
      }
      if ('concealed' in change) {
        if (!change.concealed) {
          delete parallelAttributes.concealed
        } else {
          parallelAttributes.concealed = true
        }
      }
      if ('blink' in change) {
        if (!change.blink) {
          delete parallelAttributes.blink
        } else {
          parallelAttributes.blink = true
        }
      }
      if ('lined' in change) {
        if (!change.lined) {
          delete parallelAttributes.lined
        } else {
          parallelAttributes.lined = true
        }
      }
      if ('inverted' in change) {
        if (!change.inverted) {
          delete parallelAttributes.inverted
        } else {
          parallelAttributes.inverted = true
        }
      }
      if ('protected' in change) {
        if (!change.protected) {
          delete parallelAttributes.protected
        } else {
          parallelAttributes.protected = true
        }
      }
      if ('marked' in change) {
        if (!change.marked) {
          delete parallelAttributes.marked
        } else {
          parallelAttributes.marked = true
        }
      }
    }
  }

  const clearScreen = (keepAttributes: boolean) => {
    log('clearScreen')
    for (let row = 0; row < screenRows; row++) {
      for (let column = 0; column < screenColumns; column++) {
        glyphs[row][column] = 0
        delete attrs[row][column].diacritical
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
          for (let i = 0; i < count; i++) {
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
            rows[currentRow] = rowOfOnes
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
          if (rowAccumulator.length == currentDrcsType.resolution.x) {
            if (currentDrcsType.resolution.x === 12) {
              rows[currentRow++] = [...rowAccumulator]
            } else {
              rows[currentRow++] = rowAccumulator.flatMap((x) => [x, x])
            }
            rowAccumulator = []
          }
        }
      }
    }
    iterateBlocks()
    console.log('glyph', glyphNumber, 'rows', rows)
    display.defineDrcs(glyphNumber, rows)
  }

  return {
    // Internal handlers
    setTia: (state: boolean) => {
      tia = state
      scheduleUpdate()
    },
    attributeMode: () => {
      return currentMode
    },
    updateDisplay: scheduleUpdate,

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
    clearDrcsSet: (resolutionCode: number, colorDepthCode: number) => {
      log('clearDrcsSet', { resolutionCode, colorDepthCode })
      display.fonts[5].fill(0)
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
      colors[index] = (r << 8) | (g << 4) | b
    },
    doubleSize: (doubleWidth: boolean, doubleHeight: boolean) => {
      log('doubleSize', { doubleWidth, doubleHeight })
      changeAttribute({ doubleWidth, doubleHeight })
    },
    drcsDefinitionBlocks: (charCode: number, blocks: number[][]) => {
      log(
        `drcsDefinitionBlocks starting at 0x${charCode.toString(16).padStart(2, '0')}`,
        blocks.map((block) =>
          block.map((x) => x.toString(16).padStart(2, '0')).join(' '),
        ),
      )
      blocks.forEach((block) => {
        defineDrcs(charCode - 0x20, block)
        charCode += 1
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
      changeAttribute({ leftCharset: charset }, false)
    },
    intoRightCharset: (charset: number) => {
      log('intoRightCharset', { charset })
      changeAttribute({ rightCharset: charset }, false)
    },
    invertBlinking: () => {
      log('invertBlinking')
    },
    assignFont: (charset: number, fontNumber: number) => {
      log('assignFont', { charset, fontNumber })
      charsetFont[charset] = fontNumber
    },
    mosaicOrTransparent: () => {
      log('mosaicOrTransparent')
      if (currentMode === 'parallel') {
        delete parallelAttributes.backgroundColor
      }
    },
    reset: (parallel: boolean, limited: boolean) => {
      log('reset', { parallel, limited })
      currentMode = parallel ? 'parallel' : 'serial'
      parallelAttributes = { leftCharset: 0, rightCharset: 2 }
      setCharacterSetDefaults()
      if (!limited) {
        clearScreen(false)
      }
    },
    parallelMode: () => {
      log('parallelMode')
      currentMode = 'parallel'
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
      log('resetColorDefinitions')
      for (let color = 0; color < 8; color++) {
        colors[color + 16] = colors[color]
        colors[color + 24] = colors[color]
      }
    },
    resetShortcuts: () => {
      log('resetShortcuts')
    },
    selectPalette: (palette: number) => {
      log('selectPalette', { palette })
      currentPalette = palette
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
      if (serviceBreakSave) {
        let { mode, row, column, palette, attributes } = serviceBreakSave
        currentMode = mode
        currentRow = row
        currentColumn = column
        currentPalette = palette
        parallelAttributes = attributes
        serviceBreakSave = undefined
      }
    },
    serviceBreakToRow: (row: number) => {
      log('serviceBreakToRow', { row })
      serviceBreakSave = {
        mode: currentMode,
        row: currentRow,
        column: currentColumn,
        palette: currentPalette,
        attributes: parallelAttributes,
      }
      //currentMode = 'serial'
      currentRow = row
      currentColumn = 0
      currentPalette = 0
      parallelAttributes = { ...defaultAttributes }
      parallelAttributes.backgroundColor = 0
    },
    setBgColorOfRow: (color: number) => {
      color = currentPalette * 8 + color
      log('setBgColorOfRow', { color })
      rowColors[currentRow] = color
      display.setRowColor(currentRow, colors[color])
    },
    setBgColorOfScreen: (color: number) => {
      color = currentPalette * 8 + color
      log('setBgColorOfScreen', { color })
      screenColor = color
      display.setScreenColor(colors[color])
    },
    setColorDefinitionHeader: (colorDefinitionHeader: number[]) => {
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
      changeAttribute({ foregroundColor: currentPalette * 8 + color })
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
    startDrcsSet: (resolutionCode: number, colorDepth: number) => {
      // fixme: types
      log('startDrcsSet', { resolutionCode, colorDepth })
      if (colorDepth !== 1 && colorDepth !== 2 && colorDepth !== 4) {
        log(`invalid color depth code ${colors}`)
        return
      }
      if (!(resolutionCode in drcsResolutionMap)) {
        log(`invalid resolution code ${resolutionCode}`)
        return
      }
      currentDrcsType = {
        resolution: drcsResolutionMap[
          resolutionCode as keyof typeof drcsResolutionMap
        ] as { x: number; y: number },
        colorDepth,
      }
    },
    startSelection: () => {
      log('startSelection')
    },
    switchCharsetForOneCharacter: (charset: number) => {
      log('switchCharsetForOneCharacter', { charset })
      charsetForOneCharacter = charset
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
      changeAttribute({ backgroundColor: currentPalette * 8 + color })
    },
  }
}
