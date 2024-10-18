export default (log, display) => {
    let currentRow = 0
    let currentColumn = 0
    let rows
    let columns

    let currentLeftFont = 0
    let currentRightFont = 1

    let glyphs
    let attrs

    const defaultAttributes = {
        font: display.fonts[0],
        backgroundColor: 8,
        foregroundColor: 7,
        doubleWidth: false,
        doubleHeight: false,
        boxed: false,
        concealed: false,
        blink: undefined,
        lined: false,
        inverted: false,
        protected: false,
        marked: false,
    }

    const setScreenSize = (rows_, columns_) => {
        currentRow = 0
        currentColumn = 0
        rows = rows_
        columns = columns_
        glyphs = new Array(rows).fill().map(() => new Uint8Array(columns));
        attrs = new Array(rows).fill().map(() => new Array(columns).fill().map(_ => {
            return {...defaultAttributes}
        }));
    }

    const colors = [
        0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff,
        0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff,
        0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff,
        0x000, 0xf00, 0x0f0, 0xff0, 0x00f, 0xf0f, 0x0ff, 0xfff,
    ]

    setScreenSize(24, 40)
    let wrapAround = true
    let mode = 'serial'
    let currentAttributes = {...defaultAttributes}

    const redraw = () => {
        for (let row = 0; row < rows; row++) {
            for (let column = 0; column < columns; column++) {
                const glyphIndex = glyphs[row][column]
                const attributes = attrs[row][column]
                display.drawGlyph(
                    glyphIndex,
                    row, column,
                    attributes.font,
                    0xfff, 0x000)
            }
        }
        display.render()
    }

    setInterval(redraw, 250)

    let lastCharCode = 0x20
    const putChar = (charCode) => {
        if (charCode < 0x80) {
            log(`putChar '${String.fromCharCode(charCode)}'`)
        } else {
            log(`putChar 0x${charCode.toString(16).padStart(2, '0')}`)
        }
        lastCharCode = charCode
        glyphs[currentRow][currentColumn] = (charCode & 0x7f) - 0x20
        attrs[currentRow][currentColumn].font = (charCode >= 0x80) ? display.fonts[currentRightFont] : display.fonts[currentLeftFont]
        if (charCode < 0xc0 || charCode > 0xcf) { // diacritical marks
            if (currentColumn + 1 < columns) {
                currentColumn += 1
            } else if (wrapAround) {
                currentColumn = 0
                if (currentRow + 1 < rows) {
                    currentRow += 1
                } else {
                    currentRow = 0
                }
            }
        }
    }

    const clearScreen = (clearAttributes) => {
        log('clearScreen')
        for (let row = 0; row < rows; row++) {
            for (let column = 0; column < columns; column++) {
                glyphs[row][column] = 0
                if (clearAttributes) {
                    attrs[row][column] = {...defaultAttributes}
                }
            }
        }
    }

    return {
        blink: (enabled) => {
            log('blink', {enabled})
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
        clearDrcsSet: (startCharCode, resolutionCode, colorDepthCode) => {
            log('clearDrcsSet', {startCharCode, resolutionCode, colorDepthCode})
        },
        clearToEndOfLine: () => {
            log('clearToEndOfLine')
            for (let column = currentColumn; column < columns; column++) {
                glyphs[currentRow][column] = 0
            }
        },
        clearScreen,
        cursorDown: () => {
            log('cursorDown')
            if (currentRow + 1 < rows) {
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
                currentColumn = columns - 1
                if (currentRow > 0) {
                    currentRow -= 1
                } else {
                    currentRow = rows - 1
                }
            }
        },
        cursorForward: () => {
            log('cursorRight')
            if (currentColumn + 1 < columns) {
                currentColumn += 1
            } else {
                currentColumn = 0
                if (currentRow + 1 < rows) {
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
        defineColor: (index, r, g, b) => {
            log('defineColor', {index, r, g, b})
            colors[index] = (b << 8) | (g << 4) | r
        },
        doubleSize: (width, height) => {
            log('doubleSize', {width, height})
        },
        drcsDefinitionBlocks: (blocks) => {
            log('drcsDefinitionBlocks', blocks.map(block => block.map(x => x.toString(16).padStart(2, '0')).join(' ')))
        },
        endOfPage: () => {
            log('endOfPage')
        },
        endSelection: () => {
            log('endSelection')
        },
        fastBlinking: (phase0, phase1, phase2) => {
            log('fastBlinking', {phase0, phase1, phase2})
        },
        hide: () => {
            log('hide')
        },
        hideCursor: (hidden) => {
            log('hideCursor', {hidden})
        },
        intoLeftCharset: (charset) => {
            log('intoLeftCharset', {charset})
            currentLeftFont = charset
        },
        intoRightCharset: (charset) => {
            log('intoRightCharset', {charset})
            currentRightFont = charset
        },
        invertBlinking: () => {
            log('invertBlinking')
        },
        loadCharset: (intoCharset, loadCharset) => {
            log('loadCharset', {intoCharset, loadCharset})
        },
        mosaicOrTransparent: () => {
            log('mosaicOrTransparent')
        },
        reset: (parallel, limited) => {
            log('reset', {parallel, limited})
            mode = parallel ? 'parallel' : 'serial'
            setScreenSize(24, 40)
        },
        parallelMode: () => {
            log('parallelMode')
            mode = 'parallel'
        },
        polarity: (inverted_) => {
            log('polarity', {inverted: inverted_})
            currentAttributes.polarity = inverted_
        },
        protectLine: () => {
            log('protectLine')
        },
        putChar,
        repeatLastPrintedCharacter: (count) => {
            log('repeatLastPrintedCharacter', {count})
            for (let i = 0; i < count; i++) {
                putChar(lastCharCode)
            }
        },
        resetShortcuts: () => {
            log('resetShortcuts')
        },
        selectPalette: (palette) => {
            log('selectPalette', {palette})
        },
        sendShortcut: (shortcut, suppressClearScreen) => {
            log('sendShortcut', {shortcut}, suppressClearScreen)
        },
        serialMode: () => {
            log('serialMode')
            mode = 'serial'
        },
        serviceBreakBack: () => {
            log('serviceBreakBack')
        },
        serviceBreakToRow: (row) => {
            log('serviceBreakToRow', {row})
        },
        setBgColorOfRow: (color) => {
            log('setBgColorOfRow', {color})
        },
        setBgColorOfScreen: (color) => {
            log('setBgColorOfScreen', {color})
        },
        setColorDefinitionHeader: (colorDefinitionHeader) => {
            log('setColorDefinitionHeader', colorDefinitionHeader)
        },
        setCursor: (row, column) => {
            log('setCursor', {row, col: column})
            if (row >= 0 && row < rows && column >= 0 && column < columns) {
                currentRow = row
                currentColumn = column
            } else {
                log(`new cursor position ${row}/${column} out of range`)
            }
        },
        setFgColor: (color) => {
            log('setFgColor', {color});
            currentAttributes.foregroundColor = color
        },
        setFgColorOfRow: (color) => {
            log('setFgColorOfRow', {color})
        },
        setFgColorOfScreen: (color) => {
            log('setFgColorOfScreen', {color})
        },
        setScreenFormat: (columns_, rows_, wrapAround_) => {
            log('setScreenFormat', {columns: columns_, rows: rows_, wrapAround: wrapAround_})
            setScreenSize(rows_, columns_)
            wrapAround = wrapAround_
        },
        setShortcut: (c, buf) => {
            log('setShortcut', {c, buf})
        },
        startDrcsSet: (startCharCode, resolutionCode, colorDepthCode) => {
            log('startDrcsSet', {startCharCode, resolutionCode, colorDepthCode})
        },
        startSelection: () => {
            log('startSelection')
        },
        switchCharsetForOneCharacter: (charset) => {
            log('switchCharsetForOneCharacter', {charset})
        },
        transparency: (enabled) => {
            log('transparency', {enabled})
        },
        underline: (enabled) => {
            log('underline', {enabled})
        },
        unprotectLine: () => {
            log('unprotectLine')
        },
        setBgColor: (color) => {
            log('setBgColor', {color})
        },
    }
}
