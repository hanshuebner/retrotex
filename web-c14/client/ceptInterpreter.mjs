export default (log, display) => {
    let currentRow = 0
    let currentColumn = 0
    const rows = 25
    const columns = 40
    const chars = new Array(rows).fill().map(() => new Uint8Array(columns));
    const attrs = new Array(rows).fill().map(() => new Uint8Array(columns));
    const wrapAround = true

    const redraw = () => {
        for (let row = 0; row < rows; row++) {
            for (let column = 0; column < columns; column++) {
                display.drawGlyph(chars[row][column] || 32, row, column, display.fontG0, 0xfff, 0x000)
            }
        }
        display.render()
    }

    setInterval(redraw, 250)

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
                chars[currentRow][column] = 0x20;
            }
        },
        clearScreen: () => {
            log('clearScreen')
            for (let row = 0; row < rows; row++) {
                for (let column = 0; column < columns; column++) {
                    chars[row][column] = 0x20;
                    attrs[row][column] = 0x00;
                }
            }
        },
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
        cursorLeft: () => {
            log('cursorLeft')
            if (currentColumn > 1) {
                currentColumn -= 1
            }
        },
        cursorRight: () => {
            log('cursorRight')
            if (currentColumn + 1 < columns) {
                currentColumn += 1
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
        },
        intoRightCharset: (charset) => {
            log('intoRightCharset', {charset})
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
        resetMode: (parallel, limited) => {
            log('resetMode', {parallel, limited})
        },
        parallelMode: () => {
            log('parallelMode')
        },
        polarity: (inverted) => {
            log('polarity', {inverted})
        },
        protectLine: () => {
            log('protectLine')
        },
        putChar: (charCode) => {
            if (charCode < 0x80) {
                log(`putChar '${String.fromCharCode(charCode)}'`)
            } else {
                log(`putChar 0x${charCode.toString(16).padStart(2, '0')}`)
            }
            chars[currentRow][currentColumn] = charCode
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
        },
        repeatLastPrintedCharacter: (count) => {
            log('repeatLastPrintedCharacter', {count})
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
            log('setFgColor', {color})
        },
        setFgColorOfRow: (color) => {
            log('setFgColorOfRow', {color})
        },
        setFgColorOfScreen: (color) => {
            log('setFgColorOfScreen', {color})
        },
        setResolutionTo40x24: () => {
            log('setResolutionTo40x24')
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
