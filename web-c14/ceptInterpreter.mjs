export default (log) => {
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
        clearLine: () => {
            log('clearLine')
        },
        clearScreen: () => {
            log('clearScreen')
        },
        cursorDown: () => {
            log('cursorDown')
        },
        cursorHome: () => {
            log('cursorHome')
        },
        cursorLeft: () => {
            log('cursorLeft')
        },
        cursorRight: () => {
            log('cursorRight')
        },
        cursorToBeginningOfLine: () => {
            log('cursorToBeginningOfLine')
        },
        cursorUp: () => {
            log('cursorUp')
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
        setCursor: (row, col) => {
            log('setCursor', {row, col})
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
