import type { Attributes } from './ceptInterpreter'

export const renderDebugDisplay = (
  glyphs: Uint8Array[],
  attrs: Attributes[][],
  rowColors: number[],
  screenColor: number,
) => {
  const createChip = (label: string, color: string) => {
    const chip = document.createElement('div')
    chip.style.display = 'inline-block'
    chip.style.padding = '2px 4px'
    chip.style.margin = '2px'
    chip.style.borderRadius = '4px'
    chip.style.backgroundColor = color
    chip.style.color = '#fff'
    chip.textContent = label
    return chip
  }

  const container = document.getElementById('cept-debugger')
  if (!container) {
    console.error('Element with ID "cept-debugger" not found.')
    return
  }
  container.innerHTML = ''

  container.style.display = 'grid'
  container.style.gridTemplateColumns = `repeat(${glyphs[0].length + 1}, auto)`
  container.style.gap = '4px'

  // Screen color
  const screenColorChip = createChip(
    'Screen Color',
    `#${screenColor.toString(16).padStart(3, '0')}`,
  )
  container.appendChild(screenColorChip)

  // Empty cell for alignment
  container.appendChild(document.createElement('div'))

  // Row colors and grid
  for (let row = 0; row < glyphs.length; row++) {
    // Row color
    if (rowColors[row] !== undefined) {
      const rowColorChip = createChip(
        'Row Color',
        `#${rowColors[row].toString(16).padStart(3, '0')}`,
      )
      container.appendChild(rowColorChip)
    } else {
      container.appendChild(document.createElement('div'))
    }

    for (let col = 0; col < glyphs[row].length; col++) {
      const cell = document.createElement('div')
      cell.style.border = '1px solid #ccc'
      cell.style.padding = '4px'
      cell.style.position = 'relative'

      // Glyph number
      const glyphNumber = document.createElement('div')
      glyphNumber.style.position = 'absolute'
      glyphNumber.style.top = '2px'
      glyphNumber.style.left = '2px'
      glyphNumber.textContent = glyphs[row][col].toString()
      cell.appendChild(glyphNumber)

      // Foreground and background colors
      if (attrs[row][col].foregroundColor !== undefined) {
        const fgColorChip = createChip(
          'FG',
          `#${attrs[row][col].foregroundColor!.toString(16).padStart(3, '0')}`,
        )
        fgColorChip.style.position = 'absolute'
        fgColorChip.style.top = '2px'
        fgColorChip.style.right = '2px'
        cell.appendChild(fgColorChip)
      }
      if (attrs[row][col].backgroundColor !== undefined) {
        const bgColorChip = createChip(
          'BG',
          `#${attrs[row][col].backgroundColor!.toString(16).padStart(3, '0')}`,
        )
        bgColorChip.style.position = 'absolute'
        bgColorChip.style.top = '20px'
        bgColorChip.style.right = '2px'
        cell.appendChild(bgColorChip)
      }

      // Boolean attributes
      const booleanAttributes = [
        'doubleWidth',
        'doubleHeight',
        'boxed',
        'concealed',
        'blink',
        'lined',
        'inverted',
        'protected',
        'marked',
      ]
      booleanAttributes.forEach((attr, index) => {
        if (attrs[row][col][attr as keyof Attributes]) {
          const attrChip = createChip(attr.slice(0, 2).toUpperCase(), '#007bff')
          attrChip.style.position = 'absolute'
          attrChip.style.bottom = `${index * 16}px`
          attrChip.style.left = '2px'
          cell.appendChild(attrChip)
        }
      })

      container.appendChild(cell)
    }
  }
}
