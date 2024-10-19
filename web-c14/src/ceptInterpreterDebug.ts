import type { Attributes } from './ceptInterpreter'

export const renderDebugDisplay = (
  glyphs: Uint8Array[],
  attrs: Attributes[][],
  rowColors: number[],
  screenColor: number,
) => {
  const createChip = (label: string, color: string) => {
    const chip = document.createElement('div')
    chip.style.minWidth = '1em'
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

  container.style.backgroundColor = 'white'
  container.style.fontFamily = 'Arial, sans-serif' // Modern, non-serif font
  container.style.fontSize = '12px' // Smaller font size

  // Tooltip element
  const tooltip = document.createElement('div')
  tooltip.style.position = 'absolute'
  tooltip.style.padding = '4px'
  tooltip.style.backgroundColor = '#333'
  tooltip.style.color = '#fff'
  tooltip.style.borderRadius = '4px'
  tooltip.style.visibility = 'hidden'
  tooltip.style.zIndex = '1000'
  container.appendChild(tooltip)

  // Screen color
  const screenColorChip = createChip(
    `#${screenColor.toString(16).padStart(3, '0')}`,
    `#${screenColor.toString(16).padStart(3, '0')}`,
  )
  screenColorChip.style.width = '25px'
  container.appendChild(screenColorChip)

  const grid = document.createElement('div')
  container.appendChild(grid)

  grid.style.display = 'grid'
  grid.style.gridTemplateColumns = `repeat(${glyphs[0].length + 1}, auto)`
  grid.style.gap = '4px'

  // Row colors and grid
  for (let row = 0; row < glyphs.length; row++) {
    // Row color
    const rowColorChip =
      rowColors[row] !== undefined
        ? createChip(
            `#${rowColors[row].toString(16).padStart(3, '0')}`,
            `#${rowColors[row].toString(16).padStart(3, '0')}`,
          )
        : document.createElement('div')
    rowColorChip.style.width = '25px'
    grid.appendChild(rowColorChip)

    for (let col = 0; col < glyphs[row].length; col++) {
      const cell = document.createElement('div')
      cell.style.border = '1px solid #ccc'
      cell.style.padding = '4px'
      cell.style.position = 'relative' // Ensure the cell is the positioning context
      cell.style.width = '15px'
      cell.style.height = '15px'
      cell.style.overflow = 'hidden'

      // Check if the cell has any attributes defined
      const hasAttributes =
        attrs[row][col].foregroundColor !== undefined ||
        attrs[row][col].backgroundColor !== undefined ||
        Object.values(attrs[row][col]).some((attr) => attr === true)

      if (hasAttributes) {
        cell.style.backgroundColor = '#ffeb3b' // Distinct background color
      }

      // Glyph number
      const glyphNumber = document.createElement('div')
      glyphNumber.style.position = 'absolute'
      glyphNumber.style.top = '2px'
      glyphNumber.style.left = '2px'
      glyphNumber.textContent = glyphs[row][col].toString()
      cell.appendChild(glyphNumber)

      // Mouse events for tooltip
      cell.addEventListener('mouseenter', (event) => {
        tooltip.innerHTML = ''
        for (const [key, value] of Object.entries(attrs[row][col])) {
          if (key !== 'font') {
            tooltip.innerHTML += `${key}:${value}<br>`
          }
        }
        tooltip.style.visibility = 'visible'
        tooltip.style.top = `${event.clientY + window.scrollY + 10}px`
        tooltip.style.left = `${event.clientX + window.scrollX + 10}px`
      })

      cell.addEventListener('mousemove', (event) => {
        tooltip.style.top = `${event.clientY + window.scrollY + 10}px`
        tooltip.style.left = `${event.clientX + window.scrollX + 10}px`
      })

      cell.addEventListener('mouseleave', () => {
        tooltip.style.visibility = 'hidden'
      })

      grid.appendChild(cell)
    }
  }
}
