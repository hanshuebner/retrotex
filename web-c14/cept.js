const fs = require('fs')

const MIN = (a, b) => (a < b ? a : b)
const MAX = (a, b) => (a > b ? a : b)

const HEX_PER_LINE = 16

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

const main = () => {
  const buffer = fs.readFileSync(process.argv[2])
  const p = Buffer.from(buffer)
  const totalLength = p.length
  let idx = 0

  while (idx < totalLength) {
    let l = 1
    let d = ''
    const tmpstr = []
    if (isPrintable(p[idx]) && p[idx + 1] === 0x12 && p[idx + 2] >= 0x41) {
      d = 'repeat {p[idx]} for {p[idx+2]-0x40} times'
    } else if (p[idx] === 0x08) {
      d = 'cursor left'
    } else if (p[idx] === 0x09) {
      d = 'cursor right'
    } else if (p[idx] === 0x0a) {
      d = 'cursor down'
    } else if (p[idx] === 0x0b) {
      d = 'cursor up'
    } else if (p[idx] === 0x0c) {
      d = 'clear screen'
    } else if (p[idx] === 0x0d) {
      d = 'cursor to beginning of line'
    } else if (p[idx] === 0x0e) {
      d = 'G1 into left charset'
    } else if (p[idx] === 0x0f) {
      d = 'G0 into left charset'
    } else if (p[idx] === 0x11) {
      d = 'show cursor'
    } else if (p[idx] === 0x14) {
      d = 'hide cursor'
    } else if (p[idx] === 0x18) {
      d = 'clear line'
    } else if (p[idx] === 0x19) {
      d = 'switch to G2 for one character'
    } else if (p[idx] === 0x1a) {
      d = 'end of page'
    } else if (p[idx] === 0x1d) {
      d = 'switch to G3 for one character'
    } else if (p[idx] === 0x1e) {
      d = 'cursor home'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x22 && p[idx + 2] === 0x40) {
      l = 3
      d = 'serial mode'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x22 && p[idx + 2] === 0x41) {
      l = 3
      d = 'parallel mode'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x23 && p[idx + 2] === 0x20 && (p[idx + 3] & 0xf0) === 0x40) {
      l = 4
      tmpstr.push(`set fg color of screen to ${p[idx + 3] - 0x40}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x23 && p[idx + 2] === 0x20 && (p[idx + 3] & 0xf0) === 0x50) {
      l = 4
      tmpstr.push(`set bg color of screen to ${p[idx + 3] - 0x50}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x23 && p[idx + 2] === 0x21 && (p[idx + 3] & 0xf0) === 0x40) {
      l = 4
      tmpstr.push(`set fg color of line to ${p[idx + 3] - 0x40}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x23 && p[idx + 2] === 0x21 && (p[idx + 3] & 0xf0) === 0x50) {
      l = 4
      tmpstr.push(`set bg color of line to ${p[idx + 3] - 0x50}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x28 && p[idx + 2] === 0x20 && p[idx + 3] === 0x40) {
      l = 4
      d = 'load DRCs into G0'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x28 && p[idx + 2] === 0x40) {
      l = 3
      d = 'load G0 into G0'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x28 && p[idx + 2] === 0x62) {
      l = 3
      d = 'load G2 into G0'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x28 && p[idx + 2] === 0x63) {
      l = 3
      d = 'load G1 into G0'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x28 && p[idx + 2] === 0x64) {
      l = 3
      d = 'load G3 into G0'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x29 && p[idx + 2] === 0x20 && p[idx + 3] === 0x40) {
      l = 4
      d = 'load DRCs into G1'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x29 && p[idx + 2] === 0x40) {
      l = 3
      d = 'load G0 into G1'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x29 && p[idx + 2] === 0x62) {
      l = 3
      d = 'load G2 into G1'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x29 && p[idx + 2] === 0x63) {
      l = 3
      d = 'load G1 into G1'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x29 && p[idx + 2] === 0x64) {
      l = 3
      d = 'load G3 into G1'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2a && p[idx + 2] === 0x20 && p[idx + 3] === 0x40) {
      l = 4
      d = 'load DRCs into G2'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2a && p[idx + 2] === 0x40) {
      l = 3
      d = 'load G0 into G2'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2a && p[idx + 2] === 0x62) {
      l = 3
      d = 'load G2 into G2'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2a && p[idx + 2] === 0x63) {
      l = 3
      d = 'load G1 into G2'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2a && p[idx + 2] === 0x64) {
      l = 3
      d = 'load G3 into G2'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2b && p[idx + 2] === 0x20 && p[idx + 3] === 0x40) {
      l = 4
      d = 'load DRCs into G3'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2b && p[idx + 2] === 0x40) {
      l = 3
      d = 'load G0 into G3'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2b && p[idx + 2] === 0x62) {
      l = 3
      d = 'load G2 into G3'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2b && p[idx + 2] === 0x63) {
      l = 3
      d = 'load G1 into G3'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x2b && p[idx + 2] === 0x64) {
      l = 3
      d = 'load G3 into G3'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x6e) {
      l = 2
      d = 'G2 into left charset'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x6f) {
      l = 2
      d = 'G3 into left charset'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x7c) {
      l = 2
      d = 'G3 into right charset'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x7d) {
      l = 2
      d = 'G2 into right charset'
    } else if (p[idx] === 0x1b && p[idx + 1] === 0x7e) {
      l = 2
      d = 'G1 into right charset'
    } else if (
      p[idx] === 0x1f &&
      p[idx + 1] === 0x23 &&
      p[idx + 2] === 0x20 &&
      (p[idx + 3] & 0xf0) === 0x40 &&
      (p[idx + 4] & 0xf0) === 0x40
    ) {
      l = 5
      tmpstr.push(`define characters ${decodeRes(p[idx + 3])}, ${decodeCol(p[idx + 4])} colors`)
      d = tmpstr.join('')
    } else if (
      p[idx] === 0x1f &&
      p[idx + 1] === 0x23 &&
      p[idx + 2] === 0x20 &&
      p[idx + 3] === 0x28 &&
      p[idx + 4] === 0x20 &&
      p[idx + 5] === 0x40 &&
      (p[idx + 6] & 0xf0) === 0x40 &&
      (p[idx + 7] & 0xf0) === 0x40
    ) {
      l = 8
      tmpstr.push(`clear character set ${decodeRes(p[idx + 6])}, ${decodeCol(p[idx + 7])} colors`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x23 && p[idx + 3] === 0x30) {
      l = 4
      tmpstr.push(`define characters 0x${p[idx + 2].toString(16)}+`)
      d = tmpstr.join('')
      let q = idx + 4
      // this assumes that the DRC definition is followed by another command starting with 1F, which may well not be the case
      while (q < p.length && p[q] !== 0x1f) {
        // Handle specific case
      }
    } else if (
      p[idx] === 0x1f &&
      p[idx + 1] === 0x26 &&
      p[idx + 2] === 0x20 &&
      p[idx + 3] === 0x22 &&
      p[idx + 4] === 0x20 &&
      p[idx + 5] === 0x35 &&
      p[idx + 6] === 0x40
    ) {
      l = 7
      d = 'start defining colors for DRCs'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x26 && p[idx + 2] === 0x20) {
      l = 3
      d = 'start defining colors'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x26 && p[idx + 2] === 0x21) {
      l = 3
      d = 'reset palette'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x26 && (p[idx + 2] & 0xf0) === 0x30 && (p[idx + 3] & 0xf0) === 0x30) {
      l = 4
      d = tmpstr.join('')
      let q = idx + 4
      const q_old = q
      while (p[q] !== 0x1f) {
        // Handle specific case
      }
      tmpstr.push(`define colors ${String.fromCharCode(p[idx + 2])}${String.fromCharCode(p[idx + 3])}+: `)
      const s = tmpstr.join('')
      printPalette(s, q_old, q - q_old)
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x26 && (p[idx + 2] & 0xf0) === 0x30) {
      l = 3
      tmpstr.push(`define DRC color ${p[idx + 2] - 0x30}`)
      d = tmpstr.join('')
      let q = idx + 3
      while (p[q] !== 0x1f) {
        // Handle specific case
      }
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2d) {
      l = 2
      d = 'set resolution to 40x24'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x40) {
      l = 4
      tmpstr.push(`service break to row ${p[idx + 3] - 0x40}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x4f) {
      l = 3
      d = 'service break back'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x41) {
      l = 3
      d = 'serial mode'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x42) {
      l = 3
      d = 'parallel mode'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x43) {
      l = 3
      d = 'serial limited mode'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x2f && p[idx + 2] === 0x44) {
      l = 3
      d = 'parallel limited mode'
    } else if (p[idx] === 0x1f && p[idx + 1] === 0x3d && (p[idx + 2] & 0xf0) >= 0x30) {
      if (p[idx + 3] === 0x1f) {
        l = 3
        tmpstr.push(`define shortcut #${p[idx + 2] - 0x30}`)
        d = tmpstr.join('')
      } else {
        l = 5
        tmpstr.push(
          `define shortcut #${p[idx + 2] - 0x30} "${String.fromCharCode(p[idx + 3])}${String.fromCharCode(p[idx + 4])}"`
        )
        d = tmpstr.join('')
      }
    } else if (p[idx] === 0x1f && p[idx + 1] >= 0x41 && p[idx + 2] >= 0x41) {
      l = 3
      tmpstr.push(`set cursor to line ${p[idx + 1] - 0x40}, column ${p[idx + 2] - 0x40}`)
      d = tmpstr.join('')
    } else if (isPrintable(p[idx]) && p[idx + 1] !== 0x12) {
      let q = idx
      l = 0
      tmpstr.push('"')
      while (l < HEX_PER_LINE && isPrintable(q[0]) && q[1] !== 0x12) {
        tmpstr.push(String.fromCharCode(q[0]))
        l += 1
        q += 1
      }
      tmpstr.push('"')
      d = tmpstr.join('')
    } else if (0x80 <= p[idx] && p[idx] <= 0x87) {
      tmpstr.push(`set fg color to #${p[idx] - 0x80}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x88) {
      d = 'blink on'
    } else if (p[idx] === 0x89) {
      d = 'blink off'
    } else if (p[idx] === 0x8a) {
      d = 'transparency on'
    } else if (p[idx] === 0x8b) {
      d = 'transparency off'
    } else if (p[idx] === 0x8c) {
      d = 'normal size'
    } else if (p[idx] === 0x8d) {
      d = 'double height'
    } else if (p[idx] === 0x8e) {
      d = 'double width'
    } else if (p[idx] === 0x8f) {
      d = 'double width and height'
    } else if (0x90 <= p[idx] && p[idx] <= 0x97) {
      tmpstr.push(`set bg color to #${p[idx] - 0x90}`)
      d = tmpstr.join('')
    } else if (p[idx] === 0x98) {
      d = 'hide'
    } else if (p[idx] === 0x99) {
      d = 'underline off'
    } else if (p[idx] === 0x9a) {
      d = 'underline on'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x30 && p[idx + 2] === 0x40) {
      l = 3
      d = 'select palette #0'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x30 && p[idx + 2] === 0x41) {
      l = 3
      d = 'invert blinking'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x31 && p[idx + 2] === 0x40) {
      l = 3
      d = 'select palette #1'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x31 && p[idx + 2] === 0x41) {
      l = 3
      d = 'blink palettes 0/1 or 2/3'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x31 && p[idx + 2] === 0x51) {
      l = 3
      d = 'unprotect line'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x31 && p[idx + 2] === 0x50) {
      l = 3
      d = 'protect line'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x32 && p[idx + 2] === 0x40) {
      l = 3
      d = 'select palette #2'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x32 && p[idx + 2] === 0x41) {
      l = 3
      d = 'fast blinking (on, off, off)'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x32 && p[idx + 2] === 0x53) {
      l = 3
      d = 'start selection'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x32 && p[idx + 2] === 0x54) {
      l = 3
      d = 'end selection'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x33 && p[idx + 2] === 0x40) {
      l = 3
      d = 'select palette #3'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x33 && p[idx + 2] === 0x41) {
      l = 3
      d = 'fast blinking (off, on, off)'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x34 && p[idx + 2] === 0x41) {
      l = 3
      d = 'fast blinking (off, off, on)'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x35 && p[idx + 2] === 0x41) {
      l = 3
      d = 'blinking shift right'
    } else if (p[idx] === 0x9b && p[idx + 1] === 0x36 && p[idx + 2] === 0x41) {
      l = 3
      d = 'blinking shift left'
    } else if (p[idx] === 0x9c) {
      d = 'Hintergrundfarbe schwarz bzw. normale Polarität'
    } else if (p[idx] === 0x9d) {
      d = 'Hintergrundfarbe setzen bzw. inverse Polarität'
    } else if (p[idx] === 0x9e) {
      d = 'Mosaikzeichenwiederholung bzw. Hintergrund transparent'
    } else {
      d = 'unknown'
    }

    while (l > 0) {
      const ll = MIN(l, HEX_PER_LINE)

      for (let i = 0; i < ll; i++) {
        process.stdout.write(`${p[idx].toString(16).padStart(2, '0')} `)
        idx += 1
      }
      for (let i = 0; i < 3 * (HEX_PER_LINE - ll); i++) {
        process.stdout.write(' ')
      }
      if (d) {
        console.log(`# ${d}`)
      } else {
        console.log()
      }
      d = ''
      l -= ll
    }
  }
}

if (require.main === module) {
  main()
}
