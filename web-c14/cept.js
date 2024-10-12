class CeptInterpreter {
  constructor() {
    this.setState(this.normal)
  }

  setState(handler) {
    this.state = handler.bind(this)
  }

  async next() {
    return 0
  }

  error() {
    console.log('error in data stream')
  }

  async normal() {
    const c = await this.next()
    switch (c) {
      case 0x08:
        // cursor left
        break
      case 0x09:
        // cursor right
        break
      case 0x0a:
        // cursor down
        break
      case 0x0b:
        // cursor up
        break
      case 0x0c:
        // clear screen
        break
      case 0x0d:
        // cursor to beginning of line
        break
      case 0x0e:
        // G1 into left charset
        break
      case 0x0f:
        // G0 into left charset
        break
      case 0x11:
        // show cursor
        break
      case 0x12:
        // repeat last printed character p[idx+1]-0x40 times
        break
      case 0x14:
        // hide cursor
        break
      case 0x18:
        // clear line
        break
      case 0x19:
        // switch to G2 for one character
        break
      case 0x1a:
        // end of page
        break
      case 0x1d:
        // switch to G3 for one character
        break
      case 0x1e:
        // cursor home
        break
      case 0x1b:
        // esc
        this.setState(this.esc)
        break
      case 0x1f:
        // esc
        this.setState(this.us)
        break
      case 0x9b:
        // csi
        this.setState(this.csi)
        break
      case 0x88:
        // blink on
        break
      case 0x89:
        // blink off
        break
      case 0x8a:
        // transparency on
        break
      case 0x8b:
        // transparency off
        break
      case 0x8c:
        // normal size
        break
      case 0x8d:
        // double height
        break
      case 0x8e:
        // double width
        break
      case 0x8f:
        // double width and height
        break
      case 0x98:
        // hide
        break
      case 0x99:
        // underline off
        break
      case 0x9a:
      // underline on
      case 0x9c:
        // Hintergrundfarbe schwarz bzw. normale Polarität
        break
      case 0x9d:
        // Hintergrundfarbe setzen bzw. inverse Polarität
        break
      case 0x9e:
        // Mosaikzeichenwiederholung bzw. Hintergrund transparent
        break
      default:
        if (0x80 <= c && c <= 0x87) {
          // set fg color to #${c - 0x80}
        } else if (0x90 <= c && c <= 0x97) {
          // set bg color to #${c - 0x90}
        }
    }
  }

  async esc() {
    switch (await this.next()) {
      case 0x22:
        switch (await this.next()) {
          case 0x40:
            // serial mode
            break
          case 0x41:
            // parallel mode
            break
          default:
            this.error()
        }
        break
      case 0x28:
        switch (await this.next()) {
          case 0x40:
            // load G0 into G0
            break
          case 0x62:
            // load G2 into G0
            break
          case 0x63:
            // load G1 into G0
            break
          case 0x64:
            // load G3 into G0
            break
          default:
            this.error()
        }
        break
      case 0x29:
        switch (await this.next()) {
          case 0x40:
            // load G0 into G1
            break
          case 0x62:
            // load G2 into G1
            break
          case 0x63:
            // load G1 into G1
            break
          case 0x64:
            // load G3 into G1
            break
          case 0x20:
            if ((await this.next()) === 0x40) {
              // load DRCs into G1
            } else {
              this.error()
            }
            break
          default:
            this.error()
        }
        break
      case 0x2a:
        switch (await this.next()) {
          case 0x40:
            // load G0 into G2
            break
          case 0x62:
            // load G2 into G2
            break
          case 0x63:
            // load G1 into G2
            break
          case 0x64:
            // load G3 into G2
            break
          case 0x20:
            if ((await this.next()) === 0x40) {
              // load DRCs into G2
            } else {
              this.error()
            }
            break
          default:
            this.error()
        }
        break
      case 0x2b:
        switch (await this.next()) {
          case 0x40:
            // load G0 into G3
            break
          case 0x62:
            // load G2 into G3
            break
          case 0x63:
            // load G1 into G3
            break
          case 0x64:
            // load G3 into G3
            break
          case 0x20:
            if ((await this.next()) === 0x40) {
              // load DRCs into G3
            } else {
              this.error()
            }
            break
          default:
            this.error()
        }
        break
      case 0x23:
        switch (await this.next()) {
          case 0x20: {
            const color = await this.next()
            switch (color & 0xf0) {
              case 0x40:
                // set fg color of screen to color & 0x0f
                break
              case 0x50:
                // set bg color of screen to color & 0x0f
                break
              default:
                this.error()
            }
            break
          }
          case 0x21: {
            const color = await this.next()
            switch (color & 0xf0) {
              case 0x40:
                // set fg color of line to color & 0x0f
                break
              case 0x50:
                // set bg color of line to color & 0x0f
                break
              default:
                this.error()
            }
            break
          }
          default:
            this.error()
        }
        break
      case 0x6e:
        // G2 into left charset
        break
      case 0x6f:
        // G3 into left charset
        break
      case 0x7c:
        // G3 into right charset
        break
      case 0x7d:
        // G2 into right charset
        break
      case 0x7e:
      // G1 into right charset
      default:
        this.error()
    }
    this.setState(this.normal)
  }

  async us() {
    switch (await this.next()) {
      case 0x23:
        await this.handleCharacterSet()
        break
      case 0x26:
        await this.handleColors()
        break
      case 0x2d:
        // set resolution to 40x24
        break
      case 0x2f:
        switch (await this.next()) {
          case 0x40:
            // service break to row ${p[idx + 3] - 0x40}
            break
          case 0x4f:
            // service break back
            break
          case 0x41:
            // serial mode
            break
          case 0x42:
            // parallel mode
            break
          case 0x43:
            // serial limited mode
            break
          case 0x44:
            // parallel limited mode
            break
          default:
            this.error()
        }
        break
      case 0x3d:
        await this.handleShortcuts()
        break
      default:
        await this.handleSetCursor()
    }
    this.setState(this.normal)
  }

  async csi() {
    const first = await this.next()
    const second = await this.next()

    if (first === 0x30 && second === 0x40) {
      // select palette #0
    } else if (first === 0x30 && second === 0x41) {
      // invert blinking
    } else if (first === 0x31 && second === 0x40) {
      // select palette #1
    } else if (first === 0x31 && second === 0x41) {
      // blink palettes 0/1 or 2/3
    } else if (first === 0x31 && second === 0x51) {
      // unprotect line
    } else if (first === 0x31 && second === 0x50) {
      // protect line
    } else if (first === 0x32 && second === 0x40) {
      // select palette #2
    } else if (first === 0x32 && second === 0x41) {
      // fast blinking (on, off, off)
    } else if (first === 0x32 && second === 0x53) {
      // start selection
    } else if (first === 0x32 && second === 0x54) {
      // end selection
    } else if (first === 0x33 && second === 0x40) {
      // select palette #3
    } else if (first === 0x33 && second === 0x41) {
      // fast blinking (off, on, off)
    } else if (first === 0x34 && second === 0x41) {
      // fast blinking (off, off, on)
    } else if (first === 0x35 && second === 0x41) {
      // blinking shift right
    } else if (first === 0x36 && second === 0x41) {
      // blinking shift left
    } else {
      this.error()
    }
    this.setState(this.normal)
  }
}

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
