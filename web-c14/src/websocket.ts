export interface Websocket {
  next: () => Promise<number>
  putback: () => void
  currentChunk: number[]
  send: (data: Uint8Array) => void
}

const makeWebsocket = (): Websocket => {
  const buffer: number[] = []
  let index = 0
  const currentChunk: number[] = []
  let resolveNext: undefined | ((value: unknown) => void)

  const next = async () => {
    if (index >= buffer.length) {
      await new Promise((resolve) => (resolveNext = resolve))
    }
    currentChunk.push(buffer[index])
    return buffer[index++]
  }

  const putback = () => {
    currentChunk.pop()
    index--
  }

  const currentChunkAsString = () =>
    currentChunk
      .map((c) => c.toString(16).padStart(2, '0'))
      .join(' ')
      .toUpperCase()

  const log = (message: any, ...args: any[]) =>
    console.log(`${currentChunkAsString().padEnd(24)} ${message}`, ...args)

  const error = (msg: any) => {
    console.error(`Error interpreting ${currentChunkAsString()}`, msg)
  }

  const socketUrl = `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.host}/cept/ws`
  let socket: WebSocket

  const send = (data: Uint8Array) => {
    if (socket) {
      socket?.send(data)
    } else {
      console.log('cannot send, WebSocket not connected')
    }
  }

  const openWebsocket = () => {
    console.log('Opening WebSocket', socketUrl)
    socket = new WebSocket(socketUrl)
    console.log('WebSocket opened', socket)

    socket.onmessage = async (event) => {
      const reader = new FileReader()
      reader.onload = () => {
        const arrayBuffer = reader.result
        const data = new Uint8Array(arrayBuffer as ArrayBuffer)
        buffer.push(...data)
        if (resolveNext) {
          resolveNext(null)
          resolveNext = undefined
        }
      }
      reader.readAsArrayBuffer(event.data)
    }

    socket.onerror = (event) => {
      console.error('WebSocket error:', event)
    }

    socket.onclose = (event) => {
      console.log('WebSocket closed:', event)
      setTimeout(openWebsocket, 1000)
    }
  }

  openWebsocket()

  return { next, putback, currentChunk, send }
}

export default makeWebsocket
