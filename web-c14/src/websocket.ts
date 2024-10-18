export interface Websocket {
    next: () => Promise<number>;
    putback: () => void;
    currentChunk: number[];
    send: (data: Uint8Array) => void;
}

const makeWebsocket = (): Websocket => {
    const buffer: number[] = [];
    let index = 0;
    const currentChunk: number[] = [];
    let resolveNext: undefined | ((value: unknown) => void);

    const next = async () => {
        if (index >= buffer.length) {
            await new Promise(resolve => resolveNext = resolve);
        }
        currentChunk.push(buffer[index]);
        return buffer[index++];
    };

    const putback = () => {
        currentChunk.pop();
        index--;
    };

    const currentChunkAsString = () => currentChunk.map(c => c.toString(16).padStart(2, '0')).join(' ').toUpperCase();

    const log = (message: any, ...args: any[]) =>
        console.log(`${currentChunkAsString().padEnd(24)} ${message}`, ...args);

    const error = (msg: any) => {
        console.error(`Error interpreting ${currentChunkAsString()}`, msg);
    };

    const socketUrl = `ws://${window.location.host}/cept/ws`;
    console.log(`opening websocket at ${socketUrl}`)
    const socket = new WebSocket(socketUrl);
    console.log('websocket opened', socket)

    const send = (data: Uint8Array) => {
        socket.send(data)
    }

    socket.onmessage = async (event) => {
        const reader = new FileReader();
        reader.onload = () => {
            const arrayBuffer = reader.result;
            const data = new Uint8Array(arrayBuffer as ArrayBuffer);
            buffer.push(...data);
            if (resolveNext) {
                resolveNext(null);
                resolveNext = undefined;
            }
        };
        reader.readAsArrayBuffer(event.data);
    };
    socket.onerror = (event) => {
        console.error('WebSocket error:', event);
    };

    socket.onclose = (event) => {
        console.log('WebSocket closed:', event);
    };

    return { next, putback, currentChunk, send }
}

export default makeWebsocket
