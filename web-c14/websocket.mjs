
const makeWebsocket = () => {
    const buffer = [];
    let index = 0;
    const currentChunk = [];
    let resolveNext;

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

    const log = (message, ...args) =>
        console.log(`${currentChunkAsString().padEnd(24)} ${message}`, ...args);

    const error = (msg) => {
        console.error(`Error interpreting ${currentChunkAsString()}`, msg);
    };

    const socketUrl = `ws://${window.location.host}/cept/ws`;
    console.log(`opening websocket at ${socketUrl}`)
    const socket = new WebSocket(socketUrl);
    console.log('websocket opened', socket)

    const send = (data) => {
        socket.send(data)
    }

    socket.onmessage = async (event) => {
        const reader = new FileReader();
        reader.onload = () => {
            const arrayBuffer = reader.result;
            const data = new Uint8Array(arrayBuffer);
            buffer.push(...data);
            if (resolveNext) {
                resolveNext();
                resolveNext = null;
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
