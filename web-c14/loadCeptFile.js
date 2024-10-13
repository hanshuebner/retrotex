import fs from 'fs';
import ceptDecoder from './ceptDecoder.mjs';
import ceptInterpreter from './ceptInterpreter.mjs';

if (process.argv.length < 3) {
    console.error('Usage: node loadCeptFile.mjs <inputFile>')
    process.exit(1)
}

const inputFile = process.argv[2]
const buffer = fs.readFileSync(inputFile)
const currentChunk = []
let index = 0

const next = async () => {
    if (index >= buffer.length) {
        throw new Error('End of file reached')
    }
    currentChunk.push(buffer[index])
    return buffer[index++]
}

const putback = () => {
    currentChunk.pop()
    index--
}

const currentChunkAsString = () => currentChunk.map(c => c.toString(16).padStart(2, '0')).join(' ').toUpperCase()

const log = (message, ...args) =>
    console.log(`${currentChunkAsString().padEnd(24)} ${message}`, ...args)

const error = (msg) => {
    console.error(`Error interpreting ${currentChunkAsString()}`, msg)
    process.exit(1)
}

const runDecoder = async () => {
    try {
        while (index < buffer.length) {
            await ceptDecoder(ceptInterpreter(log), next, putback, error)
            currentChunk.length = 0
        }
    } catch (err) {
        console.error('Exception:', err)
    }
}

runDecoder()
