import esbuild from 'esbuild'

const plugins = [
  {
    name: 'notifyBuild',
    setup(build) {
      build.onEnd((result) => {
        const now = new Date().toISOString()
        const errors = result.errors.length
        const warnings = result.warnings.length
        let badge = ''

        if (errors > 0) {
          badge = '\x1b[41m ERROR \x1b[0m' // Red background
        } else if (warnings > 0) {
          badge = '\x1b[43m WARNING \x1b[0m' // Orange background
        } else {
          badge = '\x1b[42m SUCCESS \x1b[0m' // Green background
        }

        let message = `${now} ${badge}`
        if (errors > 0) {
          message += ` Errors: ${errors}`
        }
        if (warnings > 0) {
          message += ` Warnings: ${warnings}`
        }

        console.log(message)
      })
    },
  },
]

const build = async () => {
  const watch = process.argv.includes('--watch')
  const debug = process.argv.includes('--debug')

  const buildOptions = {
    entryPoints: ['./src/terminal.ts'],
    bundle: true,
    outfile: './public/bundle.js',
    sourcemap: true,
    minify: !debug && !watch,
    target: ['es6'],
    platform: 'browser',
    loader: {
      '.ts': 'ts',
    },
    plugins,
  }

  const context = await esbuild.context(buildOptions)

  if (watch) {
    await context.watch()
    console.log('Watching for changes...')
  } else {
    await context.rebuild()
    console.log('Build completed')
    await context.dispose() // Clean up the context
  }
}

build().catch((e) => {
  console.log(e)
  process.exit(1)
})
