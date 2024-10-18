import esbuild from 'esbuild'

const build = async () => {

    const context = await esbuild.build({
        entryPoints: ['./src/terminal.ts'],
        bundle: true,
        outfile: './public/bundle.js',
        sourcemap: true,
        minify: true,
        target: ['es6'],
        platform: 'browser',
        loader: {
            '.ts': 'ts',
        },
    });

    if (process.argv.includes('--watch')) {
        await context.watch();
        console.log('Watching for changes...');
    } else {
        await context.rebuild();
        console.log('Build completed');
    }
}

build().catch(() => process.exit(1));
