import { builtinModules } from 'module'
import prettier from 'eslint-plugin-prettier'

export default [
  {
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
      globals: {
        ...builtinModules.reduce((acc, mod) => {
          acc[mod] = 'readonly'
          return acc
        }, {}),
        browser: true,
        es2021: true,
        node: true,
      },
    },
    ignores: ['node_modules/**', 'public/**'],
    plugins: {
      prettier,
    },
    rules: {
      'prettier/prettier': 'error',
    },
  },
]
