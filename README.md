# crpl-vscode README

This extension adds syntax for the [Creeper Reverse Polish Language](https://knucklecracker.com/wiki/doku.php?id=crpl:overview), the scripting language from the level creator of the strategy game [Creeper World 3](https://knucklecracker.com/creeperworld3/cw3.php).

## Features

(TODO: add gifs)

- Syntax highlighting and diagnostics
- Wiki lookup on hover
- Autocompletion

### Planned

Various more advanced linting features are planned, including

- Checking arity and type of operation arguments
- Variable tracking (eg scope monitoring, "Find All References")
- Linting comments (eg `# lint disable`, `# lint stackAssert * n n l`)

## Extension Settings

None yet, although the more semantic linting diagnostics which are planned will probably have toggles.

## Known Issues

The parser/linter is still a little suspect. If you find any issues, please send the dev a copy of the .crpl file you are working on.