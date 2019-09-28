# crpl-vscode README

This extension adds syntax for the [Creeper Reverse Polish Language](https://knucklecracker.com/wiki/doku.php?id=crpl:overview), the scripting language from the level creator of the strategy game [Creeper World 3](https://knucklecracker.com/creeperworld3/cw3.php).

It is no longer in development, but anyone is welcome to pick up the torch if they don't mind combing through the mess.

## Features

(TODO: add gifs)

- Syntax highlighting and diagnostics
- Wiki lookup on hover
- Variable tracking ("Find All References" and the like)
- Autocompletion

### Planned

Various more advanced linting features were planned and partially implemented in dev, including

- Checking arity and type of operation arguments
- Advanced variable tracking (scope monitoring)
- Linting comments (eg `# lint disable`, `# lint stackAssert * n n l`)

## Extension Settings

- `crpl.live`: whether or not to check the syntax every time you edit. Turn it off if you have trouble with large or extensively commented files. You can use a command (Shift-Ctrl-P or Shift-Cmd-P) to toggle this for individual files.
