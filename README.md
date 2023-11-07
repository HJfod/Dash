# Dash

A language for developing UIs, mainly intended for Geometry Dash mods.

Previously known as GDML.

> THIS IS NOT FINISHED! IT DOESN'T WORK! AT ALL! IT'S NOT FINISHED! DON'T TRY TO USE IT! PLEASE!!!

## Example

> THIS LANGUAGE IS NOT FINISHED! THIS EXAMPLE WILL **NOT** RUN!

```dart
CCLabelBMFont {
    text: "Omg god",
    pos: winSize / 2,
}
```

## Architechture

The source code for Dash is hosted in this monorepo. Here's the structure:

 * `compiler` contains the compiler for Dash written in Rust :crab:
 * `mod` contains the Dash runtime mod for GD
 * `cli` contains the command-line Dash compiler
 * `vscode` contains the VS Code Dash extension
 * `test` contains test files

The way Dash works is that it is composed of three parts: a compiler written in Rust, a runtime written in C++, and development tooling written in various languages. The compiler parses Dash source code and compiles it into Dash bytecode. It is independent of Geometry Dash, so it can be invoked from development tools aswell. The runtime is in the form of a Geode mod that executes Dash bytecode. The runtime mod also has access to the compiler, so it can compile Dash source code to bytecode and execute it on the fly. Since executing the bytecode requires Geometry Dash, development tools can't execute it, but they can use the compiler to perform type analysis among other stuff.
