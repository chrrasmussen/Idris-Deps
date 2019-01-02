# Idris-Deps

List dependencies of an Idris project. List all unique module names, display a dependency tree or find usages of a specific module.

This command line tool does not compile the Idris files, it merely inspects them for `import` statements. It classifies modules as an external module (Lib) or a local module based on their existence in the local folder.


## Building the command line tool

Build the tool with the following command (Tested with Idris v1.3.1):

```
idris --build deps.ipkg
```

This will create an executable named `deps` in the project folder.


## Usage

The executable expects the path to the root folder where the Idris files are located as well as the namespace of the entry point. It does not support `.ipkg` files.

```
Usage: ./deps <rootDir> <mainModule> [--list-all | --list-local | --list-external | --tree | --uses <module>]
```


## Examples

The following examples are run on this repository.


### List all/local/external

```
./deps src Main --list-all
```

Output:

```
Control.Monad.State (Lib)
Data.SortedMap (Lib)
Data.SortedSet (Lib)
Data.Tree
Deps.Data
Deps.Lexer
Deps.Parser
Main
System (Lib)
Text.Lexer (Lib)
Text.Parser (Lib)
```


### Show dependency tree

The dependency tree is traversed depth-first. Note that a module is only displayed once.

```
./deps src Main --tree
```

Output:

```
Main
|
+- System (Lib)
|
+- Control.Monad.State (Lib)
|
+- Data.SortedMap (Lib)
|
+- Data.SortedSet (Lib)
|
+- Data.Tree
|
+- Deps.Data
|
+- Deps.Lexer
|  |
|  `- Text.Lexer (Lib)
|
`- Deps.Parser
   |
   +- Text.Parser (Lib)
   |
   +- Deps.Data
   |
   `- Deps.Lexer
```


### Show usages of a module

```
./deps src Main --uses Deps.Data
```

Output:

```
Deps.Parser
Main
```


## License

MIT. See LICENSE.
