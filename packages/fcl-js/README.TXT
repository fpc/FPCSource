This is a package that contains a Javascript Scanner/parser/Syntax tree.

The following units are defined:

jsbase: the definition of Javascript values. Used to represent constant values.
jstree: The Javascript syntax tree elements. Used in the parser to describe a source program
jsscanner: the Javascript scanner. Currently not yet unicode-enabled.
jsparser: the Javascript parser. Builds a complete javascript syntax tree.

The tests directory contains a set of FPCUnit tests to test the scanner and parser.
It needs Lazarus to run.

Todo:
- Add more tests.
- Unicode support.
- Runtime-engine ?

The idea for the tree elements and the parser come from the Libsee library,
written by David Leonard.

Enjoy!

Michael.