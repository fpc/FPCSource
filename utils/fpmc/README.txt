This is the Free Pascal Message Compiler.

It compiles a .mc file into a .rc and .msg file which can be compiled by a
resource compiler such as windres. For example:

fpmc -l 90 -e -v -i test.mc -p -r -m
windres -i test.rc -o test.res

It also produces a .pp pascal unit which contains constant definitions
for any message aliases (SymbolicName) found in the .mc file.

The compiler recognizes the following directives in the .mc file:
MessageID
SymbolicName
Language

Other directives as defined by Microsoft are not yet supported.

The readmsg program reads and dumps a .msg message file to screen.
dumpfile produces a hex dump of a file. They were mainly used for debugging.

Enjoy !

Michael.
