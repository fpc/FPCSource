instantfpc
==========

This tool allows to execute pascal programs as unix scripts.
A unix script starts with a shebang #! and the program to execute. For example

#!/usr/bin/env instantfpc
begin
  writeln('It works');
end.

If you save the above file as test.pas and set the execute permission
(chmod a+x) you can execute the script simply with
./test.pas


Installation
============

1. Compile instantfpc.lpi using lazarus, lazbuild or via "fpc instantfpc.lpr"
2. Put the executable "instantfpc" in PATH, for example into
   /usr/bin/instantfpc or ~/bin/instantfpc.

That's all.
Now you can execute pascal programs as scripts.

