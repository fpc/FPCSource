Program Example9;

{ Program to demonstrate the Close function. }

Var F : text;

begin
 Assign (f,'Test.txt');
 ReWrite (F);
 Writeln (F,'Some text written to Test.txt');
 close (f); { Flushes contents of buffer to disk,
              closes the file. Omitting this may
              cause data NOT to be written to disk.}
end.

