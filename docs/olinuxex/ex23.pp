Program Example23;

{ Program to demonstrate the Chmod function. }

Uses oldlinux;

Var F : Text;

begin
  { Create a file }
  Assign (f,'testex21');
  Rewrite (F);
  Writeln (f,'#!/bin/sh');
  Writeln (f,'echo Some text for this file');
  Close (F);
  { Octal() makes the correct number from a
    number that LOOKS octal }
  Chmod ('testex21',octal (777));
  { File is now executable  }
  execl ('./testex21');
end.
