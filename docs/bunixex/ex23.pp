Program Example23;

{ Program to demonstrate the Chmod function. }

Uses BaseUnix,Unix;

Var F : Text;

begin
  { Create a file }
  Assign (f,'testex21');
  Rewrite (F);
  Writeln (f,'#!/bin/sh');
  Writeln (f,'echo Some text for this file');
  Close (F);
  fpChmod ('testex21',&777);
  { File is now executable  }
  execl ('./testex21');
end.
