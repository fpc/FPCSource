program Example31;

{ Program to demonstrate the Dup function. }

uses oldlinux;

var f : text;

begin
  if not dup (output,f) then
    Writeln ('Dup Failed !');
  writeln ('This is written to stdout.');
  writeln (f,'This is written to the dup file, and flushed');flush(f);
  writeln
end.