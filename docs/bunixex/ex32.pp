program Example31;

{ Program to demonstrate the Dup function. }

uses BaseUnix;

var f : text;
    i : longint;

begin
  Assign (f,'text.txt');
  Rewrite (F);
  For i:=1 to 10 do writeln (F,'Line : ',i);
  if fpdup2 (output,f)<>0 then
    Writeln ('Dup2 Failed !');
  writeln ('This is written to stdout.');
  writeln (f,'This is written to the dup file, and flushed');
  flush(f);
  writeln;
  { Remove file. Comment this if you want to check flushing.}
  fpUnlink ('text.txt');
end.