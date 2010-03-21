{ %fail }

PROGRAM test;

{$mode objfpc}
var a,b: string;

begin
  a:= 'Test A';
  b:= 'B Test';
  system.insert(a,'ing',5);
  system.insert('H World','allo',2);
  system.insert('&B',b,2);
  writeln(a);
  writeln(b);
end.

