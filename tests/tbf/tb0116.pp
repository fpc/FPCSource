{ %FAIL }
{$ifdef fpc}{$mode objfpc}{$endif}
type
  tobj1 = class
      procedure proc1 (a: char);
  end;

  tobj2 = class (tobj1)
      procedure proc1 (a: integer);
  end;

procedure tobj1.proc1 (a: char);
begin
  write('tobj1.proc1(a:char) called: ');
  writeln (a);
end;

procedure tobj2.proc1 (a: integer);
begin
  write('tobj2.proc1(a:integer) called: ');
  writeln (a);
end;

var
    obj1: tobj1;
    obj2: tobj2;
begin
  obj1:=tobj1.create;
  obj2:=tobj2.create;

  obj2.proc1 ('a');
end.
