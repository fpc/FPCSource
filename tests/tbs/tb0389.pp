{ %VERSION=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}
type
  tobj = class
      procedure proc1 (a: integer);overload;virtual;
      procedure proc1 (a: extended);overload;
  end;

  tobj1 = class(tobj)
      procedure proc1 (a: integer);overload;override;
      procedure proc1 (a: char);overload;
  end;

  tobj2 = class (tobj1)
      procedure proc1 (a: integer);override;
  end;

procedure tobj.proc1 (a: integer);
begin
  write('tobj.proc1(a:integer) called: ');
  writeln (a);
end;

procedure tobj.proc1 (a: extended);
begin
  write('tobj.proc1(a:extended) called: ');
  writeln (a);
end;

procedure tobj1.proc1 (a: integer);
begin
  write('tobj1.proc1(a:integer) called: ');
  writeln (a);
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

  obj2.proc1 (100);
  obj2.proc1 ('a');
  obj2.proc1 (123.456);
end.
