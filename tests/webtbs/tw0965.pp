{ %version=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}

type
  pobj = ^tobj;
  tobj = object
    public
      constructor init;
      destructor done; virtual;

      procedure proc1 (a: integer);overload; virtual;
      procedure proc1 (a: double);overload; virtual;
  end;

  pobj2 = ^tobj2;
  tobj2 = object (tobj)
      procedure proc1 (a: integer);overload;virtual;
  end;

var
  error : boolean;

constructor tobj.init;
begin
end;

destructor tobj.done;
begin
end;

procedure tobj.proc1 (a: integer);
begin
write('tobj.proc1(a:integer) called: ');
writeln (a);
end;

procedure tobj.proc1 (a: double);
begin
write('tobj.proc1(a:double) called: ');
writeln (a);
error:=false;
end;

procedure tobj2.proc1 (a: integer);
begin
write('tobj2.proc1(a:integer) called: ');
writeln (a);
end;

var
    obj1: pobj;
    obj2: pobj2;

begin
error:=true;
    new (obj1, init);
    new (obj2, init);
    obj2^.proc1 (444.5555);
if error then
 halt(1);

end.
