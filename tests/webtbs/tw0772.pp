var
    t : text;
type tFoo = object
       a:integer;
       constructor Create;
       procedure ReadA;
       procedure ShowA;
     end;

constructor tFoo.Create;
begin
  a:=0;
end;

procedure tFoo.ReadA;
begin
  write('a: '); Readln(t,a);
end;

procedure tFoo.ShowA;
begin
  writeln('A=',a);
end;

var Foo:tFoo;
begin

  assign(t,'tbug772.tmp');
  rewrite(t);
  writeln(t,'4');
  close(t);
  reset(t);
  Foo.Create;
  Foo.ReadA; {this leaves Foo.a untouched, but it should'nt}
  Foo.ShowA;
  if Foo.A<>4 then
   Halt(1);
  close(t);
  erase(t);
end.
