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
  write('a: '); Readln(a);
end;

procedure tFoo.ShowA;
begin
  writeln('A=',a);
end;

var Foo:tFoo;
begin
  Foo.Create;
  Foo.ReadA; {this leaves Foo.a untouched, but it should'nt}
  Foo.ShowA;
end.
