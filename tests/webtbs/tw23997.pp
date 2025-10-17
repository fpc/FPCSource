{ %NORUN }

program tw23997;

{$MODE fpc}

type TMyFunc = function (x: single): single;

function Foo (x: array of single): single; overload;
begin
  Foo := 2*x[0];
end;

function Foo (x: single): single; overload;
begin
  Foo := 2*x;
end;

procedure GoAhead (x: single; func: TMyFunc);
begin
  writeln (func (x));
end;

begin
  GoAhead (42, @Foo);
  //compiler stops on the first definition of foo
  //which does not match TMyFunc
end.

