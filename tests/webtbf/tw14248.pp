{ %fail }
program Project1;
{$mode objfpc}{$H+}

function Foo(a: Integer): Integer;
begin
  dec(a);
  if a < 0 then exit;
  Result := Result(a);
end;

begin
  Foo(10);
end.
