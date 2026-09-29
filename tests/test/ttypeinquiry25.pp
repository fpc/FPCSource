{ %FAIL }
{ "type of" a class field inside its own declaration }
program ttypeinquiry25;

{$mode objfpc}

type
  TBird = class
    f: type of f;
  end;

begin
end.
