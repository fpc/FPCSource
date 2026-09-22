{ %FAIL }
{ A type identifier is not a valid operand of "type of". }
program ttypeinquiry9;

{$mode objfpc}

var
  v: type of Integer;
begin
  v:=1;
end.
