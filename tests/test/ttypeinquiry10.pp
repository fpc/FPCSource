{ %FAIL }
{ The operand of "type of" must still type check. }
program ttypeinquiry10;

{$mode objfpc}

var
  v: type of NotDeclared;
begin
  v:=1;
end.
