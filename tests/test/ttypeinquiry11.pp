{ %FAIL }
{ An anonymous type body is not an expression. }
program ttypeinquiry11;

{$mode objfpc}

var
  v: type of (record a: integer; end);
begin
end.
