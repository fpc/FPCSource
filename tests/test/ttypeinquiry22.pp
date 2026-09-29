{ %FAIL }
{ "type of" a record field inside its own declaration in a named record }
program ttypeinquiry22;

{$mode objfpc}

type
  TRec = record
    a: integer;
    b: array of type of b;
  end;

begin
end.
