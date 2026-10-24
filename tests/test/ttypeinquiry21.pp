{ %FAIL }
{ "type of" a record field inside its own declaration }
program ttypeinquiry21;

{$mode objfpc}

var
  MyRec: record
    Arr: array of type of Arr;
  end;

begin
end.
