{ %FAIL }
{ "type of" a variable in a pointer type inside its own declaration }
program ttypeinquiry26;

{$mode objfpc}

var
  MyRec: record
    PT: ^type of MyRec;
  end;

begin
end.
