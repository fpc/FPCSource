{ %FAIL}
{$mode objfpc}
{$apptype console}

type 
  T = class
    F: Integer;
    function MoveNext(a: Integer): Boolean;
    property Current: Integer read F;
  end;

function T.MoveNext(a: Integer): Boolean; 
begin 
  Result := true; 
end;

operator enumerator(a: Integer): T;
begin
  Result := T.Create;
  Result.F := a;
end;

var
  i: Integer;
begin
  for i in 1 do Writeln(i);
end.
