{$mode objfpc}
{$apptype console}

type
  T = class
    stop: boolean;
    F: Integer;
    function MoveNext: Boolean;
    property Current: Integer read F;
  end;

Twice = type Integer;

function T.MoveNext: Boolean;
begin
  Result := not stop;
  stop := true;
end;

operator enumerator(a: Integer): T;
begin
  Result := T.Create;
  Result.F := a;
  Result.stop := false;
end;

operator enumerator(a: Twice): T;
begin
  Result := T.Create;
  Result.F := a * 2;
  Result.stop := false;
end;

var
  i: Integer;
begin
  for i in Twice(1) do
    Writeln(i);
end.