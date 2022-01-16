{ %NORUN }

program tw30179;

{$MODE DELPHI}

type
  TTest1 = record
    class function Add<T>(const A, B: T): T; static; inline;
  end;

class function TTest1.Add<T>(const A, B: T): T;
begin
  Result := A + B;
end;

procedure Main();
var
  I: Integer;
begin
  I := TTest1.Add<Integer>(1, 2); // project1.lpr(14,26) Error: Identifier not found "Add$1"
end;

begin
  Main();
end.

