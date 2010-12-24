{ %fail}
program toperator10;

{$mode delphi}

type
  TFoo = record
    F: Integer;
  end;

// don't allow class operator declaration outside the class
class operator Add(A, B: TFoo): TFoo;
begin
  Result.F := A.F + B.F;
end;

begin
end.

