{ %NORUN }

program tw25604;

{$MODE DELPHI}

type
  TA<T> = class
  private
    F1, F2: T;
    procedure Foo;
  end;

procedure TA<T>.Foo;
var
  b: Integer;
begin
  b := (b and F1) shr F2; // pass
  b := (b and not F1) or (b shl F2); // Error: Operator is not overloaded: "LongInt" shl "<undefined type>"
end;

begin
end.

