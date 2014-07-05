{ %NORUN }

program tw26180;

{$MODE DELPHI}
{$Assertions on}

type
  TA<T> = class
  private
    F: T;
    procedure Foo;
  end;

procedure TA<T>.Foo;
begin
  Assert(F <> 0); // Error: Boolean expression expected, but got "<undefined type>"
                  // same for >, <, <=, >=, =
end;

begin
end.

