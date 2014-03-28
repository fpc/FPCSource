{ %NORUN }

program tw25929;

{$MODE DELPHI}

type
  TR<T> = record
  end;

  TA<T> = class
    procedure Foo;
  end;

procedure TA<T>.Foo;
var
  r: TR<T>;
begin
  r := Default(TR<T>);
  r := Default(TR<T>); // Error: Duplicate identifier "zero_$P$PLC03_$$_TR$1"
end;

begin
end.

