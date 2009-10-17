{ %fail }
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TSealedClass = class abstract sealed
  public
  end;
begin
end.

