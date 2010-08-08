{ %fail }
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TSealedClass = class sealed
  public
  end;

  TSealedDesdentantClass = class(TSealedClass)
  public
  end;

begin
end.

