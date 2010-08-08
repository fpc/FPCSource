{ %fail }
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TSealedObject = object sealed
  public
  end;

  TSealedDesdentantObject = object(TSealedObject)
  public
  end;

begin
end.

