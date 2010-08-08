{ %fail }
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TSealedClass = class sealed
  public
    procedure TestAbstract; virtual; abstract;
  end;

begin
end.

