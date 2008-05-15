{ %fail }
program IntfDel;

{$ifdef fpc}
{$mode objfpc}
{$endif fpc}

uses
  Classes;
type
  IA = interface
  end;

  TA = class(TObject, IA, IUnknown)
  private
    FUnknown: IUnknown;
    property Unknown: IUnknown read FUnknown implements IUnknown;
  end;

begin
end.
