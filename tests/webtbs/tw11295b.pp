program IntfDel;

{$ifdef fpc}
{$mode objfpc}
{$endif fpc}

uses
  Classes;

type
  IA = interface
  end;
  IB = interface(IA)
  end;

  TA = class(TObject, IA, IB)
  private
    FA: IA;
    property A: IA read FA implements IA;
  end;

begin
end.
