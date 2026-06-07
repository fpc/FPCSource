unit unitb;

{$mode delphi}

interface

uses Generics.Collections;

type
  TThing = class
  public
    procedure DoStuff;
  end;

implementation

procedure TThing.DoStuff;
type
  { *** PROCEDURE-LOCAL NESTED TYPE — THE TRIGGER ***
    Mirrors FMX.ImgList.TCustomImageList.TimerProc.TChangedLink. }
  TLocal = record
    A: Integer;
    B: Integer;
  end;
var
  list: TList<TLocal>;
  item: TLocal;
begin
  list := TList<TLocal>.Create;
  try
    item.A := 1;
    item.B := 2;
    list.Add(item);
  finally
    list.Free;
  end;
end;

end.
