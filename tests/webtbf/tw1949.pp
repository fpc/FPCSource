{ %fail }

{$mode objfpc}{$H+}

type
  TMyProc = procedure;

  TMyClassA = class
  private
    FOnMyEvent: TMyProc;
  public
    property OnMyEvent: TMyProc read FOnMyEvent write FOnMyEvent;
  end;

  TMyClassB = class
  public
    MyClassA: TMyClassA;
    procedure DoIt;
    constructor Create;
  end;

procedure TMyClassB.DoIt;
begin

end;

constructor TMyClassB.Create;
begin
  MyClassA:=TMyClassA.Create;
  MyClassA.OnMyEvent:=@DoIt; // DoIt is 'procedure of object' -> incompatible !
end;

begin
end.
