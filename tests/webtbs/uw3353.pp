{$ifdef fpc}{$mode objfpc}{$endif}

unit uw3353;

interface

var
  err : boolean;

type
  TGPGraphics = class(TObject)
  protected
    constructor Create(graphics: pointer); reintroduce; overload;
  public
    constructor Create(a: integer); reintroduce; overload;
  end;

implementation

  constructor TGPGraphics.Create(a: integer);
  begin
    err:=false;
  end;

  constructor TGPGraphics.Create(graphics: pointer);
  begin
  end;
end.
