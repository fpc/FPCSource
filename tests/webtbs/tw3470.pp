{$ifdef fpc}{$mode delphi}{$endif}
uses Variants;

type
  IBla = interface
  end;

  TBla = class(TInterfacedObject, IBla)
  public
    constructor Create;
  end;

constructor TBla.Create;
begin
end;

var
  v: Variant;
  bla: IBla;
begin
  bla := TBla.Create;
  v := bla;
end.

