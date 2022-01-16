{ %OPT=-gh }

program tw35982;

{$mode Delphi}

uses RTTI;

type
  TSpecialAttribute = class(TCustomAttribute)
  public
    FValue: String;
    constructor Create(const AValue: String);
  end;

  constructor TSpecialAttribute.Create(const AValue: String);
  begin
    FValue := AValue;
  end;

type
  [TSpecialAttribute('Hello World!')]
  TSomeType = record
  end;

var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  HaltOnNotReleased := True;

  { Create a new Rtti context }
  LContext := TRttiContext.Create;

  { Extract type information for TSomeType type }
  LType := LContext.GetType(TypeInfo(TSomeType));

  { Search for the custom attribute and do some custom processing }
  for LAttr in LType.GetAttributes() do
    if LAttr is TSpecialAttribute then
      Writeln(TSpecialAttribute(LAttr).FValue);

  { Destroy the context }
  LContext.Free;
end.
