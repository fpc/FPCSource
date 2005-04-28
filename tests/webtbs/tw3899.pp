{ Source provided for Free Pascal Bug Report 3899 }
{ Submitted by "Stefan Glienke" on  2005-04-19 }
{ e-mail: glienke@cpa.de }
program project2;

{$mode objfpc}{$H+}

uses
  Classes;
  
type
  TZVariant = packed record
    VInteger: Int64;
  end;

  IZInterface = IUnknown;

  IZObject = interface(IZInterface)
    ['{EF46E5F7-00CF-4DDA-BED0-057D6686AEE0}']
    function Equals(const Value: IZInterface): Boolean;
  end;

  IZClonnable = interface(IZObject)
    ['{ECB7F3A4-7B2E-4130-BA66-54A2D43C0149}']
  end;

  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']
  end;
  
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    function Equals(const Value: IZInterface): Boolean; virtual;
  end;

  TZAnyValue = class(TZAbstractObject, IZAnyValue)
  private
    FValue: TZVariant;
  public
    constructor Create(Value: TZVariant);
    function Equals(const Value: IZInterface): Boolean; override;
  end;

constructor TZAnyValue.Create(Value: TZVariant);
begin
  FValue := Value;
end;

function TZAnyValue.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZAnyValue;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZAnyValue, Temp) = 0 then
    begin
      Result := False;
      Temp := nil;
    end
    else
      Result := inherited Equals(Value);
  end
  else
    Result := False;
end;

function TZAbstractObject.Equals(const Value: IZInterface): Boolean;
begin
  if Value <> nil then
  begin
    Result := (IZInterface(Self) = Value)
      or ((Self as IZInterface) = (Value as IZInterface));
  end
  else
    Result := False;
end;


var
  ARecord: TZVariant;
  AValue: IZAnyValue;

begin
  ARecord.VInteger := 42;
  AValue := TZAnyValue.Create(ARecord);

  AValue.Equals(AValue);
  AValue.Equals(AValue); // <-- this call produces av

end.
