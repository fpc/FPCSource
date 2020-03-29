program tcustomattr10;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  { TMyAttr }
  TMyAttrAttribute = class(TCustomAttribute)
    constructor Create;
  end;

type
  // The attribute should be also accessable without the Attribute suffix.
  [TMyAttr]
  TMyObject = class(TObject)
  end;

constructor TMyAttrAttribute.Create;
begin

end;

var
  at: PAttributeTable;
  AClassAttribute: TCustomAttribute;

begin
  at := GetAttributeTable(TMyObject.ClassInfo);
  if not Assigned(at) then
    Halt(1);
  if at^.AttributeCount<>1 then
    halt(2);

  AClassAttribute := GetAttribute(at,0);
  if AClassAttribute = nil then
    halt(3);
  writeln('ok');
end.

