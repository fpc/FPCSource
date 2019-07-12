program tcustomattr10;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  { TMyAttr }
  TMyAttrAttribute = class(TCustomAttribute)
  end;

type
  // The attribute should be also accessable without the Attribute suffix.
  [TMyAttr]
  TMyObject = class(TObject)
  end;

var
  ad: PAttributeData;
  AClassAttribute: TCustomAttribute;

begin
  ad := GetAttributeData(TMyObject.ClassInfo);
  if ad^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetAttribute(ad,0);
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

