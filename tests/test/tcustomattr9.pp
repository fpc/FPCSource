program tcustomattr9;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  { tmyt }
  // TCustomAttribute without constructor
  tmyt = class(TCustomAttribute);

type
  [Tmyt]
  TMyObject = class(TObject)
  end;

var
  ad: PAttributeData;
  AClassAttribute: TCustomAttribute;

begin
  ad := GetAttributeData(TMyObject.ClassInfo);
  if not Assigned(ad) then
    halt(1);
  if ad^.AttributeCount<>1 then
    halt(2);

  AClassAttribute := GetAttribute(ad,0);
  if AClassAttribute = nil then
    halt(3);
  writeln('ok');
end.

