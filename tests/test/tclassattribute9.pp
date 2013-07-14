program tclassattribute9;

{$mode objfpc}{$H+}

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
  if ad^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetAttribute(ad,0);
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

