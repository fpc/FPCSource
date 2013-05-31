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
  rtd: PExtRTTIData;
  AClassAttribute: TCustomAttribute;

begin
  rtd := GetExtRTTIData(TMyObject.ClassInfo);
  if rtd^.AttributeData^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetClassAttribute(rtd,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

