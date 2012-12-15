program tclassattribute1;

{$mode objfpc}{$H+}

uses
  typinfo;

type

  { tmyt }

  tmyt = class(TCustomAttribute)
    constructor create;
  end;

type
  [Tmyt]
  TMyObject = class(TObject)
  end;

var
  rtd: PExtRTTIData;
  AClassAttribute: TCustomAttribute;

{ tmyt }

constructor tmyt.create;
begin
  //
end;

begin
  rtd := GetExtRTTIData(TMyObject.ClassInfo);
  if GetClassAttributeCount(rtd)<>1 then
    halt(1);

  AClassAttribute := GetClassAttribute(rtd,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

