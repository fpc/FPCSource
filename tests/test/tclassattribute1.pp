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
  ad: PAttributeData;
  AClassAttribute: TCustomAttribute;

{ tmyt }

constructor tmyt.create;
begin
  //
end;

begin
  ad := GetAttributeData(TMyObject.ClassInfo);
  if ad^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetAttribute(ad,0);
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

