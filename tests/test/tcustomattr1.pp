program tcustomattr1;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

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
  if not assigned(ad) then
    halt(1);
  if ad^.AttributeCount<>1 then
    halt(2);

  AClassAttribute := GetAttribute(ad,0);
  if AClassAttribute = nil then
    halt(3);
  writeln('ok');
end.

