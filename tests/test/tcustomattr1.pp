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
  at: PAttributeTable;
  AClassAttribute: TCustomAttribute;

{ tmyt }

constructor tmyt.create;
begin
  //
end;

begin
  at := GetAttributeTable(TMyObject.ClassInfo);
  if not assigned(at) then
    halt(1);
  if at^.AttributeCount<>1 then
    halt(2);

  AClassAttribute := GetAttribute(at,0);
  if AClassAttribute = nil then
    halt(3);
  writeln('ok');
end.

