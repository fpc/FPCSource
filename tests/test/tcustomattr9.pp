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
  at: PAttributeTable;
  AClassAttribute: TCustomAttribute;

begin
  at := GetAttributeTable(TMyObject.ClassInfo);
  if not Assigned(at) then
    halt(1);
  if at^.AttributeCount<>1 then
    halt(2);

  AClassAttribute := GetAttribute(at,0);
  if AClassAttribute = nil then
    halt(3);
  writeln('ok');
end.

