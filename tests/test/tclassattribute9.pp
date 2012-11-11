program tclassattribute9;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  { tmyt }
  tmyt = class(TCustomAttribute);

type
  [Tmyt]
  TMyObject = class(TObject)
  end;

var
  td: PTypeData;
  AClassAttribute: TCustomAttribute;

begin
  td := GetTypeData(TMyObject.ClassInfo);
  if td^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetClassAttribute(td,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

