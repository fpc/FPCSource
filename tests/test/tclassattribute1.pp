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
  td: PTypeData;
  AClassAttribute: TCustomAttribute;

{ tmyt }

constructor tmyt.create;
begin
  //
end;

begin
  td := GetTypeData(TMyObject.ClassInfo);
  if td^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetClassAttribute(td,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(2);
  writeln('ok');
end.

