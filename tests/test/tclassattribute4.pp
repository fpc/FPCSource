program tclassattribute4;

{$mode objfpc}{$H+}

uses
  typinfo;

type

  { tmyt }

  tmyt = class(TCustomAttribute)
  private
    FID: integer;
  public
    constructor create(Id: integer);
  end;

type
  [Tmyt(924)]
  [Tmyt(1425)]
  TMyObject = class(TObject)
  end;

var
  td: PTypeData;
  AClassAttribute: tmyt;

{ tmyt }

constructor tmyt.create(Id: integer);
begin
  Fid := Id;
end;

begin
  td := GetTypeData(TMyObject.ClassInfo);
  if td^.AttributeCount<>2 then
    halt(1);

  AClassAttribute := GetClassAttribute(td,1) as tmyt;
  if AClassAttribute = nil then
    halt(2);
  if AClassAttribute.FID<>1425 then
    halt(3);

  AClassAttribute := GetClassAttribute(td,0) as tmyt;
  if AClassAttribute = nil then
    halt(2);
  if AClassAttribute.FID<>924 then
    halt(3);
  writeln('ok');
end.

