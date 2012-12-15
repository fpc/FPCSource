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
  rtd: PExtRTTIData;
  AClassAttribute: tmyt;

{ tmyt }

constructor tmyt.create(Id: integer);
begin
  Fid := Id;
end;

begin
  rtd := GetExtRTTIData(TMyObject.ClassInfo);

  if GetClassAttributeCount(rtd)<>2 then
    halt(1);

  AClassAttribute := GetClassAttribute(rtd,1) as tmyt;
  if AClassAttribute = nil then
    halt(2);
  if AClassAttribute.FID<>1425 then
    halt(3);

  AClassAttribute := GetClassAttribute(rtd,0) as tmyt;
  if AClassAttribute = nil then
    halt(2);
  if AClassAttribute.FID<>924 then
    halt(3);
  writeln('ok');
end.

