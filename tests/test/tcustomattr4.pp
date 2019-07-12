program tcustomattr4;

{$mode delphi}

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
  rtd: PAttributeTable;
  AClassAttribute: tmyt;

{ tmyt }

constructor tmyt.create(Id: integer);
begin
  Fid := Id;
end;

begin
  rtd := GetAttributeTable(TMyObject.ClassInfo);

  if not Assigned(rtd) then
    halt(1);
  if rtd^.AttributeCount<>2 then
    halt(2);

  AClassAttribute := GetAttribute(rtd,1) as tmyt;
  if AClassAttribute = nil then
    halt(3);
  if AClassAttribute.FID<>1425 then
    halt(4);

  AClassAttribute := GetAttribute(rtd,0) as tmyt;
  if AClassAttribute = nil then
    halt(5);
  if AClassAttribute.FID<>924 then
    halt(6);
  writeln('ok');
end.

