program tcustomattr12;

{$mode delphi}

uses
  typinfo;

type

  { tmyt }

  tmyt = class(TCustomAttribute)
  private
    FID: integer;
    FStr: String;
    FB: Boolean;
  public
    constructor create(Id: integer; aB: Boolean = False); overload;
    constructor Create(const aStr: String); overload;
  end;

type
  [Tmyt(924)]
  [Tmyt('Blubb')]
  [Tmyt(1425, True)]
  TMyObject = class(TObject)
  end;

var
  rtd: PAttributeTable;
  AClassAttribute: tmyt;

{ tmyt }

constructor tmyt.create(Id: integer; aB: Boolean);
begin
  Fid := Id;
  FB := aB;
end;

constructor tmyt.create(const aStr: String);
begin
  FStr := aStr;
end;

begin
  rtd := GetAttributeTable(TMyObject.ClassInfo);

  if not Assigned(rtd) then
    halt(1);
  if rtd^.AttributeCount<>3 then
    halt(2);

  AClassAttribute := GetAttribute(rtd,2) as tmyt;
  if AClassAttribute = nil then
    halt(3);
  if AClassAttribute.FID<>1425 then
    halt(4);
  if AClassAttribute.FStr<>'' then
    Halt(5);
  if not AClassAttribute.FB then
    Halt(6);

  AClassAttribute := GetAttribute(rtd,1) as tmyt;
  if AClassAttribute = nil then
    halt(7);
  if AClassAttribute.FID<>0 then
    halt(8);
  if AClassAttribute.FStr<>'Blubb' then
    Halt(9);
  if AClassAttribute.FB then
    Halt(10);

  AClassAttribute := GetAttribute(rtd,0) as tmyt;
  if AClassAttribute = nil then
    Halt(11);
  if AClassAttribute.FID<>924 then
    Halt(12);
  if AClassAttribute.FStr<>'' then
    Halt(13);
  if AClassAttribute.FB then
    Halt(14);

  writeln('ok');
end.

