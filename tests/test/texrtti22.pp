{$Mode ObjFpc}

uses TypInfo;

{ Check that extended RTTI writes the type information of an array property's
  index type when nothing else names that type (the program did not link) }

type
  TSub = 1..1000000;

  {$RTTI EXPLICIT
    FIELDS([vcPublic])
    PROPERTIES([vcPublic,vcPublished])
    METHODS([vcPublic,vcPublished])
  }
  TTestClass = class
  private
    function GetItem(aIndex: TSub): Integer;
  public
    property Items[aIndex: TSub]: Integer read GetItem;
  end;

  TSub2 = 1..10;

  { an open array index refers to its element type; an untyped one to none }
  TTestClass2 = class
  private
    function GetOpen(const a: array of TSub2): Integer;
    function GetFormal(const x): Integer;
  public
    property OpenItems[const a: array of TSub2]: Integer read GetOpen;
    property FormalItems[const x]: Integer read GetFormal;
  end;

function TTestClass.GetItem(aIndex: TSub): Integer;
begin
  Result := aIndex;
end;

function TTestClass2.GetOpen(const a: array of TSub2): Integer;
begin
  Result := Length(a);
end;

function TTestClass2.GetFormal(const x): Integer;
begin
  Result := 1;
end;

var
  pcd: PClassData;
  pt: PTypeInfo;
begin
  pcd:=PClassData(GetTypeData(TypeInfo(TTestClass)));
  if pcd^.ExRTTITable^.PropCount <> 1 then
    Halt(1);
  if not assigned(pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams) then
    Halt(2);
  if pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Count<>1 then
    Halt(3);
  pt:=pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Params[0].ParamType^;
  if pt^.Name<>'TSub' then
    Halt(4);
  if pt^.Kind<>tkInteger then
    Halt(5);
  if (GetTypeData(pt)^.MinValue<>1) or (GetTypeData(pt)^.MaxValue<>1000000) then
    Halt(6);
  pcd:=PClassData(GetTypeData(TypeInfo(TTestClass2)));
  if pcd^.ExRTTITable^.PropCount <> 2 then
    Halt(7);
  pt:=pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Params[0].ParamType^;
  if pt^.Name<>'TSub2' then
    Halt(8);
  if assigned(pcd^.ExRTTITable^.Prop[1]^.Info^.PropParams^.Params[0].ParamType) then
    Halt(9);
  WriteLn('Ok');
end.
