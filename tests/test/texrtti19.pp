{$Mode ObjFpc}

uses TypInfo;

{ Check that extended RTTI has information about array properties }

type
  {$RTTI EXPLICIT 
    FIELDS([vcPublic])
    PROPERTIES([vcPublic,vcPublished])
    METHODS([vcPublic,vcPublished])
  }
  TTestClass = class
  public 
    fa:integer;
    function MyMethod(const arg1: Integer): Integer;
    property TestIProp[const i: Longint]: Integer read MyMethod; 
    property TestProp: Integer read fa;
  end;

function TTestClass.MyMethod(const arg1: Integer): Integer;
begin
  Result := arg1;
end;

var
  pcd: PClassData;
begin
  pcd:=PClassData(GetTypeData(TypeInfo(TTestClass)));
  if pcd^.ExRTTITable^.PropCount <> 2 then
    Halt(1);
  if not assigned(pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams) then
    Halt(2);
  if pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Count<>1 then
    Halt(3);
  if pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Params[0].Name<>'i' then
    Halt(4);
  if not (pfconst in pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Params[0].flags) then
    Halt(5);
  if pcd^.ExRTTITable^.Prop[0]^.Info^.PropParams^.Params[0].ParamType^^.Name<>'LongInt' then
    Halt(6);
  if assigned(pcd^.ExRTTITable^.Prop[1]^.Info^.PropParams) then
    Halt(7);
  WriteLn('Ok');
end.
