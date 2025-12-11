{$Mode ObjFpc}

uses TypInfo;

{ Check that the class RTTI does not have information about array properties }

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
  published
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
  WriteLn(pcd^.PropertyTable^.PropCount);
  if pcd^.PropertyTable^.PropCount <> 1 then
    Halt(1);
  if assigned(pcd^.PropertyTable^.Prop[0]^.PropParams) then
    Halt(2);
  WriteLn('Ok');
end.
