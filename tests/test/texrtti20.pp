{$Mode ObjFPC}
uses typinfo;

type
{$RTTI EXPLICIT
FIELDS([vcPublic])
PROPERTIES([vcPublic,vcPublished])
METHODS([vcPublic,vcPublished])}
  TPropClass=class
    private
      FValue: Integer;
      class var
        FClassValue: Integer;
    public
      property InstanceProp: Integer read FValue;
      class property ClassProp: Integer read FClassValue;
  end;

var
  cd:PClassData;
begin
  cd:=PClassData(GetTypeData(TypeInfo(TPropClass)));
  if not assigned(cd^.ExRTTITable) then
    Halt(1);
  if cd^.ExRTTITable^.PropCount<>2 then
    Halt(2);
  if (cd^.ExRTTITable^.Prop[0]^.Info^.Name<>'InstanceProp') or
     (cd^.ExRTTITable^.Prop[0]^.Info^.IsStatic<>False) then
    Halt(3);
  if (cd^.ExRTTITable^.Prop[1]^.Info^.Name<>'ClassProp') or
     (cd^.ExRTTITable^.Prop[1]^.Info^.IsStatic<>True) then
    Halt(3);
  WriteLn('ok');
end.
