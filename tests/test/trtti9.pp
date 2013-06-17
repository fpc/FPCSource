program trtti9;

{$mode delphi}

uses
  typinfo;

type
  PProcedureParam = ^TProcedureParam;
  TProc = procedure(var A: Integer; S: String); stdcall;

function TestParam(Param: PProcedureParam; Flags: TParamFlags; ParamType: Pointer; Name: ShortString): Boolean;
begin
  Result := (Param^.Flags = PByte(@Flags)^) and (Param^.ParamType = ParamType) and (Param^.Name = Name);
end;

var
  Info: PTypeInfo;
  Data: PTypeData;
  Param: PProcedureParam;
begin
  Info := TypeInfo(TProc);
  if Info^.Kind <> tkProcedure then
    halt(1);
  Data := GetTypeData(Info);
  if Data^.ProcSig.CC <> ccStdCall then
    halt(2);
  if Data^.ProcSig.ResultType <> nil then
     halt(3);
  if Data^.ProcSig.ParamCount <> 2 then
     halt(4);
  Param := Data^.ProcSig.GetParam(0);
  if not TestParam(Param, [pfVar], TypeInfo(Integer), 'A') then
     halt(5);
  Param := Data^.ProcSig.GetParam(1);
  if not TestParam(Param, [], TypeInfo(String), 'S') then
     halt(6);
end.
