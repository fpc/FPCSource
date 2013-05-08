program trtti9;

{$mode delphi}

uses
  typinfo;

type
  PProcedureParam = ^TProcedureParam;
  TProc = procedure(var A: Integer; S: String); stdcall;

function TestParam(Param: PProcedureParam; Flags: Byte; ParamType: Pointer; Name: ShortString): Boolean;
begin
  Result := (Param^.Flags = Flags) and (Param^.ParamType = ParamType) and (Param^.Name = Name);
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
  Param := PProcedureParam(PAnsiChar(@Data^.ProcSig.Flags) + SizeOf(TProcedureSignature));
  if not TestParam(Param, 1, TypeInfo(Integer), 'A') then
     halt(5);
  Param := PProcedureParam(PAnsiChar(@Param^.Name) + (Length(Param^.Name) + 1) * SizeOf(AnsiChar));
  if not TestParam(Param, 0, TypeInfo(String), 'S') then
     halt(6);
end.