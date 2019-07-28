program trtti19;

{$mode objfpc}

uses
  TypInfo;

type
  TTestProc = procedure(var arg1; out arg2; constref arg3);
  TTestMethod = procedure(var arg1; out arg2; constref arg3) of object;

  PParamFlags = ^TParamFlags;
  PPPTypeInfo = ^PPTypeInfo;

var
  ti: PTypeInfo;
  td: PTypeData;
  procparam: PProcedureParam;
  pb: PByte;
  i: SizeInt;
begin
  // writeln(SizeOf(TparamFlags));
  ti := PTypeInfo(TypeInfo(TTestProc));
  td := GetTypeData(ti);
  if td^.ProcSig.ParamCount <> 3 then
    Halt(1);
  procparam := td^.ProcSig.GetParam(0);
  if Assigned(procparam^.ParamType) then
    Halt(2);
  if procparam^.ParamFlags * [pfVar] <> [pfVar] then
    Halt(3);
  procparam := td^.ProcSig.GetParam(1);
  if Assigned(procparam^.ParamType) then
    Halt(4);
  if procparam^.ParamFlags * [pfOut] <> [pfOut] then
    Halt(5);
  procparam := td^.ProcSig.GetParam(2);
  if Assigned(procparam^.ParamType) then
    Halt(6);
  if procparam^.ParamFlags * [pfConstRef] <> [pfConstRef] then
    Halt(7);
  
  ti := PTypeInfo(TypeInfo(TTestMethod));
  td := GetTypeData(ti);
  if td^.ParamCount <> 4 then
    Halt(8);
  pb := @td^.ParamList[0];
  pb := AlignTParamFlags(pb);
  if PParamFlags(pb)^ * [pfHidden, pfSelf] <> [pfHidden, pfSelf] then
    Halt(9);
  pb := pb + SizeOf(TParamFlags);
  pb := pb + SizeOf(Byte) + pb^;
  pb := pb + SizeOf(Byte) + pb^;

  pb := AlignTParamFlags(pb);
  if PParamFlags(pb)^ * [pfVar] <> [pfVar] then
    Halt(10);
  pb := pb + SizeOf(TParamFlags);
  pb := pb + SizeOf(Byte) + pb^;
  pb := pb + SizeOf(Byte) + pb^;

  pb := AlignTParamFlags(pb);
  if PParamFlags(pb)^ * [pfOut] <> [pfOut] then
    Halt(11);
  pb := pb + SizeOf(TParamFlags);
  pb := pb + SizeOf(Byte) + pb^;
  pb := pb + SizeOf(Byte) + pb^;

  pb := AlignTParamFlags(pb);
  if PParamFlags(pb)^ * [pfConstRef] <> [pfConstRef] then
    Halt(12);
  pb := pb + SizeOf(TParamFlags);
  pb := pb + SizeOf(Byte) + pb^;
  pb := pb + SizeOf(Byte) + pb^;

  pb := AlignPTypeInfo(pb + SizeOf(TCallConv));
  for i := 1 to td^.ParamCount - 1 do begin
    if PPPTypeInfo(pb)[i] <> Nil then begin
      Writeln(PPPTypeInfo(pb)[i]^^.Name);
      Halt(12 + i);
    end;
  end;

  Writeln('ok');
end.
