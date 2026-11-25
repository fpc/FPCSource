program trtti30;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

{ Check that the type information of a procedural type, of a method pointer
  type and of a record helper can refer to a type that nothing else names (the
  program did not link); that a record field or a parameter of a distinct class
  alias refers to its original's (the program did not assemble: a duplicate
  label); and that a helper of a distinct class or interface alias refers to
  its original's }

uses
  TypInfo;

type
  TProcResult = 1..1000;
  TMethResult = 1..2000;
  TProc = function: TProcResult;
  TMeth = function: TMethResult of object;

  TExtended = record
    a: LongInt;
  end;

  TExtendedHelper = record helper for TExtended
  end;

  TBase = class
  end;
  TDistinct = type TBase;
  TDistinctHelper = class helper for TDistinct
  end;

  IBase = interface
  end;
  IDistinct = type IBase;
  TIntfHelper = type helper for IDistinct
  end;

  {$RTTI EXPLICIT FIELDS([vcPublic])}
  TAliasRec = record
    f: TDistinct;
  end;
  TAliasProc = procedure(a: TDistinct);

var
  td: PTypeData;
  prd: PRecordData;
begin
  td:=GetTypeData(TypeInfo(TProc));
  if td^.ProcSig.ResultType^.Name<>'TProcResult' then
    Halt(1);
  { a method pointer type: linking is the check }
  if TypeInfo(TMeth)=nil then
    Halt(2);
  td:=GetTypeData(TypeInfo(TExtendedHelper));
  if td^.ExtendedInfo^.Name<>'TExtended' then
    Halt(3);
  if GetTypeData(TypeInfo(TDistinctHelper))^.ExtendedInfo<>TypeInfo(TBase) then
    Halt(4);
  if GetTypeData(TypeInfo(TIntfHelper))^.ExtendedInfo<>TypeInfo(IBase) then
    Halt(5);
  prd:=PRecordData(GetTypeData(TypeInfo(TAliasRec)));
  if (prd^.ExtendedFieldCount<>1) or (prd^.ExtendedFields^.Field[0]^.FieldType^<>TypeInfo(TBase)) then
    Halt(6);
  if GetTypeData(TypeInfo(TAliasProc))^.ProcSig.GetParam(0)^.ParamType<>TypeInfo(TBase) then
    Halt(7);
  WriteLn('ok');
end.
