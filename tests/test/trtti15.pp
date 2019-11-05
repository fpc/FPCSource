program trtti15;

{$mode objfpc}{$H+}

uses
  typinfo,
  sysutils;

type
  IBlubb = interface
    procedure Test;
  end;

  {$push}
  {$M+}
  ITest = interface
    procedure Test;
    function Test2: LongInt;
    procedure Test3(arg1: LongInt; arg2: String);
    function Test4(arg1: LongInt; arg2: String): String;
    function Test5(arg1: array of LongInt; arg2: Int64): Int64;
    function Test6(arg1: LongInt; arg2: String): String; stdcall;
    {$if defined(CPUI386) or defined(CPUI8086)}
    function Test7(arg1: LongInt; arg2: String): String; pascal;
    {$endif}
    function Test8(arg1: LongInt; arg2: String): String; cdecl;
    procedure Test9(var arg1; out arg2; constref arg3);
    property T: LongInt read Test2;
    property T2: LongInt read Test2;
  end;

  {$interfaces corba}
  ITestRaw = interface
    ['Test']
    function Test: LongInt;
    property T: LongInt read Test;
  end;
  {$pop}

procedure ErrorHalt(const aMsg: String; const aArgs: array of const);
begin
  if Length(aArgs) = 0 then
    Writeln(aMsg)
  else
    Writeln(Format(aMsg, aArgs));
  Halt(1);
end;

procedure TestParam(aParam: PVmtMethodParam; const aName: String; aFlags: TParamFlags; aTypeInfo: PTypeInfo);
begin
  Writeln(#9'Testing parameter ', aName);
  if not (pfHidden in aFlags) and (aParam^.Name <> aName) then
    ErrorHalt('Expected parameter name %s, but got %s', [aName, aParam^.Name]);
  if aParam^.Flags <> aFlags then
    ErrorHalt('Expected parameter flags %s, but got %s', [HexStr(Word(aFlags), 4), HexStr(Word(aParam^.Flags), 4)]);
  if Assigned(aTypeInfo) then begin
    if not Assigned(aParam^.ParamType) then
      ErrorHalt('Expected parameter type %s, but got Nil', [aTypeInfo^.Name]);
    if aParam^.ParamType^ <> aTypeInfo then
      ErrorHalt('Expected parameter type %s, but got %s', [aTypeInfo^.Name, aParam^.ParamType^^.Name]);
  end else begin
    if Assigned(aParam^.ParamType) then
      ErrorHalt('Expected Nil parameter type, but got %s', [aParam^.ParamType^^.Name])
  end;
end;

type
  TTestParam = record
    name: String;
    flags: TParamFlags;
    paramtype: PTypeInfo;
  end;

function MakeParam(const aName: String; aFlags: TParamFlags; aTypeInfo: PTypeInfo): TTestParam;
begin
  Result.name := aName;
  Result.flags := aFlags;
  Result.paramtype := aTypeInfo;
end;

procedure TestMethod(aMethod: PIntfMethodEntry; const aName: String; aKind: TMethodKind; aCC: TCallConv; aParams: array of TTestParam; aResult: PTypeInfo);
var
  c, i: LongInt;
  param: PVmtMethodParam;
begin
  Writeln('Testing method ', aName);
  if aMethod^.Name <> aName then
    ErrorHalt('Expected method name %s, but got %s', [aName, aMethod^.Name]);
  if aMethod^.CC <> aCC then
    ErrorHalt('Expected calling convention %d, but got %d', [Ord(aCC), Ord(aMethod^.CC)]);
  if aMethod^.Kind <> aKind then
    ErrorHalt('Expected method kind %d, but got %d', [Ord(aKind), Ord(aMethod^.Kind)]);
  if Assigned(aResult) and not Assigned(aMethod^.ResultType) then
    ErrorHalt('Expected result type %s, but got Nil', [aResult^.Name]);
  if Assigned(aResult) and (aResult <> aMethod^.ResultType^) then
    ErrorHalt('Expected result type %s, but got %s', [aResult^.Name, aMethod^.ResultType^^.Name]);

  { we ignore an eventual result parameter }
  if aMethod^.ParamCount < Length(aParams) then
    ErrorHalt('Expected at least %d parameters, but got %d', [Length(aParams), aMethod^.ParamCount]);

  if aMethod^.ParamCount < 1 then
    ErrorHalt('Expected at least 1 parameter, but got 0', []);

  { first parameter is always self }
  c := 1;
  TestParam(aMethod^.Param[0], aParams[0].name, aParams[0].flags, aParams[0].paramtype);

  for i := 1 to aMethod^.ParamCount - 1 do begin
    param := aMethod^.Param[i];
    if pfResult in param^.Flags then
      Continue;
    TestParam(param, aParams[c].name, aParams[c].flags, aParams[c].paramtype);
    Inc(c);
  end;

  if c <> Length(aParams) then
    ErrorHalt('Expected %d parameters, but got %d', [Length(aParams), c]);
end;

type
  TTestMethod = record
    name: String;
    cc: TCallConv;
    kind: TMethodKind;
    result: PTypeInfo;
    params: array of TTestParam;
  end;

function MakeMethod(const aName: String; aCC: TCallConv; aKind: TMethodKind; aResult: PTypeInfo; aParams: array of TTestParam): TTestMethod;
var
  i: LongInt;
begin
  Result.name := aName;
  Result.cc := aCC;
  Result.kind := aKind;
  Result.result := aResult;
  SetLength(Result.params, Length(aParams));
  for i := Low(aParams) to High(aParams) do
    Result.params[i - Low(aParams)] := aParams[i];
end;

procedure TestInterface(aIntf: PTypeData; aRaw: Boolean; aIIDStr: String; aPropCount: LongInt; aMethods: array of TTestMethod);
var
  proptable: PPropData;
  methtable: PIntfMethodTable;
  i: LongInt;
begin
  if aRaw then begin
    proptable := PInterfaceRawData(aIntf)^.PropertyTable;
    methtable := PInterfaceRawData(aIntf)^.MethodTable;
    if PInterfaceRawData(aIntf)^.IIDStr <> aIIDStr then
      ErrorHalt('Expected IIDStr ''%s'', but got ''%s''', [aIIDStr, PInterfaceRawData(aIntf)^.IIDStr]);
  end else begin
    proptable := PInterfaceData(aIntf)^.PropertyTable;
    methtable := PInterfaceData(aIntf)^.MethodTable;
  end;

  if proptable^.PropCount <> aPropCount then
    ErrorHalt('Expected %d properties, but got %d', [aPropCount, proptable^.PropCount]);

  if methtable^.Count <> Length(aMethods) then
    ErrorHalt('Expected %d methods, but got %d', [Length(aMethods), methtable^.Count]);

  if methtable^.RttiCount = $ffff then
    Exit;

  for i := 0 to methtable^.Count - 1 do begin
    TestMethod(methtable^.Method[i], aMethods[i].name, aMethods[i].kind, aMethods[i].cc, aMethods[i].params, aMethods[i].result);
  end;
end;

const
{$if defined(CPUI386) or defined(CPUI8086) or defined(CPUX86_64) or defined(CPUM68K)}
  DefaultCallingConvention = ccReg;
{$else}
  DefaultCallingConvention = ccStdCall;
{$endif}

begin
  Writeln('Testing interface ITestRaw');
  { raw interfaces don't support $M+ currently }
  TestInterface(GetTypeData(TypeInfo(ITestRaw)), True, 'Test', 0{1}, [
      MakeMethod('Test', ccReg, mkFunction, TypeInfo(LongInt), [])
    ]);

  Writeln('Testing interface ITest');
  TestInterface(GetTypeData(TypeInfo(ITest)), False, '', 2, [
      MakeMethod('Test', DefaultCallingConvention, mkProcedure, Nil, [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest))
        ]),
      MakeMethod('Test2', DefaultCallingConvention, mkFunction, TypeInfo(LongInt), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest))
        ]),
      MakeMethod('Test3', DefaultCallingConvention, mkProcedure, Nil, [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [], TypeInfo(LongInt)),
          MakeParam('arg2', [], TypeInfo(String))
        ]),
      MakeMethod('Test4', DefaultCallingConvention, mkFunction, TypeInfo(String), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [], TypeInfo(LongInt)),
          MakeParam('arg2', [], TypeInfo(String))
        ]),
      MakeMethod('Test5', DefaultCallingConvention, mkFunction, TypeInfo(Int64), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [pfArray, pfReference], TypeInfo(LongInt)),
          MakeParam('$highARG1', [pfHidden, pfHigh, pfConst], TypeInfo(SizeInt)),
          MakeParam('arg2', [], TypeInfo(Int64))
        ]),
      MakeMethod('Test6', ccStdCall, mkFunction, TypeInfo(String), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [], TypeInfo(LongInt)),
          MakeParam('arg2', [], TypeInfo(String))
        ]),
      {$if defined(CPUI386) or defined(CPUI8086)}
      MakeMethod('Test7', ccPascal, mkFunction, TypeInfo(String), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [], TypeInfo(LongInt)),
          MakeParam('arg2', [], TypeInfo(String))
        ]),
      {$endif}
      MakeMethod('Test8', ccCdecl, mkFunction, TypeInfo(String), [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [], TypeInfo(LongInt)),
          MakeParam('arg2', [], TypeInfo(String))
        ]),
      MakeMethod('Test9', DefaultCallingConvention, mkProcedure, Nil, [
          MakeParam('$self', [pfHidden, pfSelf, pfAddress], TypeInfo(ITest)),
          MakeParam('arg1', [pfVar], Nil),
          MakeParam('arg2', [pfOut], Nil),
          MakeParam('arg3', [pfConstRef], Nil)
        ])
    ]);
end.
