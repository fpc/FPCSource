unit Tests.Rtti.Impl;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

{ $define debug}

interface

uses
{$IFDEF FPC}
  fpcunit,testregistry, testutils,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  sysutils, typinfo, Rtti,
  Tests.Rtti.Util,
  tests.rtti.impltypes;

{ Note: Delphi does not provide a CreateImplementation for TRttiInvokableType
        and its descendants, so these tests are disabled for Delphi }

type
  { TTestImplBase }

  TTestImplBase = class(TTestCase)
  Protected

    InputArgs: array of TValue;
    OutputArgs: array of TValue;
    ResultValue: TValue;
    InOutMapping: array of SizeInt;
    InputUntypedTypes: array of PTypeInfo;
    InvokedMethodName: String;

    procedure OnHandleIntfMethod(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);
    procedure DoIntfImpl(aIntf: IInterface; aTypeInfo: PTypeInfo; aIndex: LongInt; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
{$ifdef fpc}
    procedure OnHandleInvokable(aInvokable: TRttiInvokableType; const aArgs: TValueArray; out aResult: TValue);
    procedure DoMethodImpl(aTypeInfo: PTypeInfo; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
    procedure DoProcImpl(aTypeInfo: PTypeInfo; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
{$endif}
    end;

(*
TTestImpl = class(TTestCase)
  private
  published
    procedure TestIntfMethods;
{$ifdef fpc}
    procedure TestMethodVars;
    procedure TestProcVars;
{$endif}
  end;
*)
  { TTestIntfMethods }
  TTestIntfMethods = class(TTestImplBase)
  private
    intf: ITestInterface;
  public
    procedure DoTestIntfImpl(aIndex: LongInt; const aInputArgs, aOutputArgs: TValueArray; const aInOutMapping: array of SizeInt; aResult: TValue);
    procedure Setup; override;
    procedure teardown; override;
    published
    Procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
    procedure Test7;
    procedure Test8;
    procedure Test9;
    procedure Test10;
    procedure Test11;
    procedure Test12;
    procedure Test13;
    procedure Test14;
    procedure Test15;
    procedure Test16;
    procedure Test17;
    procedure Test18;
    procedure Test19;
    procedure Test20;
    procedure Test21;
    procedure Test21s;
    procedure Test22;
  end;

  { TTestDirectIntfCalls }
  TIntfCallCallBack = Procedure(aIntf : ITestInterface) of object;

  TTestDirectIntfCalls = class(TTestImplBase)
  private
    Fintf: ITestInterface;
    FActualResult : TValue;
    FProgress : Boolean;
    RecordedOutputArgs : Array of TValue;
    procedure CallTestMethod1(aIntf: ITestInterface);
    procedure CallTestMethod2(aIntf: ITestInterface);
    procedure CallTestMethod3(aIntf: ITestInterface);
    procedure CallTestMethod4(aIntf: ITestInterface);
    procedure CallTestMethod5(aIntf: ITestInterface);
    procedure CallTestMethod6(aIntf: ITestInterface);
    procedure CallTestMethod7(aIntf: ITestInterface);
    procedure CallTestMethod8(aIntf: ITestInterface);
    procedure CallTestMethod9(aIntf: ITestInterface);
    procedure CallTestMethod10(aIntf: ITestInterface);
    procedure CallTestMethod11(aIntf: ITestInterface);
    procedure CallTestMethod12(aIntf: ITestInterface);
    procedure CallTestMethod13(aIntf: ITestInterface);
    procedure CallTestMethod14(aIntf: ITestInterface);
    procedure CallTestMethod15(aIntf: ITestInterface);
    procedure CallTestMethod16(aIntf: ITestInterface);
    procedure CallTestMethod17(aIntf: ITestInterface);
    procedure CallTestMethod18(aIntf: ITestInterface);
    procedure CallTestMethod19(aIntf: ITestInterface);
    procedure CallTestMethod20(aIntf: ITestInterface);
    procedure CallTestMethod21(aIntf: ITestInterface);
    procedure CallTestMethod21s(aIntf: ITestInterface);
    procedure CallTestMethod22(aIntf: ITestInterface);
    procedure DoStep(const aname: string);
  public
    procedure DoTestIntfImpl(aCall : TIntfCallCallBack; const aName : string; const aInputArgs, aOutputArgs: TValueArray; const aInOutMapping: array of SizeInt; aResult: TValue);
    procedure Setup; override;
    procedure teardown; override;
  published
    Procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
    procedure Test7;
    procedure Test8;
    procedure Test9;
    procedure Test10;
    procedure Test11;
    procedure Test12;
    procedure Test13;
    procedure Test14;
    procedure Test15;
    procedure Test16;
    procedure Test17;
    procedure Test18;
    procedure Test19;
    procedure Test20;
    procedure Test21;
    procedure Test21s;
    procedure Test22;
  end;

  { TTestMethodVars }

  TTestMethodVars = class(TTestImplBase)
  Published
    Procedure TestMethodVar1;
    Procedure TestMethodVar2;
    procedure TestMethodVar3;
    procedure TestMethodVar4;
    procedure TestMethodVar5;
    procedure TestMethodVar6;
    procedure TestMethodVar7;
    procedure TestMethodVar8;
    procedure TestMethodVar9;
    procedure TestMethodVar10;
    procedure TestMethodVar11;
    procedure TestMethodVar12;
    procedure TestMethodVar13;
    procedure TestMethodVar14;
    procedure TestMethodVar15;
    procedure TestMethodVar16;
    procedure TestMethodVar17;
    procedure TestMethodVar18;
    procedure TestMethodVar19;
    procedure TestMethodVar20;
    procedure TestMethodVar21;
    procedure TestMethodVar21as;
    procedure TestMethodVar21ss;
  end;

  { TTestProcVars }

  TTestProcVars = class (TTestImplBase)
  Published
    Procedure TestProcVar1;
    Procedure TestProcVar2;
    procedure TestProcVar3;
    procedure TestProcVar4;
    procedure TestProcVar5;
    procedure TestProcVar6;
    procedure TestProcVar7;
    procedure TestProcVar8;
    procedure TestProcVar9;
    procedure TestProcVar10;
    procedure TestProcVar11;
    procedure TestProcVar12;
    procedure TestProcVar13;
    procedure TestProcVar14;
    procedure TestProcVar15;
    procedure TestProcVar16;
    procedure TestProcVar17;
    procedure TestProcVar18;
    procedure TestProcVar19;
    procedure TestProcVar20;
    procedure TestProcVar21;
    procedure TestProcVar21as;
    procedure TestProcVar21ss;
  end;


implementation



{ TTestImpl }

procedure TTestImplBase.OnHandleIntfMethod(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);
var
  selfofs, i: SizeInt;

begin
  selfofs := 1;
  Status('In Callback');
  InvokedMethodName :=  aMethod.Name;
  //  Status('Self: ' + HexStr(Self));
  if Assigned(aMethod.ReturnType) then
    aResult := CopyValue(ResultValue);
  // Status('Setting input args');
  SetLength(InputArgs, Length(aArgs));
  for i := 0 to High(aArgs) do begin
    // Status('Arg %d: %p %p %s', [i, aArgs[i].GetReferenceToRawData, PPointer(aArgs[i].GetReferenceToRawData)^, aArgs[i].TypeInfo^.Name]);
    // Writeln('Untyped: ',Assigned(InputUntypedTypes[i]));
    if Assigned(InputUntypedTypes[i]) then
      begin
      // Writeln('Input type untyped, creating value for actual type ',InputUntypedTypes[i]^.Name);
      TValue.Make(PPointer(aArgs[i].GetReferenceToRawData)^, InputUntypedTypes[i], InputArgs[i])
      end
    else
      InputArgs[i] := CopyValue(aArgs[i]);
    // Writeln('OK');
  end;
  { Note: account for Self }
  for i := 0 to High(InOutMapping) do begin
    Move(OutputArgs[i].GetReferenceToRawData^, aArgs[InOutMapping[i] + selfofs].GetReferenceToRawData^, OutputArgs[i].DataSize);
  end;
  Status('Callback done');
end;

procedure TTestImplBase.DoIntfImpl(aIntf: IInterface; aTypeInfo: PTypeInfo; aIndex: LongInt; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
var
  context: TRttiContext;
  t: TRttiType;
  instance, res: TValue;
  method: TRttiMethod;
  i: SizeInt;
  input: array of TValue;
  intf: TRttiInterfaceType;
  params: array of TRttiParameter;
  name : string;
begin
  input:=nil;
  name := 'TestMethod' + IntToStr(aIndex);

  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiInterfaceType, 'Not a interface type: ' + aTypeInfo^.Name);
    intf := t as TRttiInterfaceType;

    method := intf.GetMethod(name);
    Check(Assigned(method), 'Method not found: ' + name);

    Status('Executing method %s', [name]);

    CheckEquals(Length(aOutputArgs), Length(aInOutMapping), 'Invalid in/out mapping');
    Check(Length(aOutputArgs) <= Length(aInputArgs), 'Output args not part of input args');

    params := method.GetParameters;

    TValue.Make(@aIntf, aTypeInfo, instance);

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs) + 1);
    SetLength(InputUntypedTypes, Length(aInputArgs) + 1);
    input[0] := instance;
    InputUntypedTypes[0] := Nil;
    for i := 0 to High(aInputArgs) do begin
      input[i + 1] := CopyValue(aInputArgs[i]);
      if not Assigned(params[i].ParamType) then
        InputUntypedTypes[i + 1] := aInputArgs[i].TypeInfo
      else
        InputUntypedTypes[i + 1] := Nil;
    end;

    SetLength(InOutMapping, Length(aInOutMapping));
    for i := 0 to High(InOutMapping) do
      InOutMapping[i] := aInOutMapping[i];
    SetLength(OutputArgs, Length(aOutputArgs));
    for i := 0 to High(OutputArgs) do
      OutputArgs[i] := CopyValue(aOutputArgs[i]);
    ResultValue := aResult;

    res := method.Invoke(instance, aInputArgs);
    Status('After invoke');

    CheckEquals(name, InvokedMethodName, 'Invoked method name differs for ' + name);
    Check(EqualValues(ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(input), Length(InputArgs), 'Count of input args differs for ' + name);
    for i := 0 to High(input) do begin
      Check(EqualValues(input[i], InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], aInputArgs[InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

{$ifdef fpc}
procedure TTestImplBase.OnHandleInvokable(aInvokable: TRttiInvokableType; const aArgs: TValueArray; out
  aResult: TValue);
var
  selfofs, i: SizeInt;
begin
  CheckTrue((aInvokable is TRttiMethodType) or (aInvokable is TRttiProcedureType), 'Invokable is not a method or procedure variable: ' + aInvokable.ClassName);

  selfofs := 0;
  if aInvokable is TRttiMethodType then
    selfofs := 1;

  Status('In Callback');
  Status('Self: ' + HexStr(Self));
  if Assigned(aInvokable.ReturnType) then
    aResult := CopyValue(ResultValue);
  Status('Setting input args');
  SetLength(InputArgs, Length(aArgs));
  for i := 0 to High(aArgs) do begin
    Status('Arg %d: %p %p', [i, aArgs[i].GetReferenceToRawData, PPointer(aArgs[i].GetReferenceToRawData)^]);
    if Assigned(InputUntypedTypes[i]) then
      TValue.Make(PPointer(aArgs[i].GetReferenceToRawData)^, InputUntypedTypes[i], InputArgs[i])
    else
      InputArgs[i] := CopyValue(aArgs[i]);
  end;
  Status('Setting output args');
  { Note: account for Self }
  for i := 0 to High(InOutMapping) do begin
    Status('OutputArg %d -> Arg %d', [i, InOutMapping[i] + selfofs]);
    { check input arg type? }
    Move(OutputArgs[i].GetReferenceToRawData^, aArgs[InOutMapping[i] + selfofs].GetReferenceToRawData^, OutputArgs[i].DataSize);
  end;
  Status('Callback done');
end;

procedure TTestImplBase.DoMethodImpl(aTypeInfo: PTypeInfo; aInputArgs,
  aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
var
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  method: TRttiMethodType;
  i: SizeInt;
  input: array of TValue;
  impl: TMethodImplementation;
  mrec: TMethod;
  name: String;
  params: array of TRttiParameter;
begin
  Input:=nil;
  name := aTypeInfo^.Name;

  impl := Nil;
  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiMethodType, 'Not a method variable: ' + name);
    method := t as TRttiMethodType;

    Status('Executing method %s', [name]);

    CheckEquals(Length(aOutputArgs), Length(aInOutMapping), 'Invalid in/out mapping');
    Check(Length(aOutputArgs) <= Length(aInputArgs), 'Output args not part of input args');

    params := method.GetParameters;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs) + 1);
    SetLength(InputUntypedTypes, Length(aInputArgs) + 1);
    input[0] := GetPointerValue(Self);
    InputUntypedTypes[0] := Nil;
    for i := 0 to High(aInputArgs) do begin
      input[i + 1] := CopyValue(aInputArgs[i]);
      if not Assigned(params[i].ParamType) then
        InputUntypedTypes[i + 1] := aInputArgs[i].TypeInfo
      else
        InputUntypedTypes[i + 1] := Nil;
    end;

    try
      impl := method.CreateImplementation({$ifdef fpc}@{$endif}OnHandleInvokable);
    except
      on e: ENotImplemented do
        Exit;
    end;
    CheckNotNull(impl, 'Method implementation is Nil');

    mrec.Data := Self;
    mrec.Code := impl.CodeAddress;
    TValue.Make(@mrec, aTypeInfo, callable);

    SetLength(InOutMapping, Length(aInOutMapping));
    for i := 0 to High(InOutMapping) do
      InOutMapping[i] := aInOutMapping[i];
    SetLength(OutputArgs, Length(aOutputArgs));
    for i := 0 to High(OutputArgs) do
      OutputArgs[i] := CopyValue(aOutputArgs[i]);
    ResultValue := aResult;

    res := method.Invoke(callable, aInputArgs);
    Status('After invoke');

    Check(EqualValues(ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(input), Length(InputArgs), 'Count of input args differs for ' + name);
    for i := 0 to High(input) do begin
      Check(EqualValues(input[i], InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], aInputArgs[InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    impl.Free;
    context.Free;
  end;
end;

procedure TTestImplBase.DoProcImpl(aTypeInfo: PTypeInfo; aInputArgs,
  aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
var
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  proc: TRttiProcedureType;
  i: SizeInt;
  input: array of TValue;
  impl: TMethodImplementation;
  name: String;
  cp: CodePointer;
  params: array of TRttiParameter;
begin
  Input:=nil;
  name := aTypeInfo^.Name;

  impl := Nil;
  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiProcedureType, 'Not a procedure variable: ' + name);
    proc := t as TRttiProcedureType;

    Status('Executing procedure %s', [name]);

    CheckEquals(Length(aOutputArgs), Length(aInOutMapping), 'Invalid in/out mapping');
    Check(Length(aOutputArgs) <= Length(aInputArgs), 'Output args not part of input args');

    params := proc.GetParameters;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    SetLength(InputUntypedTypes, Length(aInputArgs));
    for i := 0 to High(aInputArgs) do begin
      input[i] := CopyValue(aInputArgs[i]);
      if not Assigned(params[i].ParamType) then
        InputUntypedTypes[i] := aInputArgs[i].TypeInfo
      else
        InputUntypedTypes[i] := Nil;
    end;

    try
      impl := proc.CreateImplementation({$ifdef fpc}@{$endif}OnHandleInvokable);
    except
      on e: ENotImplemented do
        Exit;
    end;
    CheckNotNull(impl, 'Method implementation is Nil');

    cp := impl.CodeAddress;
    TValue.Make(@cp, aTypeInfo, callable);

    SetLength(InOutMapping, Length(aInOutMapping));
    for i := 0 to High(InOutMapping) do
      InOutMapping[i] := aInOutMapping[i];
    SetLength(OutputArgs, Length(aOutputArgs));
    for i := 0 to High(OutputArgs) do
      OutputArgs[i] := CopyValue(aOutputArgs[i]);
    ResultValue := aResult;

    res := proc.Invoke(callable, aInputArgs);
    Status('After invoke');

    Check(EqualValues(ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(input), Length(InputArgs), 'Count of input args differs for ' + name);
    for i := 0 to High(input) do begin
      Check(EqualValues(input[i], InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], aInputArgs[InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    impl.Free;
    context.Free;
  end;
end;
{$endif}


procedure TTestIntfMethods.teardown;
begin
  Intf:=Nil;
end;

procedure TTestIntfMethods.Setup;

begin
  inherited;
  try
    intf := TVirtualInterface.Create(PTypeInfo(TypeInfo(ITestInterface)), {$ifdef fpc}@{$endif}OnHandleIntfMethod) as ITestInterface;
  except
    on e: ENotImplemented do
      Ignore('TVirtualInterface not supported for ' + {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%}+': '+E.Message);
  end;
  Check(Assigned(intf), 'ITestInterface instance is Nil');
end;

procedure TTestIntfMethods.DoTestIntfImpl(aIndex: LongInt; const aInputArgs, aOutputArgs: TValueArray; const aInOutMapping: array of SizeInt; aResult: TValue);

begin
  DoIntfImpl(Intf, TypeInfo(ITestInterface),aIndex,aInputArgs,aOutputArgs,aInOutMapping,aResult);
end;

procedure TTestIntfMethods.Test1;

begin
  DoTestIntfImpl(1, [], [], [], TValue.Empty);
end;

procedure TTestIntfMethods.Test2;

begin
   DoTestIntfImpl(2, [GetIntValue(42)], [], [], GetIntValue(21));
end;

procedure TTestIntfMethods.Test3;

begin
  DoTestIntfImpl(3, [GetAnsiString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestIntfMethods.Test4;

begin
  DoTestIntfImpl(4, [GetShortString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestIntfMethods.Test5;

begin
  DoTestIntfImpl(5, [], [], [], GetAnsiString('Hello World'));
end;

procedure TTestIntfMethods.Test6;

begin
  DoTestIntfImpl(6, [], [], [], GetShortString('Hello World'));
end;

procedure TTestIntfMethods.Test7;

begin
  DoTestIntfImpl(7, [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test8;

begin
  DoTestIntfImpl(8, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test9;

begin
  DoTestIntfImpl(9, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test10;

begin
  DoTestIntfImpl(10, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test11;

begin
  DoTestIntfImpl(11, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test12;

begin
  DoTestIntfImpl(12, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test13;

begin
  DoTestIntfImpl(13, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test14;

begin
  DoTestIntfImpl( 14, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestIntfMethods.Test15;

begin
  DoTestIntfImpl(15, [
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));
end;

procedure TTestIntfMethods.Test16;

begin
  DoTestIntfImpl(16, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));
end;

procedure TTestIntfMethods.Test17;

begin
  DoTestIntfImpl(17, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestIntfMethods.Test18;

begin
  DoTestIntfImpl(18, [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestIntfMethods.Test19;

begin
  DoTestIntfImpl(19, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));
end;

procedure TTestIntfMethods.Test20;

begin
  DoTestIntfImpl(20, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));
end;

procedure TTestIntfMethods.Test21;

begin
  DoTestIntfImpl(21, [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);
end;

procedure TTestIntfMethods.Test21s;

begin
  DoTestIntfImpl(21, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);
end;

procedure TTestIntfMethods.Test22;

begin
  { for some reason this fails, though it fails in Delphi as well :/ }
  {
  DoTestIntfImpl(21, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);
  }
end;

{ TTestDirectIntfCalls }

procedure TTestDirectIntfCalls.DoStep(const aname : string);
begin
  if FProgress then
    Writeln(aname);
end;

procedure TTestDirectIntfCalls.DoTestIntfImpl(aCall: TIntfCallCallBack;
  const aName : string;
  const aInputArgs, aOutputArgs: TValueArray;
  const aInOutMapping: array of SizeInt; aResult: TValue);
var
  context: TRttiContext;
  t: TRttiType;
  instance: TValue;
  method: TRttiMethod;
  i: SizeInt;
  input: array of TValue;
  intf: TRttiInterfaceType;
  params: array of TRttiParameter;
  i6 : Int64;
begin
//  FProgress:=True;
  input:=nil;
  context := TRttiContext.Create;
  try
    t := context.GetType(TypeInfo(ITestInterface));
    Check(t is TRttiInterfaceType, 'Not a interface type !');
    intf := t as TRttiInterfaceType;
    method := intf.GetMethod(aname);
    Check(Assigned(method), 'Method not found: ' + aname);
    params := method.GetParameters;
  dostep('a');
  CheckEquals(Length(aOutputArgs), Length(aInOutMapping), 'Invalid in/out mapping');
  dostep('b');
  Check(Length(aOutputArgs) <= Length(aInputArgs), 'Output args not part of input args');
  dostep('c');
  TValue.Make(@FIntf, TypeInfo(ITestInterface), instance);
  dostep('d');
  { arguments might be modified by Invoke (Note: Copy() does not uniquify the
    IValueData of managed types) }
  SetLength(input, Length(aInputArgs) + 1);
  dostep('e');
  SetLength(InputUntypedTypes, Length(aInputArgs) + 1);
  dostep('f');
  input[0] := instance;
  dostep('g');
  InputUntypedTypes[0] := Nil;
  dostep('h');
  for i := 0 to High(aInputArgs) do begin
    dostep('h_'+IntToStr(i));
    input[i + 1] := CopyValue(aInputArgs[i]);
    dostep('h__'+IntToStr(i));
    if not Assigned(params[i].ParamType) then
      begin
      dostep('h__/'+IntToStr(i)+' : Param '+params[i].Name+' is untyped, Actual type is '+aInputArgs[i].TypeInfo^.Name);
      InputUntypedTypes[i + 1] := aInputArgs[i].TypeInfo
      end
    else
    begin
      dostep('h__\'+IntToStr(i)+' : Param '+params[i].Name+' is typed');
      InputUntypedTypes[i + 1] := Nil;
    end;
  end;
  dostep('i');

  SetLength(InOutMapping, Length(aInOutMapping));
  dostep('j');
  for i := 0 to High(InOutMapping) do
    InOutMapping[i] := aInOutMapping[i];
  dostep('k');
  SetLength(OutputArgs, Length(aOutputArgs));
  for i := 0 to High(OutputArgs) do
    OutputArgs[i] := CopyValue(aOutputArgs[i]);
  dostep('l');
  ResultValue := aResult;
  dostep('m');
  aCall(self.FIntf);
  dostep('n');
  CheckEquals(aName, InvokedMethodName, 'Invoked method name differs');
  dostep('o');
  Check(EqualValues(ResultValue, FActualResult), Format('Actual result %s value differs from saved expected %s',[FActualResult.ToString,ResultValue.ToString]));
  dostep('p');
  Check(EqualValues(aResult, FActualResult), Format('Actual result %s value differs from expected %s',[FActualResult.ToString, aResult.ToString]));
  dostep('q');
  CheckEquals(Length(input), Length(InputArgs), 'Count of input args differs');
  dostep('q1');
  for i := 0 to Length(InputArgs)-1 do
  begin
    // Can't compare untyped in direct call.
    if (I=0) or Assigned(Params[I-1].ParamType) then
      Check(EqualValues(input[i], InputArgs[i]), Format('Input argument %d differs (actual: %s, expected: %s)', [i,input[i].tostring, InputArgs[i].toString]));
  end;
  dostep('r');
  CheckEquals(Length(aOutputArgs), Length(RecordedOutputArgs), 'Count of recorded and expected output args differs');
  for i := 0 to High(aOutputArgs) do begin
    Check(EqualValues(aOutputArgs[i], RecordedOutputArgs[i]), Format('New output argument %d (%s) differs from expected output (%s)', [i + 1, RecordedOutputArgs[i].ToString, aOutputArgs[i].toString]));
  end;
  dostep('s');
  finally
    context.Free;
  end;

end;


procedure TTestDirectIntfCalls.Setup;
begin
  inherited;
  try
    Fintf := TVirtualInterface.Create(PTypeInfo(TypeInfo(ITestInterface)), {$ifdef fpc}@{$endif}OnHandleIntfMethod) as ITestInterface;
  except
    on e: ENotImplemented do
      Ignore('TVirtualInterface not supported for ' + {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%}+': '+E.Message);
  end;
  Check(Assigned(Fintf), 'ITestInterface instance is Nil');
  FActualResult:=TValue.Empty;
end;

procedure TTestDirectIntfCalls.teardown;
begin
  FIntf:=nil;
  inherited teardown;
end;

procedure TTestDirectIntfCalls.CallTestMethod1(aIntf : ITestInterface);

begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod1');
  aIntf.TestMethod1;
  DoStep('< TTestDirectIntfCalls.CallTestMethod1');
end;


procedure TTestDirectIntfCalls.Test1;
begin
  DoTestIntfImpl(@CallTestMethod1,'TestMethod1', [], [], [], TValue.Empty);
  DoStep('< TTestDirectIntfCalls.Test1');
end;

procedure TTestDirectIntfCalls.CallTestMethod2(aIntf : ITestInterface);

var
  I : Integer;

begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod2');
  I:=aIntf.TestMethod2(42);
  TValue.Make(I,TypeInfo(Sizeint),FActualResult);
  DoStep('< TTestDirectIntfCalls.CallTestMethod2');
end;

procedure TTestDirectIntfCalls.Test2;
begin
  DoTestIntfImpl(@CallTestMethod2,'TestMethod2', [GetIntValue(42)], [], [], GetIntValue(21));
end;

procedure TTestDirectIntfCalls.CallTestMethod3(aIntf: ITestInterface);
begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod3');
  aIntf.TestMethod3('Hello World');
  DoStep('< TTestDirectIntfCalls.CallTestMethod3');
end;


procedure TTestDirectIntfCalls.Test3;
begin
  DoTestIntfImpl(@CallTestMethod3,'TestMethod3', [GetAnsiString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod4(aIntf: ITestInterface);

var
  S : ShortString;

begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod4');
  S:='Hello World';
  aIntf.TestMethod4(S);
  DoStep('< TTestDirectIntfCalls.CallTestMethod4');
end;


procedure TTestDirectIntfCalls.Test4;
begin
  DoTestIntfImpl(@CallTestMethod4,'TestMethod4', [GetShortString('Hello World')], [], [], TValue.Empty);
end;


procedure TTestDirectIntfCalls.CallTestMethod5(aIntf: ITestInterface);
var
  S : AnsiString;

begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod5');
  S:=aIntf.TestMethod5();
  TValue.Make(@S,TypeInfo(AnsiString),FActualResult);
  DoStep('< TTestDirectIntfCalls.CallTestMethod5');
end;

procedure TTestDirectIntfCalls.Test5;
begin
  DoTestIntfImpl(@CallTestMethod5,'TestMethod5', [], [], [], GetAnsiString('Hello World'));
end;

procedure TTestDirectIntfCalls.CallTestMethod6(aIntf: ITestInterface);
var
  S : ShortString;

begin
  DoStep('> TTestDirectIntfCalls.CallTestMethod6');
  S:=aIntf.TestMethod6();
  TValue.Make(@S,TypeInfo(ShortString),FActualResult);
  DoStep('< TTestDirectIntfCalls.CallTestMethod6');
end;

procedure TTestDirectIntfCalls.Test6;
begin
  DoTestIntfImpl(@CallTestMethod6, 'TestMethod6', [], [], [], GetShortString('Hello World'));
end;

procedure TTestDirectIntfCalls.CallTestMethod7(aIntf: ITestInterface);

Var
  i2,i3 : SizeInt;

begin
  i2:=4321;
  i3:=0;
  aIntf.TestMethod7(1234,I2,I3,9876);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@I2,TypeInfo(SizeInt),RecordedOutputArgs[0]);
  TValue.Make(@I3,TypeInfo(SizeInt),RecordedOutputArgs[1]);
end;



procedure TTestDirectIntfCalls.Test7;
begin
  DoTestIntfImpl(@CallTestMethod7, 'TestMethod7', [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);

end;

procedure TTestDirectIntfCalls.CallTestMethod8(aIntf: ITestInterface);

var
  s2,s3 : AnsiString;

begin
  s2:='Beta';
  s3:='';
  aIntf.TestMethod8('Alpha',S2,S3,'Delta');
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(AnsiString),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(AnsiString),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test8;
begin
  DoTestIntfImpl(@CallTestMethod8, 'TestMethod8', [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod9(aIntf: ITestInterface);

Var
  S1,S2,S3,S4 : Shortstring;

begin
  S1:='Alpha';
  S2:='Beta';
  S3:='';
  S4:='Delta';
  aIntf.TestMethod9(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(ShortString),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(ShortString),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test9;
begin
  DoTestIntfImpl(@CallTestMethod9, 'TestMethod9', [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod10(aIntf: ITestInterface);

var
  S1,S2,S3,S4 : Single;

begin
  S1:=SingleArg1;
  S2:=SingleArg2in;
  S3:=0;
  S4:=SingleArg4;
  aIntf.TestMethod10(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(Single),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(Single),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test10;
begin
  DoTestIntfImpl(@CallTestMethod10, 'TestMethod10', [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod11(aIntf: ITestInterface);

var
  S1,S2,S3,S4 : Double;

begin
  S1:=DoubleArg1;
  S2:=DoubleArg2in;
  S3:=0;
  S4:=DoubleArg4;
  aIntf.TestMethod11(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(Double),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(Double),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test11;
begin
  DoTestIntfImpl(@CallTestMethod11, 'TestMethod11', [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod12(aIntf: ITestInterface);
var
  S1,S2,S3,S4 : Extended;

begin
  S1:=ExtendedArg1;
  S2:=ExtendedArg2in;
  S3:=0;
  S4:=ExtendedArg4;
  aIntf.TestMethod12(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(Extended),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(Extended),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test12;
begin
  DoTestIntfImpl(@CallTestMethod12, 'TestMethod12', [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod13(aIntf: ITestInterface);
var
  S1,S2,S3,S4 : Comp;

begin
  S1:=CompArg1;
  S2:=CompArg2in;
  S3:=0;
  S4:=CompArg4;
  aIntf.TestMethod13(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(Comp),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(Comp),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test13;
begin
  DoTestIntfImpl(@CallTestMethod13, 'TestMethod13', [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);

end;

procedure TTestDirectIntfCalls.CallTestMethod14(aIntf: ITestInterface);
var
  S1,S2,S3,S4 : Currency;

begin
  S1:=CurrencyArg1;
  S2:=CurrencyArg2in;
  S3:=0;
  S4:=CurrencyArg4;
  aIntf.TestMethod14(S1,S2,S3,S4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S2,TypeInfo(Currency),RecordedOutputArgs[0]);
  TValue.Make(@S3,TypeInfo(Currency),RecordedOutputArgs[1]);
end;


procedure TTestDirectIntfCalls.Test14;
begin
  DoTestIntfImpl( @CallTestMethod14, 'TestMethod14', [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod15(aIntf: ITestInterface);

var
  res :  SizeInt;

begin
  Res:=aIntf.TestMethod15(1,2,3,4,5,6,7,8,9,10);
  TValue.Make(@Res,TypeInfo(SizeInt),FActualResult);
end;


procedure TTestDirectIntfCalls.Test15;
begin
  DoTestIntfImpl(@CallTestMethod15, 'TestMethod15', [
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));
end;

procedure TTestDirectIntfCalls.CallTestMethod16(aIntf: ITestInterface);

var
  res : single;

begin
  Res:=aIntf.TestMethod16(SingleAddArg1,SingleAddArg2,SingleAddArg3,SingleAddArg4,SingleAddArg5,
                          SingleAddArg6,SingleAddArg7,SingleAddArg8,SingleAddArg9,SingleAddArg10);
  TValue.Make(@Res,TypeInfo(Single),FActualResult);
end;

procedure TTestDirectIntfCalls.Test16;
begin
  DoTestIntfImpl(@CallTestMethod16, 'TestMethod16', [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));
end;

procedure TTestDirectIntfCalls.CallTestMethod17(aIntf: ITestInterface);
var
  res : Double;

begin
  Res:=aIntf.TestMethod17(DoubleAddArg1,DoubleAddArg2,DoubleAddArg3,DoubleAddArg4,DoubleAddArg5,
                          DoubleAddArg6,DoubleAddArg7,DoubleAddArg8,DoubleAddArg9,DoubleAddArg10);
  TValue.Make(@Res,TypeInfo(Double),FActualResult);
end;


procedure TTestDirectIntfCalls.Test17;
begin
  DoTestIntfImpl(@CallTestMethod17, 'TestMethod17', [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestDirectIntfCalls.CallTestMethod18(aIntf: ITestInterface);
var
  res : Extended;

begin
  Res:=aIntf.TestMethod18(ExtendedAddArg1,ExtendedAddArg2,ExtendedAddArg3,ExtendedAddArg4,ExtendedAddArg5,
                          ExtendedAddArg6,ExtendedAddArg7,ExtendedAddArg8,ExtendedAddArg9,ExtendedAddArg10);
  TValue.Make(@Res,TypeInfo(Extended),FActualResult);
end;


procedure TTestDirectIntfCalls.Test18;
begin
  DoTestIntfImpl(@CallTestMethod18, 'TestMethod18', [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestDirectIntfCalls.CallTestMethod19(aIntf: ITestInterface);
var
  res : comp;

begin
  Res:=aIntf.TestMethod19(compAddArg1,compAddArg2,compAddArg3,compAddArg4,compAddArg5,
                          compAddArg6,compAddArg7,compAddArg8,compAddArg9,compAddArg10);
  TValue.Make(@Res,TypeInfo(comp),FActualResult);
end;

procedure TTestDirectIntfCalls.Test19;
begin
  DoTestIntfImpl(@CallTestMethod19, 'TestMethod19', [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));
end;

procedure TTestDirectIntfCalls.CallTestMethod20(aIntf: ITestInterface);
var
  res : Currency;

begin
  Res:=aIntf.TestMethod20(CurrencyAddArg1,CurrencyAddArg2,CurrencyAddArg3,CurrencyAddArg4,CurrencyAddArg5,
                          CurrencyAddArg6,CurrencyAddArg7,CurrencyAddArg8,CurrencyAddArg9,CurrencyAddArg10);
  TValue.Make(@Res,TypeInfo(Currency),FActualResult);
end;

procedure TTestDirectIntfCalls.Test20;
begin
  DoTestIntfImpl(@CallTestMethod20, 'TestMethod20', [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));
end;

procedure TTestDirectIntfCalls.CallTestMethod21(aIntf: ITestInterface);

var
  S1,S2,S3,S4 : Sizeint;

begin
  S1:=1234;
  S2:=4321;
  S3:=0;
  S4:=9876;
  aIntf.TestMethod21(s1,s2,s3,s4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S1,TypeInfo(SizeInt),RecordedOutputArgs[0]);
  TValue.Make(@S2,TypeInfo(SizeInt),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test21;
begin
  DoTestIntfImpl(@CallTestMethod21, 'TestMethod21', [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod21s(aIntf: ITestInterface);
var
  S1,S2,S3,S4 : AnsiString;

begin
  S1:='Alpha';
  S2:='Beta';
  S3:='';
  S4:='Delta';
  aIntf.TestMethod21(s1,s2,s3,s4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S1,TypeInfo(AnsiString),RecordedOutputArgs[0]);
  TValue.Make(@S2,TypeInfo(AnsiString),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test21s;
begin
  DoTestIntfImpl(@CallTestMethod21s, 'TestMethod21', [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);
end;

procedure TTestDirectIntfCalls.CallTestMethod22(aIntf: ITestInterface);


var
  S1,S2,S3,S4 : ShortString;

begin
  S1:='Alpha';
  S2:='Beta';
  S3:='';
  S4:='Delta';
  aIntf.TestMethod21(s1,s2,s3,s4);
  SetLength(RecordedOutputArgs,2);
  TValue.Make(@S1,TypeInfo(ShortString),RecordedOutputArgs[0]);
  TValue.Make(@S2,TypeInfo(ShortString),RecordedOutputArgs[1]);
end;

procedure TTestDirectIntfCalls.Test22;
begin
  { for some reason this fails, though it fails in Delphi as well :/ }
  (*

  DoTestIntfImpl(@CallTestMethod22, 'TestMethod21', [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);
  *)
end;

{ TTestMethodVars }

{$ifdef fpc}
procedure TTestMethodVars.TestMethodVar1;
begin
  DoMethodImpl(TypeInfo(TTestMethod1),[], [], [], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar2;
begin
  DoMethodImpl(TypeInfo(TTestMethod2),[GetIntValue(42)], [], [], GetIntValue(21));
end;

procedure TTestMethodVars.TestMethodVar3;
begin
  DoMethodImpl(TypeInfo(TTestMethod3),[GetAnsiString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar4;
begin
  DoMethodImpl(TypeInfo(TTestMethod4),[GetShortString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar5;
begin
  DoMethodImpl(TypeInfo(TTestMethod5),[], [], [], GetAnsiString('Hello World'));
end;

procedure TTestMethodVars.TestMethodVar6;
begin
  DoMethodImpl(TypeInfo(TTestMethod6),[], [], [], GetShortString('Hello World'));
end;

procedure TTestMethodVars.TestMethodVar7;
begin

  DoMethodImpl(TypeInfo(TTestMethod7),[
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar8;
begin

  DoMethodImpl(TypeInfo(TTestMethod8),[
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar9;
begin

  DoMethodImpl(TypeInfo(TTestMethod9),[
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar10;
begin
  DoMethodImpl(TypeInfo(TTestMethod10),[
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar11;
begin

  DoMethodImpl(TypeInfo(TTestMethod11),[
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar12;
begin

  DoMethodImpl(TypeInfo(TTestMethod12),[
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar13;
begin
  DoMethodImpl(TypeInfo(TTestMethod13),[
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar14;
begin

  DoMethodImpl(TypeInfo(TTestMethod14),[
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar15;
begin

  DoMethodImpl(TypeInfo(TTestMethod15),[
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));
end;

procedure TTestMethodVars.TestMethodVar16;
begin
  DoMethodImpl(TypeInfo(TTestMethod16),[
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));
end;

procedure TTestMethodVars.TestMethodVar17;
begin
  DoMethodImpl(TypeInfo(TTestMethod17),[
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestMethodVars.TestMethodVar18;
begin
  DoMethodImpl(TypeInfo(TTestMethod18),[
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestMethodVars.TestMethodVar19;
begin

  DoMethodImpl(TypeInfo(TTestMethod19),[
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));
end;

procedure TTestMethodVars.TestMethodVar20;
begin

  DoMethodImpl(TypeInfo(TTestMethod20),[
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));
end;

procedure TTestMethodVars.TestMethodVar21;
begin

  DoMethodImpl(TypeInfo(TTestMethod21),[
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar21as;
begin
  DoMethodImpl(TypeInfo(TTestMethod21),[
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);
end;

procedure TTestMethodVars.TestMethodVar21ss;
begin

  { for some reason this fails, though it fails in Delphi as well :/ }
  (*
  DoMethodImpl(TypeInfo(TTestMethod21),[
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);
  *)
end;

{ TTestProcVars }

procedure TTestProcVars.TestProcVar1;
begin
  DoProcImpl(TypeInfo(TTestProc1),[], [], [], TValue.Empty);
end;

procedure TTestProcVars.TestProcVar2;
begin
  DoProcImpl(TypeInfo(TTestProc2),[GetIntValue(42)], [], [], GetIntValue(21));
end;

procedure TTestProcVars.TestProcVar3;
begin
  DoProcImpl(TypeInfo(TTestProc3),[GetAnsiString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestProcVars.TestProcVar4;
begin
  DoProcImpl(TypeInfo(TTestProc4),[GetShortString('Hello World')], [], [], TValue.Empty);
end;

procedure TTestProcVars.TestProcVar5;
begin
  DoProcImpl(TypeInfo(TTestProc5),[], [], [], GetAnsiString('Hello World'));
end;

procedure TTestProcVars.TestProcVar6;
begin
  DoProcImpl(TypeInfo(TTestProc6),[], [], [], GetShortString('Hello World'));
end;

procedure TTestProcVars.TestProcVar7;
begin
  DoProcImpl(TypeInfo(TTestProc7),[
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar8;
begin

  DoProcImpl(TypeInfo(TTestProc8),[
      GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
    ], [
      GetAnsiString('Gamma'), GetAnsiString('Epsilon')
    ], [1, 2], TValue.Empty);


end;

procedure TTestProcVars.TestProcVar9;
begin
  DoProcImpl(TypeInfo(TTestProc9),[
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar10;
begin

  DoProcImpl(TypeInfo(TTestProc10),[
      GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
    ], [
      GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
    ], [1, 2], TValue.Empty);
end;

procedure TTestProcVars.TestProcVar11;
begin
  DoProcImpl(TypeInfo(TTestProc11),[
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);
end;

procedure TTestProcVars.TestProcVar12;
begin

  DoProcImpl(TypeInfo(TTestProc12),[
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar13;
begin
  DoProcImpl(TypeInfo(TTestProc13),[
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar14;
begin
  DoProcImpl(TypeInfo(TTestProc14),[
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar15;
begin

  DoProcImpl(TypeInfo(TTestProc15),[
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));

end;

procedure TTestProcVars.TestProcVar16;
begin

  DoProcImpl(TypeInfo(TTestProc16),[
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));

end;

procedure TTestProcVars.TestProcVar17;
begin

  DoProcImpl(TypeInfo(TTestProc17),[
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));

end;

procedure TTestProcVars.TestProcVar18;
begin

  DoProcImpl(TypeInfo(TTestProc18),[
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));

end;

procedure TTestProcVars.TestProcVar19;
begin

  DoProcImpl(TypeInfo(TTestProc19),[
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));

end;

procedure TTestProcVars.TestProcVar20;
begin

  DoProcImpl(TypeInfo(TTestProc20),[
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));

end;

procedure TTestProcVars.TestProcVar21;
begin
  DoProcImpl(TypeInfo(TTestProc21),[
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar21as;
begin

  DoProcImpl(TypeInfo(TTestProc21),[
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);

end;

procedure TTestProcVars.TestProcVar21ss;
begin

  { for some reason this fails, though it fails in Delphi as well :/ }
  {
  DoProcImpl(TypeInfo(TTestProc21),[
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);}

end;

{$endif fpc}


initialization
{$ifdef fpc}
  RegisterTest(TTestIntfMethods);
  RegisterTest(TTestMethodVars);
  RegisterTest(TTestProcVars);
  RegisterTest(TTestDirectIntfCalls)
{$else fpc}
  RegisterTest(TTestIntfMethods.Suite);
{$endif fpc}
end.

