unit Tests.Rtti.Impl;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

{.$define debug}

interface

uses
{$IFDEF FPC}
  fpcunit,testregistry, testutils,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  sysutils, typinfo, Rtti,
  Tests.Rtti.Util;

{ Note: Delphi does not provide a CreateImplementation for TRttiInvokableType
        and its descendants, so these tests are disabled for Delphi }

type
  TTestImpl = class(TTestCase)
  private
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
{$ifndef InLazIDE}
    {$ifdef fpc}generic{$endif} procedure GenDoIntfImpl<T: IInterface>(aIntf: T; aIndex: LongInt; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
    {$ifdef fpc}generic{$endif} procedure GenDoMethodImpl<T>(aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
    {$ifdef fpc}generic{$endif} procedure GenDoProcImpl<T>(aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
{$endif}
{$endif}
{$ifdef fpc}
    procedure Status(const aMsg: String); inline;
    procedure Status(const aMsg: String; const aArgs: array of const); inline;
{$endif}
  published
    procedure TestIntfMethods;
{$ifdef fpc}
    procedure TestMethodVars;
    procedure TestProcVars;
{$endif}
  end;

implementation

type
  {$push}
  {$M+}
  ITestInterface = interface
    ['{1DE799BB-BEE9-405F-9AF3-D55DE978C793}']
    procedure TestMethod1;
    function  TestMethod2(aArg1: SizeInt): SizeInt;
    procedure TestMethod3(aArg1: AnsiString);
    procedure TestMethod4(aArg1: ShortString);
    function  TestMethod5: AnsiString;
    function  TestMethod6: ShortString;
    procedure TestMethod7(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt);
    procedure TestMethod8(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString);
    procedure TestMethod9(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString);
    procedure TestMethod10(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single);
    procedure TestMethod11(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double);
    procedure TestMethod12(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended);
    procedure TestMethod13(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp);
    procedure TestMethod14(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency);
    function  TestMethod15(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
    function  TestMethod16(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
    function  TestMethod17(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
    function  TestMethod18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
    function  TestMethod19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
    function  TestMethod20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
    procedure TestMethod21(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
  end;
  {$pop}

  TTestMethod1 = procedure of object;
  TTestMethod2 = function(aArg1: SizeInt): SizeInt of object;
  TTestMethod3 = procedure(aArg1: AnsiString) of object;
  TTestMethod4 = procedure(aArg1: ShortString) of object;
  TTestMethod5 = function: AnsiString of object;
  TTestMethod6 = function: ShortString of object;
  TTestMethod7 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt) of object;
  TTestMethod8 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString) of object;
  TTestMethod9 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString) of object;
  TTestMethod10 = procedure(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single) of object;
  TTestMethod11 = procedure(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double) of object;
  TTestMethod12 = procedure(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended) of object;
  TTestMethod13 = procedure(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp) of object;
  TTestMethod14 = procedure(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency) of object;
  TTestMethod15 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt of object;
  TTestMethod16 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single of object;
  TTestMethod17 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double of object;
  TTestMethod18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended of object;
  TTestMethod19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp of object;
  TTestMethod20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency of object;
  TTestMethod21 = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4) of object;

  TTestProc1 = procedure;
  TTestProc2 = function(aArg1: SizeInt): SizeInt;
  TTestProc3 = procedure(aArg1: AnsiString);
  TTestProc4 = procedure(aArg1: ShortString);
  TTestProc5 = function: AnsiString;
  TTestProc6 = function: ShortString;
  TTestProc7 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt);
  TTestProc8 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString);
  TTestProc9 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString);
  TTestProc10 = procedure(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single);
  TTestProc11 = procedure(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double);
  TTestProc12 = procedure(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended);
  TTestProc13 = procedure(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp);
  TTestProc14 = procedure(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency);
  TTestProc15 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
  TTestProc16 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
  TTestProc17 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
  TTestProc18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
  TTestProc19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
  TTestProc20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
  TTestProc21 = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);

const
  SingleArg1: Single = 1.23;
  SingleArg2In: Single = 3.21;
  SingleArg2Out: Single = 2.34;
  SingleArg3Out: Single = 9.87;
  SingleArg4: Single = 7.89;
  SingleRes: Single = 4.32;
  SingleAddArg1 = Single(1.23);
  SingleAddArg2 = Single(2.34);
  SingleAddArg3 = Single(3.45);
  SingleAddArg4 = Single(4.56);
  SingleAddArg5 = Single(5.67);
  SingleAddArg6 = Single(9.87);
  SingleAddArg7 = Single(8.76);
  SingleAddArg8 = Single(7.65);
  SingleAddArg9 = Single(6.54);
  SingleAddArg10 = Single(5.43);
  SingleAddRes = SingleAddArg1 + SingleAddArg2 + SingleAddArg3 + SingleAddArg4 + SingleAddArg5 +
                 SingleAddArg6 + SingleAddArg7 + SingleAddArg8 + SingleAddArg9 + SingleAddArg10;

  DoubleArg1: Double = 1.23;
  DoubleArg2In: Double = 3.21;
  DoubleArg2Out: Double = 2.34;
  DoubleArg3Out: Double = 9.87;
  DoubleArg4: Double = 7.89;
  DoubleRes: Double = 4.32;
  DoubleAddArg1 = Double(1.23);
  DoubleAddArg2 = Double(2.34);
  DoubleAddArg3 = Double(3.45);
  DoubleAddArg4 = Double(4.56);
  DoubleAddArg5 = Double(5.67);
  DoubleAddArg6 = Double(9.87);
  DoubleAddArg7 = Double(8.76);
  DoubleAddArg8 = Double(7.65);
  DoubleAddArg9 = Double(6.54);
  DoubleAddArg10 = Double(5.43);
  DoubleAddRes = DoubleAddArg1 + DoubleAddArg2 + DoubleAddArg3 + DoubleAddArg4 + DoubleAddArg5 +
                 DoubleAddArg6 + DoubleAddArg7 + DoubleAddArg8 + DoubleAddArg9 + DoubleAddArg10;

  ExtendedArg1: Extended = 1.23;
  ExtendedArg2In: Extended = 3.21;
  ExtendedArg2Out: Extended = 2.34;
  ExtendedArg3Out: Extended = 9.87;
  ExtendedArg4: Extended = 7.89;
  ExtendedRes: Extended = 4.32;
  ExtendedAddArg1 = Extended(1.23);
  ExtendedAddArg2 = Extended(2.34);
  ExtendedAddArg3 = Extended(3.45);
  ExtendedAddArg4 = Extended(4.56);
  ExtendedAddArg5 = Extended(5.67);
  ExtendedAddArg6 = Extended(9.87);
  ExtendedAddArg7 = Extended(8.76);
  ExtendedAddArg8 = Extended(7.65);
  ExtendedAddArg9 = Extended(6.54);
  ExtendedAddArg10 = Extended(5.43);
  ExtendedAddRes = ExtendedAddArg1 + ExtendedAddArg2 + ExtendedAddArg3 + ExtendedAddArg4 + ExtendedAddArg5 +
                 ExtendedAddArg6 + ExtendedAddArg7 + ExtendedAddArg8 + ExtendedAddArg9 + ExtendedAddArg10;

  CurrencyArg1: Currency = 1.23;
  CurrencyArg2In: Currency = 3.21;
  CurrencyArg2Out: Currency = 2.34;
  CurrencyArg3Out: Currency = 9.87;
  CurrencyArg4: Currency = 7.89;
  CurrencyRes: Currency = 4.32;
  CurrencyAddArg1 = Currency(1.23);
  CurrencyAddArg2 = Currency(2.34);
  CurrencyAddArg3 = Currency(3.45);
  CurrencyAddArg4 = Currency(4.56);
  CurrencyAddArg5 = Currency(5.67);
  CurrencyAddArg6 = Currency(9.87);
  CurrencyAddArg7 = Currency(8.76);
  CurrencyAddArg8 = Currency(7.65);
  CurrencyAddArg9 = Currency(6.54);
  CurrencyAddArg10 = Currency(5.43);
  CurrencyAddRes = CurrencyAddArg1 + CurrencyAddArg2 + CurrencyAddArg3 + CurrencyAddArg4 + CurrencyAddArg5 +
                 CurrencyAddArg6 + CurrencyAddArg7 + CurrencyAddArg8 + CurrencyAddArg9 + CurrencyAddArg10;

  CompArg1: Comp = 123;
  CompArg2In: Comp = 321;
  CompArg2Out: Comp = 234;
  CompArg3Out: Comp = 987;
  CompArg4: Comp = 789;
  CompRes: Comp = 432;
  CompAddArg1 = Comp(123);
  CompAddArg2 = Comp(234);
  CompAddArg3 = Comp(345);
  CompAddArg4 = Comp(456);
  CompAddArg5 = Comp(567);
  CompAddArg6 = Comp(987);
  CompAddArg7 = Comp(876);
  CompAddArg8 = Comp(765);
  CompAddArg9 = Comp(654);
  CompAddArg10 = Comp(543);
  CompAddRes = CompAddArg1 + CompAddArg2 + CompAddArg3 + CompAddArg4 + CompAddArg5 +
                 CompAddArg6 + CompAddArg7 + CompAddArg8 + CompAddArg9 + CompAddArg10;

{ TTestImpl }

{$ifdef fpc}
procedure TTestImpl.Status(const aMsg: String);
begin
{$ifdef debug}
  Writeln(aMsg);
{$endif}
end;

procedure TTestImpl.Status(const aMsg: String; const aArgs: array of const);
begin
{$ifdef debug}
  Writeln(Format(aMsg, aArgs));
{$endif}
end;
{$endif}

procedure TTestImpl.OnHandleIntfMethod(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);
var
  selfofs, i: SizeInt;
  name: String;
begin
  selfofs := 1;

  Status('In Callback');
  InvokedMethodName :=  aMethod.Name;
  Status('Self: ' + HexStr(Self));
  if Assigned(aMethod.ReturnType) then
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

procedure TTestImpl.DoIntfImpl(aIntf: IInterface; aTypeInfo: PTypeInfo; aIndex: LongInt; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
var
  context: TRttiContext;
  t: TRttiType;
  instance, res: TValue;
  method: TRttiMethod;
  i: SizeInt;
  input: array of TValue;
  intf: TRttiInterfaceType;
  mrec: TMethod;
  name: String;
  params: array of TRttiParameter;
begin
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
procedure TTestImpl.OnHandleInvokable(aInvokable: TRttiInvokableType; const aArgs: TValueArray; out
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

procedure TTestImpl.DoMethodImpl(aTypeInfo: PTypeInfo; aInputArgs,
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

procedure TTestImpl.DoProcImpl(aTypeInfo: PTypeInfo; aInputArgs,
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

{$ifndef InLazIDE}
{$ifdef fpc}generic{$endif} procedure TTestImpl.GenDoIntfImpl<T>(aIntf: T; aIndex: LongInt; aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
begin
  DoIntfImpl(aIntf, TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aInOutMapping, aResult);
end;

{$ifdef fpc}generic{$endif} procedure TTestImpl.GenDoMethodImpl<T>(aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
begin
  DoMethodImpl(TypeInfo(T), aInputArgs, aOutputArgs, aInOutMapping, aResult);
end;

{$ifdef fpc}generic{$endif} procedure TTestImpl.GenDoProcImpl<T>(aInputArgs, aOutputArgs: TValueArray; aInOutMapping: array of SizeInt; aResult: TValue);
begin
  DoProcImpl(TypeInfo(T), aInputArgs, aOutputArgs, aInOutMapping, aResult);
end;
{$endif}

procedure TTestImpl.TestIntfMethods;
var
  intf: ITestInterface;
begin
  try
    intf := TVirtualInterface.Create(PTypeInfo(TypeInfo(ITestInterface)), {$ifdef fpc}@{$endif}OnHandleIntfMethod) as ITestInterface;
  except
    on e: ENotImplemented do
      Ignore('TVirtualInterface not supported for ' + {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%});
  end;
  Check(Assigned(intf), 'ITestInterface instance is Nil');

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 1, [], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 2, [GetIntValue(42)], [], [], GetIntValue(21));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 3, [GetAnsiString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 4, [GetShortString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 5, [], [], [], GetAnsiString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 6, [], [], [], GetShortString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 7, [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 8, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 9, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 10, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 11, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 12, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 13, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 14, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 15, [
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 16, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 17, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 18, [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 19, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 20, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 21, [
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 21, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);

  { for some reason this fails, though it fails in Delphi as well :/ }
  {{$ifdef fpc}specialize{$endif}GenDoIntfImpl<ITestInterface>(intf, 21, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);}
end;

{$ifdef fpc}
procedure TTestImpl.TestMethodVars;
begin
  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod1>([], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod2>([GetIntValue(42)], [], [], GetIntValue(21));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod3>([GetAnsiString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod4>([GetShortString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod5>([], [], [], GetAnsiString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod6>([], [], [], GetShortString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod7>([
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod8>([
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod9>([
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod10>([
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod11>([
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod12>([
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod13>([
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod14>([
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod15>([
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod16>([
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod17>([
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod18>([
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod19>([
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod20>([
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod21>([
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod21>([
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);

  { for some reason this fails, though it fails in Delphi as well :/ }
  {{$ifdef fpc}specialize{$endif}GenDoMethodImpl<TTestMethod21>([
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);}
end;

procedure TTestImpl.TestProcVars;
begin
  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc1>([], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc2>([GetIntValue(42)], [], [], GetIntValue(21));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc3>([GetAnsiString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc4>([GetShortString('Hello World')], [], [], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc5>([], [], [], GetAnsiString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc6>([], [], [], GetShortString('Hello World'));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc7>([
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc8>([
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc9>([
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc10>([
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
  ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc11>([
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
  ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc12>([
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
  ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc13>([
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
  ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc14>([
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
  ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
  ], [1, 2], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc15>([
    GetIntValue(1), GetIntValue(2), GetIntValue(3), GetIntValue(4), GetIntValue(5),
    GetIntValue(6), GetIntValue(7), GetIntValue(8), GetIntValue(9), GetIntValue(10)
  ], [], [], GetIntValue(11));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc16>([
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
  ], [], [], GetSingleValue(SingleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc17>([
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
  ], [], [], GetDoubleValue(DoubleAddRes));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc18>([
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
  ], [], [], GetExtendedValue(ExtendedAddRes));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc19>([
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
  ], [], [], GetCompValue(CompAddRes));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc20>([
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], [], GetCurrencyValue(CurrencyAddRes));

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc21>([
    GetIntValue(1234), GetIntValue(4321), GetIntValue(0), GetIntValue(9876)
  ], [
    GetIntValue(5678), GetIntValue(6789)
  ], [0, 1], TValue.Empty);

  {$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc21>([
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
  ], [
    GetAnsiString('Gamma'), GetAnsiString('Epsilon')
  ], [0, 1], TValue.Empty);

  { for some reason this fails, though it fails in Delphi as well :/ }
  {{$ifdef fpc}specialize{$endif}GenDoProcImpl<TTestProc21>([
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
  ], [
    GetShortString('Gamma'), GetShortString('Epsilon')
  ], [0, 1], TValue.Empty);}
end;
{$endif}

initialization
{$ifdef fpc}
  RegisterTest(TTestImpl);
{$else fpc}
  RegisterTest(TTestImpl.Suite);
{$endif fpc}
end.

