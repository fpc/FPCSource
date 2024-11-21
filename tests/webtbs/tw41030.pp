program tw41030;
{$APPTYPE CONSOLE}
{$RTTI EXPLICIT METHODS([vcPublished]) PROPERTIES([vcPublished]) FIELDS([vcPublished])}
{$M+}
{$ifdef fpc}
{$mode DELPHI}
uses
  SysUtils, TypInfo, Rtti {$ifndef WINDOWS} , ffi.manager {$endif}
  ;
{$else}
{$R *.res}
uses
  SysUtils, Rtti;
{$endif}


var ErrorCount: Integer = 0;

procedure AddError(const AMsg: string);
begin
  WriteLn(AMsg);
  Inc(ErrorCount);
end;

type
  TEnum1 = (en1_1, en1_2);
  TEnum2 = (en2_1);
  TEnum3 = en1_1..en1_1;


  { TTestObj }

  TTestObj = class
  published
    function Test(Arg: Single): Double;
    procedure Test2(var Arg: Single);
    procedure Test3(Arg: TEnum1);
    function Test4(Arg: UInt8): UInt8;
  end;


function TTestObj.Test(Arg: Single): Double;
begin
  Result := Arg + 1;
end;

procedure TTestObj.Test2(var Arg: Single);
begin
  Arg := Arg + 1;
end;

procedure TTestObj.Test3(Arg: TEnum1);
begin

end;

function TTestObj.Test4(Arg: UInt8): UInt8;
begin
  Result := Arg + 1;
end;

var
  Context: TRttiContext;
procedure ExpectedInvocationException(const AMethodName: string;
  const AInstance: TValue; const AArgs: array of TValue);
var
  HasException: boolean;
begin
  HasException := False;
  try
    Context.GetType(TTestObj).GetMethod(AMethodName).Invoke(AInstance, AArgs);
  except
{$ifndef fpc}
    on EInvalidCast do
      HasException := True;
{$endif}
    on EInvocationError do
      HasException := True;
  end;
  if not HasException then
    AddError('Expected exception on call method ' + AMethodName);
end;

procedure Check(ACondition: boolean; const AMsg: string);
begin
  if not ACondition then
    AddError(AMsg);
end;

var
  Instance: TValue;
  M: TRttiMethod;
  TempV: TValue;
begin
  Check(en1_1 = (TValue.From<TEnum1>(en1_1).Cast<TEnum3>.AsType<TEnum3>), 'en1_1 = (TValue.From<TEnum1>(en1_1).Cast<TEnum3>.AsType<TEnum3>)');
  Check(not (TValue.From<Integer>(32).TryCast(TypeInfo(AnsiChar), TempV)), 'not (TValue.From<Integer>(32).TryCast(TypeInfo(AnsiChar), V)');
  Check(not (TValue.From<Integer>(32).TryCast(TypeInfo(WideChar), TempV)), 'not (TValue.From<Integer>(32).TryCast(TypeInfo(WideChar), V)');
{$ifdef fpc}
  Check(not (TValue.From<Integer>(32).TryCast(TypeInfo(UnicodeChar), TempV)), 'not (TValue.From<Integer>(32).TryCast(TypeInfo(UnicodeChar), V)');
{$endif}
  Check(Byte(397) = (TValue.From<Integer>(397).Cast<Byte>().AsType<Byte>), 'Byte(397) = (TValue.From<Integer>(397).Cast<Byte>().AsType<Byte>)');
  Check(32 = (TValue.From<Byte>(32).Cast<Integer>().AsType<Integer>), '32 = (TValue.From<Byte>(32).Cast<Integer>().AsType<Integer>)');

  Context := TRttiContext.Create;
  Instance := TValue.From<TTestObj>(TTestObj.Create);
  M := Context.GetType(TTestObj).GetMethod('Test');
  if (M.Invoke(Instance, [TValue.From<Double>(10)]).AsType<Double>) <> 11 then
    AddError('Test(Double(10) <> 11)');

  ExpectedInvocationException('Test', TValue.From<TObject>(TObject.Create), [TValue.From<Double>(10)]);
  ExpectedInvocationException('Test2', Instance, [TValue.From<Double>(10)]);

  Context.GetType(TTestObj).GetMethod('Test3').Invoke(Instance, [TValue.From<TEnum3>(en1_1)]);
  ExpectedInvocationException('Test3', Instance, [TValue.From<TEnum2>(en2_1)]);

  Instance.AsType<TTestObj>.Free;

  Context.Free;

  if ErrorCount <> 0 then
    Halt(ErrorCount);
  WriteLn('OK');
end.