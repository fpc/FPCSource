program bfloatfunc;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

{ Utility functions }
function GetRealTime(const st: TSystemTime): Real;
  begin
    Result := st.Hour*3600.0 + st.Minute*60.0 + st.Second + st.MilliSecond/1000.0;
  end;

{$push}
{$warn 5057 off}
function GetRealTime : Real;
  var
    st:TSystemTime;
  begin
    GetLocalTime(st);
    result:=GetRealTime(st);
  end;
{$pop}

function IIf(Condition: Boolean; TrueRes, FalseRes: Integer): Integer; inline;
  begin
    if Condition then
      Result := TrueRes
    else
      Result := FalseRes;
  end;

const
{$ifdef IN_TESTS}
  ITERATIONS = 1;
{$else}
  ITERATIONS = 33554432;
{$endif}

{ TTestAncestor }
type
  TTestAncestor = class
    private
      FStartTime: Real;
      FEndTime: Real;
      FAvgTime: Real;
      procedure SetStartTime;
      procedure SetEndTime;
    protected
      procedure DoTestIteration(Iteration: Integer); virtual; abstract;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Run;
      function TestTitle: shortstring; virtual; abstract;
      function WriteResults: Boolean; virtual; abstract;
      property RunTime: Real read FAvgTime;
  end;

  TTestClass = class of TTestAncestor;

  TFloat32Test = class(TTestAncestor)
    protected
      FResultStorage: array[0..7] of Single;
      FExpected: array[0..7] of Single;
      class function IsEqual(Value, Reference: Single): Boolean; static;
    public
      function WriteResults: Boolean; override;
  end;

  TFloat32OneInputTest = class(TFloat32Test)
    protected
      FInputs: array[0..7] of Single;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Input: Single): Single; virtual; abstract;
  end;

  TFloat32TwoInputTest = class(TFloat32Test)
    protected
      FInputs: array[0..7] of array[0..1] of Single;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Input1, Input2: Single): Single; virtual; abstract;
  end;
  
  TSingleIntPair = record
    S: Single;
    N: Integer;
  end;

  TFloat32FloatIntTest = class(TFloat32Test)
    protected
      FInputs: array[0..7] of TSingleIntPair;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Float: Single; N: Integer): Single; virtual; abstract;
  end;

  TFloat64Test = class(TTestAncestor)
    protected
      FResultStorage: array[0..7] of Double;
      FExpected: array[0..7] of Double;
      class function IsEqual(Value, Reference: Double): Boolean; static;
    public
      function WriteResults: Boolean; override;
  end;

  TFloat64OneInputTest = class(TFloat64Test)
    protected
      FInputs: array[0..7] of Double;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Input: Double): Double; virtual; abstract;
  end;

  TFloat64TwoInputTest = class(TFloat64Test)
    protected
      FInputs: array[0..7] of array[0..1] of Double;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Input1, Input2: Double): Double; virtual; abstract;
  end;
  
  TDoubleIntPair = record
    D: Double;
    N: Integer;
  end;

  TFloat64FloatIntTest = class(TFloat64Test)
    protected
      FInputs: array[0..7] of TDoubleIntPair;
      procedure DoTestIteration(Iteration: Integer); override;
      function DoFunc(Float: Double; N: Integer): Double; virtual; abstract;
  end;

  { Test classes for actual tests }

  { 32-bit floating-point }
  TFloat32MinTest = class(TFloat32TwoInputTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32ImplicitMinTest = class(TFloat32MinTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      function TestTitle: shortstring; override;
  end;

  TFloat32MinSpecialTest = class(TFloat32MinTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32ImplicitMinSpecialTest = class(TFloat32MinSpecialTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      function TestTitle: shortstring; override;
  end;

  TFloat32MaxTest = class(TFloat32TwoInputTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32ImplicitMaxTest = class(TFloat32MaxTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      function TestTitle: shortstring; override;
  end;
  
  TFloat32MaxSpecialTest = class(TFloat32MaxTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32ImplicitMaxSpecialTest = class(TFloat32MaxSpecialTest)
    protected
      function DoFunc(Input1, Input2: Single): Single; override;
    public
      function TestTitle: shortstring; override;
  end;
  
  TFloat32SqrtTest = class(TFloat32OneInputTest)
    protected
      function DoFunc(Input: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32SqrtSpecialTest = class(TFloat32SqrtTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32LnTest = class(TFloat32OneInputTest)
    protected
      function DoFunc(Input: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32LnSpecialTest = class(TFloat32LnTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32ExpTest = class(TFloat32OneInputTest)
    protected
      function DoFunc(Input: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32ExpSpecialTest = class(TFloat32ExpTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32SinTest = class(TFloat32OneInputTest)
    protected
      function DoFunc(Input: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32SinSpecialTest = class(TFloat32SinTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32CosTest = class(TFloat32OneInputTest)
    protected
      function DoFunc(Input: Single): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat32CosSpecialTest = class(TFloat32CosTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32LdexpTest = class(TFloat32FloatIntTest)
    protected
      function DoFunc(Float: Single; N: Integer): Single; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat32LdexpSpecialTest = class(TFloat32LdexpTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  { 64-bit floating-point }
  TFloat64MinTest = class(TFloat64TwoInputTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64ImplicitMinTest = class(TFloat64MinTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      function TestTitle: shortstring; override;
  end;

  TFloat64MinSpecialTest = class(TFloat64MinTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64ImplicitMinSpecialTest = class(TFloat64MinSpecialTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      function TestTitle: shortstring; override;
  end;

  TFloat64MaxTest = class(TFloat64TwoInputTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64ImplicitMaxTest = class(TFloat64MaxTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      function TestTitle: shortstring; override;
  end;
  
  TFloat64MaxSpecialTest = class(TFloat64MaxTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64ImplicitMaxSpecialTest = class(TFloat64MaxSpecialTest)
    protected
      function DoFunc(Input1, Input2: Double): Double; override;
    public
      function TestTitle: shortstring; override;
  end;
  
  TFloat64SqrtTest = class(TFloat64OneInputTest)
    protected
      function DoFunc(Input: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64SqrtSpecialTest = class(TFloat64SqrtTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64LnTest = class(TFloat64OneInputTest)
    protected
      function DoFunc(Input: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64LnSpecialTest = class(TFloat64LnTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64ExpTest = class(TFloat64OneInputTest)
    protected
      function DoFunc(Input: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64ExpSpecialTest = class(TFloat64ExpTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64SinTest = class(TFloat64OneInputTest)
    protected
      function DoFunc(Input: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64SinSpecialTest = class(TFloat64SinTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64CosTest = class(TFloat64OneInputTest)
    protected
      function DoFunc(Input: Double): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;
  
  TFloat64CosSpecialTest = class(TFloat64CosTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64LdexpTest = class(TFloat64FloatIntTest)
    protected
      function DoFunc(Float: Double; N: Integer): Double; override;
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

  TFloat64LdexpSpecialTest = class(TFloat64LdexpTest)
    public
      constructor Create; override;
      function TestTitle: shortstring; override;
  end;

{ TTestAncestor }
constructor TTestAncestor.Create;
  begin
    FStartTime := 0;
    FEndTime := 0;
    FAvgTime := 0;
  end;

destructor TTestAncestor.Destroy;
  begin
    inherited Destroy;
  end;

procedure TTestAncestor.SetStartTime;
  begin
    FStartTime := GetRealTime();
  end;

procedure TTestAncestor.SetEndTime;
  begin
    FEndTime := GetRealTime();
    if FEndTime < FStartTime then { Happens if the test runs past midnight }
      FEndTime := FEndTime + 86400.0;
  end;

procedure TTestAncestor.Run;
  var
    X: Integer;
  begin
    SetStartTime;
    for X := 0 to ITERATIONS - 1 do
      DoTestIteration(X);

    SetEndTime;

    FAvgTime := FEndTime - FStartTime;
  end;

{ TFloat32Test }

class function TFloat32Test.IsEqual(Value, Reference: Single): Boolean;
  var
    Epsilon: Single;
  begin
    if IsNan(Reference) then
      Exit(IsNan(Value))
    else if IsInfinite(Reference) then
      Exit(Value = Reference) { Must be the same infinity }
    else if Abs(Reference) < 0.25 then
      Epsilon := 0.00000095367431640625 { 2^-20 ~ 10^-6 }
    else
      Epsilon := Power(2, Floor(Ln(Reference) / Ln(2)) - 18);

    Result := Abs(Value - Reference) <= Epsilon; { If Value is NaN, Result will be set to False } 
  end;

function TFloat32Test.WriteResults: Boolean;
  var
    X: Byte;
  begin
    Result := True;

    for X := 0 to High(FResultStorage) do
      if not IsEqual(FResultStorage[X], FExpected[X]) then
        begin
          WriteLn('FAIL - index ', X, '; expected ', FExpected[X], ' got ', FResultStorage[X]);
          Result := False;
          Exit;
        end;
  end;

{ TFloat32OneInputTest }

procedure TFloat32OneInputTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration]);
  end;

{ TFloat32TwoInputTest }

procedure TFloat32TwoInputTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration][0], FInputs[Iteration][1]);
  end;

{ TFloat32FloatIntTest }

procedure TFloat32FloatIntTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration].S, FInputs[Iteration].N);
  end;

{ TFloat32MinTest }
const
  MINMAX_INPUTS: array[0..7] of array[0..1] of Single = (
    (-0.5, 0.5),
    (1048576.0, 1048577.0),
    (-1048576.0, -1048577.0),
    (0.0, -0.0),
    (0.0, 1E4),
    (0.0, -1E4),
    (0.0, 1E-4),
    (0.0, -1E-4)
  );

  MIN_EXPECTED: array[0..7] of Single = (
    -0.5,
    1048576.0,
    -1048577.0,
    -0.0,
    0.0,
    -1E4,
    0.0,
    -1E-4
  );
  
  MAX_EXPECTED: array[0..7] of Single = (
    0.5,
    1048577.0,
    -1048576.0,
    -0.0,
    1E4,
    0.0,
    1E-4,
    0.0
  );
  
constructor TFloat32MinTest.Create;
begin
  inherited Create;
  Move(MINMAX_INPUTS, FInputs, SizeOf(FInputs));
  Move(MIN_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32MinTest.DoFunc(Input1, Input2: Single): Single;
  begin
    Result := Min(Input1, Input2);
  end;
  
function TFloat32MinTest.TestTitle: shortstring;
  begin
    Result := 'Min (single-precision)';
  end;

function TFloat32ImplicitMinTest.DoFunc(Input1, Input2: Single): Single;
  begin
    if Input1 < Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat32ImplicitMinTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Min (single-precision)';
  end;

{ TFloat32MaxTest }

constructor TFloat32MaxTest.Create;
begin
  inherited Create;
  Move(MINMAX_INPUTS, FInputs, SizeOf(FInputs));
  Move(MAX_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32MaxTest.DoFunc(Input1, Input2: Single): Single;
  begin
    Result := Max(Input1, Input2);
  end;
  
function TFloat32MaxTest.TestTitle: shortstring;
  begin
    Result := 'Max (single-precision)';
  end;

function TFloat32ImplicitMaxTest.DoFunc(Input1, Input2: Single): Single;
  begin
    if Input1 > Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat32ImplicitMaxTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Max (single-precision)';
  end;

{ TFloat32MinTest }
const
  MINMAX_SPECIAL_INPUTS: array[0..7] of array[0..1] of Single = (
    (NaN, 0.0),
    (0.0, NaN),
    (0.0, Infinity),
    (Infinity, 0.0),
    (0.0, NegInfinity),
    (NegInfinity, 0.0),
    (Infinity, NegInfinity),
    (NegInfinity, Infinity)
  );
  
  MIN_SPECIAL_EXPECTED: array[0..7] of Single = (
    0.0,
    NaN,
    0.0,
    0.0,
    NegInfinity,
    NegInfinity,
    NegInfinity,
    NegInfinity
  );
  
  MAX_SPECIAL_EXPECTED: array[0..7] of Single = (
    0.0,
    NaN,
    Infinity,
    Infinity,
    0.0,
    0.0,
    Infinity,
    Infinity
  );
  
constructor TFloat32MinSpecialTest.Create;
begin
  inherited Create;
  Move(MINMAX_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(MIN_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32MinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Min (special single-precision)';
  end;

function TFloat32ImplicitMinSpecialTest.DoFunc(Input1, Input2: Single): Single;
  begin
    if Input1 < Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat32ImplicitMinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Min (special single-precision)';
  end;

{ TFloat32MaxTest }

constructor TFloat32MaxSpecialTest.Create;
begin
  inherited Create;
  Move(MINMAX_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(MAX_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32MaxSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Max (special single-precision)';
  end;

function TFloat32ImplicitMaxSpecialTest.DoFunc(Input1, Input2: Single): Single;
  begin
    if Input1 > Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat32ImplicitMaxSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Max (special single-precision)';
  end;

{ TFloat32SqrtTest }
const
  SQRT_INPUTS: array[0..7] of Single = (
    4.0,
    1.0,
    256.0,
    2.0,
    100.0,
    0.5,
    3.1415926535897932384626433832795, { Pi }
    1.5707963267948966192313216916398 { Pi / 2 }
  );
  
  SQRT_EXPECTED: array[0..7] of Single = (
    2.0,
    1.0,
    16.0,
    1.414213562373095,
    10.0,
    0.707106781186548,
    1.772453850905516,
    1.253314137315500
  );

constructor TFloat32SqrtTest.Create;
begin
  inherited Create;
  Move(SQRT_INPUTS, FInputs, SizeOf(FInputs));
  Move(SQRT_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32SqrtTest.DoFunc(Input: Single): Single;
  begin
    Result := Sqrt(Input);
  end;
  
function TFloat32SqrtTest.TestTitle: shortstring;
  begin
    Result := 'sqrt(x) (single-precision)';
  end;

{ TFloat32SqrtSpecialTest }
const
  SQRT_SPECIAL_INPUTS: array[0..7] of Single = (
    0.0,
    Infinity,
    NegInfinity,
    NaN,
    -0.0,
    -1.0,
    1E6,
    NaN
  );
  
  SQRT_SPECIAL_EXPECTED: array[0..7] of Single = (
    0.0,
    Infinity,
    NaN,
    NaN,
    0.0,
    NaN,
    1E3,
    NaN
  );

constructor TFloat32SqrtSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(SQRT_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32SqrtSpecialTest.TestTitle: shortstring;
  begin
    Result := 'sqrt(x) (special single-precision)';
  end;

{ TFloat32LnTest }

const  
  LN_EXPECTED: array[0..7] of Single = (
    1.386294361119891,  
    0.0,
    5.545177444479562,
    0.693147180559945,
    4.605170185988091,
    -0.693147180559945,
    1.1447298858494002,
    0.4515827052894549
  );

constructor TFloat32LnTest.Create;
begin
  inherited Create;
  Move(SQRT_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(LN_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32LnTest.DoFunc(Input: Single): Single;
  begin
    Result := Ln(Input);
  end;
  
function TFloat32LnTest.TestTitle: shortstring;
  begin
    Result := 'ln x (single-precision)';
  end;

{ TFloat32LnSpecialTest }

const
  LN_SPECIAL_EXPECTED: array[0..7] of Single = (
    NegInfinity,
    Infinity,
    NaN,
    NaN,
    NegInfinity,
    NaN,
    13.815510557964274,
    NaN
  );

constructor TFloat32LnSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_SPECIAL_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(LN_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32LnSpecialTest.TestTitle: shortstring;
  begin
    Result := 'ln x (special single-precision)';
  end;

{ TFloat32ExpTest }
const
  EXP_EXPECTED: array[0..7] of Single = (
    54.598150033144239, 
    2.718281828459045,
    1.5114276650041035e+111,
    7.3890560989306502,
    2.6881171418161354e+43,
    1.6487212707001281, 
    23.1406926327792690,
    4.8104773809653517
  );

constructor TFloat32ExpTest.Create;
begin
  inherited Create;
  Move(SQRT_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(EXP_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32ExpTest.DoFunc(Input: Single): Single;
  begin
    Result := Exp(Input);
  end;
  
function TFloat32ExpTest.TestTitle: shortstring;
  begin
    Result := 'e^x (single-precision)';
  end;

{ TFloat32ExpSpecialTest }
const
  EXP_SPECIAL_EXPECTED: array[0..7] of Single = (
    1.0,
    Infinity,
    0,
    NaN,
    1.0,
    0.3678794411714423,
    Infinity,
    NaN
  );

constructor TFloat32ExpSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_SPECIAL_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(EXP_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32ExpSpecialTest.TestTitle: shortstring;
  begin
    Result := 'e^x (special single-precision)';
  end;

{ TFloat32SinTest }
const
  SIN_EXPECTED: array[0..7] of Single = (
    -0.756802495307928,
    0.841470984807897,
    -0.999208034107063,
    0.9092974268256817,
    -0.5063656411097588,
    0.4794255386042030,
    0.0,
    1.0
  );

constructor TFloat32SinTest.Create;
begin
  inherited Create;
  Move(SQRT_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(SIN_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32SinTest.DoFunc(Input: Single): Single;
  begin
    Result := Sin(Input);
  end;
  
function TFloat32SinTest.TestTitle: shortstring;
  begin
    Result := 'sin x (single-precision)';
  end;

{ TFloat32SinSpecialTest }
const
  SIN_SPECIAL_INPUTS: array[0..7] of Single = ( { Using different inputs as infinities are invalid }
    0.0,
    -1.0,
    1E6,
    NaN,
    -0.0,
    -1.0,
    1E6,
    NaN
  );

  SIN_SPECIAL_EXPECTED: array[0..7] of Single = (
    0.0,
    -0.8414709848078965,
    -0.3499935021712930,
    NaN,
    -0.0,
    -0.8414709848078965,
    -0.3499935021712930,    
    NaN
  );

constructor TFloat32SinSpecialTest.Create;
begin
  inherited Create;
  Move(SIN_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(SIN_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32SinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'sin x (special single-precision)';
  end;

{ TFloat32CosTest }
const
  COS_EXPECTED: array[0..7] of Single = (
    -0.6536436208636119,
    0.5403023058681397,
    -0.0397907599311577,
    -0.4161468365471424,
    0.8623188722876839,
    0.8775825618903727,
    -1.0, { Pi }
    0.0
  );

constructor TFloat32CosTest.Create;
begin
  inherited Create;
  Move(SQRT_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(COS_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32CosTest.DoFunc(Input: Single): Single;
  begin
    Result := Cos(Input);
  end;
  
function TFloat32CosTest.TestTitle: shortstring;
  begin
    Result := 'cos x (single-precision)';
  end;

{ TFloat32SinSpecialTest }
const
  COS_SPECIAL_EXPECTED: array[0..7] of Single = (
    1.0,
    0.5403023058681397,
    0.9367521275331448,
    NaN,
    1.0,
    0.5403023058681397,
    0.9367521275331448,
    NaN
  );

constructor TFloat32CosSpecialTest.Create;
begin
  inherited Create;
  Move(SIN_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(COS_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat32CosSpecialTest.TestTitle: shortstring;
  begin
    Result := 'cos x (special single-precision)';
  end;

{ TFloat32LdexpTest }
const
  LDEXP_INPUTS: array[0..7] of TSingleIntPair = (
    (S: 1.0; N: 0),
    (S: 2.7182818284590452353602874713527; N: -1),
    (S: 0.85309609426787873559547283129472; N: 6),
    (S: 0.1; N: 1),
    (S: 0.2; N: 2),
    (S: 0.3; N: 3),
    (S: 0.4; N: 4),
    (S: 0.5; N: 5)
  );

  LDEXP_EXPECTED: array[0..7] of Single = (
    1.0,
    1.3591409142295226176801437356763,
    54.598150033144239078110261202862,
    0.2,
    0.8,
    2.4,
    6.4,
    16.0
  );

constructor TFloat32LdexpTest.Create;
begin
  inherited Create;
  Move(LDEXP_INPUTS, FInputs, SizeOf(FInputs));
  Move(LDEXP_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32LdexpTest.DoFunc(Float: Single; N: Integer): Single;
  begin
    Result := LdExp(Float, N);
  end;

function TFloat32LdexpTest.TestTitle: shortstring;
  begin
    Result := 'ldexp(x, n) (single-precision)';
  end;

{ TFloat32SpecialLdexpTest }
const
  LDEXP_SPECIAL_INPUTS: array[0..7] of TSingleIntPair = (
    (S: Infinity; N: 0),
    (S: NegInfinity; N: 0),
    (S: 2.0; N: -128),
    (S: 2.0; N: 127),
    (S: NaN; N: 1),
    (S: 0.0; N: 16384),
    (S: 1.7014118346046923173168730371588e+38; N: -128),
    (S: 2.9387358770557187699218413430556e-39; N: 127)
  );

  LDEXP_SPECIAL_EXPECTED: array[0..7] of Single = (
    Infinity,
    NegInfinity,
    2.9387358770557187699218413430556e-39,
    Infinity,
    Nan,
    0.0,
    0.5,
    0.5
  );

constructor TFloat32LdexpSpecialTest.Create;
begin
  inherited Create;
  Move(LDEXP_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(LDEXP_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat32LdexpSpecialTest.TestTitle: shortstring;
  begin
    Result := 'ldexp(x, n) (special single-precision)';
  end;

{ TFloat64Test }

class function TFloat64Test.IsEqual(Value, Reference: Double): Boolean;
  var
    Epsilon: Double;
  begin
    if IsNan(Reference) then
      Exit(IsNan(Value))
    else if IsInfinite(Reference) then
      Exit(Value = Reference)
    else if Abs(Reference) < 0.25 then
      Epsilon := 9.094947017729282379150390625e-13 { 2^-40 ~ 10^-12 }
    else
      Epsilon := Power(2, Floor(Ln(Reference) / Ln(2)) - 18);

    Result := Abs(Value - Reference) <= Epsilon;     
  end;

function TFloat64Test.WriteResults: Boolean;
  var
    X: Byte;
  begin
    Result := True;

    for X := 0 to High(FResultStorage) do
      if not IsEqual(FResultStorage[X], FExpected[X]) then
        begin
          WriteLn('FAIL - index ', X, '; expected ', FExpected[X], ' got ', FResultStorage[X]);
          Result := False;
          Exit;
        end;
  end;

{ TFloat64OneInputTest }

procedure TFloat64OneInputTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration]);
  end;

{ TFloat64TwoInputTest }

procedure TFloat64TwoInputTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration][0], FInputs[Iteration][1]);
  end;

{ TFloat64FloatIntTest }

procedure TFloat64FloatIntTest.DoTestIteration(Iteration: Integer);
  begin
    Iteration := Iteration and $7;
    FResultStorage[Iteration] := DoFunc(FInputs[Iteration].D, FInputs[Iteration].N);
  end;

{ TFloat64MinTest }
const
  MINMAX_64_INPUTS: array[0..7] of array[0..1] of Double = (
    (-0.5, 0.5),
    (1048576.0, 1048577.0),
    (-1048576.0, -1048577.0),
    (0.0, -0.0),
    (0.0, 1E4),
    (0.0, -1E4),
    (0.0, 1E-4),
    (0.0, -1E-4)
  );

  MIN_64_EXPECTED: array[0..7] of Double = (
    -0.5,
    1048576.0,
    -1048577.0,
    -0.0,
    0.0,
    -1E4,
    0.0,
    -1E-4
  );
  
  MAX_64_EXPECTED: array[0..7] of Double = (
    0.5,
    1048577.0,
    -1048576.0,
    -0.0,
    1E4,
    0.0,
    1E-4,
    0.0
  );
  
constructor TFloat64MinTest.Create;
begin
  inherited Create;
  Move(MINMAX_64_INPUTS, FInputs, SizeOf(FInputs));
  Move(MIN_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64MinTest.DoFunc(Input1, Input2: Double): Double;
  begin
    Result := Min(Input1, Input2);
  end;
  
function TFloat64MinTest.TestTitle: shortstring;
  begin
    Result := 'Min (double-precision)';
  end;

function TFloat64ImplicitMinTest.DoFunc(Input1, Input2: Double): Double;
  begin
    if Input1 < Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat64ImplicitMinTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Min (double-precision)';
  end;

{ TFloat64MaxTest }

constructor TFloat64MaxTest.Create;
begin
  inherited Create;
  Move(MINMAX_64_INPUTS, FInputs, SizeOf(FInputs));
  Move(MAX_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64MaxTest.DoFunc(Input1, Input2: Double): Double;
  begin
    Result := Max(Input1, Input2);
  end;
  
function TFloat64MaxTest.TestTitle: shortstring;
  begin
    Result := 'Max (double-precision)';
  end;

function TFloat64ImplicitMaxTest.DoFunc(Input1, Input2: Double): Double;
  begin
    if Input1 > Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat64ImplicitMaxTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Max (double-precision)';
  end;

{ TFloat64MinTest }
const
  MINMAX_64_SPECIAL_INPUTS: array[0..7] of array[0..1] of Double = (
    (NaN, 0.0),
    (0.0, NaN),
    (0.0, Infinity),
    (Infinity, 0.0),
    (0.0, NegInfinity),
    (NegInfinity, 0.0),
    (Infinity, NegInfinity),
    (NegInfinity, Infinity)
  );
  
  MIN_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    0.0,
    NaN,
    0.0,
    0.0,
    NegInfinity,
    NegInfinity,
    NegInfinity,
    NegInfinity
  );
  
  MAX_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    0.0,
    NaN,
    Infinity,
    Infinity,
    0.0,
    0.0,
    Infinity,
    Infinity
  );
  
constructor TFloat64MinSpecialTest.Create;
begin
  inherited Create;
  Move(MINMAX_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(MIN_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64MinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Min (special double-precision)';
  end;

function TFloat64ImplicitMinSpecialTest.DoFunc(Input1, Input2: Double): Double;
  begin
    if Input1 < Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat64ImplicitMinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Min (special double-precision)';
  end;

{ TFloat64MaxTest }

constructor TFloat64MaxSpecialTest.Create;
begin
  inherited Create;
  Move(MINMAX_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(MAX_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64MaxSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Max (special double-precision)';
  end;

function TFloat64ImplicitMaxSpecialTest.DoFunc(Input1, Input2: Double): Double;
  begin
    if Input1 > Input2 then
      Result := Input1
    else
      Result := Input2;
  end;
  
function TFloat64ImplicitMaxSpecialTest.TestTitle: shortstring;
  begin
    Result := 'Implicit Max (special double-precision)';
  end;

{ TFloat64SqrtTest }
const
  SQRT_64_INPUTS: array[0..7] of Double = (
    4.0,
    1.0,
    256.0,
    2.0,
    100.0,
    0.5,
    3.1415926535897932384626433832795, { Pi }
    1.5707963267948966192313216916398 { Pi / 2 }
  );
  
  SQRT_64_EXPECTED: array[0..7] of Double = (
    2.0,
    1.0,
    16.0,
    1.4142135623730950488016887242097,
    10.0,
    0.70710678118654752440084436210485,
    1.7724538509055160272981674833411,
    1.2533141373155002512078826424055
  );

constructor TFloat64SqrtTest.Create;
begin
  inherited Create;
  Move(SQRT_64_INPUTS, FInputs, SizeOf(FInputs));
  Move(SQRT_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64SqrtTest.DoFunc(Input: Double): Double;
  begin
    Result := Sqrt(Input);
  end;
  
function TFloat64SqrtTest.TestTitle: shortstring;
  begin
    Result := 'sqrt(x) (double-precision)';
  end;

{ TFloat64SqrtSpecialTest }
const
  SQRT_64_SPECIAL_INPUTS: array[0..7] of Double = (
    0.0,
    Infinity,
    NegInfinity,
    NaN,
    -0.0,
    -1.0,
    1E6,
    NaN
  );
  
  SQRT_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    0.0,
    Infinity,
    NaN,
    NaN,
    0.0,
    NaN,
    1E3,
    NaN
  );

constructor TFloat64SqrtSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(SQRT_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64SqrtSpecialTest.TestTitle: shortstring;
  begin
    Result := 'sqrt(x) (special double-precision)';
  end;

{ TFloat64LnTest }

const  
  LN_64_EXPECTED: array[0..7] of Double = (
    1.3862943611198906188344642429164,
    0.0,
    5.5451774444795624753378569716654,
    0.69314718055994530941723212145818,
    4.6051701859880913680359829093687,
    -0.69314718055994530941723212145818,
    1.1447298858494001741434273513531,  
    0.45158270528945486472619522989488
  );

constructor TFloat64LnTest.Create;
begin
  inherited Create;
  Move(SQRT_64_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(LN_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64LnTest.DoFunc(Input: Double): Double;
  begin
    Result := Ln(Input);
  end;
  
function TFloat64LnTest.TestTitle: shortstring;
  begin
    Result := 'ln x (double-precision)';
  end;

{ TFloat64LnSpecialTest }

const
  LN_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    NegInfinity,
    Infinity,
    NaN,
    NaN,
    NegInfinity,
    NaN,
    13.815510557964274104107948728106,
    NaN
  );

constructor TFloat64LnSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(LN_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64LnSpecialTest.TestTitle: shortstring;
  begin
    Result := 'ln x (special double-precision)';
  end;

{ TFloat64ExpTest }
const
  EXP_64_EXPECTED: array[0..7] of Double = (
    54.598150033144239078110261202861,  
    2.7182818284590455, // remove the last 5 and append 2353602874713527 (too precise),
    1.5114276650041035425200896657073e+111,
    7.389056098930650227230427460575,
    2.68811714181613544841262555158e+43,
    1.6487212707001281468486507878142,  
    23.140692632779269005729086367949,
    4.8104773809653516554730356667038
  );

constructor TFloat64ExpTest.Create;
begin
  inherited Create;
  Move(SQRT_64_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(EXP_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64ExpTest.DoFunc(Input: Double): Double;
  begin
    Result := Exp(Input);
  end;
  
function TFloat64ExpTest.TestTitle: shortstring;
  begin
    Result := 'e^x (double-precision)';
  end;

{ TFloat64ExpSpecialTest }
const
  EXP_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    1.0,
    Infinity,
    0,
    NaN,
    1.0,
    0.36787944117144232159552377016146,
    Infinity,
    NaN
  );

constructor TFloat64ExpSpecialTest.Create;
begin
  inherited Create;
  Move(SQRT_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(EXP_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64ExpSpecialTest.TestTitle: shortstring;
  begin
    Result := 'e^x (special double-precision)';
  end;

{ TFloat64SinTest }
const
  SIN_64_EXPECTED: array[0..7] of Double = (
    -0.75680249530792825137263909451183,
    0.8414709848078965066525023216303,
    -0.99920803410706269906759056238387,
    0.90929742682568169539601986591174,
    -0.50636564110975879365655761045979,
    0.47942553860420300027328793521557,
    0.0,
    1.0
  );

constructor TFloat64SinTest.Create;
begin
  inherited Create;
  Move(SQRT_64_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(SIN_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64SinTest.DoFunc(Input: Double): Double;
  begin
    Result := Sin(Input);
  end;
  
function TFloat64SinTest.TestTitle: shortstring;
  begin
    Result := 'sin x (single-precision)';
  end;

{ TFloat64SinSpecialTest }
const
  SIN_64_SPECIAL_INPUTS: array[0..7] of Double = ( { Using different inputs as infinities are invalid }
    0.0,
    -1.0,
    1E6,
    NaN,
    -0.0,
    -1.0,
    1E6,
    NaN
  );

  SIN_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    0.0,
    -0.8414709848078965066525023216303,
    -0.34999350217129295211765248678077,
    NaN,
    -0.0,
    -0.8414709848078965066525023216303,
    -0.34999350217129295211765248678077,    
    NaN
  );

constructor TFloat64SinSpecialTest.Create;
begin
  inherited Create;
  Move(SIN_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(SIN_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64SinSpecialTest.TestTitle: shortstring;
  begin
    Result := 'sin x (double single-precision)';
  end;

{ TFloat64CosTest }
const
  COS_64_EXPECTED: array[0..7] of Double = (
    -0.65364362086361191463916818309775,
    0.54030230586813971740093660744298,
    -0.03979075993115770952448155799687,
    -0.41614683654714238699756822950076,
    0.86231887228768393410193851395084,
    0.87758256189037271611628158260383,
    -1.0, { Pi }
    0.0
  );

constructor TFloat64CosTest.Create;
begin
  inherited Create;
  Move(SQRT_64_INPUTS, FInputs, SizeOf(FInputs)); { Reuse the sqrt inputs }
  Move(COS_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64CosTest.DoFunc(Input: Double): Double;
  begin
    Result := Cos(Input);
  end;
  
function TFloat64CosTest.TestTitle: shortstring;
  begin
    Result := 'cos x (double-precision)';
  end;

{ TFloat64CosSpecialTest }
const
  COS_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    1.0,
    0.54030230586813971740093660744298,
    0.93675212753314478693853253507492,
    NaN,
    1.0,
    0.54030230586813971740093660744298,
    0.93675212753314478693853253507492,
    NaN
  );

constructor TFloat64CosSpecialTest.Create;
begin
  inherited Create;
  Move(SIN_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(COS_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;
  
function TFloat64CosSpecialTest.TestTitle: shortstring;
  begin
    Result := 'cos x (special double-precision)';
  end;

{ TFloat64LdexpTest }
const
  LDEXP_64_INPUTS: array[0..7] of TDoubleIntPair = (
    (D: 1.0; N: 0),
    (D: 2.7182818284590452353602874713527; N: -1),
    (D: 0.85309609426787873559547283129472; N: 6),
    (D: 0.1; N: 1),
    (D: 0.2; N: 2),
    (D: 0.3; N: 3),
    (D: 0.4; N: 4),
    (D: 0.5; N: 5)
  );

  LDEXP_64_EXPECTED: array[0..7] of Double = (
    1.0,
    1.3591409142295226176801437356763,
    54.598150033144239078110261202862,
    0.2,
    0.8,
    2.4,
    6.4,
    16.0
  );

constructor TFloat64LdexpTest.Create;
begin
  inherited Create;
  Move(LDEXP_64_INPUTS, FInputs, SizeOf(FInputs));
  Move(LDEXP_64_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64LdexpTest.DoFunc(Float: Double; N: Integer): Double;
  begin
    Result := LdExp(Float, N);
  end;

function TFloat64LdexpTest.TestTitle: shortstring;
  begin
    Result := 'ldexp(x, n) (double-precision)';
  end;

{ TFloat32SpecialLdexpTest }
const
  LDEXP_64_SPECIAL_INPUTS: array[0..7] of TDoubleIntPair = (
    (D: Infinity; N: 0),
    (D: NegInfinity; N: 0),
    (D: 2.0; N: -1024),
    (D: 2.0; N: 1023),
    (D: NaN; N: 1),
    (D: 0.0; N: 16384),
    (D: 8.9884656743115795386465259539451e+307; N: -1024),
    (D: 5.562684646268003457725581793331e-309; N: 1023)
  );

  LDEXP_64_SPECIAL_EXPECTED: array[0..7] of Double = (
    Infinity,
    NegInfinity,
    5.562684646268003457725581793331e-309,
    Infinity,
    Nan,
    0.0,
    0.5,
    0.5
  );

constructor TFloat64LdexpSpecialTest.Create;
begin
  inherited Create;
  Move(LDEXP_64_SPECIAL_INPUTS, FInputs, SizeOf(FInputs));
  Move(LDEXP_64_SPECIAL_EXPECTED, FExpected, SizeOf(FExpected));
end;

function TFloat64LdexpSpecialTest.TestTitle: shortstring;
  begin
    Result := 'ldexp(x, n) (special double-precision)';
  end;

{ Main function }
const
  { TCompleteByteRange and descendants }

  TestClasses: array[0..39] of TTestClass = (
    TFloat32MinTest,
    TFloat32MaxTest,
    TFloat32ImplicitMinTest,
    TFloat32ImplicitMaxTest,
    TFloat32MinSpecialTest,
    TFloat32MaxSpecialTest,
    TFloat32ImplicitMinSpecialTest,
    TFloat32ImplicitMaxSpecialTest,
    TFloat32SqrtTest,
    TFloat32SqrtSpecialTest,
    TFloat32LnTest,
    TFloat32LnSpecialTest,
    TFloat32ExpTest,
    TFloat32ExpSpecialTest,
    TFloat32SinTest,
    TFloat32SinSpecialTest,
    TFloat32CosTest,
    TFloat32CosSpecialTest,
    TFloat32LdexpTest,
    TFloat32LdexpSpecialTest,
    TFloat64MinTest,
    TFloat64MaxTest,
    TFloat64ImplicitMinTest,
    TFloat64ImplicitMaxTest,
    TFloat64MinSpecialTest,
    TFloat64MaxSpecialTest,
    TFloat64ImplicitMinSpecialTest,
    TFloat64ImplicitMaxSpecialTest,
    TFloat64SqrtTest,
    TFloat64SqrtSpecialTest,
    TFloat64LnTest,
    TFloat64LnSpecialTest,
    TFloat64ExpTest,
    TFloat64ExpSpecialTest,
    TFloat64SinTest,
    TFloat64SinSpecialTest,
    TFloat64CosTest,
    TFloat64CosSpecialTest,
    TFloat64LdexpTest,
    TFloat64LdexpSpecialTest
  );

var
  CurrentObject: TTestAncestor;
  Failed: Boolean;
  X: Integer;
  SummedUpAverageDuration, AverageDuration : Double;
begin
  SetExceptionMask(GetExceptionMask + [exOverflow, exZeroDivide, exInvalidOp]);
  SummedUpAverageDuration := 0.0;
  Failed := False;
  WriteLn('Floating-point compilation and timing test');
  WriteLn('------------------------------------------');
  for X := low(TestClasses) to High(TestClasses) do
    begin
      try
        CurrentObject := TestClasses[X].Create;
        try
          Write(CurrentObject.TestTitle:40, ' - ');
          CurrentObject.Run;

          if CurrentObject.WriteResults then
            begin
              AverageDuration := ((CurrentObject.RunTime * 1000000000.0) / ITERATIONS);
              WriteLn('Pass - average iteration duration: ', AverageDuration:1:3, ' ns');
              SummedUpAverageDuration := SummedUpAverageDuration + AverageDuration;
            end
          else
            { Final average isn't processed if a test failed, so there's no need
              to calculate and add the average duration to it }
            Failed := True;

        finally
          CurrentObject.Free;
        end;
      except on E: Exception do
        begin
          WriteLn('Exception "', E.ClassName, '" raised while running test object of class "', TestClasses[X].ClassName, '"');
          Failed := True;
        end;
      end;
    end;

  if Failed then
    Halt(1);

  WriteLn(#10'ok');
  WriteLn('- Sum of average durations: ', SummedUpAverageDuration:1:3, ' ns');
  WriteLn('- Overall average duration: ', (SummedUpAverageDuration / Length(TestClasses)):1:3, ' ns');
end.
