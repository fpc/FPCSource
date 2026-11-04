{
    Tests for conversion of custom variants to simple types,
    mixed operations between custom and simple variants and
    string arguments of late-bound calls on invokeable variants.
}

unit utcVariants;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpcunit, testregistry;

type
  { Payload of a test custom variant }
  TTestVarValue = class
  public
    // Stored value
    Value: Double;
    // Create a payload with value aValue
    constructor Create(aValue: Double);
  end;

  { Custom variant type whose CastTo returns varDouble or varOleStr regardless of the requested type }
  TTestVariantType = class(TCustomVariantType)
  public
    // Free the payload
    procedure Clear(var V: TVarData); override;
    // Copy the payload
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    // Create a custom variant from a simple variant
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    // Convert a custom variant to a simple variant
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    // Apply opAdd or opMultiply on two custom variants
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); override;
  end;

  { TTestCustomVariants }

  TTestCustomVariants = class(TTestCase)
  private
    FVar: Variant;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestToInteger;
    procedure TestToInt64;
    procedure TestToQWord;
    procedure TestToBoolean;
    procedure TestToDouble;
    procedure TestToCurrency;
    procedure TestToDateTime;
    procedure TestToAnsiString;
    procedure TestToShortString;
    procedure TestToWideString;
    procedure TestToUnicodeString;
    procedure TestImplicitLeftOperand;
    procedure TestImplicitRightOperand;
    procedure TestImplicitMultiply;
    procedure TestImplicitResultToInteger;
  end;

  { Invokeable variant type: Probe records its argument, Append adds '!' to a by-reference string }
  TTestInvokeType = class(TInvokeableVariantType)
  public
    // Reset the variant
    procedure Clear(var V: TVarData); override;
    // Copy the variant
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    // Handle the Probe and Append procedures
    function DoProcedure(const V: TVarData; const Name: AnsiString; const Arguments: TVarDataArray): Boolean; override;
  end;

  { TTestInvokeableVariants }

  TTestInvokeableVariants = class(TTestCase)
  private
    FVar: Variant;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAnsiStringArgument;
    procedure TestUnicodeStringArgument;
    procedure TestVarAnsiStringArgument;
    procedure TestVarUnicodeStringArgument;
  end;

// Create a test custom variant holding aValue
function TestVar(aValue: Double): Variant;

implementation

const
{$IFDEF MSWINDOWS}
  ExpectedAnsiType = varOleStr;
  ExpectedUnicodeType = varOleStr;
{$ELSE}
  ExpectedAnsiType = varString;
  ExpectedUnicodeType = varUString;
{$ENDIF}

var
  TestVariantType: TTestVariantType;
  TestInvokeType: TTestInvokeType;
  ProbeType: TVarType;
  ProbeValue: UnicodeString;

// Create a test custom variant holding aValue
function TestVar(aValue: Double): Variant;

begin
  VarClear(Result);
  TVarData(Result).vType := TestVariantType.VarType;
  TVarData(Result).vPointer := TTestVarValue.Create(aValue);
end;


// Return the value stored in custom variant aData
function TestVarValue(const aData: TVarData): Double;

begin
  Result := TTestVarValue(aData.vPointer).Value;
end;


{ TTestVarValue }

constructor TTestVarValue.Create(aValue: Double);

begin
  Value := aValue;
end;


{ TTestVariantType }

procedure TTestVariantType.Clear(var V: TVarData);

begin
  TTestVarValue(V.vPointer).Free;
  V.vPointer := nil;
  V.vType := varEmpty;
end;


procedure TTestVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);

begin
  Dest.vType := VarType;
  Dest.vPointer := TTestVarValue.Create(TestVarValue(Source));
end;


procedure TTestVariantType.Cast(var Dest: TVarData; const Source: TVarData);

var
  lValue: Double;

begin
  lValue := Double(Variant(Source));
  VarClear(Variant(Dest));
  Dest.vType := VarType;
  Dest.vPointer := TTestVarValue.Create(lValue);
end;


procedure TTestVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);

begin
  case AVarType of
    varString, varOleStr, varUString:
      Variant(Dest) := WideString(FloatToStr(TestVarValue(Source)));
  else
    Variant(Dest) := TestVarValue(Source);
  end;
end;


procedure TTestVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp);

begin
  case Operation of
    opAdd:
      TTestVarValue(Left.vPointer).Value := TestVarValue(Left) + TestVarValue(Right);
    opMultiply:
      TTestVarValue(Left.vPointer).Value := TestVarValue(Left) * TestVarValue(Right);
  else
    RaiseInvalidOp;
  end;
end;


{ TTestCustomVariants }

procedure TTestCustomVariants.SetUp;

begin
  FVar := TestVar(42);
end;


procedure TTestCustomVariants.TearDown;

begin
  VarClear(FVar);
end;


procedure TTestCustomVariants.TestHookUp;

begin
  AssertEquals('Custom type', TestVariantType.VarType, VarType(FVar));
  AssertEquals('Value', 42, TestVarValue(TVarData(FVar)), 0);
end;


procedure TTestCustomVariants.TestToInteger;

var
  lInt: Integer;

begin
  lInt := FVar;
  AssertEquals('Implicit integer', 42, lInt);
  AssertEquals('Explicit integer', 42, Integer(FVar));
end;


procedure TTestCustomVariants.TestToInt64;

var
  lInt: Int64;

begin
  lInt := FVar;
  AssertEquals('Implicit int64', 42, lInt);
  AssertEquals('Explicit int64', 42, Int64(FVar));
end;


procedure TTestCustomVariants.TestToQWord;

var
  lInt: QWord;

begin
  lInt := FVar;
  AssertEquals('Implicit qword', 42, lInt);
end;


procedure TTestCustomVariants.TestToBoolean;

var
  lBool: Boolean;

begin
  lBool := FVar;
  AssertTrue('Non-zero is true', lBool);
  FVar := TestVar(0);
  AssertFalse('Zero is false', Boolean(FVar));
end;


procedure TTestCustomVariants.TestToDouble;

var
  lDouble: Double;

begin
  lDouble := FVar;
  AssertEquals('Implicit double', 42, lDouble, 0);
end;


procedure TTestCustomVariants.TestToCurrency;

var
  lCurr: Currency;

begin
  lCurr := FVar;
  AssertEquals('Implicit currency', 42, lCurr, 0);
end;


procedure TTestCustomVariants.TestToDateTime;

var
  lDate: TDateTime;

begin
  lDate := FVar;
  AssertEquals('Implicit date', 42, lDate, 0);
end;


procedure TTestCustomVariants.TestToAnsiString;

var
  lStr: AnsiString;

begin
  lStr := FVar;
  AssertEquals('Implicit ansistring', '42', lStr);
  AssertEquals('Explicit ansistring', '42', AnsiString(FVar));
end;


procedure TTestCustomVariants.TestToShortString;

var
  lStr: ShortString;

begin
  lStr := FVar;
  AssertEquals('Implicit shortstring', '42', lStr);
end;


procedure TTestCustomVariants.TestToWideString;

var
  lStr: WideString;

begin
  lStr := FVar;
  AssertEquals('Implicit widestring', '42', lStr);
end;


procedure TTestCustomVariants.TestToUnicodeString;

var
  lStr: UnicodeString;

begin
  lStr := FVar;
  AssertEquals('Implicit unicodestring', '42', lStr);
end;


procedure TTestCustomVariants.TestImplicitLeftOperand;

var
  lRes: Variant;

begin
  lRes := 1 + FVar;
  AssertEquals('Result has custom type', TestVariantType.VarType, VarType(lRes));
  AssertEquals('Result value', 43, TestVarValue(TVarData(lRes)), 0);
  AssertEquals('Operand unchanged', 42, TestVarValue(TVarData(FVar)), 0);
end;


procedure TTestCustomVariants.TestImplicitRightOperand;

var
  lRes: Variant;

begin
  lRes := FVar + 1.5;
  AssertEquals('Result has custom type', TestVariantType.VarType, VarType(lRes));
  AssertEquals('Result value', 43.5, TestVarValue(TVarData(lRes)), 0);
  AssertEquals('Operand unchanged', 42, TestVarValue(TVarData(FVar)), 0);
end;


procedure TTestCustomVariants.TestImplicitMultiply;

var
  lLeft: Variant;
  lRes: Variant;

begin
  lLeft := 2;
  lRes := lLeft * FVar;
  AssertEquals('Result value', 84, TestVarValue(TVarData(lRes)), 0);
  AssertEquals('Left operand unchanged', 2, Integer(lLeft));
end;


procedure TTestCustomVariants.TestImplicitResultToInteger;

var
  lInt: Integer;

begin
  lInt := 1 + FVar;
  AssertEquals('Sum converted to integer', 43, lInt);
end;


// Return the string value of late-bound call argument aArg
function ArgString(const aArg: TVarData): UnicodeString;

var
  lByRef: Boolean;

begin
  lByRef := (aArg.vType and varByRef) <> 0;
  case aArg.vType and varTypeMask of
    varOleStr:
      if lByRef then
        Result := PWideChar(PPointer(aArg.vPointer)^)
      else
        Result := aArg.vOleStr;
    varUString:
      if lByRef then
        Result := PUnicodeString(aArg.vPointer)^
      else
        Result := UnicodeString(aArg.vUString);
    varString:
      if lByRef then
        Result := UnicodeString(PAnsiString(aArg.vPointer)^)
      else
        Result := UnicodeString(AnsiString(aArg.vString));
  else
    Result := '';
  end;
end;


// Append '!' to the by-reference string argument aArg
function AppendToArg(const aArg: TVarData): Boolean;

begin
  Result := (aArg.vType and varByRef) <> 0;
  if not Result then
    exit;
  case aArg.vType and varTypeMask of
    varOleStr:
      PWideString(aArg.vPointer)^ := PWideString(aArg.vPointer)^ + '!';
    varUString:
      PUnicodeString(aArg.vPointer)^ := PUnicodeString(aArg.vPointer)^ + '!';
    varString:
      PAnsiString(aArg.vPointer)^ := PAnsiString(aArg.vPointer)^ + '!';
  else
    Result := False;
  end;
end;


{ TTestInvokeType }

procedure TTestInvokeType.Clear(var V: TVarData);

begin
  V.vType := varEmpty;
end;


procedure TTestInvokeType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);

begin
  Dest.vType := VarType;
end;


function TTestInvokeType.DoProcedure(const V: TVarData; const Name: AnsiString; const Arguments: TVarDataArray): Boolean;

begin
  Result := Length(Arguments) = 1;
  if not Result then
    exit;
  ProbeType := Arguments[0].vType;
  ProbeValue := ArgString(Arguments[0]);
  if SameText(Name, 'Probe') then
    Result := True
  else if SameText(Name, 'Append') then
    Result := AppendToArg(Arguments[0])
  else
    Result := False;
end;


{ TTestInvokeableVariants }

procedure TTestInvokeableVariants.SetUp;

begin
  VarClear(FVar);
  TVarData(FVar).vType := TestInvokeType.VarType;
  ProbeType := varEmpty;
  ProbeValue := '';
end;


procedure TTestInvokeableVariants.TearDown;

begin
  VarClear(FVar);
end;


procedure TTestInvokeableVariants.TestAnsiStringArgument;

var
  lStr: AnsiString;

begin
  lStr := 'abc';
  FVar.Probe(lStr + 'def');
  AssertEquals('AnsiString argument type', ExpectedAnsiType, ProbeType);
  AssertEquals('AnsiString argument value', 'abcdef', ProbeValue);
end;


procedure TTestInvokeableVariants.TestUnicodeStringArgument;

var
  lStr: UnicodeString;

begin
  lStr := 'caf' + WideChar($E9);
  FVar.Probe(lStr + 's');
  AssertEquals('UnicodeString argument type', ExpectedUnicodeType, ProbeType);
  AssertEquals('UnicodeString argument value', 'caf' + WideChar($E9) + 's', ProbeValue);
end;


procedure TTestInvokeableVariants.TestVarAnsiStringArgument;

var
  lStr: AnsiString;

begin
  lStr := 'abc';
  FVar.Append(lStr);
  AssertEquals('var AnsiString argument type', ExpectedAnsiType or varByRef, ProbeType);
  AssertEquals('var AnsiString argument value', 'abc', ProbeValue);
  AssertEquals('var AnsiString argument written back', 'abc!', lStr);
end;


procedure TTestInvokeableVariants.TestVarUnicodeStringArgument;

var
  lStr: UnicodeString;

begin
  lStr := 'caf' + WideChar($E9);
  FVar.Append(lStr);
  AssertEquals('var UnicodeString argument type', ExpectedUnicodeType or varByRef, ProbeType);
  AssertEquals('var UnicodeString argument value', 'caf' + WideChar($E9), ProbeValue);
  AssertEquals('var UnicodeString argument written back', 'caf' + WideChar($E9) + '!', lStr);
end;


initialization
  TestVariantType := TTestVariantType.Create;
  TestInvokeType := TTestInvokeType.Create;
  RegisterTest(TTestCustomVariants);
  RegisterTest(TTestInvokeableVariants);
finalization
  FreeAndNil(TestInvokeType);
  FreeAndNil(TestVariantType);
end.
