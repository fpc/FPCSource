{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ A variant value out of Currency's range raises EVariantOverflowError when it is
  converted to Currency, as in Delphi: by an assignment, by VarAsType and as an
  operand beside a currency. An int64 or a qword used to wrap, or to raise
  EInvalidOp on i386; a float raised EVariantError, or EInvalidOp on x86_64,
  and without an extended type one with a fraction was refused though it fits. }
program tvarcurrrange;

{$mode delphi}

uses
  sysutils, math, variants;

const
  cMaxInt = Int64(922337203685477);
  cVarQWord = $15;
{$if defined(FPUX87) or defined(FPUSSE)}
  { the x87 scales a double without rounding it; with SSE alone, i386 still
    does its double arithmetic there }
  cScaledInDouble = False;
  cScaleTolerance = 0;
{$else}
  { elsewhere a double is scaled in double precision, x86_64 included }
  cScaledInDouble = True;
  cScaleTolerance = 2048;
{$endif}

var
  gFailed: Integer = 0;

procedure Failed(aRow: Integer; const aWhat: string);
begin
  WriteLn('row ', aRow, ' failed: ', aWhat);
  Inc(gFailed);
end;

function RawOf(const aValue: Currency): Int64;
begin
  Result := PInt64(@aValue)^;
end;

{ the signs agree, so the difference cannot wrap }
function Near(aGot, aWant, aTolerance: Int64): Boolean;
begin
  Result := ((aGot < 0) = (aWant < 0)) and (Abs(aGot - aWant) <= aTolerance);
end;

{ the assignment and VarAsType refuse the value }
procedure Refused(aRow: Integer; const aVar: Variant);
var
  lCurr: Currency;
  lOut: Variant;
begin
  try
    lCurr := aVar;
    Failed(aRow, 'assignment gave a value');
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'assignment raised ' + E.ClassName);
  end;
  try
    lCurr := Currency(aVar);
    Failed(aRow, 'cast gave a value');
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'cast raised ' + E.ClassName);
  end;
{$ifndef FPUNONE}
  try
    lOut := VarAsType(aVar, varCurrency);
    Failed(aRow, 'VarAsType gave a value');
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'VarAsType raised ' + E.ClassName);
  end;
{$endif}
end;

{ the assignment and VarAsType give the value, by its 64-bit representation }
procedure Accepted(aRow: Integer; const aVar: Variant; aRaw: Int64; aTolerance: Int64 = 0);
var
  lCurr: Currency;
  lOut: Variant;
begin
  try
    lCurr := aVar;
    if not Near(RawOf(lCurr), aRaw, aTolerance) then
      Failed(aRow, 'assignment gave ' + IntToStr(RawOf(lCurr)));
    lCurr := Currency(aVar);
    if not Near(RawOf(lCurr), aRaw, aTolerance) then
      Failed(aRow, 'cast gave ' + IntToStr(RawOf(lCurr)));
{$ifndef FPUNONE}
    lOut := VarAsType(aVar, varCurrency);
    if not Near(RawOf(TVarData(lOut).VCurrency), aRaw, aTolerance) then
      Failed(aRow, 'VarAsType gave ' + IntToStr(RawOf(TVarData(lOut).VCurrency)));
{$endif}
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

function FromQWord(aBits: QWord): Variant;
begin
  VarClear(Result);
  TVarData(Result).VType := cVarQWord;
  TVarData(Result).VQWord := aBits;
end;

{$ifndef FPUNONE}
{ under a directed rounding mode an edge of the range is right, within the
  scaling's precision; where the scaling rounds to a double, the top under rmUp
  scales to 2^63, which no currency holds, and is refused }
procedure UpwardEdge(aRow: Integer; const aVar: Variant; aMode: TFPURoundingMode; aRaw: Int64; aRefused: Boolean);
var
  lOld: TFPURoundingMode;
  lCurr: Currency;
begin
  lOld := GetRoundMode;
  SetRoundMode(aMode);
  try
    { a target that cannot set the rounding mode (wasm32) has no row here }
    if GetRoundMode <> aMode then
      exit;
    try
      lCurr := aVar;
      if aRefused or not Near(RawOf(lCurr), aRaw, cScaleTolerance) then
        Failed(aRow, 'gave ' + IntToStr(RawOf(lCurr)));
    except
      on E: EVariantOverflowError do
        if not aRefused then
          Failed(aRow, 'refused a value that fits');
      on E: Exception do
        Failed(aRow, 'raised ' + E.ClassName);
    end;
  finally
    SetRoundMode(lOld);
  end;
end;

type
  { a custom variant type holding a double in the variant itself, which it
    hands to every conversion }
  TDoubleVarType = class(TCustomVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

procedure TDoubleVarType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
end;

procedure TDoubleVarType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  Dest := Source;
end;

procedure TDoubleVarType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
  Dest.VType := varDouble;
  Dest.VDouble := Source.VDouble;
end;

var
  gDoubleType: TDoubleVarType;

{ the double a custom type's handler gives is checked as any other, by the
  assignment and the cast; VarAsType hands back the handler's own result }
procedure Custom(aRow: Integer; aValue: Double; aRaw: Int64; aRefused: Boolean);
var
  lVar: Variant;
  lCurr: Currency;
  lPass: Integer;
begin
  TVarData(lVar).VType := gDoubleType.VarType;
  TVarData(lVar).VDouble := aValue;
  for lPass := 1 to 2 do
    try
      if lPass = 1 then
        lCurr := lVar
      else
        lCurr := Currency(lVar);
      if aRefused or (RawOf(lCurr) <> aRaw) then
        Failed(aRow, 'gave ' + IntToStr(RawOf(lCurr)));
    except
      on E: EVariantOverflowError do
        if not aRefused then
          Failed(aRow, 'refused');
      on E: Exception do
        Failed(aRow, 'raised ' + E.ClassName);
    end;
end;

{ an operand beside a currency }
procedure OperandRefused(aRow: Integer; const aVar: Variant);
var
  lCurr, lRes: Variant;
  lEqual: Boolean;
begin
  lCurr := Currency(1);
  try
    lRes := aVar + lCurr;
    Failed(aRow, 'add gave a value');
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'add raised ' + E.ClassName);
  end;
  try
    lRes := lCurr - aVar;
    Failed(aRow, 'subtract gave a value');
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'subtract raised ' + E.ClassName);
  end;
  try
    lEqual := aVar = lCurr;
    Failed(aRow, 'equal gave ' + BoolToStr(lEqual, True));
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'equal raised ' + E.ClassName);
  end;
end;
{$endif}

var
  lVar, lInner: Variant;
  lInt: Int64;
  lBits: QWord;
{$ifndef FPUNONE}
  lDouble: Double;
  lSingle: Single;
  lMasked: Boolean;
  lMask: TFPUExceptionMask;
{$endif}
begin
  { int64 }
  Accepted(1, Variant(cMaxInt), cMaxInt * 10000);
  Accepted(2, Variant(-cMaxInt), -cMaxInt * 10000);
  Refused(3, Variant(cMaxInt + 1));
  Refused(4, Variant(-cMaxInt - 1));
  Refused(5, Variant(High(Int64)));
  Refused(6, Variant(Low(Int64)));

  { qword }
  Accepted(7, FromQWord(5), 50000);
  Accepted(8, FromQWord(QWord(cMaxInt)), cMaxInt * 10000);
  Refused(9, FromQWord(QWord(cMaxInt) + 1));
  Refused(10, FromQWord(High(QWord)));

  { held by reference, and in a variant held by reference }
  lInt := cMaxInt;
  TVarData(lVar).VType := varInt64 or varByRef;
  TVarData(lVar).VPointer := @lInt;
  Accepted(11, lVar, cMaxInt * 10000);
  lInt := cMaxInt + 1;
  Refused(12, lVar);
  lBits := QWord(cMaxInt);
  TVarData(lVar).VType := cVarQWord or varByRef;
  TVarData(lVar).VPointer := @lBits;
  Accepted(13, lVar, cMaxInt * 10000);
  lBits := QWord(cMaxInt) + 1;
  Refused(14, lVar);
  lInner := cMaxInt + 1;
  TVarData(lVar).VType := varVariant or varByRef;
  TVarData(lVar).VPointer := @lInner;
  Refused(15, lVar);
  TVarData(lVar).VType := varEmpty;

  { a value that fits is untouched by the check }
  Accepted(16, Variant(Int64(5)), 50000);
  Accepted(17, Variant(Int64(-5)), -50000);
  Accepted(18, Variant(LongInt(High(LongInt))), Int64(High(LongInt)) * 10000);

{$ifndef FPUNONE}
  { floats: 922337203685477.625 is the first double that does not fit }
  Accepted(19, Variant(Double(922337203685477.5)), 9223372036854775000, cScaleTolerance);
  Accepted(20, Variant(Double(-922337203685477.5)), -9223372036854775000, cScaleTolerance);
  UpwardEdge(32, Variant(Double(922337203685477.5)), rmUp, 9223372036854775000, cScaledInDouble);
  UpwardEdge(33, Variant(Double(-922337203685477.5)), rmDown, -9223372036854775000, False);
  Refused(21, Variant(Double(922337203685477.625)));
  Refused(22, Variant(Double(-922337203685477.625)));
  Refused(23, Variant(Double(1e300)));
  { a NaN is refused too, as by Delphi on win32, with invalid operations masked
    or not, held directly and by reference }
  lDouble := NaN;
  for lMasked := False to True do
  begin
    lMask := GetExceptionMask;
    if lMasked then
      SetExceptionMask(lMask + [exInvalidOp])
    else
      SetExceptionMask(lMask - [exInvalidOp]);
    try
      Refused(38 + Ord(lMasked) * 2, Variant(lDouble));
      TVarData(lVar).VType := varDouble or varByRef;
      TVarData(lVar).VPointer := @lDouble;
      Refused(39 + Ord(lMasked) * 2, lVar);
      TVarData(lVar).VType := varEmpty;
    finally
      SetExceptionMask(lMask);
    end;
  end;
  lSingle := 1e15;
  VarClear(lVar);
  TVarData(lVar).VType := varSingle;
  TVarData(lVar).VSingle := lSingle;
  Refused(24, lVar);
  TVarData(lVar).VType := varSingle or varByRef;
  TVarData(lVar).VPointer := @lSingle;
  Refused(31, lVar);
  TVarData(lVar).VType := varEmpty;
  { a single is scaled as a double, as in Delphi: 1.23455 is below the half }
  VarClear(lVar);
  TVarData(lVar).VType := varSingle;
  TVarData(lVar).VSingle := 1.23455;
  Accepted(34, lVar, 12345);
  VarClear(lVar);
  lDouble := 1e15;
  TVarData(lVar).VType := varDate or varByRef;
  TVarData(lVar).VPointer := @lDouble;
  Refused(35, lVar);
  TVarData(lVar).VType := varEmpty;
  VarClear(lVar);
  TVarData(lVar).VType := varDate;
  TVarData(lVar).VDate := 1e15;
  Refused(25, lVar);
  lDouble := 1e15;
  TVarData(lVar).VType := varDouble or varByRef;
  TVarData(lVar).VPointer := @lDouble;
  Refused(26, lVar);
  lDouble := 922337203685477.5;
  Accepted(27, lVar, 9223372036854775000, cScaleTolerance);
  TVarData(lVar).VType := varEmpty;
  OperandRefused(28, Variant(Double(1e15)));

  { operands }
  OperandRefused(29, Variant(cMaxInt + 1));
  OperandRefused(30, FromQWord(QWord(cMaxInt) + 1));

  { a custom variant type }
  gDoubleType := TDoubleVarType.Create;
  Custom(36, 2.5, 25000, False);
  Custom(37, 1e15, 0, True);
  gDoubleType.Free;
{$endif}

  if gFailed <> 0 then
    Halt(1);
  WriteLn('ok');
end.
