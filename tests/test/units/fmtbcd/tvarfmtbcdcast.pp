{ The FmtBCD custom variant type's casts, as in Delphi: every simple source casts to
  VarFMTBcd, text exactly; a BCD casts to text with all its digits; to Int64, LongWord
  and QWord a fraction or a value no int64 holds raises EVariantTypeCastError and what
  an int64 holds outside the target raises EVariantOverflowError. A cast out of the
  type used to go through a double. }
program tvarfmtbcdcast;

{$mode delphi}

uses
  sysutils, variants, fmtbcd;

var
  gFailed: Integer = 0;

procedure Failed(aRow: Integer; const aWhat: string);
begin
  WriteLn('row ', aRow, ' failed: ', aWhat);
  Inc(gFailed);
end;

{ the cast gives a BCD of the value written in aText }
procedure ToBcd(aRow: Integer; const aSrc: Variant; const aText: string);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(aSrc, VarFMTBcd);
    if VarType(lOut) <> VarFMTBcd then
      Failed(aRow, 'type ' + IntToStr(VarType(lOut)))
    else if BCDCompare(VarToBCD(lOut), StrToBCD(aText)) <> 0 then
      Failed(aRow, 'value ' + BCDToStr(VarToBCD(lOut)));
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

{ the cast raises eBCDException or a descendant: text that is not a number }
procedure ToBcdRefused(aRow: Integer; const aSrc: Variant);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(aSrc, VarFMTBcd);
    Failed(aRow, 'gave ' + BCDToStr(VarToBCD(lOut)));
  except
    on E: eBCDException do
      ;
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure ToBcdCastError(aRow: Integer; const aSrc: Variant);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(aSrc, VarFMTBcd);
    Failed(aRow, 'gave ' + BCDToStr(VarToBCD(lOut)));
  except
    on E: EVariantTypeCastError do
      ;
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

function Bcd(const aText: string): Variant;
begin
  Result := VarFMTBcdCreate(StrToBCD(aText));
end;

procedure ToText(aRow: Integer; const aText: string; aType: Word);
var
  lOut: Variant;
  lGot: string;
begin
  try
    lOut := VarAsType(Bcd(aText), aType);
    lGot := VarToStr(lOut);
    if VarType(lOut) <> aType then
      Failed(aRow, 'type ' + IntToStr(VarType(lOut)))
    else if lGot <> aText then
      Failed(aRow, 'text ' + lGot);
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

{ aValue, by VarAsType and by an assignment or the routine a read of the type uses }
procedure ToInt64(aRow: Integer; const aText: string; aValue: Int64);
var
  lOut: Variant;
  lGot: Int64;
begin
  try
    lOut := VarAsType(Bcd(aText), varInt64);
    if VarType(lOut) <> varInt64 then
      Failed(aRow, 'type ' + IntToStr(VarType(lOut)))
    else if TVarData(lOut).vInt64 <> aValue then
      Failed(aRow, 'value ' + IntToStr(TVarData(lOut).vInt64));
    lGot := Bcd(aText);
    if lGot <> aValue then
      Failed(aRow, 'assigned ' + IntToStr(lGot));
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure ToQWord(aRow: Integer; const aText: string; aValue: QWord);
var
  lOut: Variant;
  lGot: QWord;
begin
  try
    lOut := VarAsType(Bcd(aText), varQWord);
    if VarType(lOut) <> varQWord then
      Failed(aRow, 'type ' + IntToStr(VarType(lOut)))
    else if TVarData(lOut).vQWord <> aValue then
      Failed(aRow, 'value ' + UIntToStr(TVarData(lOut).vQWord));
    lGot := Bcd(aText);
    if lGot <> aValue then
      Failed(aRow, 'assigned ' + UIntToStr(lGot));
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure ToLongWord(aRow: Integer; const aText: string; aValue: LongWord);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(Bcd(aText), varLongWord);
    if VarType(lOut) <> varLongWord then
      Failed(aRow, 'type ' + IntToStr(VarType(lOut)))
    else if TVarData(lOut).vLongWord <> aValue then
      Failed(aRow, 'value ' + UIntToStr(TVarData(lOut).vLongWord));
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure CastError(aRow: Integer; const aText: string; aType: Word);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(Bcd(aText), aType);
    Failed(aRow, 'gave ' + VarToStr(lOut));
  except
    on E: EVariantTypeCastError do
      ;
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure Overflow(aRow: Integer; const aText: string; aType: Word);
var
  lOut: Variant;
begin
  try
    lOut := VarAsType(Bcd(aText), aType);
    Failed(aRow, 'gave ' + VarToStr(lOut));
  except
    on E: EVariantOverflowError do
      ;
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

{ add and multiply with a BCD on the right give a BCD }
procedure LeftOperand(aRow: Integer; const aLeft: Variant; const aSum, aProduct: string);
var
  lOut: Variant;
begin
  try
    lOut := aLeft + Bcd('7.5');
    if VarType(lOut) <> VarFMTBcd then
      Failed(aRow, 'sum type ' + IntToStr(VarType(lOut)))
    else if BCDCompare(VarToBCD(lOut), StrToBCD(aSum)) <> 0 then
      Failed(aRow, 'sum ' + BCDToStr(VarToBCD(lOut)));
    lOut := aLeft * Bcd('7.5');
    if VarType(lOut) <> VarFMTBcd then
      Failed(aRow, 'product type ' + IntToStr(VarType(lOut)))
    else if BCDCompare(VarToBCD(lOut), StrToBCD(aProduct)) <> 0 then
      Failed(aRow, 'product ' + BCDToStr(VarToBCD(lOut)));
  except
    on E: Exception do
      Failed(aRow, 'raised ' + E.ClassName);
  end;
end;

procedure RefuseConversion(Source: PAnsiChar; CP: TSystemCodePage; var Dest: UnicodeString; Len: SizeInt);
begin
  raise EConvertError.Create('injected');
end;

procedure FailConversionToText(const aV: Variant);
var
  lOut: Variant;
begin
  try
    VarCast(lOut, aV, varUString);
    Failed(66, 'the injected failure did not arrive');
  except
    on E: EConvertError do
      ;
  end;
end;

{ a cast into a variant that holds a string or a BCD, and one variant as both }
procedure CastOverHeldValues;
var
  lV, lVar: Variant;
  lIdx: Integer;
begin
  for lIdx := 1 to 100 do
  begin
    lV := StringOfChar('x', 200);
    VarCast(lV, Integer(7), VarFMTBcd);
    VarCast(lV, lV, varInt64);
    lV := Bcd('7');
    lVar := Bcd('8');
    VarCast(lV, lVar, varInt64);
  end;
end;

var
  lV, lOut: Variant;
  lWide: WideString;
  lAnsi: AnsiString;
  lUni: UnicodeString;
  lI64: Int64;
  lDbl: Double;
  lCur: Currency;
  lInt: Integer;
  lBool: WordBool;
  lVar: Variant;
  lRef: TVarData;
  lUsed: PtrUInt;
  lQW: QWord;
  lSaved, lChanged: TUnicodeStringManager;
  lGrown: Boolean;
begin
  { the text below is written with a dot }
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := #0;

  { text to the type, exactly }
  lWide := '12.5';
  ToBcd(1, lWide, '12.5');
  lAnsi := '12.5';
  ToBcd(2, lAnsi, '12.5');
  lUni := '12.5';
  ToBcd(3, lUni, '12.5');
  lWide := '';
  ToBcd(4, lWide, '0');
  lWide := '1e3';
  ToBcd(5, lWide, '1000');
  lWide := ' 5 ';
  ToBcd(6, lWide, '5');
  lWide := '-5';
  ToBcd(7, lWide, '-5');
  lWide := '18446744073709551617.25';
  ToBcd(8, lWide, '18446744073709551617.25');
  lWide := 'abc';
  ToBcdRefused(9, lWide);
  lWide := '1.5.5';
  ToBcdRefused(10, lWide);
  lWide := '1234567890123456789012345678901234567890123456789012345678901234567890';
  ToBcdRefused(11, lWide);

  { numbers to the type }
  ToBcd(20, Integer(7), '7');
  ToBcd(21, Byte(7), '7');
  lI64 := High(Int64);
  ToBcd(22, lI64, '9223372036854775807');
  lI64 := Low(Int64);
  ToBcd(31, lI64, '-9223372036854775808');
  if BCDCompare(VarToBCD(lI64), StrToBCD('-9223372036854775808')) <> 0 then
    Failed(32, 'VarToBCD of Low(Int64)');
  lQW := High(QWord);
  ToBcd(33, lQW, '18446744073709551615');
  lDbl := 1.5;
  ToBcd(23, lDbl, '1.5');
  lCur := 1.5;
  { a currency's BCD keeps four places, so read its value as a double }
  lOut := VarAsType(lCur, VarFMTBcd);
  if (VarType(lOut) <> VarFMTBcd) or (BCDToDouble(VarToBCD(lOut)) <> 1.5) then
    Failed(24, 'currency');
  if BCDToStr(VarToBCD(lOut)) <> '1.5000' then
    Failed(34, 'currency text ' + BCDToStr(VarToBCD(lOut)));
  if BCDCompare(VarToBCD(True), StrToBCD('-1')) <> 0 then
    Failed(35, 'VarToBCD of True');
  ToBcd(25, True, '-1');
  ToBcd(26, False, '0');
  ToBcd(27, Unassigned, '0');
  ToBcd(28, VarFromDateTime(2.5), '2.5');
  ToBcd(29, Bcd('7.25'), '7.25');
  ToBcdCastError(30, Null);

  { by reference }
  lInt := 9;
  lRef.vType := varInteger or varByRef;
  lRef.vPointer := @lInt;
  ToBcd(40, Variant(lRef), '9');
  lRef.vType := varDouble or varByRef;
  lRef.vPointer := @lDbl;
  ToBcd(41, Variant(lRef), '1.5');
  lBool := True;
  lRef.vType := varBoolean or varByRef;
  lRef.vPointer := @lBool;
  ToBcd(42, Variant(lRef), '-1');
  lWide := '3.5';
  lRef.vType := varOleStr or varByRef;
  lRef.vPointer := @lWide;
  ToBcd(43, Variant(lRef), '3.5');
  lVar := lWide;
  lRef.vType := varVariant or varByRef;
  lRef.vPointer := @lVar;
  ToBcd(44, Variant(lRef), '3.5');
  lAnsi := '3.5';
  lRef.vType := varString or varByRef;
  lRef.vPointer := @lAnsi;
  ToBcdCastError(45, Variant(lRef));
  lUni := '3.5';
  lRef.vType := varUString or varByRef;
  lRef.vPointer := @lUni;
  ToBcdCastError(46, Variant(lRef));
  lQW := High(QWord);
  lRef.vType := varQWord or varByRef;
  lRef.vPointer := @lQW;
  ToBcd(47, Variant(lRef), '18446744073709551615');

  { VarToBCD }
  try
    lV := Null;
    VarToBCD(lV);
    Failed(50, 'gave a value');
  except
    on E: EVariantTypeCastError do
      ;
    on E: Exception do
      Failed(50, 'raised ' + E.ClassName);
  end;

  { one variant as source and destination }
  lV := Integer(7);
  VarCast(lV, lV, VarFMTBcd);
  if (VarType(lV) <> VarFMTBcd) or (BCDCompare(VarToBCD(lV), StrToBCD('7')) <> 0) then
    Failed(60, 'to the type');
  VarCast(lV, lV, varInt64);
  if (VarType(lV) <> varInt64) or (TVarData(lV).vInt64 <> 7) then
    Failed(61, 'to int64');
  lV := Bcd('7.5');
  VarCast(lV, lV, varOleStr);
  if (VarType(lV) <> varOleStr) or (VarToStr(lV) <> '7.5') then
    Failed(62, 'to olestr');
  lV := Bcd('7.5');
  VarCast(lV, lV, varUString);
  if (VarType(lV) <> varUString) or (VarToStr(lV) <> '7.5') then
    Failed(63, 'to ustring');

  { what the destination held is released }
  lUsed := GetFPCHeapStatus.CurrHeapUsed;
  CastOverHeldValues;
  if GetFPCHeapStatus.CurrHeapUsed <> lUsed then
    Failed(64, 'heap used grew by ' + IntToStr(GetFPCHeapStatus.CurrHeapUsed - lUsed));

  { not a value the type can read }
  ToBcdCastError(65, VarArrayOf([1, 2]));

  { a failed conversion to text releases its temporary }
  GetUnicodeStringManager(lSaved);
  lChanged := lSaved;
  lChanged.Ansi2UnicodeMoveProc := @RefuseConversion;
  lV := Bcd('123');
  SetUnicodeStringManager(lChanged);
  try
    { once first: raising and catching allocates what it keeps }
    FailConversionToText(lV);
    lUsed := GetFPCHeapStatus.CurrHeapUsed;
    FailConversionToText(lV);
    lGrown := GetFPCHeapStatus.CurrHeapUsed <> lUsed;
  finally
    SetUnicodeStringManager(lSaved);
  end;
  if lGrown then
    Failed(67, 'heap used grew');

  { a BCD to text, every digit }
  ToText(70, '18446744073709551617', varOleStr);
  ToText(71, '18446744073709551617', varUString);
  ToText(72, '18446744073709551617', varString);
  ToText(73, '100000000000000000000000000000', varOleStr);
  ToText(74, '-7.5', varOleStr);
  ToText(75, '-7.5', varUString);
  lWide := Bcd('18446744073709551617');
  if lWide <> '18446744073709551617' then
    Failed(76, 'assigned to a widestring');
  lUni := Bcd('18446744073709551617');
  if lUni <> '18446744073709551617' then
    Failed(77, 'assigned to a unicodestring');
  lAnsi := Bcd('18446744073709551617');
  if lAnsi <> '18446744073709551617' then
    Failed(78, 'assigned to an ansistring');

  { to Int64 }
  ToInt64(80, '7', 7);
  ToInt64(81, '7.0', 7);
  ToInt64(82, '-7', -7);
  ToInt64(83, '0', 0);
  ToInt64(84, '9223372036854775807', High(Int64));
  ToInt64(85, '-9223372036854775808', Low(Int64));
  CastError(86, '7.5', varInt64);
  CastError(87, '-7.5', varInt64);
  CastError(88, '0.5', varInt64);
  CastError(89, '9223372036854775808', varInt64);
  CastError(90, '-9223372036854775809', varInt64);
  CastError(91, '18446744073709551616', varInt64);
  CastError(92, '100000000000000000000000000000', varInt64);
  try
    lI64 := Bcd('7.5');
    Failed(93, 'assigned ' + IntToStr(lI64));
  except
    on E: EVariantTypeCastError do
      ;
    on E: Exception do
      Failed(93, 'raised ' + E.ClassName);
  end;

  { to LongWord }
  ToLongWord(100, '300', 300);
  ToLongWord(101, '4294967295', High(LongWord));
  ToLongWord(102, '0', 0);
  Overflow(103, '-1', varLongWord);
  Overflow(104, '4294967296', varLongWord);
  Overflow(105, '5000000000', varLongWord);
  Overflow(106, '9223372036854775807', varLongWord);
  Overflow(107, '-9223372036854775808', varLongWord);
  CastError(108, '7.5', varLongWord);
  CastError(109, '-7.5', varLongWord);
  CastError(110, '9223372036854775808', varLongWord);
  CastError(111, '-9223372036854775809', varLongWord);

  { to QWord; past High(Int64) is FPC's: Delphi refuses a value it could hold }
  ToQWord(120, '5000000000', 5000000000);
  ToQWord(121, '0', 0);
  ToQWord(122, '9223372036854775807', QWord(High(Int64)));
  ToQWord(123, '9223372036854775808', QWord(High(Int64)) + 1);
  ToQWord(124, '18446744073709551615', High(QWord));
  Overflow(125, '-1', varQWord);
  Overflow(126, '-9223372036854775808', varQWord);
  CastError(127, '7.5', varQWord);
  CastError(128, '-7.5', varQWord);
  CastError(129, '18446744073709551616', varQWord);
  CastError(130, '-9223372036854775809', varQWord);

  { an operator's left operand }
  LeftOperand(140, Integer(1), '8.5', '7.5');
  LeftOperand(141, Byte(1), '8.5', '7.5');
  lI64 := 1;
  LeftOperand(142, lI64, '8.5', '7.5');
  LeftOperand(143, lCur, '9', '11.25');
  LeftOperand(144, True, '6.5', '-7.5');
  LeftOperand(145, Unassigned, '7.5', '0');
  lWide := '1.5';
  LeftOperand(146, lWide, '9', '11.25');
  lAnsi := '1.5';
  LeftOperand(147, lAnsi, '9', '11.25');
  LeftOperand(148, VarFromDateTime(1.5), '9', '11.25');
  LeftOperand(149, Bcd('1.5'), '9', '11.25');

  { a double on the left keeps a double, unlike Delphi }
  lOut := lDbl + Bcd('7.5');
  if (VarType(lOut) <> varDouble) or (TVarData(lOut).vDouble <> 9) then
    Failed(160, 'double left');
  lVar := Unassigned;
  TVarData(lVar).vType := varSingle;
  TVarData(lVar).vSingle := 1.5;
  lOut := lVar + Bcd('7.5');
  if (VarType(lOut) <> VarFMTBcd) or (BCDCompare(VarToBCD(lOut), StrToBCD('9')) <> 0) then
    Failed(161, 'single left');

  { the rows that were right already }
  lOut := Bcd('7.5') + lWide;
  if (VarType(lOut) <> VarFMTBcd) or (BCDCompare(VarToBCD(lOut), StrToBCD('9')) <> 0) then
    Failed(170, 'text right');
  lOut := VarAsType(Bcd('7.5'), varDouble);
  if (VarType(lOut) <> varDouble) or (TVarData(lOut).vDouble <> 7.5) then
    Failed(171, 'to double');
  lOut := VarAsType(Bcd('7.5'), varInteger);
  if (VarType(lOut) <> varInteger) or (TVarData(lOut).vInteger <> 8) then
    Failed(172, 'to integer');

  if gFailed = 0 then
    WriteLn('ok')
  else
    Halt(1);
end.
