{ This test has been checked against Delphi XE3 and XE5, both 32-bit and 64-bit
  versions }

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IF defined(CPU32) or defined(CPU64)}
    {$DEFINE Enable_Test}
  {$ENDIF}
{$ELSE}
  {$DEFINE Enable_Test}
{$ENDIF}

{$APPTYPE console}

{$IFDEF Enable_Test}
uses
  SysUtils, Variants;

var
  Error: Boolean;

function VariantType2String(basicType: Integer): string;
begin
  case basicType of
    varEmpty     : Result := 'varEmpty';
    varNull      : Result := 'varNull';
    varSmallInt  : Result := 'varSmallInt';
    varInteger   : Result := 'varInteger';
    varSingle    : Result := 'varSingle';
    varDouble    : Result := 'varDouble';
    varCurrency  : Result := 'varCurrency';
    varDate      : Result := 'varDate';
    varOleStr    : Result := 'varOleStr';
    varDispatch  : Result := 'varDispatch';
    varError     : Result := 'varError';
    varBoolean   : Result := 'varBoolean';
    varVariant   : Result := 'varVariant';
    varUnknown   : Result := 'varUnknown';
    varByte      : Result := 'varByte';
    varWord      : Result := 'varWord';
    varLongWord  : Result := 'varLongWord';
    varInt64     : Result := 'varInt64';
    varStrArg    : Result := 'varStrArg';
    varString    : Result := 'varString';
    varAny       : Result := 'varAny';
    varTypeMask  : Result := 'varTypeMask';
    varShortInt  : Result := 'varShortInt';
    varUInt64    : Result := 'varUInt64';
    else
      Result := IntToStr(basicType);
  end;
end;

procedure CheckBasicVariantType(varVar: Variant; expectedType: Integer);
var
  basicType  : Integer;
begin
  basicType := VarType(varVar) and VarTypeMask;

  if basicType = expectedType then
    Writeln(VariantType2String(basicType))
  else
  begin
    Writeln(VariantType2String(basicType), ' (ERROR! Expected: ', VariantType2String(expectedType), ')');
    Error := True;
  end;
end;

var
  shortint_1, shortint_2: ShortInt;
  smallint_1, smallint_2: SmallInt;
  integer_1, integer_2: Integer;
  int64_1, int64_2: Int64;
  byte_1, byte_2: Byte;
  word_1, word_2: Word;
  longword_1, longword_2: LongWord;
  uint64_1, uint64_2: UInt64;
  v: Variant;
begin
  Error := False;
  shortint_1 := 1; shortint_2 := 1;
  smallint_1 := 1; smallint_2 := 1;
  integer_1 := 1; integer_2 := 1;
  int64_1 := 1; int64_2 := 1;
  byte_1 := 1; byte_2 := 1;
  word_1 := 1; word_2 := 1;
  longword_1 := 1; longword_2 := 1;
  uint64_1 := 1; uint64_2 := 1;

  Write('shortint and shortint: ':25);
  v := shortint_1 and shortint_2;
  CheckBasicVariantType(v, varShortInt);
  Write('shortint and smallint: ':25);
  v := shortint_1 and smallint_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('shortint and integer: ':25);
  v := shortint_1 and integer_2;
  CheckBasicVariantType(v, varInteger);
  Write('shortint and int64: ':25);
  v := shortint_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('shortint and byte: ':25);
  v := shortint_1 and byte_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('shortint and word: ':25);
  v := shortint_1 and word_2;
  CheckBasicVariantType(v, varInteger);
  Write('shortint and longword: ':25);
  v := shortint_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('shortint and uint64: ':25);
  v := shortint_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('smallint and shortint: ':25);
  v := smallint_1 and shortint_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('smallint and smallint: ':25);
  v := smallint_1 and smallint_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('smallint and integer: ':25);
  v := smallint_1 and integer_2;
  CheckBasicVariantType(v, varInteger);
  Write('smallint and int64: ':25);
  v := smallint_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('smallint and byte: ':25);
  v := smallint_1 and byte_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('smallint and word: ':25);
  v := smallint_1 and word_2;
  CheckBasicVariantType(v, varInteger);
  Write('smallint and longword: ':25);
  v := smallint_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('smallint and uint64: ':25);
  v := smallint_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('integer and shortint: ':25);
  v := integer_1 and shortint_2;
  CheckBasicVariantType(v, varInteger);
  Write('integer and smallint: ':25);
  v := integer_1 and smallint_2;
  CheckBasicVariantType(v, varInteger);
  Write('integer and integer: ':25);
  v := integer_1 and integer_2;
  CheckBasicVariantType(v, varInteger);
  Write('integer and int64: ':25);
  v := integer_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('integer and byte: ':25);
  v := integer_1 and byte_2;
  CheckBasicVariantType(v, varInteger);
  Write('integer and word: ':25);
  v := integer_1 and word_2;
  CheckBasicVariantType(v, varInteger);
  Write('integer and longword: ':25);
  v := integer_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('integer and uint64: ':25);
  v := integer_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('int64 and shortint: ':25);
  v := int64_1 and shortint_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and smallint: ':25);
  v := int64_1 and smallint_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and integer: ':25);
  v := int64_1 and integer_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and int64: ':25);
  v := int64_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and byte: ':25);
  v := int64_1 and byte_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and word: ':25);
  v := int64_1 and word_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and longword: ':25);
  v := int64_1 and longword_2;
  CheckBasicVariantType(v, varInt64);
  Write('int64 and uint64: ':25);
  v := int64_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('byte and shortint: ':25);
  v := byte_1 and shortint_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('byte and smallint: ':25);
  v := byte_1 and smallint_2;
  CheckBasicVariantType(v, varSmallInt);
  Write('byte and integer: ':25);
  v := byte_1 and integer_2;
  CheckBasicVariantType(v, varInteger);
  Write('byte and int64: ':25);
  v := byte_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('byte and byte: ':25);
  v := byte_1 and byte_2;
  CheckBasicVariantType(v, varByte);
  Write('byte and word: ':25);
  v := byte_1 and word_2;
  CheckBasicVariantType(v, varWord);
  Write('byte and longword: ':25);
  v := byte_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('byte and uint64: ':25);
  v := byte_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('word and shortint: ':25);
  v := word_1 and shortint_2;
  CheckBasicVariantType(v, varInteger);
  Write('word and smallint: ':25);
  v := word_1 and smallint_2;
  CheckBasicVariantType(v, varInteger);
  Write('word and integer: ':25);
  v := word_1 and integer_2;
  CheckBasicVariantType(v, varInteger);
  Write('word and int64: ':25);
  v := word_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('word and byte: ':25);
  v := word_1 and byte_2;
  CheckBasicVariantType(v, varWord);
  Write('word and word: ':25);
  v := word_1 and word_2;
  CheckBasicVariantType(v, varWord);
  Write('word and longword: ':25);
  v := word_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('word and uint64: ':25);
  v := word_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('longword and shortint: ':25);
  v := longword_1 and shortint_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and smallint: ':25);
  v := longword_1 and smallint_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and integer: ':25);
  v := longword_1 and integer_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and int64: ':25);
  v := longword_1 and int64_2;
  CheckBasicVariantType(v, varInt64);
  Write('longword and byte: ':25);
  v := longword_1 and byte_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and word: ':25);
  v := longword_1 and word_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and longword: ':25);
  v := longword_1 and longword_2;
  CheckBasicVariantType(v, varLongWord);
  Write('longword and uint64: ':25);
  v := longword_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and shortint: ':25);
  v := uint64_1 and shortint_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and smallint: ':25);
  v := uint64_1 and smallint_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and integer: ':25);
  v := uint64_1 and integer_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and int64: ':25);
  v := uint64_1 and int64_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and byte: ':25);
  v := uint64_1 and byte_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and word: ':25);
  v := uint64_1 and word_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and longword: ':25);
  v := uint64_1 and longword_2;
  CheckBasicVariantType(v, varUInt64);
  Write('uint64 and uint64: ':25);
  v := uint64_1 and uint64_2;
  CheckBasicVariantType(v, varUInt64);
  if Error then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Success!');
end.
{$ELSE Enable_Test}
begin
end.
{$ENDIF Enable_Test}
