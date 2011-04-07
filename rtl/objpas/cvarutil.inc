{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000,2001 by the Free Pascal development team

    Interface and OS-dependent part of variant support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Procedure VariantTypeMismatch; overload;
begin
  Raise EVariantError.CreateCode(VAR_TYPEMISMATCH);
end;

Procedure VariantTypeMismatch(const SourceType, DestType: TVarType);
begin
  { ignore the types for now ... }
  Raise EVariantError.CreateCode(VAR_TYPEMISMATCH);
end;


Function ExceptionToVariantError (E : Exception): HResult;

begin
  If E is EoutOfMemory then
    Result:=VAR_OUTOFMEMORY
  else
    Result:=VAR_EXCEPTION;
end;

{ ---------------------------------------------------------------------
    OS-independent functions not present in Windows
  ---------------------------------------------------------------------}

{--- SmallInt ---}

Function WStrToSmallInt(p: Pointer) : SmallInt;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varSmallInt);
end;

Function LStrToSmallInt(p: Pointer) : SmallInt;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varSmallInt);
end;

function UStrToSmallInt(p: Pointer): SmallInt;
var
  Error: Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varSmallInt);
end;

Function VariantToSmallInt(const VargSrc : TVarData) : SmallInt;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToSmallInt', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := smallint(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := smallint(Round(vSingle));
        varDouble   : Result := smallint(Round(vDouble));
        varDate     : Result := smallint(Round(vDate));
{$endif}
        varCurrency : Result := smallint(Round(vCurrency));
        varBoolean  : Result := smallint(SmallInt(vBoolean));
        varVariant  : Result := VariantToSmallInt(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := smallint(vWord);
        varLongWord : Result := smallint(vLongWord);
        varInt64    : Result := smallint(vInt64);
        varQword    : Result := smallint(vQWord);
        varOleStr   : Result := WStrToSmallInt(vOleStr);
        varString   : Result := LStrToSmallInt(vString);
        varUString  : Result := UStrToSmallInt(vString);
      else
        VariantTypeMismatch(vType, varSmallInt);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := smallint(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := smallint(Round(PSingle(vPointer)^));
        varDouble   : Result := smallint(Round(PDouble(vPointer)^));
        varDate     : Result := smallint(Round(PDate(vPointer)^));
{$endif}
        varCurrency : Result := smallint(Round(PCurrency(vPointer)^));
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToSmallInt(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := smallint(PWord(vPointer)^);
        varLongWord : Result := smallint(PLongWord(vPointer)^);
        varInt64    : Result := smallint(PInt64(vPointer)^);
        varQword    : Result := smallint(PQWord(vPointer)^);
        varOleStr   : Result := WStrToSmallInt(PPointer(vPointer)^);
        varString   : Result := LStrToSmallInt(PPointer(vPointer)^);
        varUString  : Result := UStrToSmallInt(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varSmallInt);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varSmallInt);
    else { array or something like that }
      VariantTypeMismatch(vType, varSmallInt);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToSmallInt -> ', Result);
  end; {$ENDIF}
end;

{--- ShortInt ---}

Function WStrToShortInt(p: Pointer) : ShortInt;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varShortInt);
end;

Function LStrToShortInt(p: Pointer) : ShortInt;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varShortInt);
end;

Function UStrToShortInt(p: Pointer) : ShortInt;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varShortInt);
end;

Function VariantToShortInt(const VargSrc : TVarData) : ShortInt;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToShortInt', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := shortint(vSmallInt);
        varShortInt : Result := vShortInt;
        varInteger  : Result := shortint(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := shortint(Round(vSingle));
        varDouble   : Result := shortint(Round(vDouble));
        varDate     : Result := shortint(Round(vDate));
{$endif}
        varCurrency : Result := shortint(Round(vCurrency));
        varBoolean  : Result := shortint(vBoolean);
        varVariant  : Result := VariantToShortInt(PVarData(vPointer)^);
        varByte     : Result := shortint(vByte);
        varWord     : Result := shortint(vWord);
        varLongWord : Result := shortint(vLongWord);
        varInt64    : Result := shortint(vInt64);
        varQword    : Result := shortint(vQWord);
        varOleStr   : Result := WStrToShortInt(vOleStr);
        varString   : Result := LStrToShortInt(vString);
        varUString  : Result := UStrToShortInt(vString);
      else
        VariantTypeMismatch(vType, varShortInt);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := shortint(PSmallInt(vPointer)^);
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := shortint(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := shortint(Round(PSingle(vPointer)^));
        varDouble   : Result := shortint(Round(PDouble(vPointer)^));
        varDate     : Result := shortint(Round(PDate(vPointer)^));
{$endif}
        varCurrency : Result := shortint(Round(PCurrency(vPointer)^));
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToShortInt(PVarData(vPointer)^);
        varByte     : Result := shortint(PByte(vPointer)^);
        varWord     : Result := shortint(PWord(vPointer)^);
        varLongWord : Result := shortint(PLongWord(vPointer)^);
        varInt64    : Result := shortint(PInt64(vPointer)^);
        varQword    : Result := shortint(PQWord(vPointer)^);
        varOleStr   : Result := WStrToShortInt(PPointer(vPointer)^);
        varString   : Result := LStrToShortInt(PPointer(vPointer)^);
        varUString  : Result := UStrToShortInt(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varShortInt);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varShortInt);
    else { array or something like that }
      VariantTypeMismatch(vType, varShortInt);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToShortInt -> ', Result);
  end; {$ENDIF}
end;

{--- LongInt ---}

Function WStrToLongInt(p: Pointer) : LongInt;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varInteger);
end;

Function LStrToLongInt(p: Pointer) : LongInt;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varInteger);
end;

Function UStrToLongInt(p: Pointer) : LongInt;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varInteger);
end;

Function VariantToLongInt(const VargSrc : TVarData) : LongInt;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToLongInt', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
{$ifndef FPUNONE}
        varSingle   : Result := longint(Round(vSingle));
        varDouble   : Result := longint(Round(vDouble));
        varDate     : Result := longint(Round(vDate));
{$endif}
        varCurrency : Result := longint(Round(vCurrency));
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToLongInt(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := longint(vLongWord);
        varInt64    : Result := longint(vInt64);
        varQword    : Result := longint(vQWord);
        varOleStr   : Result := WStrToLongInt(vOleStr);
        varString   : Result := LStrToLongInt(vString);
        varUString  : Result := UStrToLongInt(vString);
      else
        VariantTypeMismatch(vType, varInteger);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
{$ifndef FPUNONE}
        varSingle   : Result := longint(Round(PSingle(vPointer)^));
        varDouble   : Result := longint(Round(PDouble(vPointer)^));
        varDate     : Result := longint(Round(PDate(vPointer)^));
{$endif}
        varCurrency : Result := longint(Round(PCurrency(vPointer)^));
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToLongInt(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := longint(PLongWord(vPointer)^);
        varInt64    : Result := longint(PInt64(vPointer)^);
        varQword    : Result := longint(PQWord(vPointer)^);
        varOleStr   : Result := WStrToLongInt(PPointer(vPointer)^);
        varString   : Result := LStrToLongInt(PPointer(vPointer)^);
        varUString  : Result := UStrToLongInt(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varInteger);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varInteger);
    else { array or something like that }
      VariantTypeMismatch(vType, varInteger);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToLongInt -> ', Result);
  end; {$ENDIF}
end;

{--- Cardinal ---}

Function WStrToCardinal(p: Pointer) : Cardinal;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varLongWord);
end;

Function LStrToCardinal(p: Pointer) : Cardinal;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varLongWord);
end;

Function UStrToCardinal(p: Pointer) : Cardinal;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varLongWord);
end;

Function VariantToCardinal(const VargSrc : TVarData) : Cardinal;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToCardinal', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := cardinal(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := cardinal(Round(vSingle));
        varDouble   : Result := cardinal(Round(vDouble));
        varDate     : Result := cardinal(Round(vDate));
{$endif}
        varCurrency : Result := cardinal(Round(vCurrency));
        varBoolean  : Result := cardinal(SmallInt(vBoolean));
        varVariant  : Result := VariantToCardinal(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := cardinal(vInt64);
        varQword    : Result := cardinal(vQWord);
        varOleStr   : Result := WStrToCardinal(vOleStr);
        varString   : Result := LStrToCardinal(vString);
        varUString  : Result := UStrToCardinal(vString);
      else
        VariantTypeMismatch(vType, varLongWord);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := cardinal(PSmallInt(vPointer)^);
        varShortInt : Result := cardinal(PShortInt(vPointer)^);
        varInteger  : Result := cardinal(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := cardinal(Round(PSingle(vPointer)^));
        varDouble   : Result := cardinal(Round(PDouble(vPointer)^));
        varDate     : Result := cardinal(Round(PDate(vPointer)^));
{$endif}
        varCurrency : Result := cardinal(Round(PCurrency(vPointer)^));
        varBoolean  : Result := cardinal(SmallInt(PWordBool(vPointer)^));
        varVariant  : Result := VariantToCardinal(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := cardinal(PInt64(vPointer)^);
        varQword    : Result := cardinal(PQWord(vPointer)^);
        varOleStr   : Result := WStrToCardinal(PPointer(vPointer)^);
        varString   : Result := LStrToCardinal(PPointer(vPointer)^);
        varUString  : Result := UStrToCardinal(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varLongWord);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varLongWord);
    else { array or something like that }
      VariantTypeMismatch(vType, varLongWord);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToCardinal -> ', Result);
  end; {$ENDIF}
end;

procedure PrepareFloatStr(var s: ShortString);
var
  i, j  : Byte;
begin
  j := 1;
  for i := 1 to Length(s) do
    if s[i] <> DefaultFormatSettings.ThousandSeparator then begin
      if s[i] = DefaultFormatSettings.DecimalSeparator then
        s[j] := '.'
      else
        s[j] := s[i];
      Inc(j);
    end;
  SetLength(s, Pred(j));
end;

{--- Single ---}

{$ifndef FPUNONE}

Function WStrToSingle(p: Pointer) : Single;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(WideString(p)) > 255 then
    VariantTypeMismatch(varOleStr, varSingle);

  s := WideString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varSingle);
end;

Function LStrToSingle(p: Pointer) : Single;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(AnsiString(p)) > 255 then
    VariantTypeMismatch(varString, varSingle);

  s := AnsiString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varSingle);
end;

Function UStrToSingle(p: Pointer) : Single;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(UnicodeString(p)) > 255 then
    VariantTypeMismatch(varUString, varSingle);

  s := UnicodeString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varSingle);
end;


Function VariantToSingle(const VargSrc : TVarData) : Single;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToSingle', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
        varSingle   : Result := vSingle;
        varDouble   : Result := vDouble;
        varCurrency : Result := vCurrency;
        varDate     : Result := vDate;
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToSingle(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := vInt64;
        varQword    : Result := vQWord;
        varOleStr   : Result := WStrToSingle(vOleStr);
        varString   : Result := LStrToSingle(vString);
        varUString  : Result := UStrToSingle(vString);
      else
        VariantTypeMismatch(vType, varSingle);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
        varSingle   : Result := PSingle(vPointer)^;
        varDouble   : Result := PDouble(vPointer)^;
        varCurrency : Result := PCurrency(vPointer)^;
        varDate     : Result := PDate(vPointer)^;
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToSingle(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := PInt64(vPointer)^;
        varQword    : Result := PQWord(vPointer)^;
        varOleStr   : Result := WStrToSingle(PPointer(vPointer)^);
        varString   : Result := LStrToSingle(PPointer(vPointer)^);
        varUString  : Result := UStrToSingle(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varSingle);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varSingle);
    else { array or something like that }
      VariantTypeMismatch(vType, varSingle);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToSingle -> ', Result);
  end; {$ENDIF}
end;

{--- Double ---}

Function WStrToDouble(p: Pointer) : Double;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(WideString(p)) > 255 then
    VariantTypeMismatch(varOleStr, varDouble);

  s := WideString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varDouble);
end;

Function LStrToDouble(p: Pointer) : Double;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(AnsiString(p)) > 255 then
    VariantTypeMismatch(varString, varDouble);

  s := AnsiString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varDouble);
end;

Function UStrToDouble(p: Pointer) : Double;
var
  s     : ShortString;
  Error : Word;
begin
  if Length(UnicodeString(p)) > 255 then
    VariantTypeMismatch(varUString, varDouble);

  s := UnicodeString(p);
  PrepareFloatStr(s);

  Val(s, Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varDouble);
end;

Function VariantToDouble(const VargSrc : TVarData) : Double;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToDouble', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
        varSingle   : Result := vSingle;
        varDouble   : Result := vDouble;
        varCurrency : Result := vCurrency;
        varDate     : Result := vDate;
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToDouble(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := vInt64;
        varQword    : Result := vQWord;
        varOleStr   : Result := WStrToDouble(vOleStr);
        varString   : Result := LStrToDouble(vString);
        varUString  : Result := UStrToDouble(vString);
      else
        VariantTypeMismatch(vType, varDouble);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
        varSingle   : Result := PSingle(vPointer)^;
        varDouble   : Result := PDouble(vPointer)^;
        varCurrency : Result := PCurrency(vPointer)^;
        varDate     : Result := PDate(vPointer)^;
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToDouble(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := PInt64(vPointer)^;
        varQword    : Result := PQWord(vPointer)^;
        varOleStr   : Result := WStrToDouble(PPointer(vPointer)^);
        varString   : Result := LStrToDouble(PPointer(vPointer)^);
        varUString  : Result := UStrToDouble(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varDouble);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varDouble);
    else { array or something like that }
      VariantTypeMismatch(vType, varDouble);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToDouble -> ', Result);
  end; {$ENDIF}
end;

{$endif FPUNONE}

{--- Currency ---}

Function WStrToCurrency(p: Pointer) : Currency;
var
  s     : ShortString;
  Error : Word;
  {$IFNDEF FPC_HAS_STR_CURRENCY}
  Temp  : Extended;
  {$ENDIF FPC_HAS_STR_CURRENCY}
begin
  if Length(WideString(p)) > 255 then
    VariantTypeMismatch(varOleStr, varCurrency);

  s := WideString(p);
  PrepareFloatStr(s);

  {$IFDEF FPC_HAS_STR_CURRENCY}
  Val(s, Result, Error);
  {$ELSE FPC_HAS_STR_CURRENCY} { needed for platforms where Currency = Int64 }
  Val(s, Temp, Error);
  Result := Temp;
  {$ENDIF FPC_HAS_STR_CURRENCY}

  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varCurrency);
end;

Function LStrToCurrency(p: Pointer) : Currency;
var
  s     : ShortString;
  Error : Word;
  {$IFNDEF FPC_HAS_STR_CURRENCY}
  Temp  : Extended;
  {$ENDIF FPC_HAS_STR_CURRENCY}
begin
  if Length(AnsiString(p)) > 255 then
    VariantTypeMismatch(varString, varCurrency);

  s := AnsiString(p);
  PrepareFloatStr(s);

  {$IFDEF FPC_HAS_STR_CURRENCY}
  Val(s, Result, Error);
  {$ELSE FPC_HAS_STR_CURRENCY} { needed for platforms where Currency = Int64 }
  Val(s, Temp, Error);
  Result := Temp;
  {$ENDIF FPC_HAS_STR_CURRENCY}

  if Error <> 0 then
    VariantTypeMismatch(varString, varCurrency);
end;

Function UStrToCurrency(p: Pointer) : Currency;
var
  s     : ShortString;
  Error : Word;
  {$IFNDEF FPC_HAS_STR_CURRENCY}
  Temp  : Extended;
  {$ENDIF FPC_HAS_STR_CURRENCY}
begin
  if Length(UnicodeString(p)) > 255 then
    VariantTypeMismatch(varUString, varCurrency);

  s := UnicodeString(p);
  PrepareFloatStr(s);

  {$IFDEF FPC_HAS_STR_CURRENCY}
  Val(s, Result, Error);
  {$ELSE FPC_HAS_STR_CURRENCY} { needed for platforms where Currency = Int64 }
  Val(s, Temp, Error);
  Result := Temp;
  {$ENDIF FPC_HAS_STR_CURRENCY}

  if Error <> 0 then
    VariantTypeMismatch(varUString, varCurrency);
end;

Function VariantToCurrency(const VargSrc : TVarData) : Currency;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToCurrency', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
{$ifndef FPUNONE}
        varSingle   : begin
          if (vSingle > MaxCurrency) or (vSingle < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := vSingle;
        end;
        varDouble   : begin
          if (vDouble > MaxCurrency) or (vDouble < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := vDouble;
        end;
        varDate     : begin
          if (vDate > MaxCurrency) or (vDate < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := vDate;
        end;
{$endif}
        varCurrency : Result := vCurrency;
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToCurrency(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := vInt64;
        varQword    : Result := currency(vQWord);
        varOleStr   : Result := WStrToCurrency(vOleStr);
        varString   : Result := LStrToCurrency(vString);
        varUString  : Result := UStrToCurrency(vString);
      else
        VariantTypeMismatch(vType, varCurrency);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
{$ifndef FPUNONE}
        varSingle   : begin
          if (PSingle(vPointer)^ > MaxCurrency) or (PSingle(vPointer)^ < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := PSingle(vPointer)^;
        end;
        varDouble   : begin
          if (PDouble(vPointer)^ > MaxCurrency) or (PDouble(vPointer)^ < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := PDouble(vPointer)^;
        end;
        varDate     : begin
          if (PDate(vPointer)^ > MaxCurrency) or (PDate(vPointer)^ < MinCurrency) then
            VariantTypeMismatch(vType, varCurrency);
          Result := PDate(vPointer)^;
        end;
{$endif}
        varCurrency : Result := PCurrency(vPointer)^;
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToCurrency(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := PInt64(vPointer)^;
        varQword    : Result := currency(PQWord(vPointer)^);
        varOleStr   : Result := WStrToCurrency(PPointer(vPointer)^);
        varString   : Result := LStrToCurrency(PPointer(vPointer)^);
        varUString  : Result := UStrToCurrency(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varCurrency);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varCurrency);
    else { array or something like that }
      VariantTypeMismatch(vType, varCurrency);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToCurrency -> ', Result);
  end; {$ENDIF}
end;

{--- Date ---}

{$ifndef FPUNONE}

Function WStrToDate(p: Pointer) : TDateTime;
var
  s: string;
begin
  s := WideString(p);

  if not TryStrToDateTime(s, Result) then
    VariantTypeMismatch(varOleStr, varDate);
end;

Function LStrToDate(p: Pointer) : TDateTime;
begin
  if not TryStrToDateTime(AnsiString(p), Result) then
    VariantTypeMismatch(varString, varDate);
end;

Function UStrToDate(p: Pointer) : TDateTime;
begin
  if not TryStrToDateTime(UnicodeString(p), Result) then
    VariantTypeMismatch(varUString, varDate);
end;

Function VariantToDate(const VargSrc : TVarData) : TDateTime;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToDate', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
        varSingle   : Result := vSingle;
        varDouble   : Result := vDouble;
        varCurrency : Result := vCurrency;
        varDate     : Result := vDate;
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToDate(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := vInt64;
        varQword    : Result := vQWord;
        varOleStr   : Result := WStrToDate(vOleStr);
        varString   : Result := LStrToDate(vString);
        varUString  : Result := UStrToDate(vString);
      else
        VariantTypeMismatch(vType, varDate);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
        varSingle   : Result := PSingle(vPointer)^;
        varDouble   : Result := PDouble(vPointer)^;
        varCurrency : Result := PCurrency(vPointer)^;
        varDate     : Result := PDate(vPointer)^;
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToDate(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := PInt64(vPointer)^;
        varQword    : Result := PQWord(vPointer)^;
        varOleStr   : Result := WStrToDate(PPointer(vPointer)^);
        varString   : Result := LStrToDate(PPointer(vPointer)^);
        varUString  : Result := UStrToDate(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varDate);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varDate);
    else { array or something like that }
      VariantTypeMismatch(vType, varDate);
    end;
  if (Result < MinDateTime) or (Result > MaxDateTime) then
    VariantTypeMismatch(VargSrc.vType, varDate);

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToDate -> ', Result);
  end; {$ENDIF}
end;

{$endif}

{--- Boolean ---}

Function WStrToBoolean(p: Pointer) : Boolean;
begin
  if not TryStrToBool(WideString(p), Result) then
    VariantTypeMismatch(varOleStr, varBoolean);
end;

Function LStrToBoolean(p: Pointer) : Boolean;
begin
  if not TryStrToBool(AnsiString(p), Result) then
    VariantTypeMismatch(varString, varBoolean);
end;

Function UStrToBoolean(p: Pointer) : Boolean;
begin
  if not TryStrToBool(UnicodeString(p), Result) then
    VariantTypeMismatch(varUString, varBoolean);
end;

Function VariantToBoolean(const VargSrc : TVarData) : Boolean;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToBoolean', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := False;
        varSmallInt : Result := vSmallInt <> 0;
        varShortInt : Result := vShortInt <> 0;
        varInteger  : Result := vInteger <> 0;
{$ifndef FPUNONE}
        varSingle   : Result := vSingle <> 0;
        varDouble   : Result := vDouble <> 0;
        varCurrency : Result := vCurrency <> 0;
        varDate     : Result := vDate <> 0;
{$endif}
        varBoolean  : Result := vBoolean;
        varVariant  : Result := VariantToBoolean(PVarData(vPointer)^);
        varByte     : Result := vByte <> 0;
        varWord     : Result := vWord <> 0;
        varLongWord : Result := vLongWord <> 0;
        varInt64    : Result := vInt64 <> 0;
        varQword    : Result := vQWord <> 0;
        varOleStr   : Result := WStrToBoolean(vOleStr);
        varString   : Result := LStrToBoolean(vString);
        varUString  : Result := UStrToBoolean(vString);
      else
        VariantTypeMismatch(vType, varBoolean);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^ <> 0;
        varShortInt : Result := PShortInt(vPointer)^ <> 0;
        varInteger  : Result := PInteger(vPointer)^ <> 0;
{$ifndef FPUNONE}
        varSingle   : Result := PSingle(vPointer)^ <> 0;
        varDouble   : Result := PDouble(vPointer)^ <> 0;
        varCurrency : Result := PCurrency(vPointer)^ <> 0;
        varDate     : Result := PDate(vPointer)^ <> 0;
{$endif}
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^) <> 0;
        varVariant  : Result := VariantToBoolean(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^ <> 0;
        varWord     : Result := PWord(vPointer)^ <> 0;
        varLongWord : Result := PLongWord(vPointer)^ <> 0;
        varInt64    : Result := PInt64(vPointer)^ <> 0;
        varQword    : Result := PQWord(vPointer)^ <> 0;
        varOleStr   : Result := WStrToBoolean(PPointer(vPointer)^);
        varString   : Result := LStrToBoolean(PPointer(vPointer)^);
        varUString  : Result := UStrToBoolean(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varBoolean);
      end else { pointer is nil }
        Result := False;
    else { array or something like that }
      VariantTypeMismatch(vType, varBoolean);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToBoolean -> ', Result);
  end; {$ENDIF}
end;

{--- Byte ---}

Function WStrToByte(p: Pointer) : Byte;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varByte);
end;

Function LStrToByte(p: Pointer) : Byte;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varByte);
end;

Function UStrToByte(p: Pointer) : Byte;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varByte);
end;

Function VariantToByte(const VargSrc : TVarData) : Byte;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToByte', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := byte(vSmallInt);
        varShortInt : Result := byte(vShortInt);
        varInteger  : Result := byte(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := byte(Round(vSingle));
        varDouble   : Result := byte(Round(vDouble));
        varCurrency : Result := byte(Round(vCurrency));
        varDate     : Result := byte(Round(vDate));
{$endif}
        varBoolean  : Result := byte(SmallInt(vBoolean));
        varVariant  : Result := VariantToByte(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := byte(vWord);
        varLongWord : Result := byte(vLongWord);
        varInt64    : Result := byte(vInt64);
        varQword    : Result := byte(vQWord);
        varOleStr   : Result := WStrToByte(vOleStr);
        varString   : Result := LStrToByte(vString);
        varUString  : Result := UStrToByte(vString);
      else
        VariantTypeMismatch(vType, varByte);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := byte(PSmallInt(vPointer)^);
        varShortInt : Result := byte(PShortInt(vPointer)^);
        varInteger  : Result := byte(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := byte(Round(PSingle(vPointer)^));
        varDouble   : Result := byte(Round(PDouble(vPointer)^));
        varCurrency : Result := byte(Round(PCurrency(vPointer)^));
        varDate     : Result := byte(Round(PDate(vPointer)^));
{$endif}
        varBoolean  : Result := byte(SmallInt(PWordBool(vPointer)^));
        varVariant  : Result := byte(VariantToByte(PVarData(vPointer)^));
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := byte(PWord(vPointer)^);
        varLongWord : Result := byte(PLongWord(vPointer)^);
        varInt64    : Result := byte(PInt64(vPointer)^);
        varQword    : Result := byte(PQWord(vPointer)^);
        varOleStr   : Result := WStrToByte(PPointer(vPointer)^);
        varString   : Result := LStrToByte(PPointer(vPointer)^);
        varUString  : Result := UStrToByte(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varByte);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varByte);
    else { array or something like that }
      VariantTypeMismatch(vType, varByte);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToByte -> ', Result);
  end; {$ENDIF}
end;

{--- Int64 ---}

Function WStrToInt64(p: Pointer) : Int64;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varInt64);
end;

Function LStrToInt64(p: Pointer) : Int64;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varInt64);
end;

Function UStrToInt64(p: Pointer) : Int64;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varInt64);
end;

Function VariantToInt64(const VargSrc : TVarData) : Int64;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToInt64', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := vSmallInt;
        varShortInt : Result := vShortInt;
        varInteger  : Result := vInteger;
{$ifndef FPUNONE}
        varSingle   : Result := Round(vSingle);
        varDouble   : Result := Round(vDouble);
        varCurrency : Result := Round(vCurrency);
        varDate     : Result := Round(vDate);
{$endif}
        varBoolean  : Result := SmallInt(vBoolean);
        varVariant  : Result := VariantToInt64(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := vInt64;
        varQword    : Result := int64(vQWord);
        varOleStr   : Result := WStrToInt64(vOleStr);
        varString   : Result := LStrToInt64(vString);
        varUString  : Result := UStrToInt64(vString);
      else
        VariantTypeMismatch(vType, varInt64);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := PSmallInt(vPointer)^;
        varShortInt : Result := PShortInt(vPointer)^;
        varInteger  : Result := PInteger(vPointer)^;
{$ifndef FPUNONE}
        varSingle   : Result := Round(PSingle(vPointer)^);
        varDouble   : Result := Round(PDouble(vPointer)^);
        varCurrency : Result := Round(PCurrency(vPointer)^);
        varDate     : Result := Round(PDate(vPointer)^);
{$endif}
        varBoolean  : Result := SmallInt(PWordBool(vPointer)^);
        varVariant  : Result := VariantToInt64(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := PInt64(vPointer)^;
        varQword    : Result := PQWord(vPointer)^;
        varOleStr   : Result := WStrToInt64(PPointer(vPointer)^);
        varString   : Result := LStrToInt64(PPointer(vPointer)^);
        varUString  : Result := UStrToInt64(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varInt64);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varInt64);
    else { array or something like that }
      VariantTypeMismatch(vType, varInt64);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToInt64 -> ', Result);
  end; {$ENDIF}
end;

{--- QWord ---}

Function WStrToQWord(p: Pointer) : QWord;
var
  Error : Word;
begin
  Val(WideString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varOleStr, varQWord);
end;

Function LStrToQWord(p: Pointer) : QWord;
var
  Error : Word;
begin
  Val(AnsiString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varString, varQWord);
end;

Function UStrToQWord(p: Pointer) : QWord;
var
  Error : Word;
begin
  Val(UnicodeString(p), Result, Error);
  if Error <> 0 then
    VariantTypeMismatch(varUString, varQWord);
end;

Function VariantToQWord(const VargSrc : TVarData) : QWord;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToQWord', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := 0;
        varSmallInt : Result := qword(vSmallInt);
        varShortInt : Result := qword(vShortInt);
        varInteger  : Result := qword(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := qword(Round(vSingle));
        varDouble   : Result := qword(Round(vDouble));
        varCurrency : Result := qword(Round(vCurrency));
        varDate     : Result := qword(Round(vDate));
{$endif}
        varBoolean  : Result := qword(SmallInt(vBoolean));
        varVariant  : Result := VariantToQWord(PVarData(vPointer)^);
        varByte     : Result := vByte;
        varWord     : Result := vWord;
        varLongWord : Result := vLongWord;
        varInt64    : Result := qword(vInt64);
        varQword    : Result := vQWord;
        varOleStr   : Result := WStrToQWord(vOleStr);
        varString   : Result := LStrToQWord(vString);
        varUString  : Result := UStrToQWord(vString);
      else
        VariantTypeMismatch(vType, varQWord);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := qword(PSmallInt(vPointer)^);
        varShortInt : Result := qword(PShortInt(vPointer)^);
        varInteger  : Result := qword(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := qword(Round(PSingle(vPointer)^));
        varDouble   : Result := qword(Round(PDouble(vPointer)^));
        varCurrency : Result := qword(Round(PCurrency(vPointer)^));
        varDate     : Result := qword(Round(PDate(vPointer)^));
{$endif}
        varBoolean  : Result := qword(SmallInt(PWordBool(vPointer)^));
        varVariant  : Result := VariantToQWord(PVarData(vPointer)^);
        varByte     : Result := PByte(vPointer)^;
        varWord     : Result := PWord(vPointer)^;
        varLongWord : Result := PLongWord(vPointer)^;
        varInt64    : Result := qword(PInt64(vPointer)^);
        varQword    : Result := PQWord(vPointer)^;
        varOleStr   : Result := WStrToQWord(PPointer(vPointer)^);
        varString   : Result := LStrToQWord(PPointer(vPointer)^);
        varUString  : Result := UStrToQWord(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varQWord);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varQWord);
    else { array or something like that }
      VariantTypeMismatch(vType, varQWord);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToQWord -> ', Result);
  end; {$ENDIF}
end;

function VarDateToString(DT: TDateTime): AnsiString;
begin
  if Trunc(DT) = 0 then
    Result := TimeToStr(DT)
  else
    Result := DateTimeToStr(DT);
end;

{--- WideString ---}

Function VariantToWideString(const VargSrc : TVarData) : WideString;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToWideString', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := '';
        varSmallInt : Result := IntToStr(vSmallInt);
        varShortInt : Result := IntToStr(vShortInt);
        varInteger  : Result := IntToStr(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := FloatToStr(vSingle);
        varDouble   : Result := FloatToStr(vDouble);
        varCurrency : Result := FloatToStr(vCurrency);
        varDate     : Result := VarDateToString(vDate);
{$endif}
        varBoolean  : Result := BoolToStr(vBoolean, True);
        varVariant  : Result := VariantToWideString(PVarData(vPointer)^);
        varByte     : Result := IntToStr(vByte);
        varWord     : Result := IntToStr(vWord);
        varLongWord : Result := IntToStr(vLongWord);
        varInt64    : Result := IntToStr(vInt64);
        varQword    : Result := IntToStr(vQWord);
        varOleStr   : Result := WideString(Pointer(vOleStr));
        varString   : Result := AnsiString(vString);
        varUString  : Result := UnicodeString(vString);
      else
        VariantTypeMismatch(vType, varOleStr);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := IntToStr(PSmallInt(vPointer)^);
        varShortInt : Result := IntToStr(PShortInt(vPointer)^);
        varInteger  : Result := IntToStr(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := FloatToStr(PSingle(vPointer)^);
        varDouble   : Result := FloatToStr(PDouble(vPointer)^);
        varCurrency : Result := FloatToStr(PCurrency(vPointer)^);
        varDate     : Result := VarDateToString(PDate(vPointer)^);
{$endif}
        varBoolean  : Result := BoolToStr(PWordBool(vPointer)^, True);
        varVariant  : Result := VariantToWideString(PVarData(vPointer)^);
        varByte     : Result := IntToStr(PByte(vPointer)^);
        varWord     : Result := IntToStr(PWord(vPointer)^);
        varLongWord : Result := IntToStr(PLongWord(vPointer)^);
        varInt64    : Result := IntToStr(PInt64(vPointer)^);
        varQword    : Result := IntToStr(PQWord(vPointer)^);
        varOleStr   : Result := WideString(PPointer(vPointer)^);
        varString   : Result := AnsiString(PPointer(vPointer)^);
        varUString  : Result := UnicodeString(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varOleStr);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varOleStr);
    else { array or something like that }
      VariantTypeMismatch(vType, varOleStr);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToWideString -> ', Result);
  end; {$ENDIF}
end;

{--- AnsiString ---}

Function VariantToAnsiString(const VargSrc : TVarData) : AnsiString;
begin
  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  DumpVariant('VariantToAnsiString', VargSrc);
  end; {$ENDIF}

  with VargSrc do
    case vType and not varTypeMask of
      0: case vType of
        varEmpty    : Result := '';
        varSmallInt : Result := IntToStr(vSmallInt);
        varShortInt : Result := IntToStr(vShortInt);
        varInteger  : Result := IntToStr(vInteger);
{$ifndef FPUNONE}
        varSingle   : Result := FloatToStr(vSingle);
        varDouble   : Result := FloatToStr(vDouble);
        varCurrency : Result := FloatToStr(vCurrency);
        varDate     : Result := VarDateToString(vDate);
{$endif}
        varBoolean  : Result := BoolToStr(vBoolean, True);
        varVariant  : Result := VariantToAnsiString(PVarData(vPointer)^);
        varByte     : Result := IntToStr(vByte);
        varWord     : Result := IntToStr(vWord);
        varLongWord : Result := IntToStr(vLongWord);
        varInt64    : Result := IntToStr(vInt64);
        varQword    : Result := IntToStr(vQWord);
        varOleStr   : Result := WideString(Pointer(vOleStr));
        varString   : Result := AnsiString(vString);
        varUString  : Result := UnicodeString(vString);
      else
        VariantTypeMismatch(vType, varString);
      end;
      varByRef: if Assigned(vPointer) then case vType and varTypeMask of
        varSmallInt : Result := IntToStr(PSmallInt(vPointer)^);
        varShortInt : Result := IntToStr(PShortInt(vPointer)^);
        varInteger  : Result := IntToStr(PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   : Result := FloatToStr(PSingle(vPointer)^);
        varDouble   : Result := FloatToStr(PDouble(vPointer)^);
        varCurrency : Result := FloatToStr(PCurrency(vPointer)^);
        varDate     : Result := VarDateToString(PDate(vPointer)^);
{$endif}
        varBoolean  : Result := BoolToStr(PWordBool(vPointer)^, True);
        varVariant  : Result := VariantToAnsiString(PVarData(vPointer)^);
        varByte     : Result := IntToStr(PByte(vPointer)^);
        varWord     : Result := IntToStr(PWord(vPointer)^);
        varLongWord : Result := IntToStr(PLongWord(vPointer)^);
        varInt64    : Result := IntToStr(PInt64(vPointer)^);
        varQword    : Result := IntToStr(PQWord(vPointer)^);
        varOleStr   : Result := WideString(PPointer(vPointer)^);
        varString   : Result := AnsiString(PPointer(vPointer)^);
        varUString  : Result := UnicodeString(PPointer(vPointer)^);
      else { other vtype }
        VariantTypeMismatch(vType, varString);
      end else { pointer is nil }
        VariantTypeMismatch(vType, varString);
    else { array or something like that }
      VariantTypeMismatch(vType, varString);
    end;

  {$IFDEF DEBUG_VARUTILS} if __DEBUG_VARUTILS then begin
  WriteLn('VariantToAnsiString -> ', Result);
  end; {$ENDIF}
end;


Function VariantToShortString(const VargSrc : TVarData) : ShortString;
begin
  Result:=VariantToAnsiString(VargSrc);
end;

{ ---------------------------------------------------------------------
    Some debug routines
  ---------------------------------------------------------------------}


Procedure DumpVariant(const VSrc : Variant);
begin
  DumpVariant(Output, '', TVarData(VSrc));
end;

Procedure DumpVariant(const aName: string; const VSrc : Variant);
begin
  DumpVariant(Output, aName, TVarData(VSrc));
end;

Procedure DumpVariant(Var F : Text; const VSrc : Variant);
begin
  DumpVariant(F, '', TVarData(VSrc));
end;

procedure DumpVariant(var F : Text; const aName: string; const VSrc : Variant); 
begin
  DumpVariant(F, aName, TVarData(VSrc));
end;

Procedure DumpVariant(const VargSrc : TVarData);
begin
  DumpVariant(Output, '', VargSrc);
end;

Procedure DumpVariant(const aName: string; const VargSrc : TVarData);
begin
  DumpVariant(Output, aName, VargSrc);
end;

Procedure DumpVariant(Var F : Text; const VargSrc : TVarData);
begin
  DumpVariant(F, '', VargSrc);
end;

const
  VarTypeStrings : array [varEmpty..varQword] of string = (
    'empty',     { varempty    = 0 }
    'null',      { varnull     = 1 }
    'smallint',  { varsmallint = 2 }
    'integer',   { varinteger  = 3 }
    'single',    { varsingle   = 4 }
    'double',    { vardouble   = 5 }
    'currency',  { varcurrency = 6 }
    'date',      { vardate     = 7 }
    'olestr',    { varolestr   = 8 }
    'dispatch',  { vardispatch = 9 }
    'error',     { varerror    = 10 }
    'boolean',   { varboolean  = 11 }
    'variant',   { varvariant  = 12 }
    'unknown',   { varunknown  = 13 }
    'decimal',   { vardecimal  = 14 }
    'undefined',
    'shortint',  { varshortint = 16 }
    'byte',      { varbyte     = 17 }
    'word',      { varword     = 18 }
    'longword',  { varlongword = 19 }
    'int64',     { varint64    = 20 }
    'qword');    { varqword    = 21 }

Procedure DumpVariant(Var F : Text; const aName: string; const VargSrc : TVarData);

Var
  i: Integer;

begin
  Writeln(F,'---> ', aName, ' at $', HexStr(@VargSrc), ' <----------------');
  with VargSrc do begin

    if vType and varByRef = varByRef then
      Writeln(F,'Variant is by reference.');

    if vType and varArray = varArray then
      Writeln(F,'Variant is an array.');

    if vType and not (varTypeMask or varArray or varByRef) <> 0 then
      Writeln(F,'Variant has unknown flags set in type: $', IntToHex(vType, 4));


    If (vType and varTypeMask) in [varEmpty..varQword] then
      Writeln(F,'Variant has type : ', VarTypeStrings[vType and varTypeMask])
    else If (vType and varTypeMask) = varString then
      Writeln(F,'Variant has type : string')
    else if (vType and varTypeMask) = varUString then
      Writeln(F,'Variant has type : UnicodeString')
    else
      Writeln(F,'Variant has unknown type : $', IntToHex(vType and varTypeMask, 4));

    Write('Bytes :');
    for i := 0 to 13 do
      Write(IntToHex(VBytes[i], 2),' ');
    WriteLn;

    if vType and varArray = varArray then begin
      Writeln(F,'---< ', aName, ' at $', HexStr(@VargSrc), ' >----------------');
      Writeln(F);
      Exit;
    end;

    If vType <> varEmpty then begin
      Write(F,'Value is: [');

      if (vType and varByRef = varByRef) or (vType and varTypeMask = varVariant) then
        if not Assigned(vPointer) then begin
          WriteLn(F, 'nil]');
          Writeln(F,'---< ', aName, ' at $', HexStr(@VargSrc), ' >----------------');
          Writeln(F);
          Exit;
        end;

      case vType of
        varNull                 : Write(F, 'Null');
        varSmallInt             : Write(F, vSmallInt);
        varInteger              : Write(F, vInteger);
{$ifndef FPUNONE}
        varSingle               : Write(F, vSingle);
        varDouble               : Write(F, vDouble);
        varCurrency             : Write(F, vCurrency);
        varDate                 : Write(F, vDate);
{$endif}
        varOleStr               : Write(F, WideString(Pointer(vOleStr)));
        varError                : Write(F, IntToHex(Cardinal(vError), 8));
        varBoolean              : Write(F, vBoolean);
        varVariant, varVariant or varByRef : begin
          WriteLn(' dereferencing -> ]');
          DumpVariant(F, aName+'^', PVarData(vPointer)^);
          Exit;
        end;
        varShortInt             : Write(F, vShortInt);
        varByte                 : Write(F, vByte);
        varWord                 : Write(F, vWord);
        varLongWord             : Write(F, vLongWord);
        varInt64                : Write(F, vInt64);
        varQword                : Write(F, vQWord);
        varString               : Write(F, AnsiString(vString));
        varNull     or varByRef : Write(F, 'Null');
        varSmallInt or varByRef : Write(F, PSmallInt(vPointer)^);
        varInteger  or varByRef : Write(F, PInteger(vPointer)^);
{$ifndef FPUNONE}
        varSingle   or varByRef : Write(F, PSingle(vPointer)^);
        varDouble   or varByRef : Write(F, PDouble(vPointer)^);
        varCurrency or varByRef : Write(F, PCurrency(vPointer)^);
        varDate     or varByRef : Write(F, PDate(vPointer)^);
{$endif}
        varOleStr   or varByRef : Write(F, WideString(PPointer(vPointer)^));
        varError    or varByRef : Write(F, IntToHex(Cardinal(PLongWord(vPointer)^), 8));
        varBoolean  or varByRef : Write(F, PWordBool(vPointer)^);
        varShortInt or varByRef : Write(F, PShortInt(vPointer)^);
        varByte     or varByRef : Write(F, PByte(vPointer)^);
        varWord     or varByRef : Write(F, PWord(vPointer)^);
        varLongWord or varByRef : Write(F, PLongWord(vPointer)^);
        varInt64    or varByRef : Write(F, PInt64(vPointer)^);
        varQword    or varByRef : Write(F, PQWord(vPointer)^);
        varString   or varByRef : Write(F, AnsiString(PPointer(vPointer)^));
      else
        Write(F, 'Unsupported');
      end;
      WriteLn(F, ']');
    end;
  end;

  Writeln(F,'---< ', aName, ' at $', HexStr(@VargSrc), ' >----------------');
  Writeln(F);
end;



