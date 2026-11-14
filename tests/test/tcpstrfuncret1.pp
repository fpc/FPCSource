{ %skipcpu=jvm }
program tcpstrfuncret1;

{ A function declared to return a string with a code page must not start from a
  string in another code page when its result goes to a temporary, whose slot other
  temporaries share, or to a destination declared with another code page: SetLength
  keeps the code page of a string already in the result's location. }

{$mode objfpc}
{$h+}
{$inline on}

{$if defined(go32v2) or defined(wasi)}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_CPALL_UNIT}
  cpall,
{$endif}
  sysutils;

type
  TString866 = type AnsiString(866);
  TUtf8Func = function(aFirst, aSecond: Byte): Utf8String;

  THolder = class
    function Build(aFirst, aSecond: Byte): Utf8String;
  end;

  EExpected = class(Exception);

const
  { neither the raw functions' code page nor one a function here declares }
  cpDefault = 1251;
  cpRaw = 1252;

var
  gBad: Integer = 0;

procedure Check(const aName: ShortString; aGot, aWant: LongInt);
begin
  if aGot <> aWant then begin
    Inc(gBad);
    Writeln('FAIL ', aName, ': got ', aGot, ', want ', aWant);
  end;
end;

function BuildUtf8(aFirst, aSecond: Byte): Utf8String;
begin
  SetLength(Result, 2);
  PByte(Pointer(Result))[0] := aFirst;
  PByte(Pointer(Result))[1] := aSecond;
end;

function BuildUtf8Inline(aFirst, aSecond: Byte): Utf8String; inline;
begin
  SetLength(Result, 2);
  PByte(Pointer(Result))[0] := aFirst;
  PByte(Pointer(Result))[1] := aSecond;
end;

function BuildAnsi: AnsiString;
begin
  SetLength(Result, 1);
  PByte(Pointer(Result))[0] := $41;
end;

function Build866(aFirst: Byte): TString866;
begin
  SetLength(Result, 1);
  PByte(Pointer(Result))[0] := aFirst;
end;

function THolder.Build(aFirst, aSecond: Byte): Utf8String;
begin
  SetLength(Result, 2);
  PByte(Pointer(Result))[0] := aFirst;
  PByte(Pointer(Result))[1] := aSecond;
end;

function RawCp(aValue: Integer): RawByteString;
begin
  SetLength(Result, 1);
  PByte(Pointer(Result))[0] := $30 + aValue;
  SetCodePage(Result, cpRaw, False);
end;

{ a function with a declared code page that returns another one: no conversion is
  made from RawByteString }
function Utf8FromRaw(aValue: Integer): Utf8String;
begin
  Result := RawCp(aValue);
end;

function RawSetLength: RawByteString;
begin
  SetLength(Result, 2);
end;

function BuildRaising: Utf8String;
begin
  SetLength(Result, 2);
  PByte(Pointer(Result))[0] := $58;
  raise EExpected.Create('expected');
end;

function BuildRaisingInline: Utf8String; inline;
begin
  SetLength(Result, 2);
  PByte(Pointer(Result))[0] := $58;
  raise EExpected.Create('expected');
end;

function CpOfRaw(const aValue: RawByteString): LongInt;
begin
  Result := StringCodePage(aValue);
end;

function CpOfUtf8(const aValue: Utf8String): LongInt;
begin
  Result := StringCodePage(aValue);
end;

function CpOfAnsi(const aValue: AnsiString): LongInt;
begin
  Result := StringCodePage(aValue);
end;

procedure KeepRaw(const aValue: RawByteString);
begin
  if Length(aValue) < 0 then
    Halt(2);
end;

procedure KeepUtf8(const aValue: Utf8String);
begin
  if Length(aValue) < 0 then
    Halt(2);
end;

procedure KeepAnsi(const aValue: AnsiString);
begin
  if Length(aValue) < 0 then
    Halt(2);
end;

{ the destination of an assignment, holding another code page }
procedure TestAssign;
var
  lRaw: RawByteString;
begin
  lRaw := RawCp(1);
  lRaw := BuildUtf8($C3, $A9);
  Check('assignment to a RawByteString variable', StringCodePage(lRaw), CP_UTF8);
  lRaw := RawCp(1);
  lRaw := BuildUtf8Inline($C3, $A9);
  Check('the same, inlined', StringCodePage(lRaw), CP_UTF8);
  lRaw := RawCp(1);
  lRaw := Build866($41);
  Check('the same, another code page', StringCodePage(lRaw), 866);
  lRaw := RawCp(1);
  Utf8String(lRaw) := BuildUtf8($C3, $A9);
  Check('a typecast RawByteString variable', StringCodePage(lRaw), CP_UTF8);
end;

{ the same through a function's own result }
function ResultFromUtf8: RawByteString;
begin
  Result := RawCp(2);
  Result := BuildUtf8($C3, $A9);
end;

{ a temporary for a RawByteString parameter, its slot used by a RawByteString function
  between the calls }
procedure TestParameterRaw;
var
  lIdx: Integer;
  lFunc: TUtf8Func;
  lHolder: THolder;
begin
  lFunc := @BuildUtf8;
  lHolder := THolder.Create;
  for lIdx := 0 to 1 do begin
    Check('RawByteString parameter', CpOfRaw(BuildUtf8($C3, $A9)), CP_UTF8);
    KeepRaw(RawCp(lIdx));
    Check('RawByteString parameter, inlined', CpOfRaw(BuildUtf8Inline($C3, $A9)), CP_UTF8);
    KeepRaw(RawCp(lIdx));
    Check('RawByteString parameter, procedure variable', CpOfRaw(lFunc($C3, $A9)), CP_UTF8);
    KeepRaw(RawCp(lIdx));
    Check('RawByteString parameter, method', CpOfRaw(lHolder.Build($C3, $A9)), CP_UTF8);
    KeepRaw(RawCp(lIdx));
  end;
  lHolder.Free;
end;

{ a temporary of the function's own type: a RawByteString function given to a parameter
  of that type fills the same slot without a conversion }
procedure TestParameterTyped;
var
  lIdx: Integer;
begin
  for lIdx := 0 to 1 do begin
    Check('Utf8String parameter', CpOfUtf8(BuildUtf8($C3, $A9)), CP_UTF8);
    KeepUtf8(RawCp(lIdx));
    Check('Utf8String parameter, inlined', CpOfUtf8(BuildUtf8Inline($C3, $A9)), CP_UTF8);
    KeepUtf8(RawCp(lIdx));
    Check('AnsiString parameter', CpOfAnsi(BuildAnsi), cpDefault);
    KeepAnsi(RawCp(lIdx));
  end;
end;

{ the two directions in one loop: their temporaries have different types and do not
  meet.  This fails for a fix that types only a code-paged function's temporary by its
  declared result, which brings them together }
procedure TestParameterBoth;
var
  lIdx: Integer;
begin
  for lIdx := 0 to 1 do begin
    Check('both directions', CpOfRaw(BuildUtf8($C3, $A9)), CP_UTF8);
    KeepUtf8(RawCp(lIdx));
  end;
end;

{ the function before it, declared with a code page, returned another one }
procedure TestParameterAfterOwnType;
var
  lIdx: Integer;
begin
  for lIdx := 0 to 1 do begin
    Check('after a function returning another code page', CpOfUtf8(BuildUtf8($C3, $A9)), CP_UTF8);
    KeepUtf8(Utf8FromRaw(lIdx));
  end;
end;

{ a multiple concatenation, whose operands are an array of RawByteString: C0 80 is
  ill-formed, so a conversion of any operand through UTF-16 changes the bytes }
procedure CheckBytes(const aName: ShortString; const aGot: Utf8String);
var
  lOk: Boolean;
begin
  lOk := (Length(aGot) = 6) and
         (PByte(Pointer(aGot))[0] = $61) and (PByte(Pointer(aGot))[1] = $62) and
         (PByte(Pointer(aGot))[2] = $C0) and (PByte(Pointer(aGot))[3] = $80) and
         (PByte(Pointer(aGot))[4] = $63) and (PByte(Pointer(aGot))[5] = $64);
  Check(aName, Ord(lOk), 1);
end;

procedure TestConcat;
var
  lSrc: Utf8String;
  lIdx: Integer;
begin
  for lIdx := 0 to 1 do begin
    lSrc := BuildUtf8($C0, $80);
    lSrc := BuildUtf8($61, $62) + lSrc + BuildUtf8($63, $64);
    CheckBytes('concatenation', lSrc);
    KeepRaw(RawCp(lIdx));
  end;
end;

{ the shape an inlined IntToStr gives: its Str goes through a RawByteString result,
  in the default code page, which is not the function's }
procedure TestIntToStr;
var
  lSrc: Utf8String;
  lIdx: Integer;
begin
  for lIdx := 0 to 1 do begin
    lSrc := BuildUtf8($C0, $80);
    lSrc := BuildUtf8($61, $62) + lSrc + BuildUtf8($63, $64);
    CheckBytes('concatenation, IntToStr after it', lSrc);
    if IntToStr(lIdx) = '' then
      Halt(3);
  end;
end;

{ a function raising: a destination of another code page still holds what RawCp(4) gave
  it, since it is assigned only after the function returned }
procedure CheckUntouched(const aName: ShortString; const aValue: RawByteString);
begin
  Check(aName,
        Ord((Length(aValue) = 1) and
            (StringCodePage(aValue) = cpRaw) and
            (PByte(Pointer(aValue))[0] = $34)),
        1);
end;

procedure TestRaising;
var
  lRaw: RawByteString;
begin
  lRaw := RawCp(4);
  try
    lRaw := BuildRaising();
    Check('the function did not raise', 0, 1);
  except
    on EExpected do ;
  end;
  CheckUntouched('a function raising, RawByteString destination', lRaw);
  lRaw := RawCp(4);
  try
    lRaw := BuildRaisingInline();
    Check('the inlined function did not raise', 0, 1);
  except
    on EExpected do ;
  end;
  CheckUntouched('a function raising, inlined', lRaw);
end;

{ what must not change.  A function declared RawByteString has no code page of its
  own: it takes the one its destination holds, and no conversion is made.  A variable
  keeps the code page of what it holds through its own SetLength. }
procedure TestUnchanged;
var
  lUtf8: Utf8String;
  lRaw: RawByteString;
  lAnsi: AnsiString;
begin
  lUtf8 := BuildUtf8($C3, $A9);
  lUtf8 := RawSetLength();
  Check('a RawByteString function, Utf8String destination', StringCodePage(lUtf8), CP_UTF8);
  lRaw := RawCp(3);
  Check('the destination holds the raw code page', StringCodePage(lRaw), cpRaw);
  lRaw := RawSetLength();
  Check('the destination''s code page is kept', StringCodePage(lRaw), cpRaw);
  lAnsi := RawCp(3);
  Check('no conversion to an AnsiString variable', StringCodePage(lAnsi), cpRaw);
  SetLength(lAnsi, 4);
  Check('its SetLength keeps that code page', StringCodePage(lAnsi), cpRaw);
  lAnsi := Utf8Encode(UnicodeString('a'));
  SetLength(lAnsi, 4);
  Check('the same after Utf8Encode', StringCodePage(lAnsi), CP_UTF8);
end;

begin
  DefaultSystemCodePage := cpDefault;
  TestAssign;
  Check('a function''s own result', StringCodePage(ResultFromUtf8()), CP_UTF8);
  TestParameterRaw;
  TestParameterTyped;
  TestParameterBoth;
  TestParameterAfterOwnType;
  TestConcat;
  TestIntToStr;
  TestRaising;
  TestUnchanged;
  if gBad <> 0 then
    Halt(1);
  Writeln('ok');
end.
