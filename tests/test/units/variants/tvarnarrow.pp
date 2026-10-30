{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ converting a variant to an integer type, as Delphi does: VarAsType raises
  EVariantOverflowError when the value does not fit the destination type; an
  assignment raises it when the value does not fit a longint and otherwise
  narrows, except an assignment to a dword, which takes the low 32 bits of the
  int64 value; a boolean is taken as its bit pattern and is never checked.
  The expected results are Delphi's, except where a comment says otherwise }
program tvarnarrow;

{$mode objfpc}{$H+}

uses
  sysutils, typinfo, variants;

type
  TRecordingType = class(TCustomVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

  { an IDispatch whose default property returns a value }
  TValueDispatch = class(TInterfacedObject, IDispatch)
  private
    FValue: Variant;
  public
    constructor Create(const aValue: Variant);
    function GetTypeInfoCount(out count: LongInt): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: LongInt; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const iid: TGUID; names: Pointer;
      NameCount, LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: LongInt; const iid: TGUID; LocaleID: LongInt;
      Flags: Word; var params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  TBit = (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15,
          b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31);
  TBits = set of TBit;
  {$M+}
  TSetHolder = class
  private
    FBits: TBits;
  published
    property Bits: TBits read FBits write FBits;
  end;
  {$M-}

const
  destTypes: array[0..6] of TVarType = (varShortInt, varSmallInt, varInteger, varByte, varWord, varLongWord, varInt64);
  destNames: array[0..6] of string = ('shortint', 'smallint', 'longint', 'byte', 'word', 'dword', 'int64');

var
  errors: longint = 0;
  asked: TVarType;
  answerBig: boolean = false;
  recording: TRecordingType;

procedure TRecordingType.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
end;

procedure TRecordingType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  Dest := Source;
end;

procedure TRecordingType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
begin
  asked := AVarType;
  Dest.vType := AVarType;
  case AVarType of
    varInteger:
      if answerBig then
        begin
          Dest.vType := varInt64;
          Dest.vInt64 := 4294967296;
        end
      else
        Dest.vInteger := 7;
    varInt64:
      Dest.vInt64 := 7;
  else
    Dest.vInt64 := 0;
  end;
end;

constructor TValueDispatch.Create(const aValue: Variant);
begin
  inherited Create;
  FValue := aValue;
end;

function TValueDispatch.GetTypeInfoCount(out count: LongInt): HResult; stdcall;
begin
  count := 0;
  Result := S_OK;
end;

function TValueDispatch.GetTypeInfo(Index, LocaleID: LongInt; out TypeInfo): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TValueDispatch.GetIDsOfNames(const iid: TGUID; names: Pointer;
  NameCount, LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TValueDispatch.Invoke(DispID: LongInt; const iid: TGUID; LocaleID: LongInt;
  Flags: Word; var params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
begin
  if (DispID <> 0) or (Flags and 2 = 0) then
    Exit(E_NOTIMPL);
  if Assigned(VarResult) then
    PVariant(VarResult)^ := FValue;
  Result := S_OK;
end;

procedure check(const what, got, want: string);
begin
  if got = want then
    writeln('ok   ', what, ' = ', got)
  else
    begin
      writeln('FAIL ', what, ' = ', got, ', expected ', want);
      inc(errors);
    end;
end;

{ OVF stands for EVariantOverflowError exactly }
function outcome(e: Exception): string;
begin
  if e.ClassType = EVariantOverflowError then
    result := 'OVF'
  else
    result := e.ClassName;
end;

function viaCast(const v: variant; dest: longint): string;
var
  r: variant;
begin
  try
    r := VarAsType(v, destTypes[dest]);
    if VarType(r) = destTypes[dest] then
      result := VarToStr(r)
    else
      result := 'vartype ' + IntToStr(VarType(r));
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function viaAssign(const v: variant; dest: longint): string;
var
  s: shortint;
  sm: smallint;
  l: longint;
  b: byte;
  w: word;
  d: dword;
  i: int64;
begin
  try
    case dest of
      0: begin s := v; result := IntToStr(s); end;
      1: begin sm := v; result := IntToStr(sm); end;
      2: begin l := v; result := IntToStr(l); end;
      3: begin b := v; result := IntToStr(b); end;
      4: begin w := v; result := IntToStr(w); end;
      5: begin d := v; result := IntToStr(d); end;
    else
      begin i := v; result := IntToStr(i); end;
    end;
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function viaOleByte(const v: olevariant): string;
var
  b: byte;
begin
  try
    b := v;
    result := IntToStr(b);
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function viaOleDword(const v: olevariant): string;
var
  d: dword;
begin
  try
    d := v;
    result := IntToStr(d);
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function castMessage(const v: variant; varType: TVarType): string;
begin
  try
    VarAsType(v, varType);
    result := 'no exception';
  except
    on e: Exception do
      result := e.Message;
  end;
end;

function assignMessage(const v: variant): string;
var
  l: longint;
begin
  try
    l := v;
    result := 'no exception, ' + IntToStr(l);
  except
    on e: Exception do
      result := e.Message;
  end;
end;

function viaVarCast(const v: variant; varType: TVarType): string;
var
  r: variant;
begin
  try
    VarCast(r, v, varType);
    result := VarToStr(r);
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function viaError(const v: variant): string;
var
  e: TError;
begin
  try
    e := v;
    result := IntToStr(e);
  except
    on x: Exception do
      result := outcome(x);
  end;
end;

function viaNativeInt(const v: variant): string;
var
  n: NativeInt;
begin
  try
    n := v;
    result := IntToStr(n);
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

function viaNativeUInt(const v: variant): string;
var
  n: NativeUInt;
begin
  try
    n := v;
    result := IntToStr(n);
  except
    on e: Exception do
      result := outcome(e);
  end;
end;

type
  TResults = array of string;

function words(const s: string): TResults;
var
  i, start: longint;
begin
  result := nil;
  start := 1;
  for i := 1 to length(s) + 1 do
    if (i > length(s)) or (s[i] = ' ') then
      begin
        setlength(result, length(result) + 1);
        result[high(result)] := copy(s, start, i - start);
        start := i + 1;
      end;
end;

{ wantCast and wantAssign each give the result for shortint, smallint,
  longint, byte, word, dword and int64, separated by spaces }
procedure row(const name: string; const v: variant; const wantCast, wantAssign: string);
var
  dest: longint;
  casts, assigns: TResults;
begin
  casts := words(wantCast);
  assigns := words(wantAssign);
  if (length(casts) <> 7) or (length(assigns) <> 7) then
    begin
      writeln('FAIL row ', name, ' is malformed');
      inc(errors);
      exit;
    end;
  for dest := 0 to 6 do
    begin
      check('VarAsType(' + name + ', ' + destNames[dest] + ')', viaCast(v, dest), casts[dest]);
      check(destNames[dest] + ' := ' + name, viaAssign(v, dest), assigns[dest]);
    end;
end;

{ SetPropValue takes a number for a set as its bit mask, whatever the number's
  type -- not a Delphi row: Delphi takes a number there only as a varInteger.
  The mask is read from a set, so that the test holds for any set layout }
function viaSetMask(const v: variant): string;
var
  h: TSetHolder;
begin
  h := TSetHolder.Create;
  try
    try
      SetPropValue(h, 'Bits', v);
      result := IntToStr(int64(longword(h.Bits)));
    except
      on e: Exception do
        result := outcome(e);
    end;
  finally
    h.Free;
  end;
end;

procedure checkSetMasks;
var
  bits: TBits;
  mask: longword;
  want: string;
begin
  bits := [b0, b31];
  mask := longword(bits);
  want := IntToStr(int64(mask));
  check('set from longword mask', viaSetMask(mask), want);
  check('set from longint mask', viaSetMask(longint(mask)), want);
  check('set from int64 mask', viaSetMask(int64(mask)), want);
  check('set from int64 mask beyond 32 bits', viaSetMask(int64(mask) + int64($100000000)), want);
  check('set from qword mask', viaSetMask(qword(mask)), want);
{$ifndef FPUNONE}
  check('set from double mask', viaSetMask(double(mask)), want);
{$endif}
end;

function askedFor(const v: variant; toDword: boolean): string;
var
  l: longint;
  d: dword;
begin
  asked := varEmpty;
  if toDword then
    begin
      d := v;
      result := IntToStr(asked) + ' gives ' + IntToStr(d);
    end
  else
    begin
      l := v;
      result := IntToStr(asked) + ' gives ' + IntToStr(l);
    end;
end;

function askedForOle(const v: olevariant): string;
var
  d: dword;
begin
  asked := varEmpty;
  d := v;
  result := IntToStr(asked) + ' gives ' + IntToStr(d);
end;

var
  v, inner, dref: variant;
  o, custom: olevariant;
  customVar: variant;
  disp: IDispatch;
  i64: int64;
  q64: qword;
  li: longint;
  wb: wordbool;
{$ifndef FPUNONE}
  dbl: double;
  sgl: single;
  cur: currency;
{$endif}
  s: ansistring;
  ws: widestring;
  us: unicodestring;
begin
  v := longint(0);
  row('longint 0', v, '0 0 0 0 0 0 0', '0 0 0 0 0 0 0');
  v := longint(200);
  row('longint 200', v, 'OVF 200 200 200 200 200 200', '-56 200 200 200 200 200 200');
  v := longint(-1);
  row('longint -1', v, '-1 -1 -1 OVF OVF OVF -1', '-1 -1 -1 255 65535 4294967295 -1');
  v := longint(300);
  row('longint 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  v := longint(70000);
  row('longint 70000', v, 'OVF OVF 70000 OVF OVF 70000 70000', '112 4464 70000 112 4464 70000 70000');
  v := longint(127);
  row('longint 127', v, '127 127 127 127 127 127 127', '127 127 127 127 127 127 127');
  v := longint(128);
  row('longint 128', v, 'OVF 128 128 128 128 128 128', '-128 128 128 128 128 128 128');
  v := longint(-128);
  row('longint -128', v, '-128 -128 -128 OVF OVF OVF -128', '-128 -128 -128 128 65408 4294967168 -128');
  v := longint(-129);
  row('longint -129', v, 'OVF -129 -129 OVF OVF OVF -129', '127 -129 -129 127 65407 4294967167 -129');
  v := longint(255);
  row('longint 255', v, 'OVF 255 255 255 255 255 255', '-1 255 255 255 255 255 255');
  v := longint(256);
  row('longint 256', v, 'OVF 256 256 OVF 256 256 256', '0 256 256 0 256 256 256');
  v := longint(32767);
  row('longint 32767', v, 'OVF 32767 32767 OVF 32767 32767 32767', '-1 32767 32767 255 32767 32767 32767');
  v := longint(32768);
  row('longint 32768', v, 'OVF OVF 32768 OVF 32768 32768 32768', '0 -32768 32768 0 32768 32768 32768');
  v := longint(-32768);
  row('longint -32768', v, 'OVF -32768 -32768 OVF OVF OVF -32768', '0 -32768 -32768 0 32768 4294934528 -32768');
  v := longint(-32769);
  row('longint -32769', v, 'OVF OVF -32769 OVF OVF OVF -32769', '-1 32767 -32769 255 32767 4294934527 -32769');
  v := int64(65535);
  row('int64 65535', v, 'OVF OVF 65535 OVF 65535 65535 65535', '-1 -1 65535 255 65535 65535 65535');
  v := int64(65536);
  row('int64 65536', v, 'OVF OVF 65536 OVF OVF 65536 65536', '0 0 65536 0 0 65536 65536');
  v := int64(2147483647);
  row('int64 2147483647', v, 'OVF OVF 2147483647 OVF OVF 2147483647 2147483647', '-1 -1 2147483647 255 65535 2147483647 2147483647');
  v := int64(-2147483648);
  row('int64 -2147483648', v, 'OVF OVF -2147483648 OVF OVF OVF -2147483648', '0 0 -2147483648 0 0 2147483648 -2147483648');
  v := int64(-2147483649);
  row('int64 -2147483649', v, 'OVF OVF OVF OVF OVF OVF -2147483649', 'OVF OVF OVF OVF OVF 2147483647 -2147483649');
  v := int64(4294967295);
  row('int64 4294967295', v, 'OVF OVF OVF OVF OVF 4294967295 4294967295', 'OVF OVF OVF OVF OVF 4294967295 4294967295');
  v := int64(2147483648);
  row('int64 2147483648', v, 'OVF OVF OVF OVF OVF 2147483648 2147483648', 'OVF OVF OVF OVF OVF 2147483648 2147483648');
  v := int64(4294967296);
  row('int64 4294967296', v, 'OVF OVF OVF OVF OVF OVF 4294967296', 'OVF OVF OVF OVF OVF 0 4294967296');
  v := dword(4294967295);
  row('dword 4294967295', v, 'OVF OVF OVF OVF OVF 4294967295 4294967295', 'OVF OVF OVF OVF OVF 4294967295 4294967295');
  v := qword(300);
  row('qword 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  v := qword(255);
  row('qword 255', v, 'OVF 255 255 255 255 255 255', '-1 255 255 255 255 255 255');
  v := qword(4294967295);
  row('qword 4294967295', v, 'OVF OVF OVF OVF OVF 4294967295 4294967295', 'OVF OVF OVF OVF OVF 4294967295 4294967295');
  v := qword(4294967296);
  row('qword 4294967296', v, 'OVF OVF OVF OVF OVF OVF 4294967296', 'OVF OVF OVF OVF OVF 0 4294967296');
  v := high(qword);
  row('qword 18446744073709551615', v, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  v := True;
  row('boolean True', v, '-1 -1 -1 255 65535 4294967295 -1', '-1 -1 -1 255 65535 4294967295 -1');
  v := False;
  row('boolean False', v, '0 0 0 0 0 0 0', '0 0 0 0 0 0 0');

{$ifndef FPUNONE}
  dbl := 300;
  v := dbl;
  row('double 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  dbl := 1e10;
  v := dbl;
  row('double 1e10', v, 'OVF OVF OVF OVF OVF OVF 10000000000', 'OVF OVF OVF OVF OVF 1410065408 10000000000');
  dbl := 127.5;
  v := dbl;
  row('double 127.5', v, 'OVF 128 128 128 128 128 128', '-128 128 128 128 128 128 128');
  dbl := -128.5;
  v := dbl;
  row('double -128.5', v, '-128 -128 -128 OVF OVF OVF -128', '-128 -128 -128 128 65408 4294967168 -128');
  dbl := 4294967295.4;
  v := dbl;
  row('double 4294967295.4', v, 'OVF OVF OVF OVF OVF 4294967295 4294967295', 'OVF OVF OVF OVF OVF 4294967295 4294967295');
  sgl := 300;
  v := sgl;
  row('single 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  v := VarFromDateTime(300);
  row('date 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  cur := 300;
  v := cur;
  row('currency 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  cur := 100000000000000;
  v := cur;
  row('currency 1e14', v, 'OVF OVF OVF OVF OVF OVF 100000000000000', 'OVF OVF OVF OVF OVF 276447232 100000000000000');
{$endif FPUNONE}

  s := '300';
  v := s;
  row('ansistring 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  s := '-1';
  v := s;
  row('ansistring -1', v, '-1 -1 -1 OVF OVF OVF -1', '-1 -1 -1 255 65535 4294967295 -1');
  s := '4294967296';
  v := s;
  row('ansistring 4294967296', v, 'OVF OVF OVF OVF OVF OVF 4294967296', 'OVF OVF OVF OVF OVF 0 4294967296');
  s := '2147483647';
  v := s;
  row('ansistring 2147483647', v, 'OVF OVF 2147483647 OVF OVF 2147483647 2147483647', '-1 -1 2147483647 255 65535 2147483647 2147483647');
  s := '2147483648';
  v := s;
  row('ansistring 2147483648', v, 'OVF OVF OVF OVF OVF 2147483648 2147483648', 'OVF OVF OVF OVF OVF 2147483648 2147483648');
  s := '-2147483648';
  v := s;
  row('ansistring -2147483648', v, 'OVF OVF -2147483648 OVF OVF OVF -2147483648', '0 0 -2147483648 0 0 2147483648 -2147483648');
  s := '-2147483649';
  v := s;
  row('ansistring -2147483649', v, 'OVF OVF OVF OVF OVF OVF -2147483649', 'OVF OVF OVF OVF OVF 2147483647 -2147483649');
  { a hexadecimal string is read as a 32-bit pattern for every type but
    dword and int64, as Delphi's Val fallback reads it }
  s := '$FF';
  v := s;
  row('ansistring $FF', v, 'OVF 255 255 255 255 255 255', '-1 255 255 255 255 255 255');
  s := '$7FFFFFFF';
  v := s;
  row('ansistring $7FFFFFFF', v, 'OVF OVF 2147483647 OVF OVF 2147483647 2147483647', '-1 -1 2147483647 255 65535 2147483647 2147483647');
  s := '$FFFFFFFF';
  v := s;
  row('ansistring $FFFFFFFF', v, '-1 -1 -1 OVF OVF 4294967295 4294967295', '-1 -1 -1 255 65535 4294967295 4294967295');
  s := '-$1';
  v := s;
  row('ansistring -$1', v, '-1 -1 -1 OVF OVF OVF -1', '-1 -1 -1 255 65535 4294967295 -1');
  s := '-0';
  v := s;
  row('ansistring -0', v, '0 0 0 0 0 0 0', '0 0 0 0 0 0 0');
  s := '-$7FFFFFFF';
  v := s;
  row('ansistring -$7FFFFFFF', v, 'OVF OVF -2147483647 OVF OVF OVF -2147483647', '1 1 -2147483647 1 1 2147483649 -2147483647');
  s := ' 300';
  v := s;
  row('ansistring  300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  s := '&377';
  v := s;
  row('ansistring &377', v, 'OVF 255 255 255 255 255 255', '-1 255 255 255 255 255 255');
  s := '0x100';
  v := s;
  row('ansistring 0x100', v, 'OVF 256 256 OVF 256 256 256', '0 256 256 0 256 256 256');
  { Delphi gives other results for these on the types below dword: there it
    tries the OS parser first, which in a locale whose currency sign is '$'
    reads '$100', '$80000000', '$100000000', '-$80000000' and '-$80' as
    those decimal amounts, and it raises EVariantTypeCastError for a
    hexadecimal string beyond 32 bits }
  s := '$100';
  v := s;
  row('ansistring $100', v, 'OVF 256 256 OVF 256 256 256', '0 256 256 0 256 256 256');
  s := '$80000000';
  v := s;
  row('ansistring $80000000', v, 'OVF OVF -2147483648 OVF OVF 2147483648 2147483648', '0 0 -2147483648 0 0 2147483648 2147483648');
  s := '$100000000';
  v := s;
  row('ansistring $100000000', v, 'OVF OVF OVF OVF OVF OVF 4294967296', 'OVF OVF OVF OVF OVF 0 4294967296');
  s := '$1FFFFFFFF';
  v := s;
  row('ansistring $1FFFFFFFF', v, 'OVF OVF OVF OVF OVF OVF 8589934591', 'OVF OVF OVF OVF OVF 4294967295 8589934591');
  s := '$FFFFFFFFFFFFFFFF';
  v := s;
  row('ansistring $FFFFFFFFFFFFFFFF', v, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  s := '-$80000000';
  v := s;
  row('ansistring -$80000000', v, 'OVF OVF -2147483648 OVF OVF OVF -2147483648', '0 0 -2147483648 0 0 2147483648 -2147483648');
  s := '-$80';
  v := s;
  row('ansistring -$80', v, '-128 -128 -128 OVF OVF OVF -128', '-128 -128 -128 128 65408 4294967168 -128');
  { on 32- and 64-bit CPUs Val refuses a negative hexadecimal magnitude
    beyond the signed range, so this overflows on the types below dword,
    where Delphi's Val negates the 32-bit pattern and gives 1; the longint
    Val of 16- and 8-bit CPUs reads it as Delphi's does }
  s := '-$FFFFFFFF';
  v := s;
{$if defined(cpu16) or defined(cpu8)}
  row('ansistring -$FFFFFFFF', v, '1 1 1 1 1 OVF -4294967295', '1 1 1 1 1 1 -4294967295');
{$else}
  row('ansistring -$FFFFFFFF', v, 'OVF OVF OVF OVF OVF OVF -4294967295', 'OVF OVF OVF OVF OVF 1 -4294967295');
{$endif}
  { Val cannot read a hexadecimal string beyond 64 bits at either width, so
    it keeps its EVariantError; Delphi reads this one, whose digits are all
    decimal, as the decimal number }
  s := '$10000000000000000';
  v := s;
  row('ansistring $10000000000000000', v, 'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError',
    'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError');
  { Val's binary spelling, which Delphi does not accept }
  s := '%11111111';
  v := s;
  row('ansistring %11111111', v, 'OVF 255 255 255 255 255 255', '-1 255 255 255 255 255 255');
  { Val refuses a trailing blank, which Delphi's OS parser accepts }
  s := '300 ';
  v := s;
  row('ansistring 300 with a trailing blank', v, 'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError',
    'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError');
  { a string that is not a number keeps its EVariantError (Delphi raises
    EVariantTypeCastError) }
  s := 'abc';
  v := s;
  row('ansistring abc', v, 'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError',
    'EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError EVariantError');
  ws := '300';
  v := ws;
  row('widestring 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  us := '300';
  v := us;
  row('unicodestring 300', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');

  { variants made by hand that refer to their values }
  v := Unassigned;
  i64 := 300;
  TVarData(v).vType := varInt64 or varByRef;
  TVarData(v).vPointer := @i64;
  row('int64 300 by reference', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
{$ifndef cpu16}
  { a 16-bit CPU reads a longint held by reference as 16 bits, a defect of
    its own outside this test's subject }
  li := 65536;
  TVarData(v).vType := varInteger or varByRef;
  TVarData(v).vPointer := @li;
  row('longint 65536 by reference', v, 'OVF OVF 65536 OVF OVF 65536 65536', '0 0 65536 0 0 65536 65536');
  li := -1;
  row('longint -1 by reference', v, '-1 -1 -1 OVF OVF OVF -1', '-1 -1 -1 255 65535 4294967295 -1');
{$endif cpu16}
  wb := True;
  TVarData(v).vType := varBoolean or varByRef;
  TVarData(v).vPointer := @wb;
  { Delphi raises EVariantOverflowError for the byte, word and dword casts of
    this one, unlike for a boolean held directly or in a variant }
  row('boolean True by reference', v, '-1 -1 -1 255 65535 4294967295 -1', '-1 -1 -1 255 65535 4294967295 -1');
  inner := True;
  TVarData(v).vType := varVariant or varByRef;
  TVarData(v).vPointer := @TVarData(inner);
  row('variant(boolean True) by reference', v, '-1 -1 -1 255 65535 4294967295 -1', '-1 -1 -1 255 65535 4294967295 -1');
  inner := int64(300);
  row('variant(int64 300) by reference', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  inner := high(qword);
  row('variant(qword 18446744073709551615) by reference', v, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  q64 := high(qword);
  TVarData(v).vType := varQWord or varByRef;
  TVarData(v).vPointer := @q64;
  row('qword 18446744073709551615 by reference', v, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  q64 := 300;
  row('qword 300 by reference', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  inner := qword(300);
  TVarData(v).vType := varVariant or varByRef;
  TVarData(v).vPointer := @TVarData(inner);
  row('variant(qword 300) by reference', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  inner := '$FFFFFFFF';
  row('variant(string $FFFFFFFF) by reference', v, '-1 -1 -1 OVF OVF 4294967295 4294967295', '-1 -1 -1 255 65535 4294967295 4294967295');
  inner := '2147483648';
  row('variant(string 2147483648) by reference', v, 'OVF OVF OVF OVF OVF 2147483648 2147483648', 'OVF OVF OVF OVF OVF 2147483648 2147483648');
  ws := '$FFFFFFFF';
  TVarData(v).vType := varOleStr or varByRef;
  TVarData(v).vPointer := @ws;
  row('widestring $FFFFFFFF by reference', v, '-1 -1 -1 OVF OVF 4294967295 4294967295', '-1 -1 -1 255 65535 4294967295 4294967295');
  ws := '2147483648';
  row('widestring 2147483648 by reference', v, 'OVF OVF OVF OVF OVF 2147483648 2147483648', 'OVF OVF OVF OVF OVF 2147483648 2147483648');
  { Delphi raises EVariantBadVarTypeError for an ansistring or a
    unicodestring held by reference }
  s := '300';
  TVarData(v).vType := varString or varByRef;
  TVarData(v).vPointer := @s;
  row('ansistring 300 by reference', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  us := '-1';
  TVarData(v).vType := varUString or varByRef;
  TVarData(v).vPointer := @us;
  row('unicodestring -1 by reference', v, '-1 -1 -1 OVF OVF OVF -1', '-1 -1 -1 255 65535 4294967295 -1');
  i64 := 300;
  TVarData(v).vType := varInt64 or varByRef;
  TVarData(v).vPointer := @i64;
  check('message of VarAsType(int64 300 by reference, shortint)', castMessage(v, varShortInt),
    'Overflow while converting variant of type (Ref to Int64) into type (ShortInt)');
  TVarData(v).vType := varVariant or varByRef;
  { a variant referring to nothing keeps its type mismatch (Delphi reads
    through the nil pointer) }
  TVarData(v).vPointer := nil;
  check('VarAsType(variant by reference to nil, byte)', viaCast(v, 3), 'EVariantError');
  check('byte := variant by reference to nil', viaAssign(v, 3), 'EVariantError');
  TVarData(v).vType := varEmpty;

  o := longint(-1);
  check('byte := olevariant(longint -1)', viaOleByte(o), '255');
  check('dword := olevariant(longint -1)', viaOleDword(o), '4294967295');
  o := longint(300);
  check('byte := olevariant(longint 300)', viaOleByte(o), '44');
  check('dword := olevariant(longint 300)', viaOleDword(o), '300');
  o := int64(4294967296);
  check('byte := olevariant(int64 4294967296)', viaOleByte(o), 'OVF');
  check('dword := olevariant(int64 4294967296)', viaOleDword(o), '0');

  v := int64(4294967296);
  check('message of VarAsType(int64 4294967296, byte)', castMessage(v, varByte),
    'Overflow while converting variant of type (Int64) into type (Byte)');
  check('message of longint := int64 4294967296', assignMessage(v),
    'Overflow while converting variant of type (Int64) into type (Integer)');
  check('VarCast(int64 4294967296, word)', viaVarCast(v, varWord), 'OVF');
  check('terror := int64 4294967296', viaError(v), 'OVF');
  { nativeint and nativeuint convert as the integer types of their size }
{$if sizeof(PtrInt)=8}
  check('nativeint := int64 4294967296', viaNativeInt(v), '4294967296');
  check('nativeuint := int64 4294967296', viaNativeUInt(v), '4294967296');
{$elseif sizeof(PtrInt)=4}
  check('nativeint := int64 4294967296', viaNativeInt(v), 'OVF');
  check('nativeuint := int64 4294967296', viaNativeUInt(v), '0');
{$endif}
  v := longint(300);
  check('VarCast(longint 300, word)', viaVarCast(v, varWord), '300');
  v := longint(-5);
  check('terror := longint -5', viaError(v), '-5');
{$if sizeof(PtrInt)=8}
  check('nativeuint := longint -5', viaNativeUInt(v), '18446744073709551611');
{$elseif sizeof(PtrInt)=4}
  check('nativeuint := longint -5', viaNativeUInt(v), '4294967291');
{$endif}
  checkSetMasks;

  recording := TRecordingType.Create;
  { made by hand, so that no conversion asks the type for anything }
  TVarData(customVar).vType := recording.VarType;
  TVarData(customVar).vInt64 := 7;
  TVarData(custom).vType := recording.VarType;
  TVarData(custom).vInt64 := 7;
  check('custom to longint via variant asks', askedFor(customVar, false), IntToStr(varInteger) + ' gives 7');
  check('custom to dword via variant asks', askedFor(customVar, true), IntToStr(varInt64) + ' gives 7');
  check('custom to dword via olevariant asks', askedForOle(custom), IntToStr(varInt64) + ' gives 7');
  { not a Delphi row: see viaSetMask }
  asked := varEmpty;
  check('custom to a set gives', viaSetMask(customVar), '7');
  check('custom to a set asks', IntToStr(asked), IntToStr(varInteger));
  { not a Delphi row: a type answering a longint request with an int64, whose
    answer is checked as any value is }
  answerBig := true;
  check('longint := custom answering int64 4294967296', viaAssign(customVar, 2), 'OVF');
  answerBig := false;
  customVar := Unassigned;
  custom := Unassigned;
  recording.Free;

  { an IDispatch is converted by its default value, with that value's results
    (not measured against Delphi) }
  disp := TValueDispatch.Create(True);
  v := disp;
  row('dispatch(boolean True)', v, '-1 -1 -1 255 65535 4294967295 -1', '-1 -1 -1 255 65535 4294967295 -1');
  disp := TValueDispatch.Create(high(qword));
  v := disp;
  row('dispatch(qword 18446744073709551615)', v, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  TVarData(dref).vType := varDispatch or varByRef;
  TVarData(dref).vPointer := @TVarData(v).vDispatch;
  row('dispatch(qword 18446744073709551615) by reference', dref, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  TVarData(dref).vType := varVariant or varByRef;
  TVarData(dref).vPointer := @TVarData(v);
  row('variant(dispatch(qword 18446744073709551615)) by reference', dref, 'OVF OVF OVF OVF OVF OVF -1', 'OVF OVF OVF OVF OVF 4294967295 -1');
  TVarData(dref).vType := varEmpty;
  disp := TValueDispatch.Create(longint(300));
  v := disp;
  row('dispatch(longint 300)', v, 'OVF 300 300 OVF 300 300 300', '44 300 300 44 300 300 300');
  { a default value that is itself an IDispatch is refused, as VarUtils refuses it }
  disp := TValueDispatch.Create(v);
  v := disp;
  check('VarAsType(dispatch(dispatch), byte)', viaCast(v, 3), 'EVariantError');
  check('byte := dispatch(dispatch)', viaAssign(v, 3), 'EVariantError');
  v := Unassigned;
  disp := nil;

  if errors <> 0 then
    begin
      writeln(errors, ' failure(s)');
      halt(1);
    end;
  writeln('ok');
end.
