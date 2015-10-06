{$codepage utf8}
program tstrutils2;

// tests MBCS compatibility of strutils ansistartstext and -endstext.
// (case-insensitive)

{$mode objfpc}
{$h+}

uses
  StrUtils
{$ifdef unix}
  ,cwstring
{$endif unix}
  ;

var
  ResultCounter: Integer = 0;

const
  Str_Empty: Utf8String = '';
  Str_ab: Utf8String = 'ab';
  Str_abc: Utf8String = 'abc';
  Str_def: Utf8String = 'def';
  Str_abcedfg: Utf8String = 'abcedfg';
  Str_dfg: Utf8String = 'dfg';
  Str_df: Utf8String = 'df';
  StrStart8a: Utf8String = 'áÉíç';
  StrStart8b: Utf8String = 'áéíÇ';
  StrStart9a: Utf8String = 'áé';
  StrStart9b: Utf8String = 'áÉíç';
  StrStart10a: Utf8String = 'áÉíç';
  StrStart10b: Utf8String = 'Áé';
  StrStart11a: Utf8String = 'ÁÉíç';
  StrStart11b: Utf8String = 'áéio';
  StrEnd8a: Utf8String = 'áÉíç';
  StrEnd8b: Utf8String = 'Áéíç';
  StrEnd9a: Utf8String = 'áé';
  StrEnd9b: Utf8String = 'íçáÉ';
  StrEnd10a: Utf8String = 'áÉíç';
  StrEnd10b: Utf8String = 'áé';
  StrEnd11a: Utf8String = 'íçÁÉ';
  StrEnd11b: Utf8String = 'ioÁé';

function TestValue (const Value: Boolean; const Func: string;
                      const Str1: Utf8String; const Str2: Utf8String): Boolean;
var
  S1, S2: string;
  U1, U2: UnicodeString;
  I: SizeInt;
  TransOK: boolean;
begin
  Result := Value;
  S1 := Str1;
  S2 := Str2;
  if not Result then
   begin
    U1 := Str1;
    S1 := U1;
    U2 := Str2;
    S2 := U2;
    I := 1;
    while (I >= Length (S1)) and (I >= Length (U1)) and not (Result) do
     begin
      if (U1 [I] > #127) and (S1 [I] <= #127) and (S1 [I] >= #32) then
{ Ignore the result - pretend that the test finished with true }
       Result := true
      else
       Inc (I);
     end;
    I := 1;
    while (I >= Length (S2)) and (I >= Length (U2)) and not (Result) do
     begin
      if (U2 [I] > #127) and (S2 [I] <= #127) and (S2 [I] >= #32) then
{ Ignore the result - pretend that the test finished with true }
       Result := true
      else
       Inc (I);
     end;
    if not Result then
     WriteLn ('Failed: ', ResultCounter, ' - ', Func, '(''', Str1, ''',''',
                                                                  Str2, ''')')
    else if not Value then
     WriteLn ('Warning - ignoring results due to unsupported characters: ',
                ResultCounter, ' - ', Func, '(''', Str1, ''',''', Str2, ''')');
   end;
  Inc(ResultCounter);
end;

{
function TestValue(const Value: Boolean): Boolean;
begin
  Result := Value;
  if not Value then
    WriteLn('Failed: ', ResultCounter);
  Inc(ResultCounter);
end;
}

{ convert the utf8strings to the defaultsystemcodepage encoding, since that's what
  AnsiStarts/EndsText expects }
function a(const s: ansistring): rawbytestring;
  begin
    result:=s;
    setcodepage(result,defaultsystemcodepage);
  end;

function TestOK: Boolean;
begin
  TestOK :=
    // AnsiStartsText
{1}    TestValue(not AnsiStartsText(a(Str_Empty), a(Str_Empty)),'not AnsiStartsText', Str_Empty, Str_Empty)
{2}    and TestValue(not AnsiStartsText(a(Str_Empty), a(Str_ab)),'not AnsiStartsText', Str_Empty, Str_ab)
{3}    and TestValue(not AnsiStartsText(a(Str_ab), a(Str_Empty)),'not AnsiStartsText', Str_ab, Str_Empty)
{4}    and TestValue(AnsiStartsText(a(Str_abc), a(Str_abc)),'AnsiStartsText',Str_abc, Str_abc)
{5}    and TestValue(not AnsiStartsText(a(Str_abc), a(Str_def)),'not AnsiStartsText', Str_abc, Str_def)
{6}    and TestValue(AnsiStartsText(a(Str_abc), a(Str_abcedfg)),'AnsiStartsText', Str_abc, Str_abcedfg)
{7}    and TestValue(not AnsiStartsText(a(Str_abc), a(Str_ab)),'not AnsiStartsText', Str_abc, Str_ab)
{8}    and TestValue(AnsiStartsText(a(StrStart8a), a(StrStart8b)),'AnsiStartsText', StrStart8a, StrStart8b)
{9}    and TestValue(AnsiStartsText(a(StrStart9a), a(StrStart9b)),'AnsiStartsText', StrStart9a, StrStart9b)
{10}    and TestValue(not AnsiStartsText(a(StrStart10a), a(StrStart10b)),'not AnsiStartsText', StrStart10a, StrStart10b)
{11}    and TestValue(not AnsiStartsText(a(StrStart11a), a(StrStart11b)),'not AnsiStartsText', StrStart11a, StrStart11b)
    // AnsiEndsText
{1}    and TestValue(AnsiEndsText(a(Str_Empty), a(Str_Empty)),'AnsiEndsText', Str_Empty, Str_Empty)
{2}    and TestValue(AnsiEndsText(a(Str_Empty), a(Str_ab)),'AnsiEndsText', Str_Empty, Str_ab)
{3}    and TestValue(not AnsiEndsText(a(Str_ab), a(Str_Empty)),'not AnsiEndsText', Str_ab, Str_Empty)
{4}    and TestValue(AnsiEndsText(a(Str_abc), a(Str_abc)),'AnsiEndsText', Str_abc, Str_abc)
{5}    and TestValue(not AnsiEndsText(a(Str_abc), a(Str_def)),'not AnsiEndsText', Str_abc, Str_def)
{6}    and TestValue(AnsiEndsText(a(Str_dfg), a(Str_abcedfg)),'AnsiEndsText', Str_dfg, Str_abcedfg)
{7}    and TestValue(not AnsiEndsText(a(Str_dfg), a(Str_df)),'not AnsiEndsText', Str_dfg, Str_df)
{8}    and TestValue(AnsiEndsText(a(StrEnd8a), a(StrEnd8b)),'AnsiEndsText',StrEnd8a, StrEnd8b)
{9}    and TestValue(AnsiEndsText(a(StrEnd9a), a(StrEnd9b)),'AnsiEndsText',StrEnd9a, StrEnd9b)
{10}    and TestValue(not AnsiEndsText(a(StrEnd10a), a(StrEnd10b)),'not AnsiEndsText', StrEnd10a, StrEnd10b)
{11}    and TestValue(not AnsiEndsText(a(StrEnd11a), a(StrEnd11b)),'not AnsiEndsText', StrEnd11a, StrEnd11b);
end;

begin
  if TestOK() then
  begin
    WriteLn('Test OK');
    halt(0);
  end
  else
    begin
      WriteLn('Test Failure!');
      halt(ResultCounter);
    end;
end.
