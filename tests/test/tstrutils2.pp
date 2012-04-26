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

function TestValue(const Value: Boolean): Boolean;
begin
  Result := Value;
  if not Value then
    WriteLn('Failed: ', ResultCounter);
  Inc(ResultCounter);
end;

function TestOK: Boolean;
begin
  TestOK :=
    // AnsiStartsText
    TestValue(not AnsiStartsText('', ''))
    and TestValue(not AnsiStartsText('', 'ab'))
    and TestValue(not AnsiStartsText('ab', ''))
    and TestValue(AnsiStartsText('abc', 'abc'))
    and TestValue(not AnsiStartsText('abc', 'def'))
    and TestValue(AnsiStartsText('abc', 'abcedfg'))
    and TestValue(not AnsiStartsText('abc', 'ab'))
    and TestValue(AnsiStartsText('áÉíç', 'áéíÇ'))
    and TestValue(AnsiStartsText('áé', 'áÉíç'))
    and TestValue(not AnsiStartsText('áÉíç', 'Áé'))
    and TestValue(not AnsiStartsText('ÁÉíç', 'áéio'))
    // AnsiEndsText
    and TestValue(AnsiEndsText('', ''))
    and TestValue(AnsiEndsText('', 'ab'))
    and TestValue(not AnsiEndsText('ab', ''))
    and TestValue(AnsiEndsText('abc', 'abc'))
    and TestValue(not AnsiEndsText('abc', 'def'))
    and TestValue(AnsiEndsText('dfg', 'abcedfg'))
    and TestValue(not AnsiEndsText('dfg', 'df'))
    and TestValue(AnsiEndsText('áÉíç', 'Áéíç'))
    and TestValue(AnsiEndsText('áé', 'íçáÉ'))
    and TestValue(not AnsiEndsText('áÉíç', 'áé'))
    and TestValue(not AnsiEndsText('íçÁÉ', 'ioÁé'));
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
