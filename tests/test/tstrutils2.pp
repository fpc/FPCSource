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
    not AnsiStartsText('', '')
    and not AnsiStartsText('', 'ab')
    and not AnsiStartsText('ab', '')
    and AnsiStartsText('abc', 'abc')
    and not AnsiStartsText('abc', 'def')
    and AnsiStartsText('abc', 'abcedfg')
    and not AnsiStartsText('abc', 'ab')
    and AnsiStartsText('áÉíç', 'áéíÇ')
    and AnsiStartsText('áé', 'áÉíç')
    and not AnsiStartsText('áÉíç', 'Áé')
    and not AnsiStartsText('ÁÉíç', 'áéio')
    // AnsiEndsText
    and AnsiEndsText('', '')
    and AnsiEndsText('', 'ab')
    and not AnsiEndsText('ab', '')
    and AnsiEndsText('abc', 'abc')
    and not AnsiEndsText('abc', 'def')
    and AnsiEndsText('dfg', 'abcedfg')
    and not AnsiEndsText('dfg', 'df')
    and AnsiEndsText('áÉíç', 'Áéíç')
    and AnsiEndsText('áé', 'íçáÉ')
    and not AnsiEndsText('áÉíç', 'áé')
    and not AnsiEndsText('íçÁÉ', 'ioÁé');
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
      halt(1);
    end;
end.
