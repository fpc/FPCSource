program tstrutils1;

// tests MBCS compatibility of strutils ansistartsstr and -endsstr.

{$mode objfpc}
{$h+}

uses
  {SysUtils, }
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
  StrUtils;

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
    // AnsiStartsStr
    not AnsiStartsStr('', '')
    and not AnsiStartsStr('', 'ab')
    and not AnsiStartsStr('ab', '')
    and AnsiStartsStr('abc', 'abc')
    and not AnsiStartsStr('abc', 'def')
    and AnsiStartsStr('abc', 'abcedfg')
    and not AnsiStartsStr('abc', 'ab')
    and AnsiStartsStr('áéíç', 'áéíç')
    and AnsiStartsStr('áé', 'áéíç')
    and not AnsiStartsStr('áéíç', 'áé')
    and not AnsiStartsStr('áéíç', 'áéio')
    // AnsiEndsStr
    and AnsiEndsStr('', '')
    and AnsiEndsStr('', 'ab')
    and not AnsiEndsStr('ab', '')
    and AnsiEndsStr('abc', 'abc')
    and not AnsiEndsStr('abc', 'def')
    and AnsiEndsStr('dfg', 'abcedfg')
    and not AnsiEndsStr('dfg', 'df')
    and AnsiEndsStr('áéíç', 'áéíç')
    and AnsiEndsStr('áé', 'íçáé')
    and not AnsiEndsStr('áéíç', 'áé')
    and not AnsiEndsStr('íçáé', 'ioáé');
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
