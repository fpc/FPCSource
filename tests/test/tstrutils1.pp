program tstrutils1;

// tests MBCS compatibility of strutils ansistartsstr and -endsstr.

{$mode objfpc}
{$h+}
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}

uses
  {SysUtils, }
{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
{$else USE_INTERNAL_UNICODE}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
{$endif def USE_INTERNAL_UNICODE}
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
    TestValue( AnsiStartsStr('', ''))
    and TestValue(AnsiStartsStr('', 'ab'))
    and TestValue(not AnsiStartsStr('ab', ''))
    and TestValue(AnsiStartsStr('abc', 'abc'))
    and TestValue(not AnsiStartsStr('abc', 'def'))
    and TestValue(AnsiStartsStr('abc', 'abcedfg'))
    and TestValue(not AnsiStartsStr('abc', 'ab'))
    and TestValue(AnsiStartsStr('áéíç', 'áéíç'))
    and TestValue(AnsiStartsStr('áé', 'áéíç'))
    and TestValue(not AnsiStartsStr('áéíç', 'áé'))
    and TestValue(not AnsiStartsStr('áéíç', 'áéio'))
    // AnsiEndsStr
    and TestValue(AnsiEndsStr('', ''))
    and TestValue(AnsiEndsStr('', 'ab'))
    and TestValue(not AnsiEndsStr('ab', ''))
    and TestValue(AnsiEndsStr('abc', 'abc'))
    and TestValue(not AnsiEndsStr('abc', 'def'))
    and TestValue(AnsiEndsStr('dfg', 'abcedfg'))
    and TestValue(not AnsiEndsStr('dfg', 'df'))
    and TestValue(AnsiEndsStr('áéíç', 'áéíç'))
    and TestValue(AnsiEndsStr('áé', 'íçáé'))
    and TestValue(not AnsiEndsStr('áéíç', 'áé'))
    and TestValue(not AnsiEndsStr('íçáé', 'ioáé'));
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
