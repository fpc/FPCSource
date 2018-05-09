{$ifdef go32v2}
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
  ts850 = type AnsiString(850);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  x : ts850;
  i : Integer;
  ua : array[0..7] of UnicodeChar;
  uc : UnicodeChar;
  us : UnicodeString;
begin
  x := 'abc'#$00A9#$00AE'123';
  ua := x;
  us := x;
  for i := 1 to Length(us) do
    begin
      uc := us[i];
      if (uc <> ua[i-1]) then begin
        writeln(i);
        doerror(2);
      end;
    end;

  WriteLn('Ok');
end.
