{$apptype console}
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
{$endif}
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
  ts866 = type AnsiString(866);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  s : ts866;
  i : Integer;
  ss : ShortString;
begin
  s := '123'#196#200#250;
  ss := s;
  if (Length(s) <> Length(ss)) then
    doerror(1);
  for i := 1 to Length(s) do
    begin
      if (Byte(ss[i]) <> Byte(s[i])) then
        doerror(2)
    end;

  WriteLn('Ok');
end.
