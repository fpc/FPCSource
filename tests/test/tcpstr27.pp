{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}

{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}
{$else def USE_INTERNAL_UNICODE}
uses
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
 { The unit strings is not really used here,
   but simpifies the conditional construction
   for fpwidestring and unicodeducet use }
  strings;
{$endif def USE_INTERNAL_UNICODE}


var
  data: array[0..3] of widechar;

procedure error(code: longint);
begin
  writeln('Error code: ', code);
  Halt(code);
end;

procedure check(const s: ansistring; code: longint);
var
  us: unicodestring;
begin
  if StringCodePage(s) <> DefaultSystemCodePage then begin
    writeln('Incorrect string code page: ', StringCodePage(s), '. Expected: ', DefaultSystemCodePage, '.');
    error(code);
  end;
  us:=s;
  if (Length(us) = 3) and (us[1] = data[0]) and (us[2] = data[1]) and (us[3] = data[2]) then
    exit;
  writeln('Incorrect string: ', us);
  error(code);
end;

procedure test(cp: TSystemCodePage; code: longint);
var
  s: ansistring;
begin
  writeln('Testing default code page ', cp, '...');
  DefaultSystemCodePage:=cp;
  // Test fpc_unicodestr_to_ansistr
  s:=unicodestring(data);
  check(s, code + 1);
  // Test fpc_widestr_to_ansistr
  s:=widestring(data);
  check(s, code + 2);
  // Test fpc_widechararray_to_ansistr
  s:=data;
  check(s, code + 3);
  // Test fpc_pwidechar_to_ansistr
  s:=PWideChar(data);
  check(s, code + 4);
  // Test fpc_uchar_to_ansistr
  s:=data[0] + data[1] + data[2];
  check(s, code + 5);
end;

begin
  // Cyrillic АБВ, null-terminated
  data[0]:=widechar($410);
  data[1]:=widechar($411);
  data[2]:=widechar($412);
  data[3]:=#0;
  writeln('Original string: ', unicodestring(data));
  test(CP_UTF8, 0);
  test(1251, 10);
  writeln('Test OK.');
end.
