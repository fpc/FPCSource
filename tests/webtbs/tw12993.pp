{$codepage cp866}

{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
uses
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet;
{$endif}
{$endif}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif}
var
  s: ansistring;
  ws, ws3: widestring;
begin
    SetMultiByteConversionCodePage(866);
    // must not be converted
    s := '£';
    if (length(s)<>1) or
       (s[1]<> #163) then
      halt(1);

    ws := '££';
    writeln(ws);
    s:=ws;
    ws:=s;

    ws3 := '£';
    if ws3[1]<>ws[1] then
      halt(2);
    writeln(ws3);
    s:=ws3;
    ws3:=s;

    delete(ws,1,1);

    if (ws<>ws3) then
      halt(3);
end.
