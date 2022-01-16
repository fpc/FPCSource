program tw29585;
{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ELSE}
{$APPTYPE Console}
{$ENDIF}

uses
  {$ifndef FPC}Windows,{$endif}Sysutils;

{$IFNDEF FPC}
type
  tsystemcodepage = word;
{$ENDIF}

Type
  tstr1251 = type ansistring(1251);

const
  utf8data: array[0..10] of ansichar = #$C3#$A9#$C2#$BA#$C3#$AE#$C5#$93#$E2#$88#$82;
  utf8data_in_utf16: unicodestring = #$00E9#$00BA#$00EE#$0153#$2202;

  invalidutf8data: array[0..3] of ansichar = #$80#$81#$82#$83;
  invalidutf8data_utf_16a: unicodestring = '????';
  invalidutf8data_utf_16b: unicodestring = #$fffd#$fffd#$fffd#$fffd;


procedure error(l: longint; const u: unicodestring);
  var
    i: longint;
  begin
    writeln('error for test ',l);
    write('result: ');
    for i:=low(u) to high(u) do
      write('#$',inttohex(ord(u[i]),2));
    writeln;
    halt(l);
  end;


procedure initarray(p: pbyte; const data: array of ansichar);
  var
    i: longint;
  begin
    for i:=low(data) to high(data) do
      p[i]:=ord(data[i]);
  end;


procedure initstr(var s: rawbytestring; cp: tsystemcodepage; const data: array of ansichar); overload;
  var
    i: longint;
  begin
    setlength(s,length(data));
    setcodepage(s,cp,false);
    for i:=low(data) to high(data) do
      s[i+1]:=data[i];
  end;


procedure initstr(var s: shortstring; const data: array of ansichar); overload;
  var
    i: longint;
  begin
    setlength(s,length(data));
    for i:=low(data) to high(data) do
      s[i+1]:=data[i];
  end;


procedure testvalidutf8;
  var
    s1251: tstr1251;
    rs: rawbytestring;
    utf8: utf8string;
    s: ansistring;
    ss: shortstring;
    ba: array[low(utf8data)..high(utf8data)] of byte;
    bc: array[low(utf8data)..high(utf8data)] of ansichar;
    bcc: array[low(utf8data)..high(utf8data)+1] of ansichar;
    w: unicodestring;
  begin
    initstr(rawbytestring(s1251),1251,utf8data);
    w:=UTF8ToString(s1251);
    if w<>utf8data_in_utf16 then
      error(1,w);

    initstr(rs,0,utf8data);
    w:=UTF8ToString(rs);
    if w<>utf8data_in_utf16 then
      error(2,w);

    initstr(rawbytestring(utf8),CP_UTF8,utf8data);
    w:=UTF8ToString(utf8);
    if w<>utf8data_in_utf16 then
      error(3,w);

    initstr(rawbytestring(s),defaultsystemcodepage,utf8data);
    w:=UTF8ToString(s);
    if w<>utf8data_in_utf16 then
      error(4,w);

    initstr(ss,utf8data);
    w:=UTF8ToString(ss);
    if w<>utf8data_in_utf16 then
      error(5,w);

    initarray(@bcc[0],utf8data);
    bcc[high(bcc)]:=#0;
    w:=UTF8ToString(@bcc[0]);
    if w<>utf8data_in_utf16 then
      error(6,w);

{$ifndef cpujvm}
    initarray(@ba[0],utf8data);
    w:=UTF8ToString(ba);
    if w<>utf8data_in_utf16 then
      error(7,w);

    initarray(@bc[0],utf8data);
    w:=UTF8ToString(bc);
    if w<>utf8data_in_utf16 then
      error(8,w);
{$endif not cpujvm}
  end;


procedure testinvalidutf8;
  var
    s1251: tstr1251;
    rs: rawbytestring;
    utf8: utf8string;
    s: ansistring;
    ss: shortstring;
    ba: array[low(invalidutf8data)..high(invalidutf8data)] of byte;
    bc: array[low(invalidutf8data)..high(invalidutf8data)] of ansichar;
    bcc: array[low(invalidutf8data)..high(invalidutf8data)+1] of ansichar;
    w: unicodestring;
  begin
    initstr(rawbytestring(s1251),1251,invalidutf8data);
    w:=UTF8ToString(s1251);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(11,w);

    initstr(rs,0,invalidutf8data);
    w:=UTF8ToString(rs);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(12,w);

    initstr(rawbytestring(utf8),CP_UTF8,invalidutf8data);
    w:=UTF8ToString(utf8);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(13,w);

    initstr(rawbytestring(s),defaultsystemcodepage,invalidutf8data);
    w:=UTF8ToString(s);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(14,w);

    initstr(ss,invalidutf8data);
    w:=UTF8ToString(ss);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(15,w);

    initarray(@bcc[0],invalidutf8data);
    bcc[high(bcc)]:=#0;
    w:=UTF8ToString(@bcc[0]);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(16,w);

{$ifndef cpujvm}
    initarray(@ba[0],invalidutf8data);
    w:=UTF8ToString(ba);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(17,w);

    initarray(@bc[0],invalidutf8data);
    w:=UTF8ToString(bc);
    if (w<>invalidutf8data_utf_16a) and
       (w<>invalidutf8data_utf_16b) then
      error(18,w);
{$endif not cpujvm}
  end;


begin
  testvalidutf8;
  testinvalidutf8;
end.
