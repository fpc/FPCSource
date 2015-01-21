{ this file is stored in utf8, but we don't tell the compiler so that the string
  constant gets code page 0/CP_ACP; this test is to make sure that
  fpc_ansistr_to_widechararray() translates CP_ACP to the actual value of
  DefaultSystemCodePage before calling widestringmanager.ansi2widemoveproc
}

{$ifdef unix}
uses
  cwstring;
{$endif}
{$r+}
var
  u8: ansistring;
  a: array[0..10] of unicodechar;
  u16: unicodestring;
  i: longint;
begin
  DefaultSystemCodePage:=CP_UTF8;
  u8:='èà';
  a:=u8;
  u16:=unicodestring(u8);
  for i:=0 to 1 do
    begin
      writeln('u16[',i-low(a)+low(u16),'] = $',hexstr(ord(u16[i-low(a)+low(u16)]),2));
      writeln('a[',i,'] = $',hexstr(ord(a[i]),2));
      if u16[i-low(a)+low(u16)]<>a[i] then
        halt(i+1);
    end;
  if a[2]<>#0 then
    halt(3);
end.
