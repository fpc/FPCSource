program tuplow;

{$mode objfpc}
{$h+}

uses
  SysUtils;

procedure writestring(const s: ansistring);
  var
    i: longint;
  begin
    for i:=1 to length(s) do
      if (s[i]<=#32) or (s[i]>=#127) then
        write('#',ord(s[i]),' ')
      else
        write(s[i],' ');
    writeln;
  end;

procedure writestring(const s: unicodestring);
  var
    i: longint;
  begin
    for i:=1 to length(s) do
      if (s[i]<=#0032) or (s[i]>=#0127) then
        write('#',ord(s[i]),' ')
      else
        write(s[i],' ');
    writeln;
  end;

procedure error(const s1,s2: ansistring; nr: longint);
var
  i: longint;
begin
  writeln('error ',nr);
  write('  Got: ');
  writestring(s1);
  write('  Expected: ');
  writestring(s2);
  halt(nr);
end;

procedure error(const s1,s2: unicodestring; nr: longint);
var
  i: longint;
begin
  writeln('error ',nr);
  write('  Got: ');
  writestring(s1);
  write('  Expected: ');
  writestring(s2);
  halt(nr);
end;



procedure testuplowansi;
  const
    str = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'aAbBcCdD'#0'fF';
    upperstr = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'AABBCCDD'#0'FF';
    lowerstr = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'aabbccdd'#0'ff';
  var
    s1, s2: ansistring;
  begin
    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=uppercase(s1);
    if s1<>upperstr then
      error(s1,upperstr,1);
    if s2<>str then
      error(s2,str,2);

    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=lowercase(s1);
    if s1<>lowerstr then
      error(s1,lowerstr,3);
    if s2<>str then
      error(s2,str,4);
 end;


procedure testuplowwide;
  const
    str = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'AABBCCDD'#0000'FF';
    upperstr = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'AABBCCDD'#0000'FF';
    lowerstr = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'aabbccdd'#0000'ff';
  var
    s1, s2: unicodestring;
  begin
    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=uppercase(s1);
    if s1<>upperstr then
      error(s1,upperstr,5);
    if s2<>str then
      error(s2,str,6);

    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=lowercase(s1);
    if s1<>lowerstr then
      error(s1,lowerstr,7);
    if s2<>str then
      error(s2,str,8);
 end;

begin
  testuplowansi;
  testuplowwide;
end.
