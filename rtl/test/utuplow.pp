unit utuplow;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils;

Implementation

uses punit, utrtl;

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
begin
  writeln('error ',nr);
  write('  Got: ');
  writestring(s1);
  write('  Expected: ');
  writestring(s2);
  halt(nr);
end;

procedure error(const s1,s2: unicodestring; nr: longint);
begin
  writeln('error ',nr);
  write('  Got: ');
  writestring(s1);
  write('  Expected: ');
  writestring(s2);
  halt(nr);
end;



Function testuplowansi : string;

  const
    str = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'aAbBcCdD'#0'fF';
    upperstr = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'AABBCCDD'#0'FF';
    lowerstr = #1#2#0#3#128#129#130#131#132#133#134#135#136#137#138#139'aabbccdd'#0'ff';
  var
    s1, s2: ansistring;
  begin
    Result:='';
    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=uppercase(s1);
    if not AssertEquals('error 1',upperstr,S1) then exit;
    if not AssertEquals('error 2',str,S2) then exit;
    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=lowercase(s1);
    if not AssertEquals('Error 3',lowerstr,S1) then exit;
    if not AssertEquals('Error 4',str,S2) then exit;
 end;


Function testuplowwide : String;
  const
    str = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'AABBCCDD'#0000'FF';
    upperstr = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'AABBCCDD'#0000'FF';
    lowerstr = #$0001#$0002#$0000#$0003#0128#0129#0130#0131#0132#0133#0134#0135#0136#0137#0138#0139'aabbccdd'#0000'ff';
  var
    s1, s2: unicodestring;
  begin
    Result:='';
    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=uppercase(s1);
    if not AssertEquals('error 5',upperstr,S1) then exit;
    if not AssertEquals('error 6',str,S2) then exit;

    s1:=str;
    uniquestring(s1);
    s2:=s1;
    s1:=lowercase(s1);
    if not AssertEquals('Error 7',lowerstr,S1) then exit;
    if not AssertEquals('Error 8',str,S2) then exit;
 end;

begin
  SysUtilsTest('testuplowansi',@testuplowansi);
  SysUtilsTest('testuplowwide',@testuplowwide);
end.
