{$mode objfpc}{$H+}


uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  Classes, SysUtils;

function localUnicodeToUTF8(u: cardinal; Buf: PChar): integer;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToUTF8: invalid unicode: '+IntToStr(u));
  end;

begin
  case u of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(u));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (u shr 6)));
        Buf[1]:=char(byte($80 or (u and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (u shr 12)));
        Buf[1]:=char(byte((u shr 6) and $3f) or $80);
        Buf[2]:=char(byte(u and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (u shr 18)));
        Buf[1]:=char(byte((u shr 12) and $3f) or $80);
        Buf[2]:=char(byte((u shr 6) and $3f) or $80);
        Buf[3]:=char(byte(u and $3f) or $80);
      end;
  else
    RaiseInvalidUnicode;
  end;
end;

function localUnicodeToUTF8(u: cardinal): shortstring;
begin
  Result[0]:=chr(localUnicodeToUTF8(u,@Result[1]));
end;


function localUnicodeToUTF16(u: cardinal): widestring;
begin
  // u should be <= $10FFFF to fit into UTF-16

  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=widechar(u)
  else
    Result:=widechar($D800+((u - $10000) shr 10))+widechar($DC00+((u - $10000) and $3ff));
end;


function UnicodeToCESU8(u: cardinal; Buf: PChar): integer;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToCESU8: invalid unicode: '+IntToStr(u));
  end;

var
  st: widestring;
begin
  case u of
    0..$ffff:
      begin
        Result:=localUnicodeToUTF8(u,Buf);
      end;
    $10000..$10ffff:
      begin
        st := localUnicodeToUTF16(u);

        Result:=6;
        Buf[0]:=char(byte($e0 or (ord(st[1]) shr 12)));
        Buf[1]:=char(byte((ord(st[1]) shr 6) and $3f) or $80);
        Buf[2]:=char(byte(ord(st[1]) and $3f) or $80);
        Buf[3]:=char(byte($e0 or (ord(st[2]) shr 12)));
        Buf[4]:=char(byte((ord(st[2]) shr 6) and $3f) or $80);
        Buf[5]:=char(byte(ord(st[2]) and $3f) or $80);
     end;
  else
    RaiseInvalidUnicode;
  end;
end;

function UnicodeToCESU8(u: cardinal): utf8string;
begin
  setlength(result,1000);
  setlength(result,UnicodeToCESU8(u,@Result[1]));
end;

procedure dotest;
var
  s1,s2: utf8string;
  w1,w2: unicodestring;
  s3,s4: utf8string;
  i: longint;
begin
  s1 := localUnicodeToUTF8 ($10300);
  s2 := UnicodeToCESU8 ($10300);
  setlength(w1,20);
  setlength(w2,20);
  // -1 because UTF8ToUnicode returns a null-terminated string
  setlength(w1,UTF8ToUnicode(punicodechar(@w1[1]),length(w1),pchar(s1),Length(s1))-1);
  setlength(w2,UTF8ToUnicode(punicodechar(@w2[1]),length(w2),pchar(s2),Length(s2))-1);
(*
  writeln('len: ',length(w1),' - "',w1,'"');
  write('  ');
  for i:= 1 to length(w1) do
    write('#$',hexstr(ord(w1[i]),4));
  writeln;
  writeln('len: ',length(w2),' - "',w2,'"');
  write('  ');
  for i:= 1 to length(w2) do
    write('#$',hexstr(ord(w2[i]),4));
  writeln;
  writeln;
*)
  
  setlength(s3,20);
  setlength(s4,20);
  // -1 because UnicodeToUTF8 returns a null-terminated string
  setlength(s3,UnicodeToUTF8(@s3[1],length(s3),punicodechar(@w1[1]),length(w1))-1);
  setlength(s4,UnicodeToUTF8(@s4[1],length(s4),punicodechar(@w2[1]),length(w2))-1);
  
  if (s3<>s1) or
     { invalid: CESU-8 }
     (w2<>'??') or
     (s4<>'??') then
    begin
      writeln('len: ',length(s1),' - "',s1,'"');
      write('  ');
      for i:= 1 to length(s1) do
        write('#$',hexstr(ord(s1[i]),2));
      writeln;
      writeln('len: ',length(s2),' - "',s2,'"');
      write('  ');
      for i:= 1 to length(s2) do
        write('#$',hexstr(ord(s2[i]),2));
      writeln;
      writeln('len: ',length(s3),' - "',s3,'"');
      write('  ');
      for i:= 1 to length(s3) do
        write('#$',hexstr(ord(s3[i]),2));
      writeln;
      writeln('len: ',length(s4),' - "',s4,'"');
      write('  ');
      for i:= 1 to length(s4) do
        write('#$',hexstr(ord(s4[i]),2));
      writeln;
      halt(1);
    end;
end;

begin
  dotest;
end.

