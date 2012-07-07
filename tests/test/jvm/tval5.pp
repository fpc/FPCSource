program tval5;

{$mode objfpc}

{$ifndef cpujvm}
uses
  sysutils;
{$endif}

procedure testcard;
const
  h = 1;
  hexch : array[0..15] of char='0123456789ABCDEF';
var
  c: cardinal;
  l: longint;
  s: shortstring;
  b, b2: byte;
  ch, ch2: char;
{$ifdef cpu64}
  caught: boolean;
{$endif cpu64}
begin
  s:='$0fffffff';
  for b := low(hexch) to high(hexch) do
    begin
      s[2]:=hexch[b];
      val(s,c,l);
      if (l<>0) then
        halt(b+h);
    end;
    
  s:='$fffffff0';
  for b := low(hexch) to high(hexch) do
    begin
      s[length(s)]:=hexch[b];
      val(s,c,l);
      if (l<>0) then
        halt(b+16+h);
    end;

  setlength(s,10);
  s[1]:='$';
  for b2:= 1 to high(hexch) do
    begin
      for b := 2 to length(s)-1 do
        s[b]:=hexch[b2];
      for b := low(hexch) to high(hexch) do
        begin
          s[length(s)]:=hexch[b];
{$ifdef cpu64}
{$r+}
          try
            caught:=false;
{$endif cpu64}
            val(s,c,l);
{$ifdef cpu64}
          except on e : jlthrowable do
            caught:=true;
          end;
          if not caught then
{$else cpu64}
          if (l=0) then
{$endif}
            halt(b2+32+h);
        end;
    end;

  s:='0294967295';
  for ch := '0' to '4' do
    begin
      s[1]:=ch;
      val(s,c,l);
      if (l<>0) then
        halt(ord(ch)-ord('0')+b+49+h);
    end;
    
  s:='4294967290';
  for ch := '0' to '5' do
    begin
      s[length(s)]:=ch;
      val(s,c,l);
      if (l<>0) then
        halt(ord(ch)-ord('0')+b+54+h);
    end;

  s:='4294967290';
  for ch := '6' to '9' do
    begin
      s[length(s)]:=ch;
{$ifdef cpu64}
{$r+}
      try
        caught:=false;
{$endif cpu64}
          val(s,c,l);
{$ifdef cpu64}
      except on e : jlthrowable do
        caught:=true;
      end;
      if not caught then
{$else cpu64}
      if (l=0) then
{$endif cpu64}
        halt(ord(ch)-ord('0')+b+54+h);
    end;

  setlength(s,length('4294967295')+1);
  for ch2:= '1' to '3' do
    begin
      for b := 1 to length(s)-1 do
        s[b]:=ch2;
      for ch := '0' to '9' do
        begin
          s[length(s)]:=ch;
{$ifdef cpu64}
{$r+}
          try
            caught:=false;
{$endif cpu64}
            val(s,c,l);
{$ifdef cpu64}
          except on e : jlthrowable do
            caught:=true;
          end;
          if not caught then
{$else cpu64}
          if (l=0) then
{$endif cpu64}
            halt(ord(ch2)-ord('1')+65+h);
        end;
    end;

end;


procedure testqword;
const
  h = 71;
  hexch : array[0..15] of char='0123456789ABCDEF';
var
  c: qword;
  l: longint;
  s: shortstring;
  b, b2: byte;
  ch, ch2: char;
begin
  s:='$0fffffffffffffff';
  for b := low(hexch) to high(hexch) do
    begin
      s[2]:=hexch[b];
      val(s,c,l);
      if (l<>0) then
        halt(b+h);
    end;
    
  s:='$fffffffffffffff0';
  for b := low(hexch) to high(hexch) do
    begin
      s[length(s)]:=hexch[b];
      val(s,c,l);
      if (l<>0) then
        halt(b+16+h);
    end;

  setlength(s,18);
  s[1]:='$';
  for b2:= 1 to high(hexch) do
    begin
      for b := 2 to length(s)-1 do
        s[b]:=hexch[b2];
      for b := low(hexch) to high(hexch) do
        begin
          s[length(s)]:=hexch[b];
          val(s,c,l);
          if (l=0) then
            halt(b2+32+h);
        end;
    end;

  s:='18446744073709551615';
  for ch := '0' to '1' do
    begin
      s[1]:=ch;
      val(s,c,l);
      if (l<>0) then
        halt(ord(ch)-ord('0')+b+49+h);
    end;
    
  s:='18446744073709551615';
  for ch := '0' to '5' do
    begin
      s[length(s)]:=ch;
      val(s,c,l);
      if (l<>0) then
        halt(ord(ch)-ord('0')+b+54+h);
    end;

  s:='18446744073709551615';
  for ch := '6' to '9' do
    begin
      s[length(s)]:=ch;
      val(s,c,l);
      if (l=0) then
        halt(ord(ch)-ord('0')+b+54+h);
    end;

  setlength(s,length('18446744073709551615')+1);
  for ch2:= '1' to '1' do
    begin
      for b := 1 to length(s)-1 do
        s[b]:=ch2;
      for ch := '0' to '9' do
        begin
          s[length(s)]:=ch;
          val(s,c,l);
          if (l=0) then
            halt(ord(ch2)-ord('1')+61+h);
        end;
    end;

end;

begin
  testcard;
  testqword;
end.
