{ %OPT=-O2 }
{ tests case statements compiled into linear lists, in particular the
  check of the first label which is combined with the subtraction }

{$mode objfpc}

function signed_first0(l : longint) : longint;
  begin
    case l of
      0: result:=1;
      5: result:=2;
      7..9: result:=3;
      else
        result:=0;
    end;
  end;


function signed_first0_ref(l : longint) : longint;
  begin
    if l=0 then
      result:=1
    else if l=5 then
      result:=2
    else if (l>=7) and (l<=9) then
      result:=3
    else
      result:=0;
  end;


function unsigned_first1(c : cardinal) : longint;
  begin
    case c of
      1: result:=1;
      3: result:=2;
      10..12: result:=3;
      else
        result:=0;
    end;
  end;


function unsigned_first1_ref(c : cardinal) : longint;
  begin
    if c=1 then
      result:=1
    else if c=3 then
      result:=2
    else if (c>=10) and (c<=12) then
      result:=3
    else
      result:=0;
  end;


function byte_firstrange(b : byte) : longint;
  begin
    case b of
      46..47: result:=1;
      48: result:=2;
      50..57: result:=3;
      else
        result:=0;
    end;
  end;


function byte_firstrange_ref(b : byte) : longint;
  begin
    if (b>=46) and (b<=47) then
      result:=1
    else if b=48 then
      result:=2
    else if (b>=50) and (b<=57) then
      result:=3
    else
      result:=0;
  end;


function smallint_first(s : smallint) : longint;
  begin
    case s of
      1000: result:=1;
      1001: result:=2;
      2000..2001: result:=3;
      else
        result:=0;
    end;
  end;


function smallint_first_ref(s : smallint) : longint;
  begin
    if s=1000 then
      result:=1
    else if s=1001 then
      result:=2
    else if (s>=2000) and (s<=2001) then
      result:=3
    else
      result:=0;
  end;


function int64_first(i : int64) : longint;
  begin
    case i of
      $100000000: result:=1;
      $100000005: result:=2;
      $200000000..$200000002: result:=3;
      else
        result:=0;
    end;
  end;


function int64_first_ref(i : int64) : longint;
  begin
    if i=$100000000 then
      result:=1
    else if i=$100000005 then
      result:=2
    else if (i>=$200000000) and (i<=$200000002) then
      result:=3
    else
      result:=0;
  end;


var
  i : longint;
  j : int64;
begin
  for i:=-20 to 300 do
    begin
      if signed_first0(i)<>signed_first0_ref(i) then
        halt(1);
      if unsigned_first1(cardinal(i))<>unsigned_first1_ref(cardinal(i)) then
        halt(2);
      if byte_firstrange(byte(i))<>byte_firstrange_ref(byte(i)) then
        halt(3);
    end;
  if signed_first0(low(longint))<>0 then
    halt(4);
  if signed_first0(high(longint))<>0 then
    halt(5);
  for i:=-1100 to 2100 do
    if smallint_first(i)<>smallint_first_ref(i) then
      halt(6);
  if smallint_first(low(smallint))<>0 then
    halt(7);
  for i:=-10 to 10 do
    begin
      j:=$100000000+i;
      if int64_first(j)<>int64_first_ref(j) then
        halt(8);
      j:=$200000000+i;
      if int64_first(j)<>int64_first_ref(j) then
        halt(9);
      if int64_first(i)<>int64_first_ref(i) then
        halt(10);
    end;
  if int64_first(low(int64))<>0 then
    halt(11);
  writeln('ok');
end.
