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


function signed_singles(l : longint) : longint;
  begin
    case l of
      1: result:=1;
      2: result:=2;
      7: result:=3;
      else
        result:=0;
    end;
  end;


function signed_singles_ref(l : longint) : longint;
  begin
    if l=1 then
      result:=1
    else if l=2 then
      result:=2
    else if l=7 then
      result:=3
    else
      result:=0;
  end;


function unsigned_singles(c : cardinal) : longint;
  begin
    case c of
      3: result:=1;
      4: result:=2;
      $fffffff0: result:=3;
      else
        result:=0;
    end;
  end;


function unsigned_singles_ref(c : cardinal) : longint;
  begin
    if c=3 then
      result:=1
    else if c=4 then
      result:=2
    else if c=$fffffff0 then
      result:=3
    else
      result:=0;
  end;


function byte_mixed(b : byte) : longint;
  begin
    case b of
      0: result:=1;
      1..5: result:=2;
      7: result:=3;
      8..9: result:=4;
      11: result:=5;
      12: result:=6;
      13..20: result:=7;
      else
        result:=0;
    end;
  end;


function byte_mixed_ref(b : byte) : longint;
  begin
    if b=0 then
      result:=1
    else if (b>=1) and (b<=5) then
      result:=2
    else if b=7 then
      result:=3
    else if (b>=8) and (b<=9) then
      result:=4
    else if b=11 then
      result:=5
    else if b=12 then
      result:=6
    else if (b>=13) and (b<=20) then
      result:=7
    else
      result:=0;
  end;


function signed_mixed(l : longint) : longint;
  begin
    case l of
      0: result:=1;
      1..5: result:=2;
      7: result:=3;
      8..9: result:=4;
      11: result:=5;
      12: result:=6;
      13..20: result:=7;
      else
        result:=0;
    end;
  end;


function signed_mixed_ref(l : longint) : longint;
  begin
    if l=0 then
      result:=1
    else if (l>=1) and (l<=5) then
      result:=2
    else if l=7 then
      result:=3
    else if (l>=8) and (l<=9) then
      result:=4
    else if l=11 then
      result:=5
    else if l=12 then
      result:=6
    else if (l>=13) and (l<=20) then
      result:=7
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
      if signed_singles(i)<>signed_singles_ref(i) then
        halt(12);
      if unsigned_singles(cardinal(i))<>unsigned_singles_ref(cardinal(i)) then
        halt(13);
      if unsigned_singles($fffffff0+cardinal(i))<>unsigned_singles_ref($fffffff0+cardinal(i)) then
        halt(14);
      if byte_mixed(byte(i))<>byte_mixed_ref(byte(i)) then
        halt(17);
      if signed_mixed(i)<>signed_mixed_ref(i) then
        halt(18);
    end;
  if signed_mixed(low(longint))<>0 then
    halt(19);
  if signed_mixed(high(longint))<>0 then
    halt(20);
  if signed_singles(low(longint)+1)<>0 then
    halt(15);
  if signed_singles(high(longint))<>0 then
    halt(16);
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
