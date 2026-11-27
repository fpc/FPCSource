{ sets with elements >255 and a set base <>0 in the default packset mode }
{$mode objfpc}
program tset10;
type
  { 13 bytes }
  t1 = set of 1000..1100;
  { 4 bytes, not starting at a byte boundary }
  t2 = set of 1003..1020;
  { 2 bytes }
  t3 = set of 1000..1010;
  { 3 bytes are rounded up to 4 }
  t4 = set of 1000..1017;
  t5 = set of 0..1100;
  te = (e0=600,e1,e2,e3=700,e4);
  tes = set of te;

const
  c1 : t1 = [1000,1050,1100];
  c2 : t2 = [1003,1020];
  ce : tes = [e1,e4];

var
  s1, s1b : t1;
  s2 : t2;
  s3 : t3;
  s4 : t4;
  s5 : t5;
  es : tes;
  i, j, cnt : longint;
  e : te;

procedure fail(n : longint);
  begin
    writeln('fail ',n);
    halt(n);
  end;

begin
  if sizeof(t1)<>13 then fail(1);
  if sizeof(t2)<>4 then fail(2);
  if sizeof(t3)<>2 then fail(3);
  if sizeof(t4)<>4 then fail(4);
  if sizeof(t5)<>138 then fail(5);

  { constants }
  s1:=[1000,1050..1052,1100];
  if not((1000 in s1) and (1051 in s1) and (1100 in s1)) or (1001 in s1) or (1053 in s1) then fail(10);
  if s1<>[1000,1050,1051,1052,1100] then fail(11);
  if not(1050 in c1) or (1051 in c1) then fail(12);
  if c2<>[1003,1020] then fail(13);

  { variable elements }
  i:=1001; j:=1099;
  s1:=[i,j];
  if s1<>[1001,1099] then fail(20);
  s1:=[i..i+2];
  if s1<>[1001..1003] then fail(21);
  s1:=s1+[j];
  if s1<>[1001..1003,1099] then fail(22);
  s1:=s1-[i];
  if s1<>[1002,1003,1099] then fail(23);
  include(s1,1100);
  exclude(s1,1002);
  if s1<>[1003,1099,1100] then fail(24);
  if not(j in s1) or (i in s1) then fail(25);
  i:=5;
  if i in s1 then fail(26);
  i:=2000;
  if i in s1 then fail(27);

  { set operators }
  s1:=[1000..1010];
  s1b:=[1005..1020];
  if s1*s1b<>[1005..1010] then fail(30);
  if s1+s1b<>[1000..1020] then fail(31);
  if s1-s1b<>[1000..1004] then fail(32);
  if s1><s1b<>[1000..1004,1011..1020] then fail(33);
  if not([1006..1008]<=s1) or ([1006..1012]<=s1) then fail(34);
  if not(s1>=[1000]) then fail(35);

  { small sets with base }
  i:=1005;
  s2:=[1003,i,1020];
  if not((1003 in s2) and (1005 in s2) and (1020 in s2)) or (1004 in s2) then fail(40);
  include(s2,1010);
  exclude(s2,1003);
  if s2<>[1005,1010,1020] then fail(41);
  s3:=[1000,1010];
  include(s3,i);
  if s3<>[1000,1005,1010] then fail(42);
  s4:=[1017];
  s4:=s4+[i];
  if not((1017 in s4) and (1005 in s4)) or (1016 in s4) then fail(43);

  { conversion between sets with different bases }
  s1:=[1003,1020,1050];
  s2:=s1;
  if s2<>[1003,1020] then fail(50);
  s5:=s1;
  if s5<>[1003,1020,1050] then fail(51);
  s5:=[5,1001,1100];
  s1:=s5;
  if s1<>[1001,1100] then fail(52);
  s1:=s2;
  if s1<>[1003,1020] then fail(53);
  s2:=[1010];
  s3:=s2;
  if s3<>[1010] then fail(54);

  { for..in }
  s1:=[1000,1042,1100];
  cnt:=0;
  for i in s1 do
    begin
      if not(i in [1000,1042,1100]) then fail(60);
      inc(cnt);
    end;
  if cnt<>3 then fail(61);

  { enums with values >255 }
  es:=[e0,e3];
  e:=e4;
  include(es,e);
  if es<>[e0,e3,e4] then fail(70);
  if not(e1 in ce) or (e2 in ce) then fail(71);
  es:=es*ce;
  if es<>[e4] then fail(72);

  { in with constant sets containing elements >255 }
  i:=1042;
  if not(i in [1000,1042,1100]) or not(i in [1040..1045]) or not(i in [1042]) then fail(80);
  i:=1043;
  if (i in [1000,1042,1100]) or not(i in [1040..1045,1050]) then fail(81);
  i:=42;
  if i in [1000,1042,1100] then fail(82);

  writeln('ok');
end.
