{ sets with elements >255 with all $packset values }
{$mode objfpc}
program tset12;
uses
  typinfo;

type
  te = (e0=600,e1,e2,e3=700,e4);
  { default packset }
  tref = set of 1000..1100;

{$packset 1}
type
  tl1 = set of 1000..1100;
  tz1 = set of 0..1000;
  ts1 = set of 300..301;
  te1 = set of te;
const
  cl1: tl1 = [1000,1050..1052,1100];
{$packset 2}
type
  tl2 = set of 1000..1100;
  tz2 = set of 0..1000;
  ts2 = set of 300..301;
  te2 = set of te;
const
  cl2: tl2 = [1000,1050..1052,1100];
{$packset 4}
type
  tl4 = set of 1000..1100;
  tz4 = set of 0..1000;
  ts4 = set of 300..301;
  te4 = set of te;
const
  cl4: tl4 = [1000,1050..1052,1100];
{$packset 8}
type
  tl8 = set of 1000..1100;
  tz8 = set of 0..1000;
  ts8 = set of 300..301;
  te8 = set of te;
const
  cl8: tl8 = [1000,1050..1052,1100];
{$packset default}

procedure error(nr: longint);
  begin
    writeln('error ',nr);
    halt(1);
  end;

function f1(a: tl1; var b: tl1): tl1;
  begin
    b:=a+[1077];
    result:=a*b;
  end;

procedure test1;
  var
    s, t, u: tl1;
    z: tz1;
    sm: ts1;
    es: te1;
    r: tref;
    i, j, k, cnt, sum: longint;
    e: te;
  begin
    if sizeof(tl1)<>13 then
      error(101);
    if sizeof(tz1)<>126 then
      error(102);
    if sizeof(ts1)<>1 then
      error(103);
    if sizeof(te1)<>13 then
      error(104);

    { lowest and highest element }
    s:=[1000,1100];
    if not(1000 in s) or not(1100 in s) or (1001 in s) or (1099 in s) then
      error(110);
    i:=1050;
    s:=s+[i];
    include(s,1001);
    exclude(s,1000);
    if s<>[1001,1050,1100] then
      error(111);

    { range constructor with variable bounds }
    j:=1060;
    k:=1063;
    t:=[j..k,i];
    if t<>[1050,1060..1063] then
      error(112);

    { operators }
    if s*t<>[1050] then
      error(113);
    if s-t<>[1001,1100] then
      error(114);
    if s+t<>[1001,1050,1060..1063,1100] then
      error(115);
    if s><t<>[1001,1060..1063,1100] then
      error(116);
    if not([1050]<=t) or (t<=s) or not(t>=[1061,1062]) or (s=t) then
      error(117);

    { in with variable left side, also outside the set range }
    i:=1042;
    if (i in s) or (i in [1000,1043,1100]) or not(i in [1000,1042,1100]) then
      error(118);
    i:=1100;
    if not(i in s) then
      error(119);
    i:=999;
    if i in s then
      error(120);
    i:=1101;
    if i in s then
      error(121);
    i:=100000;
    if i in s then
      error(122);

    { for..in }
    cnt:=0;
    sum:=0;
    for i in s do
      begin
        inc(cnt);
        inc(sum,i);
      end;
    if (cnt<>3) or (sum<>1001+1050+1100) then
      error(123);

    { typed constant }
    if cl1<>[1000,1050..1052,1100] then
      error(124);

    { parameters and function results }
    u:=f1(s,t);
    if (t<>[1001,1050,1077,1100]) or (u<>s) then
      error(125);

    { conversions from and to a set with the default packset }
    r:=s;
    if r<>[1001,1050,1100] then
      error(126);
    r:=[1000,1099];
    s:=r;
    if s<>[1000,1099] then
      error(127);

    { set of 0..1000 }
    z:=[0,255,256,999];
    i:=1000;
    include(z,i);
    exclude(z,255);
    if z<>[0,256,999,1000] then
      error(130);
    i:=256;
    if not(i in z) or (257 in z) then
      error(131);
    s:=[1000];
    z:=s;
    if z<>[1000] then
      error(132);

    { two elements far from 0 }
    sm:=[301];
    i:=300;
    include(sm,i);
    if sm<>[300,301] then
      error(140);
    i:=302;
    if i in sm then
      error(141);

    { enum with values >255 }
    es:=[e0,e4];
    include(es,e3);
    e:=e4;
    if not(e3 in es) or (e1 in es) or not(e in es) then
      error(150);
    cnt:=0;
    for e in es do
      inc(cnt);
    if cnt<>3 then
      error(151);

    { RTTI contains the set base }
    s:=[1001,1050,1100];
    if SetToString(PTypeInfo(TypeInfo(tl1)),@s)<>'1001,1050,1100' then
      error(160);
    s:=[];
    StringToSet(PTypeInfo(TypeInfo(tl1)),'1000,1063',@s);
    if s<>[1000,1063] then
      error(161);
  end;

function f2(a: tl2; var b: tl2): tl2;
  begin
    b:=a+[1077];
    result:=a*b;
  end;

procedure test2;
  var
    s, t, u: tl2;
    z: tz2;
    sm: ts2;
    es: te2;
    r: tref;
    i, j, k, cnt, sum: longint;
    e: te;
  begin
    if sizeof(tl2)<>14 then
      error(201);
    if sizeof(tz2)<>126 then
      error(202);
    if sizeof(ts2)<>2 then
      error(203);
    if sizeof(te2)<>14 then
      error(204);

    { lowest and highest element }
    s:=[1000,1100];
    if not(1000 in s) or not(1100 in s) or (1001 in s) or (1099 in s) then
      error(210);
    i:=1050;
    s:=s+[i];
    include(s,1001);
    exclude(s,1000);
    if s<>[1001,1050,1100] then
      error(211);

    { range constructor with variable bounds }
    j:=1060;
    k:=1063;
    t:=[j..k,i];
    if t<>[1050,1060..1063] then
      error(212);

    { operators }
    if s*t<>[1050] then
      error(213);
    if s-t<>[1001,1100] then
      error(214);
    if s+t<>[1001,1050,1060..1063,1100] then
      error(215);
    if s><t<>[1001,1060..1063,1100] then
      error(216);
    if not([1050]<=t) or (t<=s) or not(t>=[1061,1062]) or (s=t) then
      error(217);

    { in with variable left side, also outside the set range }
    i:=1042;
    if (i in s) or (i in [1000,1043,1100]) or not(i in [1000,1042,1100]) then
      error(218);
    i:=1100;
    if not(i in s) then
      error(219);
    i:=999;
    if i in s then
      error(220);
    i:=1101;
    if i in s then
      error(221);
    i:=100000;
    if i in s then
      error(222);

    { for..in }
    cnt:=0;
    sum:=0;
    for i in s do
      begin
        inc(cnt);
        inc(sum,i);
      end;
    if (cnt<>3) or (sum<>1001+1050+1100) then
      error(223);

    { typed constant }
    if cl2<>[1000,1050..1052,1100] then
      error(224);

    { parameters and function results }
    u:=f2(s,t);
    if (t<>[1001,1050,1077,1100]) or (u<>s) then
      error(225);

    { conversions from and to a set with the default packset }
    r:=s;
    if r<>[1001,1050,1100] then
      error(226);
    r:=[1000,1099];
    s:=r;
    if s<>[1000,1099] then
      error(227);

    { set of 0..1000 }
    z:=[0,255,256,999];
    i:=1000;
    include(z,i);
    exclude(z,255);
    if z<>[0,256,999,1000] then
      error(230);
    i:=256;
    if not(i in z) or (257 in z) then
      error(231);
    s:=[1000];
    z:=s;
    if z<>[1000] then
      error(232);

    { two elements far from 0 }
    sm:=[301];
    i:=300;
    include(sm,i);
    if sm<>[300,301] then
      error(240);
    i:=302;
    if i in sm then
      error(241);

    { enum with values >255 }
    es:=[e0,e4];
    include(es,e3);
    e:=e4;
    if not(e3 in es) or (e1 in es) or not(e in es) then
      error(250);
    cnt:=0;
    for e in es do
      inc(cnt);
    if cnt<>3 then
      error(251);

    { RTTI contains the set base }
    s:=[1001,1050,1100];
    if SetToString(PTypeInfo(TypeInfo(tl2)),@s)<>'1001,1050,1100' then
      error(260);
    s:=[];
    StringToSet(PTypeInfo(TypeInfo(tl2)),'1000,1063',@s);
    if s<>[1000,1063] then
      error(261);
  end;

function f4(a: tl4; var b: tl4): tl4;
  begin
    b:=a+[1077];
    result:=a*b;
  end;

procedure test4;
  var
    s, t, u: tl4;
    z: tz4;
    sm: ts4;
    es: te4;
    r: tref;
    i, j, k, cnt, sum: longint;
    e: te;
  begin
    if sizeof(tl4)<>16 then
      error(401);
    if sizeof(tz4)<>128 then
      error(402);
    if sizeof(ts4)<>4 then
      error(403);
    if sizeof(te4)<>16 then
      error(404);

    { lowest and highest element }
    s:=[1000,1100];
    if not(1000 in s) or not(1100 in s) or (1001 in s) or (1099 in s) then
      error(410);
    i:=1050;
    s:=s+[i];
    include(s,1001);
    exclude(s,1000);
    if s<>[1001,1050,1100] then
      error(411);

    { range constructor with variable bounds }
    j:=1060;
    k:=1063;
    t:=[j..k,i];
    if t<>[1050,1060..1063] then
      error(412);

    { operators }
    if s*t<>[1050] then
      error(413);
    if s-t<>[1001,1100] then
      error(414);
    if s+t<>[1001,1050,1060..1063,1100] then
      error(415);
    if s><t<>[1001,1060..1063,1100] then
      error(416);
    if not([1050]<=t) or (t<=s) or not(t>=[1061,1062]) or (s=t) then
      error(417);

    { in with variable left side, also outside the set range }
    i:=1042;
    if (i in s) or (i in [1000,1043,1100]) or not(i in [1000,1042,1100]) then
      error(418);
    i:=1100;
    if not(i in s) then
      error(419);
    i:=999;
    if i in s then
      error(420);
    i:=1101;
    if i in s then
      error(421);
    i:=100000;
    if i in s then
      error(422);

    { for..in }
    cnt:=0;
    sum:=0;
    for i in s do
      begin
        inc(cnt);
        inc(sum,i);
      end;
    if (cnt<>3) or (sum<>1001+1050+1100) then
      error(423);

    { typed constant }
    if cl4<>[1000,1050..1052,1100] then
      error(424);

    { parameters and function results }
    u:=f4(s,t);
    if (t<>[1001,1050,1077,1100]) or (u<>s) then
      error(425);

    { conversions from and to a set with the default packset }
    r:=s;
    if r<>[1001,1050,1100] then
      error(426);
    r:=[1000,1099];
    s:=r;
    if s<>[1000,1099] then
      error(427);

    { set of 0..1000 }
    z:=[0,255,256,999];
    i:=1000;
    include(z,i);
    exclude(z,255);
    if z<>[0,256,999,1000] then
      error(430);
    i:=256;
    if not(i in z) or (257 in z) then
      error(431);
    s:=[1000];
    z:=s;
    if z<>[1000] then
      error(432);

    { two elements far from 0 }
    sm:=[301];
    i:=300;
    include(sm,i);
    if sm<>[300,301] then
      error(440);
    i:=302;
    if i in sm then
      error(441);

    { enum with values >255 }
    es:=[e0,e4];
    include(es,e3);
    e:=e4;
    if not(e3 in es) or (e1 in es) or not(e in es) then
      error(450);
    cnt:=0;
    for e in es do
      inc(cnt);
    if cnt<>3 then
      error(451);

    { RTTI contains the set base }
    s:=[1001,1050,1100];
    if SetToString(PTypeInfo(TypeInfo(tl4)),@s)<>'1001,1050,1100' then
      error(460);
    s:=[];
    StringToSet(PTypeInfo(TypeInfo(tl4)),'1000,1063',@s);
    if s<>[1000,1063] then
      error(461);
  end;

function f8(a: tl8; var b: tl8): tl8;
  begin
    b:=a+[1077];
    result:=a*b;
  end;

procedure test8;
  var
    s, t, u: tl8;
    z: tz8;
    sm: ts8;
    es: te8;
    r: tref;
    i, j, k, cnt, sum: longint;
    e: te;
  begin
    if sizeof(tl8)<>24 then
      error(801);
    if sizeof(tz8)<>128 then
      error(802);
    if sizeof(ts8)<>8 then
      error(803);
    if sizeof(te8)<>16 then
      error(804);

    { lowest and highest element }
    s:=[1000,1100];
    if not(1000 in s) or not(1100 in s) or (1001 in s) or (1099 in s) then
      error(810);
    i:=1050;
    s:=s+[i];
    include(s,1001);
    exclude(s,1000);
    if s<>[1001,1050,1100] then
      error(811);

    { range constructor with variable bounds }
    j:=1060;
    k:=1063;
    t:=[j..k,i];
    if t<>[1050,1060..1063] then
      error(812);

    { operators }
    if s*t<>[1050] then
      error(813);
    if s-t<>[1001,1100] then
      error(814);
    if s+t<>[1001,1050,1060..1063,1100] then
      error(815);
    if s><t<>[1001,1060..1063,1100] then
      error(816);
    if not([1050]<=t) or (t<=s) or not(t>=[1061,1062]) or (s=t) then
      error(817);

    { in with variable left side, also outside the set range }
    i:=1042;
    if (i in s) or (i in [1000,1043,1100]) or not(i in [1000,1042,1100]) then
      error(818);
    i:=1100;
    if not(i in s) then
      error(819);
    i:=999;
    if i in s then
      error(820);
    i:=1101;
    if i in s then
      error(821);
    i:=100000;
    if i in s then
      error(822);

    { for..in }
    cnt:=0;
    sum:=0;
    for i in s do
      begin
        inc(cnt);
        inc(sum,i);
      end;
    if (cnt<>3) or (sum<>1001+1050+1100) then
      error(823);

    { typed constant }
    if cl8<>[1000,1050..1052,1100] then
      error(824);

    { parameters and function results }
    u:=f8(s,t);
    if (t<>[1001,1050,1077,1100]) or (u<>s) then
      error(825);

    { conversions from and to a set with the default packset }
    r:=s;
    if r<>[1001,1050,1100] then
      error(826);
    r:=[1000,1099];
    s:=r;
    if s<>[1000,1099] then
      error(827);

    { set of 0..1000 }
    z:=[0,255,256,999];
    i:=1000;
    include(z,i);
    exclude(z,255);
    if z<>[0,256,999,1000] then
      error(830);
    i:=256;
    if not(i in z) or (257 in z) then
      error(831);
    s:=[1000];
    z:=s;
    if z<>[1000] then
      error(832);

    { two elements far from 0 }
    sm:=[301];
    i:=300;
    include(sm,i);
    if sm<>[300,301] then
      error(840);
    i:=302;
    if i in sm then
      error(841);

    { enum with values >255 }
    es:=[e0,e4];
    include(es,e3);
    e:=e4;
    if not(e3 in es) or (e1 in es) or not(e in es) then
      error(850);
    cnt:=0;
    for e in es do
      inc(cnt);
    if cnt<>3 then
      error(851);

    { RTTI contains the set base }
    s:=[1001,1050,1100];
    if SetToString(PTypeInfo(TypeInfo(tl8)),@s)<>'1001,1050,1100' then
      error(860);
    s:=[];
    StringToSet(PTypeInfo(TypeInfo(tl8)),'1000,1063',@s);
    if s<>[1000,1063] then
      error(861);
  end;

begin
  test1;
  test2;
  test4;
  test8;
  writeln('ok');
end.
