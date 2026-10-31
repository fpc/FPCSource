{ tests unaligned loads/stores and copies; on aarch64 they are performed with
  normal loads/stores instead of being split into byte accesses on all
  targets except embedded }
{$mode objfpc}

type
  tpackedrec = packed record
    b: byte;
    w: word;
    l: longint;
    q: int64;
    s: single;
    d: double;
    c: currency;
    p: pointer;
  end;

  tpackedarr = packed array[0..3] of tpackedrec;

  tb3 = array[0..2] of byte;
  tr3 = packed record
    pad: byte;
    d: tb3;
  end;

  tb5 = array[0..4] of byte;
  tr5 = packed record
    pad: byte;
    d: tb5;
  end;

  tb6 = array[0..5] of byte;
  tr6 = packed record
    pad: byte;
    d: tb6;
  end;

  tb7 = array[0..6] of byte;
  tr7 = packed record
    pad: byte;
    d: tb7;
  end;

  tb9 = array[0..8] of byte;
  tr9 = packed record
    pad: byte;
    d: tb9;
  end;

  tb12 = array[0..11] of byte;
  tr12 = packed record
    pad: byte;
    d: tb12;
  end;

  tb15 = array[0..14] of byte;
  tr15 = packed record
    pad: byte;
    d: tb15;
  end;

  tb16 = array[0..15] of byte;
  tr16 = packed record
    pad: byte;
    d: tb16;
  end;

  tb17 = array[0..16] of byte;
  tr17 = packed record
    pad: byte;
    d: tb17;
  end;

  tb24 = array[0..23] of byte;
  tr24 = packed record
    pad: byte;
    d: tb24;
  end;

  tb31 = array[0..30] of byte;
  tr31 = packed record
    pad: byte;
    d: tb31;
  end;

  tb32 = array[0..31] of byte;
  tr32 = packed record
    pad: byte;
    d: tb32;
  end;

  tb33 = array[0..32] of byte;
  tr33 = packed record
    pad: byte;
    d: tb33;
  end;

  tb40 = array[0..39] of byte;
  tr40 = packed record
    pad: byte;
    d: tb40;
  end;

  tb48 = array[0..47] of byte;
  tr48 = packed record
    pad: byte;
    d: tb48;
  end;

  tb63 = array[0..62] of byte;
  tr63 = packed record
    pad: byte;
    d: tb63;
  end;

  tb64 = array[0..63] of byte;
  tr64 = packed record
    pad: byte;
    d: tb64;
  end;

  tb65 = array[0..64] of byte;
  tr65 = packed record
    pad: byte;
    d: tb65;
  end;

  tb100 = array[0..99] of byte;
  tr100 = packed record
    pad: byte;
    d: tb100;
  end;

var
  gpr: tpackedrec;
  gpa: tpackedarr;

  gs3, gd3: tr3;
  gs5, gd5: tr5;
  gs6, gd6: tr6;
  gs7, gd7: tr7;
  gs9, gd9: tr9;
  gs12, gd12: tr12;
  gs15, gd15: tr15;
  gs16, gd16: tr16;
  gs17, gd17: tr17;
  gs24, gd24: tr24;
  gs31, gd31: tr31;
  gs32, gd32: tr32;
  gs33, gd33: tr33;
  gs40, gd40: tr40;
  gs48, gd48: tr48;
  gs63, gd63: tr63;
  gs64, gd64: tr64;
  gs65, gd65: tr65;
  gs100, gd100: tr100;

procedure fill(var b; len, seed: longint);
  var
    i: longint;
  begin
    for i:=0 to len-1 do
      pbyte(@b)[i]:=byte(i*7+seed);
  end;

procedure check(const b; len, seed, err: longint);
  var
    i: longint;
  begin
    for i:=0 to len-1 do
      if pbyte(@b)[i]<>byte(i*7+seed) then
        begin
          writeln('error ',err,' at byte ',i);
          halt(err);
        end;
  end;

procedure testfields(var r: tpackedrec);
  var
    l: tpackedrec;
  begin
    r.b:=$12;
    r.w:=$3456;
    r.l:=longint($789abcde);
    r.q:=int64($0123456789abcdef);
    r.s:=1.5;
    r.d:=-2.25;
    r.c:=12345.6789;
    r.p:=@r;
    l:=r;
    if (l.b<>$12) or (l.w<>$3456) or (l.l<>longint($789abcde)) or
       (l.q<>int64($0123456789abcdef)) or (l.s<>1.5) or (l.d<>-2.25) or
       (l.c<>12345.6789) or (l.p<>@r) then
      halt(1);
    inc(l.w);
    inc(l.l);
    inc(l.q);
    l.s:=l.s*2;
    l.d:=l.d*2;
    l.c:=l.c*2;
    r:=l;
    if (r.w<>$3457) or (r.l<>longint($789abcdf)) or
       (r.q<>int64($0123456789abcdf0)) or (r.s<>3.0) or (r.d<>-4.5) or
       (r.c<>24691.3578) then
      halt(2);
  end;

procedure testlocalcopies;
  var

    ls3, ld3: tr3;
    ls5, ld5: tr5;
    ls6, ld6: tr6;
    ls7, ld7: tr7;
    ls9, ld9: tr9;
    ls12, ld12: tr12;
    ls15, ld15: tr15;
    ls16, ld16: tr16;
    ls17, ld17: tr17;
    ls24, ld24: tr24;
    ls31, ld31: tr31;
    ls32, ld32: tr32;
    ls33, ld33: tr33;
    ls40, ld40: tr40;
    ls48, ld48: tr48;
    ls63, ld63: tr63;
    ls64, ld64: tr64;
    ls65, ld65: tr65;
    ls100, ld100: tr100;
  begin
    fill(ls3.d,3,0);
    ld3.pad:=$55;
    ld3.d:=ls3.d;
    if ld3.pad<>$55 then halt(10);
    check(ld3.d,3,0,100);
    fill(ls5.d,5,1);
    ld5.pad:=$55;
    ld5.d:=ls5.d;
    if ld5.pad<>$55 then halt(11);
    check(ld5.d,5,1,101);
    fill(ls6.d,6,2);
    ld6.pad:=$55;
    ld6.d:=ls6.d;
    if ld6.pad<>$55 then halt(12);
    check(ld6.d,6,2,102);
    fill(ls7.d,7,3);
    ld7.pad:=$55;
    ld7.d:=ls7.d;
    if ld7.pad<>$55 then halt(13);
    check(ld7.d,7,3,103);
    fill(ls9.d,9,4);
    ld9.pad:=$55;
    ld9.d:=ls9.d;
    if ld9.pad<>$55 then halt(14);
    check(ld9.d,9,4,104);
    fill(ls12.d,12,5);
    ld12.pad:=$55;
    ld12.d:=ls12.d;
    if ld12.pad<>$55 then halt(15);
    check(ld12.d,12,5,105);
    fill(ls15.d,15,6);
    ld15.pad:=$55;
    ld15.d:=ls15.d;
    if ld15.pad<>$55 then halt(16);
    check(ld15.d,15,6,106);
    fill(ls16.d,16,7);
    ld16.pad:=$55;
    ld16.d:=ls16.d;
    if ld16.pad<>$55 then halt(17);
    check(ld16.d,16,7,107);
    fill(ls17.d,17,8);
    ld17.pad:=$55;
    ld17.d:=ls17.d;
    if ld17.pad<>$55 then halt(18);
    check(ld17.d,17,8,108);
    fill(ls24.d,24,9);
    ld24.pad:=$55;
    ld24.d:=ls24.d;
    if ld24.pad<>$55 then halt(19);
    check(ld24.d,24,9,109);
    fill(ls31.d,31,10);
    ld31.pad:=$55;
    ld31.d:=ls31.d;
    if ld31.pad<>$55 then halt(20);
    check(ld31.d,31,10,110);
    fill(ls32.d,32,11);
    ld32.pad:=$55;
    ld32.d:=ls32.d;
    if ld32.pad<>$55 then halt(21);
    check(ld32.d,32,11,111);
    fill(ls33.d,33,12);
    ld33.pad:=$55;
    ld33.d:=ls33.d;
    if ld33.pad<>$55 then halt(22);
    check(ld33.d,33,12,112);
    fill(ls40.d,40,13);
    ld40.pad:=$55;
    ld40.d:=ls40.d;
    if ld40.pad<>$55 then halt(23);
    check(ld40.d,40,13,113);
    fill(ls48.d,48,14);
    ld48.pad:=$55;
    ld48.d:=ls48.d;
    if ld48.pad<>$55 then halt(24);
    check(ld48.d,48,14,114);
    fill(ls63.d,63,15);
    ld63.pad:=$55;
    ld63.d:=ls63.d;
    if ld63.pad<>$55 then halt(25);
    check(ld63.d,63,15,115);
    fill(ls64.d,64,16);
    ld64.pad:=$55;
    ld64.d:=ls64.d;
    if ld64.pad<>$55 then halt(26);
    check(ld64.d,64,16,116);
    fill(ls65.d,65,17);
    ld65.pad:=$55;
    ld65.d:=ls65.d;
    if ld65.pad<>$55 then halt(27);
    check(ld65.d,65,17,117);
    fill(ls100.d,100,18);
    ld100.pad:=$55;
    ld100.d:=ls100.d;
    if ld100.pad<>$55 then halt(28);
    check(ld100.d,100,18,118);
  end;

procedure testglobalcopies;
  begin
    fill(gs3.d,3,50);
    gd3.pad:=$aa;
    gd3.d:=gs3.d;
    if gd3.pad<>$aa then halt(30);
    check(gd3.d,3,50,150);
    fill(gs5.d,5,51);
    gd5.pad:=$aa;
    gd5.d:=gs5.d;
    if gd5.pad<>$aa then halt(31);
    check(gd5.d,5,51,151);
    fill(gs6.d,6,52);
    gd6.pad:=$aa;
    gd6.d:=gs6.d;
    if gd6.pad<>$aa then halt(32);
    check(gd6.d,6,52,152);
    fill(gs7.d,7,53);
    gd7.pad:=$aa;
    gd7.d:=gs7.d;
    if gd7.pad<>$aa then halt(33);
    check(gd7.d,7,53,153);
    fill(gs9.d,9,54);
    gd9.pad:=$aa;
    gd9.d:=gs9.d;
    if gd9.pad<>$aa then halt(34);
    check(gd9.d,9,54,154);
    fill(gs12.d,12,55);
    gd12.pad:=$aa;
    gd12.d:=gs12.d;
    if gd12.pad<>$aa then halt(35);
    check(gd12.d,12,55,155);
    fill(gs15.d,15,56);
    gd15.pad:=$aa;
    gd15.d:=gs15.d;
    if gd15.pad<>$aa then halt(36);
    check(gd15.d,15,56,156);
    fill(gs16.d,16,57);
    gd16.pad:=$aa;
    gd16.d:=gs16.d;
    if gd16.pad<>$aa then halt(37);
    check(gd16.d,16,57,157);
    fill(gs17.d,17,58);
    gd17.pad:=$aa;
    gd17.d:=gs17.d;
    if gd17.pad<>$aa then halt(38);
    check(gd17.d,17,58,158);
    fill(gs24.d,24,59);
    gd24.pad:=$aa;
    gd24.d:=gs24.d;
    if gd24.pad<>$aa then halt(39);
    check(gd24.d,24,59,159);
    fill(gs31.d,31,60);
    gd31.pad:=$aa;
    gd31.d:=gs31.d;
    if gd31.pad<>$aa then halt(40);
    check(gd31.d,31,60,160);
    fill(gs32.d,32,61);
    gd32.pad:=$aa;
    gd32.d:=gs32.d;
    if gd32.pad<>$aa then halt(41);
    check(gd32.d,32,61,161);
    fill(gs33.d,33,62);
    gd33.pad:=$aa;
    gd33.d:=gs33.d;
    if gd33.pad<>$aa then halt(42);
    check(gd33.d,33,62,162);
    fill(gs40.d,40,63);
    gd40.pad:=$aa;
    gd40.d:=gs40.d;
    if gd40.pad<>$aa then halt(43);
    check(gd40.d,40,63,163);
    fill(gs48.d,48,64);
    gd48.pad:=$aa;
    gd48.d:=gs48.d;
    if gd48.pad<>$aa then halt(44);
    check(gd48.d,48,64,164);
    fill(gs63.d,63,65);
    gd63.pad:=$aa;
    gd63.d:=gs63.d;
    if gd63.pad<>$aa then halt(45);
    check(gd63.d,63,65,165);
    fill(gs64.d,64,66);
    gd64.pad:=$aa;
    gd64.d:=gs64.d;
    if gd64.pad<>$aa then halt(46);
    check(gd64.d,64,66,166);
    fill(gs65.d,65,67);
    gd65.pad:=$aa;
    gd65.d:=gs65.d;
    if gd65.pad<>$aa then halt(47);
    check(gd65.d,65,67,167);
    fill(gs100.d,100,68);
    gd100.pad:=$aa;
    gd100.d:=gs100.d;
    if gd100.pad<>$aa then halt(48);
    check(gd100.d,100,68,168);
  end;

var
  i: longint;
begin
  testfields(gpr);
  for i:=low(gpa) to high(gpa) do
    testfields(gpa[i]);
  testlocalcopies;
  testglobalcopies;
  writeln('ok');
end.
