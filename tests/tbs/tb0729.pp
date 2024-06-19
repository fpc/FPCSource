{ tests loads/stores of local symbols, on aarch64 the page offset of such
  symbols is integrated into the reference of the load/store if possible }
{$mode objfpc}
{$J+}
type
  tpackedrec = packed record
    b: byte;
    w: word;
    l: longint;
    q: int64;
  end;

  tbytes8 = packed array[0..7] of byte;

  tvec = record
    a, b: double;
  end;

function getsingle(x: single): single;
  begin
    result:=x*1.25+0.5;
  end;

function getdouble(x: double): double;
  begin
    result:=x*1.25+3.25e100;
  end;

function getbyte: byte;
  const
    c: byte = 17;
  begin
    result:=c;
    inc(c);
  end;

function getword: word;
  const
    c: word = 1717;
  begin
    result:=c;
    inc(c);
  end;

function getlongint: longint;
  const
    c: longint = 171717;
  begin
    result:=c;
    inc(c);
  end;

function getint64: int64;
  const
    c: int64 = $123456789abc;
  begin
    result:=c;
    inc(c);
  end;

function getsinglevar: single;
  const
    c: single = 1.5;
  begin
    result:=c;
    c:=c+1.0;
  end;

function getdoublevar: double;
  const
    c: double = 2.5;
  begin
    result:=c;
    c:=c+1.0;
  end;

function getpackedrec: tpackedrec;
  const
    c: tpackedrec = (b: 1; w: 2; l: 3; q: 4);
  begin
    result:=c;
    inc(c.b);
    inc(c.w);
    inc(c.l);
    inc(c.q);
  end;

function getbytes8: tbytes8;
  const
    c: tbytes8 = (1,2,3,4,5,6,7,8);
  begin
    result:=c;
    inc(c[0]);
    inc(c[7]);
  end;

function getvec: tvec;
  const
    c: tvec = (a: 1.0; b: 2.0);
  begin
    result:=c;
    c.a:=c.a+1.0;
    c.b:=c.b+1.0;
  end;

var
  r: tpackedrec;
  b8: tbytes8;
  v: tvec;
begin
  if abs(getsingle(2.0)-3.0)>1e-6 then
    halt(1);
  if abs(getdouble(2.0)-(2.5+3.25e100))>1e90 then
    halt(2);
  if (getbyte<>17) or (getbyte<>18) then
    halt(3);
  if (getword<>1717) or (getword<>1718) then
    halt(4);
  if (getlongint<>171717) or (getlongint<>171718) then
    halt(5);
  if (getint64<>$123456789abc) or (getint64<>$123456789abd) then
    halt(6);
  if (getsinglevar<>1.5) or (getsinglevar<>2.5) then
    halt(7);
  if (getdoublevar<>2.5) or (getdoublevar<>3.5) then
    halt(8);
  r:=getpackedrec;
  if (r.b<>1) or (r.w<>2) or (r.l<>3) or (r.q<>4) then
    halt(9);
  r:=getpackedrec;
  if (r.b<>2) or (r.w<>3) or (r.l<>4) or (r.q<>5) then
    halt(10);
  b8:=getbytes8;
  if (b8[0]<>1) or (b8[3]<>4) or (b8[7]<>8) then
    halt(11);
  b8:=getbytes8;
  if (b8[0]<>2) or (b8[3]<>4) or (b8[7]<>9) then
    halt(12);
  v:=getvec;
  if (v.a<>1.0) or (v.b<>2.0) then
    halt(13);
  v:=getvec;
  if (v.a<>2.0) or (v.b<>3.0) then
    halt(14);
  writeln('ok');
end.
