{ %OPT=-O3 }
{ tests removal of redundant jp instructions after floating point comparisons;
  all comparisons must be false for NaN operands, except <> }
program tfloatjp1;

{$mode objfpc}

uses
  math;

var
  errors: longint;

procedure check(b, expected: boolean; id: longint);
  begin
    if b<>expected then
      begin
        writeln('error ',id);
        inc(errors);
      end;
  end;

generic procedure testcmp<T>(a, b: T; base: longint);
  var
    r: longint;
    lt, le, gt, ge, eq, ne: boolean;
  begin
    if IsNan(a) or IsNan(b) then
      begin
        lt:=false; le:=false; gt:=false; ge:=false; eq:=false; ne:=true;
      end
    else
      begin
        lt:=a<b;
        le:=not(b<a);
        gt:=b<a;
        ge:=not(a<b);
        eq:=not(a<b) and not(b<a);
        ne:=not eq;
      end;

    { jumps }
    r:=0;
    if a<b then r:=r or 1;
    if a<=b then r:=r or 2;
    if a>b then r:=r or 4;
    if a>=b then r:=r or 8;
    if a=b then r:=r or 16;
    if a<>b then r:=r or 32;
    check(r and 1<>0,lt,base+1);
    check(r and 2<>0,le,base+2);
    check(r and 4<>0,gt,base+3);
    check(r and 8<>0,ge,base+4);
    check(r and 16<>0,eq,base+5);
    check(r and 32<>0,ne,base+6);

    { negated jumps; not(a<b) is turned into a>=b with fast math (and,
      without the fix in tnotnode.simplify, always), which differs for NaN
      operands, see tests/test/cg/tnotfloat1.pp }
    if not(IsNan(a) or IsNan(b)) then
      begin
        r:=0;
        if not(a<b) then r:=r or 1;
        if not(a<=b) then r:=r or 2;
        if not(a>b) then r:=r or 4;
        if not(a>=b) then r:=r or 8;
        if not(a=b) then r:=r or 16;
        if not(a<>b) then r:=r or 32;
        check(r and 1=0,lt,base+11);
        check(r and 2=0,le,base+12);
        check(r and 4=0,gt,base+13);
        check(r and 8=0,ge,base+14);
        check(r and 16=0,eq,base+15);
        check(r and 32=0,ne,base+16);
      end;

    { boolean results }
    check(a<b,lt,base+21);
    check(a<=b,le,base+22);
    check(a>b,gt,base+23);
    check(a>=b,ge,base+24);
    check(a=b,eq,base+25);
    check(a<>b,ne,base+26);

    { combined conditions }
    check((a>=0) and (a<=b),(not IsNan(a)) and (a>=0) and le,base+31);
    check((a<0) or (a>b),(not IsNan(a)) and ((a<0) or gt),base+32);
    check(not((a>=0) and (a<=b)),not((not IsNan(a)) and (a>=0) and le),base+33);

    { exits }
    r:=0;
    repeat
      inc(r);
    until (r>3) or (a<=b);
    check(r=1,le,base+41);
    if not(IsNan(a) or IsNan(b)) then
      begin
        r:=0;
        while (r<3) and not(a>b) do
          inc(r);
        check(r=3,not gt,base+42);
      end;
  end;

generic procedure testall<T>(base: longint);
  const
    vals: array[0..5] of double = (-1.5, 0.0, 1.0, 2.5, 1e300, -1e300);
  var
    i, j: longint;
    qnan: T;
  begin
    qnan:=NaN;
    for i:=low(vals) to high(vals) do
      begin
        for j:=low(vals) to high(vals) do
          specialize testcmp<T>(vals[i],vals[j],base);
        specialize testcmp<T>(vals[i],qnan,base+100);
        specialize testcmp<T>(qnan,vals[i],base+200);
      end;
    specialize testcmp<T>(qnan,qnan,base+300);
  end;

begin
  errors:=0;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  specialize testall<single>(1000);
  specialize testall<double>(2000);
  specialize testall<extended>(3000);
  if errors<>0 then
    halt(1);
  writeln('ok');
end.
