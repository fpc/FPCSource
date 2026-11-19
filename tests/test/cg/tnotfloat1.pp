{ not(a<b) etc. must be true if an operand is NaN: it must not be turned
  into a>=b for floating point types (except with fast math, which -O4
  includes) }
program tnotfloat1;

{$optimization nofastmath}

uses
  math;

var
  errors: longint;

procedure check(b: boolean; id: longint);
  begin
    if not b then
      begin
        writeln('error ',id);
        inc(errors);
      end;
  end;

procedure test_single(a, b: single; base: longint);
  var
    r: boolean;
  begin
    check(not(a<b),base+1);
    check(not(a<=b),base+2);
    check(not(a>b),base+3);
    check(not(a>=b),base+4);
    check(not(a=b),base+5);
    r:=not(a<b); check(r,base+11);
    r:=not(a<=b); check(r,base+12);
    r:=not(a>b); check(r,base+13);
    r:=not(a>=b); check(r,base+14);
    r:=not(a=b); check(r,base+15);
    r:=not(a<>b); check(not r,base+16);
  end;

procedure test_double(a, b: double; base: longint);
  var
    r: boolean;
  begin
    check(not(a<b),base+1);
    check(not(a<=b),base+2);
    check(not(a>b),base+3);
    check(not(a>=b),base+4);
    check(not(a=b),base+5);
    r:=not(a<b); check(r,base+11);
    r:=not(a<=b); check(r,base+12);
    r:=not(a>b); check(r,base+13);
    r:=not(a>=b); check(r,base+14);
    r:=not(a=b); check(r,base+15);
    r:=not(a<>b); check(not r,base+16);
  end;

procedure test_extended(a, b: extended; base: longint);
  var
    r: boolean;
  begin
    check(not(a<b),base+1);
    check(not(a<=b),base+2);
    check(not(a>b),base+3);
    check(not(a>=b),base+4);
    check(not(a=b),base+5);
    r:=not(a<b); check(r,base+11);
    r:=not(a<=b); check(r,base+12);
    r:=not(a>b); check(r,base+13);
    r:=not(a>=b); check(r,base+14);
    r:=not(a=b); check(r,base+15);
    r:=not(a<>b); check(not r,base+16);
  end;

begin
  errors:=0;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  test_single(1.0,NaN,100);
  test_single(NaN,1.0,200);
  test_double(1.0,NaN,300);
  test_double(NaN,1.0,400);
  test_extended(1.0,NaN,500);
  test_extended(NaN,1.0,600);
  if errors<>0 then
    halt(1);
  writeln('ok');
end.
