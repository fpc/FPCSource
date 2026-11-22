{ tests the folding of (x op c1) op c2 for integers and the removal of
  redundant integer type conversions it might leave over }
{$mode objfpc}
{$Q-}
{$R-}
uses
  sysutils;

procedure check(b: boolean; code: longint);
  begin
    if not b then
      begin
        writeln('error ', code);
        halt(code);
      end;
  end;

function fold1(x: longint): longint;
  begin
    result:=(x-1)+1;
  end;

function fold2(x: longint): longint;
  begin
    result:=(x+5)-7;
  end;

function fold3(x: longint): longint;
  begin
    result:=(x-3)-4;
  end;

function fold4(x: qword): qword;
  begin
    result:=(x-1)+3;
  end;

function fold5(x: qword): qword;
  begin
    result:=(x+1)-3;
  end;

function fold6(x: int64): int64;
  begin
    result:=(x-high(int64))-2;
  end;

function fold7(x: byte): longint;
  begin
    result:=(x+200)-100;
  end;

function fold8(x: longint): cardinal;
  begin
    result:=cardinal(int64(x));
  end;

function fold9(x: longint): longint;
  begin
    result:=longint(int64(x));
  end;

{$Q+}
function foldovf(x: int64): int64;
  begin
    result:=(x-1)+1;
  end;
{$Q-}

function loopcount(n: longint): longint;
  var
    i: longint;
  begin
    result:=0;
    for i:=0 to n-1 do
      inc(result);
  end;

function loopsum(p: plongint; n: longint): longint;
  var
    i: longint;
  begin
    result:=0;
    for i:=0 to n-1 do
      begin
        inc(result,p^);
        inc(p);
      end;
  end;

function loopcount64(n: int64): int64;
  var
    i: int64;
  begin
    result:=0;
    for i:=2 to n+1 do
      inc(result);
  end;

var
  a: array[0..9] of longint;
  i: longint;
  ok: boolean;
begin
  check(fold1(low(longint))=low(longint),1);
  check(fold1(5)=5,2);
  check(fold2(low(longint)+1)=high(longint),3);
  check(fold2(10)=8,4);
  check(fold3(low(longint))=high(longint)-6,5);
  check(fold4(high(qword))=1,6);
  check(fold5(1)=high(qword),7);
  check(fold6(low(int64))=-1,8);
  check(fold7(255)=355,9);
  check(fold7(0)=100,10);
  check(fold8(-1)=high(cardinal),11);
  check(fold9(low(longint))=low(longint),12);
  check(foldovf(5)=5,13);
  ok:=false;
  try
    foldovf(low(int64));
  except
    on EIntOverflow do
      ok:=true;
  end;
  check(ok,14);
  check(loopcount(0)=0,15);
  check(loopcount(1)=1,16);
  check(loopcount(10)=10,17);
  check(loopcount(-5)=0,18);
  for i:=0 to 9 do
    a[i]:=i;
  check(loopsum(@a[0],10)=45,19);
  check(loopsum(@a[0],0)=0,20);
  check(loopcount64(0)=0,21);
  check(loopcount64(1)=1,22);
  check(loopcount64(11)=11,23);
  writeln('ok');
end.
