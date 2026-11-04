program tcurrency2;

{ An intermediate currency value that is not a whole number of ten-thousandths
  (an operand already divided by 10000, a quotient) must keep its fraction
  when it is saved across a call that evaluates the other operand }

{$mode objfpc}

var
  errors: longint = 0;

function F(c: currency): currency;
begin
  result:=c;
end;

procedure check(const name: string; got, expected: currency);
begin
  if got<>expected then
    begin
      writeln(name,': got ',got:0:4,', expected ',expected:0:4);
      inc(errors);
    end;
end;

var
  a, b, c: currency;
  i: longint;
  ca: array[0..1] of currency;

function idx(n: longint): longint;
begin
  result:=n;
end;

begin
  a:=2.5;
  b:=1.5;
  c:=12.34;
  check('call*call',F(a)*F(b),3.75);
  check('call*call, negative',F(-a)*F(b),-3.75);
  check('call*call, both negative',F(-a)*F(-b),3.75);
  check('call*call, smaller fraction',F(c)*F(0.5),6.17);
  check('call*call*call',F(a)*F(b)*F(b),5.625);
  check('call*(call*call)',F(a)*(F(b)*F(b)),5.625);
  ca[0]:=a;
  ca[1]:=b;
  check('element*element, indexed by calls',ca[idx(0)]*ca[idx(1)],3.75);
  check('call/call*call',F(a)/F(b)*F(b),2.5);
{$ifndef FPC_CURRENCY_IS_INT64}
  { a comparison saves its left operand the same way; where currency is an
    int64, the comparison converts the quotient to a currency, 0.3333 }
  if not (F(1)/F(3)>F(0.3333)) then
    begin
      writeln('call/call>call: false, expected true');
      inc(errors);
    end;
{$endif FPC_CURRENCY_IS_INT64}
  i:=3;
  check('call*integer',F(a)*i,7.5);
  if errors<>0 then
    halt(1);
  writeln('ok');
end.
