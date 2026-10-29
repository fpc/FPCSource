{ %SKIPCPU=jvm }
{ Ord() of a boolean converted to currency is scaled like any other integer:
  the integer to currency conversion must not be folded into the boolean to
  integer conversion below it (on targets where currency is an int64). The
  fold is only made without range and overflow checking. }
program tb0767;

{$mode objfpc}
{$R-}{$Q-}

procedure StoreOrd(const aSource; var aTarget);
begin
  Currency(aTarget):=Ord(Boolean(ByteBool(aSource)));
end;

procedure Check(value: Currency; expected: int64; code: longint);
begin
  if PInt64(@value)^<>expected*10000 then
    begin
      writeln('error ',code,': got ',PInt64(@value)^,', expected ',expected*10000);
      halt(code);
    end;
end;

var
  b: boolean;
  b16: boolean16;
  b32: boolean32;
  b64: boolean64;
  src: byte;
  c: currency;
begin
  b:=true;
  c:=ord(b);
  Check(c,1,1);
  c:=byte(b);
  Check(c,1,2);
  b16:=true;
  c:=ord(b16);
  Check(c,1,3);
  b32:=true;
  c:=ord(b32);
  Check(c,1,4);
  b64:=true;
  c:=ord(b64);
  Check(c,1,5);
  src:=1;
  StoreOrd(src,c);
  Check(c,1,6);
  b:=false;
  c:=ord(b);
  Check(c,0,7);
  c:=ord(true);
  Check(c,1,8);
  writeln('ok');
end.
