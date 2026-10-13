{ Double2Real rounds to nearest, flushes tiny values to zero and overflows, issue #30714 }
program treal48;

{$mode objfpc}{$h+}

uses
  SysUtils, Real48Utils;

type
  TReal48Bytes = array[0..5] of Byte;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns the double with bit pattern aBits
function BitsToDouble(aBits: Int64): Double;

begin
  Move(aBits,Result,SizeOf(Result));
end;


// Returns the bit pattern of aValue
function DoubleToBits(aValue: Double): Int64;

begin
  Move(aValue,Result,SizeOf(Result));
end;


// Returns true if aA and aB have the same bytes
function SameReal48(const aA, aB: Real48): Boolean;

begin
  Result:=CompareByte(aA,aB,SizeOf(Real48))=0;
end;


// Returns true if aValue is all zero bytes
function IsZeroReal48(const aValue: Real48): Boolean;

var
  lZero: Real48;

begin
  FillChar(lZero,SizeOf(lZero),0);
  Result:=SameReal48(aValue,lZero);
end;


// Returns true if Double2Real raises an exception of class aClass for aValue
function Raises(aValue: Double; aClass: ExceptClass): Boolean;

begin
  Result:=False;
  try
    Double2Real(aValue);
  except
    on E: Exception do
      Result:=E.ClassType=aClass;
  end;
end;


var
  lReal, lGot, lNext: Real48;
  lBits, lBase: Int64;
  lDouble: Double;
  i, j: Integer;

begin
  RandSeed:=30714;

  for i:=1 to 10000 do
    begin
    for j:=0 to 5 do
      TReal48Bytes(lReal)[j]:=Random(256);
    if TReal48Bytes(lReal)[0]=0 then
      TReal48Bytes(lReal)[0]:=1;
    lGot:=Double2Real(Real2Double(lReal));
{$ifdef verbose}
    write('lReal=');
    for j:=0 to 5 do
      write(hexstr(TReal48Bytes(lReal)[j],2));
    writeln;
    write(' lGot=');
    for j:=0 to 5 do
      write(hexstr(TReal48Bytes(lGot)[j],2));
    writeln;
{$endif}
    Check(1,SameReal48(lGot,lReal),'real48 to double and back is exact');
    end;

  for i:=1 to 10000 do
    begin
    for j:=0 to 5 do
      TReal48Bytes(lReal)[j]:=Random(256);
    if (TReal48Bytes(lReal)[0]=0) or (TReal48Bytes(lReal)[0]=255) then
      TReal48Bytes(lReal)[0]:=128;
    lBase:=DoubleToBits(Real2Double(lReal));
    lNext:=Double2Real(BitsToDouble(lBase+(1 shl 13)));
    lGot:=Double2Real(BitsToDouble(lBase+1+Random($FFF)));
    Check(2,SameReal48(lGot,lReal),'dropped bits below half round down');
    lGot:=Double2Real(BitsToDouble(lBase+$1001+Random($FFF)));
    Check(3,SameReal48(lGot,lNext),'dropped bits above half round up');
    lGot:=Double2Real(BitsToDouble(lBase+$1000));
    if (lBase and (1 shl 13))=0 then
      Check(4,SameReal48(lGot,lReal),'tie with even mantissa stays')
    else
      Check(5,SameReal48(lGot,lNext),'tie with odd mantissa rounds up');
    end;

  lGot:=Double2Real(1/3);
  Check(11,Abs(Real2Double(lGot)-1/3)<=Abs(Real2Double(Double2Real(BitsToDouble(DoubleToBits(1/3) and not Int64($1FFF))))-1/3),
    '1/3 rounds to the nearest real48');
  lBits:=DoubleToBits(2.0)-1;
  Check(12,Real2Double(Double2Real(BitsToDouble(lBits)))=2.0,'mantissa of all ones rounds up into the exponent');

  Check(21,IsZeroReal48(Double2Real(0.0)),'zero is zero');
  lDouble:=-0.0;
  Check(22,IsZeroReal48(Double2Real(lDouble)),'negative zero is zero');
  Check(23,IsZeroReal48(Double2Real(BitsToDouble(1))),'denormal is zero');
  Check(24,IsZeroReal48(Double2Real(1e-300)),'1e-300 is zero');
  Check(25,IsZeroReal48(Double2Real(-1e-40)),'-1e-40 is zero');
  lDouble:=BitsToDouble(Int64(1023-128) shl 52);
  Check(26,Real2Double(Double2Real(lDouble))=lDouble,'2^-128 is exact');

  lDouble:=BitsToDouble((Int64(1023+126) shl 52) or (Int64(1) shl 52-(1 shl 13)));
  Check(31,Real2Double(Double2Real(lDouble))=lDouble,'largest real48 is exact');
  Check(32,Raises(lDouble*(1+1/(Int64(1) shl 40)),EOverflow),'value rounding past the largest real48 overflows');
  Check(33,Raises(1e39,EOverflow),'1e39 overflows');
  Check(34,Raises(-1e39,EOverflow),'-1e39 overflows');
  Check(35,Raises(BitsToDouble($7FF0000000000000),EOverflow),'Inf overflows');
  Check(36,Raises(BitsToDouble($7FF8000000000000),EInvalidOp),'NaN is an invalid operation');

  softfloat_exception_mask:=softfloat_exception_mask+[exOverflow,exInvalidOp];
  Check(41,Real2Double(Double2Real(1e39))=lDouble,'masked overflow gives the largest real48');
  Check(42,Real2Double(Double2Real(-1e39))=-lDouble,'masked negative overflow gives the negative largest real48');
  Check(43,IsZeroReal48(Double2Real(BitsToDouble($7FF8000000000000))),'masked NaN gives zero');
  writeln('ok');
end.
