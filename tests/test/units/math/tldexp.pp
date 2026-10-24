{ Ldexp for huge and tiny exponents, subnormal results and rounding, issue #34125 }
program tldexp;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

type
  TDoubleCase = record
    x: QWord;
    p: Integer;
    ref: QWord;
  end;
  TSingleCase = record
    x: LongWord;
    p: Integer;
    ref: LongWord;
  end;

const
  { bit patterns of x and of the correctly rounded x*2^p }
  DoubleCases: array[1..18] of TDoubleCase = (
    (x: QWord($3FF0000000000000); p: -1030; ref: QWord($0000100000000000)), { 1.0, -1030 -> 8.691694759794e-311 }
    (x: QWord($0DF0000000000000); p: 1500; ref: QWord($6BB0000000000000)), { 2^-800, 1500 -> 2^700 }
    (x: QWord($6BB0000000000000); p: -1200; ref: QWord($20B0000000000000)), { 2^700, -1200 -> 2^-500 }
    (x: QWord($BFF0000000000000); p: -1030; ref: QWord($8000100000000000)), { -1.0, -1030 -> -8.691694759794e-311 }
    (x: QWord($3FF0000000000000); p: -1074; ref: QWord($0000000000000001)), { 1.0, -1074 -> 5e-324 }
    (x: QWord($3FF0000000000000); p: -1075; ref: QWord($0000000000000000)), { 1.0, -1075 -> 0.0 }
    (x: QWord($3FF8000000000000); p: -1075; ref: QWord($0000000000000001)), { 1.5, -1075 -> 5e-324 }
    (x: QWord($4008000000000000); p: -1075; ref: QWord($0000000000000002)), { 3.0, -1075 -> 1e-323 }
    (x: QWord($3FF0000000000000); p: -1076; ref: QWord($0000000000000000)), { 1.0, -1076 -> 0.0 }
    (x: QWord($3FEFFFFFFFFFFFFF); p: -1022; ref: QWord($0010000000000000)), { 1-2^-53, -1022 -> 2.2250738585072014e-308 }
    (x: QWord($3FEFFFFFFFFFFFFE); p: -1022; ref: QWord($000FFFFFFFFFFFFF)), { 1-2^-52, -1022 -> 2.225073858507201e-308 }
    (x: QWord($0000000000000001); p: 1074; ref: QWord($3FF0000000000000)), { 5e-324, 1074 -> 1.0 }
    (x: QWord($0000000000000001); p: -1; ref: QWord($0000000000000000)), { 5e-324, -1 -> 0.0 }
    (x: QWord($3FF0000000000000); p: 1023; ref: QWord($7FE0000000000000)), { 1.0, 1023 -> 8.98846567431158e+307 }
    (x: QWord($3FE8000000000000); p: 1025; ref: QWord($7FF0000000000000)), { 0.75, 1025 -> +Inf }
    (x: QWord($3FF0000000000000); p: 1024; ref: QWord($7FF0000000000000)), { 1.0, 1024 -> +Inf }
    (x: QWord($BFF0000000000000); p: 2000; ref: QWord($FFF0000000000000)), { -1.0, 2000 -> -Inf }
    (x: QWord($3FB999999999999A); p: -1040; ref: QWord($0000000066666666))  { 0.1, -1040 -> 8.48798316e-315 }
  );
  SingleCases: array[1..15] of TSingleCase = (
    (x: $3F800000; p: -140; ref: $00000200), { 1.0, -140 }
    (x: $0D800000; p: 150; ref: $58800000), { 2^-100, 150 -> 2^50 }
    (x: $71800000; p: -150; ref: $26800000), { 2^100, -150 -> 2^-50 }
    (x: $BF800000; p: -140; ref: $80000200), { -1.0, -140 }
    (x: $3F800000; p: -149; ref: $00000001), { 1.0, -149 }
    (x: $3F800000; p: -150; ref: $00000000), { 1.0, -150 }
    (x: $3FC00000; p: -150; ref: $00000001), { 1.5, -150 }
    (x: $40400000; p: -150; ref: $00000002), { 3.0, -150 }
    (x: $3F800000; p: -151; ref: $00000000), { 1.0, -151 }
    (x: $3F7FFFFF; p: -126; ref: $00800000), { 1-2^-24, -126 -> smallest normal }
    (x: $3F7FFFFE; p: -126; ref: $007FFFFF), { 1-2^-23, -126 }
    (x: $00000001; p: 149; ref: $3F800000), { 2^-149, 149 -> 1.0 }
    (x: $3F800000; p: 127; ref: $7F000000), { 1.0, 127 }
    (x: $3F800000; p: 128; ref: $7F800000), { 1.0, 128 -> +Inf }
    (x: $3DCCCCCD; p: -130; ref: $0000CCCD)  { 0.1, -130 }
  );


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Checks the Double overload of Ldexp against DoubleCases
procedure CheckDouble(aCode: Integer);

var
  i: Integer;
  lX, lValue: Double;
  lGot: QWord;

begin
  for i:=Low(DoubleCases) to High(DoubleCases) do
    begin
    Move(DoubleCases[i].x,lX,SizeOf(lX));
    lValue:=Ldexp(lX,DoubleCases[i].p);
    Move(lValue,lGot,SizeOf(lGot));
    Check(aCode+i,lGot=DoubleCases[i].ref,Format('Double Ldexp($%.16x,%d)=$%.16x, expected $%.16x',
      [DoubleCases[i].x,DoubleCases[i].p,lGot,DoubleCases[i].ref]));
    end;
end;


// Checks the Single overload of Ldexp against SingleCases
procedure CheckSingle(aCode: Integer);

var
  i: Integer;
  lX, lValue: Single;
  lGot: LongWord;

begin
  for i:=Low(SingleCases) to High(SingleCases) do
    begin
    Move(SingleCases[i].x,lX,SizeOf(lX));
    lValue:=Ldexp(lX,SingleCases[i].p);
    Move(lValue,lGot,SizeOf(lGot));
    Check(aCode+i,lGot=SingleCases[i].ref,Format('Single Ldexp($%.8x,%d)=$%.8x, expected $%.8x',
      [SingleCases[i].x,SingleCases[i].p,lGot,SingleCases[i].ref]));
    end;
end;


{$ifdef FPC_HAS_TYPE_EXTENDED}
// Checks the Extended overload of Ldexp against exact powers of two
procedure CheckExtended(aCode: Integer);

var
  lOne, lHalf, lTwo, lX: Extended;

begin
  lOne:=1;
  lHalf:=0.5;
  lTwo:=2;
  Check(aCode+1,Ldexp(lOne,-16400)=IntPower(lHalf,16400),'Extended Ldexp(1,-16400)');
  Check(aCode+2,Ldexp(Ldexp(lOne,-8000),15000)=IntPower(lTwo,7000),'Extended Ldexp(Ldexp(1,-8000),15000)');
  Check(aCode+3,Ldexp(Ldexp(lOne,8000),-15000)=IntPower(lHalf,7000),'Extended Ldexp(Ldexp(1,8000),-15000)');
  Check(aCode+4,Ldexp(lOne,-16445)=IntPower(lHalf,16445),'Extended Ldexp(1,-16445) is the smallest subnormal');
  Check(aCode+5,Ldexp(lOne,-16446)=0,'Extended Ldexp(1,-16446) is 0');
  lX:=1.5;
  Check(aCode+8,Ldexp(lX,-16446)=IntPower(lHalf,16445),'Extended Ldexp(1.5,-16446) is the smallest subnormal');
  Check(aCode+9,Ldexp(lX,-16447)=0,'Extended Ldexp(1.5,-16447) is 0');
  lX:=lOne-Ldexp(lOne,-64);
  Check(aCode+6,Ldexp(lX,-16382)=IntPower(lHalf,16382),'Extended Ldexp(1-2^-64,-16382) is the smallest normal');
  Check(aCode+7,Ldexp(lOne,16384)=Infinity,'Extended Ldexp(1,16384) is +Inf');
end;
{$endif}


var
  lZero, lX, lValue: Double;
  lBits: QWord;

begin
  CheckDouble(100);
  CheckSingle(200);
{$ifdef FPC_HAS_TYPE_EXTENDED}
  CheckExtended(300);
{$endif}
  lZero:=0;
  lX:=-lZero;
  lValue:=Ldexp(lX,5);
  Move(lValue,lBits,SizeOf(lBits));
  Check(10,lBits=QWord($8000000000000000),'Ldexp(-0,5) is -0');
  lX:=Infinity;
  Check(11,Ldexp(lX,-5000)=Infinity,'Ldexp(+Inf,-5000) is +Inf');
  lX:=NaN;
  Check(12,IsNan(Ldexp(lX,3)),'Ldexp(NaN,3) is NaN');
  writeln('ok');
end.
