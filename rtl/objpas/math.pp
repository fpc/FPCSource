{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit is an equivalent to the Delphi math unit
  (with some improvements)

  What's to do:
    o a lot of function :), search for !!!!
    o some statistical functions
    o all financial functions
    o optimizations
}

unit math;
interface

{$MODE objfpc}
{$ifdef VER1_0}
  { we don't assume cross compiling from 1.0.x-m68k ... }
  {$define FPC_HAS_TYPE_EXTENDED}
{$endif VER1_0}

    uses
       sysutils;

    const { Ranges of the IEEE floating point types, including denormals }
{$ifdef FPC_HAS_TYPE_SINGLE}
      MinSingle    =  1.5e-45;
      MaxSingle    =  3.4e+38;
{$endif FPC_HAS_TYPE_SINGLE}
{$ifdef FPC_HAS_TYPE_DOUBLE}
      MinDouble    =  5.0e-324;
      MaxDouble    =  1.7e+308;
{$endif FPC_HAS_TYPE_DOUBLE}
{$ifdef FPC_HAS_TYPE_EXTENDED}
      MinExtended  =  3.4e-4932;
      MaxExtended  =  1.1e+4932;
{$endif FPC_HAS_TYPE_EXTENDED}
{$ifdef FPC_HAS_TYPE_COMP}
      MinComp      = -9.223372036854775807e+18;
      MaxComp      =  9.223372036854775807e+18;
{$endif FPC_HAS_TYPE_COMP}

    type
       { the original delphi functions use extended as argument, }
       { but I would prefer double, because 8 bytes is a very    }
       { natural size for the processor                          }
       { WARNING : changing float type will                      }
       { break all assembler code  PM                            }
{$ifdef FPC_HAS_TYPE_FLOAT128}
         float = float128;

      const
         MinFloat = MinFloat128;
         MaxFloat = MaxFloat128;
{$else FPC_HAS_TYPE_FLOAT128}
  {$ifdef FPC_HAS_TYPE_EXTENDED}
         float = extended;

      const
         MinFloat = MinExtended;
         MaxFloat = MaxExtended;
  {$else FPC_HAS_TYPE_EXTENDED}
    {$ifdef FPC_HAS_TYPE_DOUBLE}
         float = double;

      const
         MinFloat = MinDouble;
         MaxFloat = MaxDouble;
    {$else FPC_HAS_TYPE_DOUBLE}
      {$ifdef FPC_HAS_TYPE_SINGLE}
         float = single;

      const
         MinFloat = MinSingle;
         MaxFloat = MaxSingle;
      {$else FPC_HAS_TYPE_SINGLE}
        {$error At least one floating point type must be supported}
      {$endif FPC_HAS_TYPE_SINGLE}
    {$endif FPC_HAS_TYPE_DOUBLE}
  {$endif FPC_HAS_TYPE_EXTENDED}
{$endif FPC_HAS_TYPE_FLOAT128}

    type
       PFloat = ^Float;
       PInteger = ^Integer;

       tpaymenttime = (ptendofperiod,ptstartofperiod);

       einvalidargument = class(ematherror);

       TValueRelationship = -1..1;

    const
       EqualsValue = 0;
       LessThanValue = Low(TValueRelationship);
       GreaterThanValue = High(TValueRelationship);

{ Min/max determination }
function MinIntValue(const Data: array of Integer): Integer;
function MaxIntValue(const Data: array of Integer): Integer;

{ Extra, not present in Delphi, but used frequently  }
function Min(a, b: Integer): Integer;
function Max(a, b: Integer): Integer;
function Min(a, b: Cardinal): Cardinal;
function Max(a, b: Cardinal): Cardinal;
function Min(a, b: Int64): Int64;
function Max(a, b: Int64): Int64;
function Min(a, b: Single): Single;
function Max(a, b: Single): Single;
function Min(a, b: Double): Double;
function Max(a, b: Double): Double;
function Min(a, b: Extended): Extended;
function Max(a, b: Extended): Extended;

{ angle conversion }

function degtorad(deg : float) : float;
function radtodeg(rad : float) : float;
function gradtorad(grad : float) : float;
function radtograd(rad : float) : float;
function degtograd(deg : float) : float;
function gradtodeg(grad : float) : float;
{ one cycle are 2*Pi rad }
function cycletorad(cycle : float) : float;
function radtocycle(rad : float) : float;

{ trigoniometric functions }

function tan(x : float) : float;
function cotan(x : float) : float;
procedure sincos(theta : float;var sinus,cosinus : float);

{ inverse functions }

function arccos(x : float) : float;
function arcsin(x : float) : float;

{ calculates arctan(x/y) and returns an angle in the correct quadrant }
function arctan2(x,y : float) : float;

{ hyperbolic functions }

function cosh(x : float) : float;
function sinh(x : float) : float;
function tanh(x : float) : float;

{ area functions }

{ delphi names: }
function arccosh(x : float) : float;
function arcsinh(x : float) : float;
function arctanh(x : float) : float;
{ IMHO the function should be called as follows (FK) }
function arcosh(x : float) : float;
function arsinh(x : float) : float;
function artanh(x : float) : float;

{ triangle functions }

{ returns the length of the hypotenuse of a right triangle }
{ if x and y are the other sides                           }
function hypot(x,y : float) : float;

{ logarithm functions }

function log10(x : float) : float;
function log2(x : float) : float;
function logn(n,x : float) : float;

{ returns natural logarithm of x+1 }
function lnxp1(x : float) : float;

{ exponential functions }

function power(base,exponent : float) : float;
{ base^exponent }
function intpower(base : float;const exponent : Integer) : float;

{ number converting }

{ rounds x towards positive infinity }
function ceil(x : float) : Integer;
{ rounds x towards negative infinity }
function floor(x : float) : Integer;

{ misc. functions }

{ splits x into mantissa and exponent (to base 2) }
procedure Frexp(X: float; var Mantissa: float; var Exponent: integer);
{ returns x*(2^p) }
function ldexp(x : float; const p : Integer) : float;

{ statistical functions }

function mean(const data : array of float) : float;
function sum(const data : array of float) : float;
function mean(const data : PFloat; Const N : longint) : float;
function sum(const data : PFloat; Const N : Longint) : float;
function sumofsquares(const data : array of float) : float;
function sumofsquares(const data : PFloat; Const N : Integer) : float;
{ calculates the sum and the sum of squares of data }
procedure sumsandsquares(const data : array of float;
  var sum,sumofsquares : float);
procedure sumsandsquares(const data : PFloat; Const N : Integer;
  var sum,sumofsquares : float);
function minvalue(const data : array of float) : float;
function minvalue(const data : array of integer) : Integer;
function minvalue(const data : PFloat; Const N : Integer) : float;
function MinValue(const Data : PInteger; Const N : Integer): Integer;
function maxvalue(const data : array of float) : float;
function maxvalue(const data : array of integer) : Integer;
function maxvalue(const data : PFloat; Const N : Integer) : float;
function maxvalue(const data : PInteger; Const N : Integer) : Integer;
{ calculates the standard deviation }
function stddev(const data : array of float) : float;
function stddev(const data : PFloat; Const N : Integer) : float;
{ calculates the mean and stddev }
procedure meanandstddev(const data : array of float;
  var mean,stddev : float);
procedure meanandstddev(const data : PFloat;
  Const N : Longint;var mean,stddev : float);
function variance(const data : array of float) : float;
function totalvariance(const data : array of float) : float;
function variance(const data : PFloat; Const N : Integer) : float;
function totalvariance(const data : PFloat; Const N : Integer) : float;
{ returns random values with gaussian distribution }
function randg(mean,stddev : float) : float;

{ I don't know what the following functions do: }
function popnstddev(const data : array of float) : float;
function popnstddev(const data : PFloat; Const N : Integer) : float;
function popnvariance(const data : PFloat; Const N : Integer) : float;
function popnvariance(const data : array of float) : float;
procedure momentskewkurtosis(const data : array of float;
  var m1,m2,m3,m4,skew,kurtosis : float);
procedure momentskewkurtosis(const data : PFloat; Const N : Integer;
  var m1,m2,m3,m4,skew,kurtosis : float);

{ geometrical function }

{ returns the euclidean L2 norm }
function norm(const data : array of float) : float;
function norm(const data : PFloat; Const N : Integer) : float;

{ include cpu specific stuff }
{$i mathuh.inc}

implementation

ResourceString
  SMathError = 'Math Error : %s';
  SInvalidArgument = 'Invalid argument';

Procedure DoMathError(Const S : String);
begin
  Raise EMathError.CreateFmt(SMathError,[S]);
end;

Procedure InvalidArgument;

begin
  Raise EInvalidArgument.Create(SInvalidArgument);
end;

function degtorad(deg : float) : float;

  begin
     degtorad:=deg*(pi/180.0);
  end;

function radtodeg(rad : float) : float;

  begin
     radtodeg:=rad*(180.0/pi);
  end;

function gradtorad(grad : float) : float;

  begin
     gradtorad:=grad*(pi/200.0);
  end;

function radtograd(rad : float) : float;

  begin
     radtograd:=rad*(200.0/pi);
  end;

function degtograd(deg : float) : float;

  begin
     degtograd:=deg*(200.0/180.0);
  end;

function gradtodeg(grad : float) : float;

  begin
     gradtodeg:=grad*(180.0/200.0);
  end;

function cycletorad(cycle : float) : float;

  begin
     cycletorad:=(2*pi)*cycle;
  end;

function radtocycle(rad : float) : float;

  begin
     { avoid division }
     radtocycle:=rad*(1/(2*pi));
  end;

function tan(x : float) : float;

  begin
     Tan:=Sin(x)/Cos(x)
  end;

function cotan(x : float) : float;

  begin
     cotan:=Cos(X)/Sin(X);
  end;

procedure sincos(theta : float;var sinus,cosinus : float);

  begin
    sinus:=sin(theta);
    cosinus:=cos(theta);
  end;

{ Sign, ArcSin and ArcCos from Arjan van Dijk (arjan.vanDijk@User.METAIR.WAU.NL) }

function sign(x : float) : float;
begin
  if      x > 0 then sign :=  1.0
  else if x < 0 then sign := -1.0
  else               sign :=  0.0;
end;

function arcsin(x : float) : float;
begin
  if abs(x) > 1 then InvalidArgument
  else if abs(x) < 0.5 then
    arcsin := arctan(x/sqrt(1-sqr(x)))
  else
    arcsin := sign(x) * (pi*0.5 - arctan(sqrt(1 / sqr(x) - 1)));
end;

function Arccos(x : Float) : Float;
begin
  arccos := pi*0.5 - arcsin(x);
end;


function arctan2( x,y : float) : float;
  begin
     ArcTan2:=ArcTan(x/y);
  end;

function cosh(x : float) : float;

  var
     temp : float;

  begin
     temp:=exp(x);
     cosh:=0.5*(temp+1.0/temp);
  end;

function sinh(x : float) : float;

  var
     temp : float;

  begin
     temp:=exp(x);
     sinh:=0.5*(temp-1.0/temp);
  end;

Const MaxTanh = 5678.22249441322; // Ln(MaxExtended)/2

function tanh(x : float) : float;

  var Temp : float;

  begin
     if x>MaxTanh then exit(1.0)
     else if x<-MaxTanh then exit (-1.0);
     temp:=exp(-2*x);
     tanh:=(1-temp)/(1+temp)
  end;

function arccosh(x : float) : float;

  begin
     arccosh:=arcosh(x);
  end;

function arcsinh(x : float) : float;

  begin
     arcsinh:=arsinh(x);
  end;

function arctanh(x : float) : float;

  begin
     if x>1 then InvalidArgument;
     arctanh:=artanh(x);
  end;

function arcosh(x : float) : float;

  begin
     if x<1 then InvalidArgument;
     arcosh:=Ln(x+Sqrt(x*x-1));
  end;

function arsinh(x : float) : float;

  begin
     arsinh:=Ln(x+Sqrt(1+x*x));
  end;

function artanh(x : float) : float;
  begin
    If abs(x)>1 then InvalidArgument;
    artanh:=(Ln((1+x)/(1-x)))*0.5;
  end;

function hypot(x,y : float) : float;

  begin
     hypot:=Sqrt(x*x+y*y)
  end;

function log10(x : float) : float;

  begin
     log10:=ln(x)/ln(10);
  end;

function log2(x : float) : float;

  begin
     log2:=ln(x)/ln(2)
  end;

function logn(n,x : float) : float;

  begin
     if n<0 then InvalidArgument;
     logn:=ln(x)/ln(n);
  end;

function lnxp1(x : float) : float;

  begin
     if x<-1 then
       InvalidArgument;
     lnxp1:=ln(1+x);
  end;

function power(base,exponent : float) : float;

  begin
    If Exponent=0.0 then
      Result:=1.0
    else
      If base>0.0 then
        Power:=exp(exponent * ln (base))
      else if base=0.0 then
        Result:=0.0
      else
        InvalidArgument
  end;

function intpower(base : float;const exponent : Integer) : float;

  var
     i : longint;

  begin
     i:=abs(exponent);
     intpower:=1.0;
     while i>0 do
       begin
          while (i and 1)=0 do
            begin
               i:=i shr 1;
               base:=sqr(base);
            end;
          i:=i-1;
          intpower:=intpower*base;
       end;
     if exponent<0 then
       intpower:=1.0/intpower;
  end;

function ceil(x : float) : integer;

  begin
    Ceil:=Trunc(x);
    If Frac(x)>0 then
      Ceil:=Ceil+1;
  end;

function floor(x : float) : integer;

  begin
     Floor:=Trunc(x);
     If Frac(x)<0 then
       Floor := Floor-1;
  end;

procedure Frexp(X: float; var Mantissa: float; var Exponent: integer);

  begin
      Exponent :=0;
      if (abs(x)<0.5) then
       While (abs(x)<0.5) do
       begin
         x := x*2;
         Dec(Exponent);
       end
      else
       While (abs(x)>1) do
       begin
         x := x/2;
         Inc(Exponent);
       end;
      mantissa := x;
  end;

function ldexp(x : float;const p : Integer) : float;

  begin
     ldexp:=x*intpower(2.0,p);
  end;

function mean(const data : array of float) : float;

  begin
     Result:=Mean(@data[0],High(Data)+1);
  end;

function mean(const data : PFloat; Const N : longint) : float;

  begin
     mean:=sum(Data,N);
     mean:=mean/N;
  end;

function sum(const data : array of float) : float;

  begin
     Result:=Sum(@Data[0],High(Data)+1);
  end;

function sum(const data : PFloat;Const N : longint) : float;

  var
     i : longint;

  begin
     sum:=0.0;
     for i:=0 to N-1 do
       sum:=sum+data[i];
  end;

 function sumofsquares(const data : array of float) : float;

 begin
   Result:=sumofsquares(@data[0],High(Data)+1);
 end;

 function sumofsquares(const data : PFloat; Const N : Integer) : float;

  var
     i : longint;

  begin
     sumofsquares:=0.0;
     for i:=0 to N-1 do
       sumofsquares:=sumofsquares+sqr(data[i]);
  end;

procedure sumsandsquares(const data : array of float;
  var sum,sumofsquares : float);

begin
  sumsandsquares (@Data[0],High(Data)+1,Sum,sumofsquares);
end;

procedure sumsandsquares(const data : PFloat; Const N : Integer;
  var sum,sumofsquares : float);

  var
     i : Integer;
     temp : float;

  begin
     sumofsquares:=0.0;
     sum:=0.0;
     for i:=0 to N-1 do
       begin
          temp:=data[i];
          sumofsquares:=sumofsquares+sqr(temp);
          sum:=sum+temp;
       end;
  end;



function stddev(const data : array of float) : float;

begin
  Result:=Stddev(@Data[0],High(Data)+1)
end;

function stddev(const data : PFloat; Const N : Integer) : float;

  begin
     StdDev:=Sqrt(Variance(Data,N));
  end;

procedure meanandstddev(const data : array of float;
  var mean,stddev : float);

begin
  Meanandstddev(@Data[0],High(Data)+1,Mean,stddev);
end;

procedure meanandstddev(const data : PFloat;
  Const N : Longint;var mean,stddev : float);

Var I : longint;

begin
  Mean:=0;
  StdDev:=0;
  For I:=0 to N-1 do
    begin
    Mean:=Mean+Data[i];
    StdDev:=StdDev+Sqr(Data[i]);
    end;
  Mean:=Mean/N;
  StdDev:=(StdDev-N*Sqr(Mean));
  If N>1 then
    StdDev:=Sqrt(Stddev/(N-1))
  else
    StdDev:=0;
end;

function variance(const data : array of float) : float;

  begin
     Variance:=Variance(@Data[0],High(Data)+1);
  end;

function variance(const data : PFloat; Const N : Integer) : float;

  begin
     If N=1 then
       Result:=0
     else
       Result:=TotalVariance(Data,N)/(N-1);
  end;

function totalvariance(const data : array of float) : float;

begin
  Result:=TotalVariance(@Data[0],High(Data)+1);
end;

function totalvariance(const data : Pfloat;Const N : Integer) : float;

   var S,SS : Float;

  begin
    If N=1 then
      Result:=0
    else
      begin
      SumsAndSquares(Data,N,S,SS);
      Result := SS-Sqr(S)/N;
      end;
  end;

function randg(mean,stddev : float) : float;

  Var U1,S2 : Float;

  begin
     repeat
       u1:= 2*random-1;
       S2:=Sqr(U1)+sqr(2*random-1);
     until s2<1;
     randg:=Sqrt(-2*ln(S2)/S2)*u1*stddev+Mean;
  end;

function popnstddev(const data : array of float) : float;

  begin
     PopnStdDev:=Sqrt(PopnVariance(@Data[0],High(Data)+1));
  end;

function popnstddev(const data : PFloat; Const N : Integer) : float;

  begin
     PopnStdDev:=Sqrt(PopnVariance(Data,N));
  end;

function popnvariance(const data : array of float) : float;

begin
  popnvariance:=popnvariance(@data[0],high(Data)+1);
end;

function popnvariance(const data : PFloat; Const N : Integer) : float;

  begin
     PopnVariance:=TotalVariance(Data,N)/N;
  end;

procedure momentskewkurtosis(const data : array of float;
  var m1,m2,m3,m4,skew,kurtosis : float);

begin
  momentskewkurtosis(@Data[0],High(Data)+1,m1,m2,m3,m4,skew,kurtosis);
end;

procedure momentskewkurtosis(const data : PFloat; Const N : Integer;
  var m1,m2,m3,m4,skew,kurtosis : float);

  Var S,SS,SC,SQ,invN,Acc,M1S,S2N,S3N,temp : Float;
      I : Longint;

  begin
     invN:=1.0/N;
     s:=0;
     ss:=0;
     sq:=0;
     sc:=0;
     for i:=0 to N-1 do
       begin
       temp:=Data[i];   { faster }
       S:=S+temp;
       acc:=temp*temp;
       ss:=ss+acc;
       Acc:=acc*temp;
       Sc:=sc+acc;
       acc:=acc*temp;
       sq:=sq+acc;
       end;
     M1:=s*invN;
     M1S:=M1*M1;
     S2N:=SS*invN;
     S3N:=SC*invN;
     M2:=S2N-M1S;
     M3:=S3N-(M1*3*S2N) + 2*M1S*M1;
     M4:=SQ*invN - (M1 * 4 * S3N) + (M1S*6*S2N-3*Sqr(M1S));
     Skew:=M3*power(M2,-3/2);
     Kurtosis:=M4 / Sqr(M2);
  end;

function norm(const data : array of float) : float;

  begin
     norm:=Norm(@data[0],High(Data)+1);
  end;

function norm(const data : PFloat; Const N : Integer) : float;

  begin
     norm:=sqrt(sumofsquares(data,N));
  end;


function MinIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] < Result Then Result := Data[I];
end;

function MinValue(const Data: array of Integer): Integer;

begin
  Result:=MinValue(Pinteger(@Data[0]),High(Data)+1)
end;

function MinValue(const Data: PInteger; Const N : Integer): Integer;
var
  I: Integer;
begin
  Result := Data[0];
  For I := 1 To N-1 do
    If Data[I] < Result Then Result := Data[I];
end;


function minvalue(const data : array of float) : float;

begin
   Result:=minvalue(PFloat(@data[0]),High(Data)+1);
end;

function minvalue(const data : PFloat; Const N : Integer) : float;

var
   i : longint;

begin
   { get an initial value }
   minvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]<minvalue then
       minvalue:=data[i];
end;

function MaxIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] > Result Then Result := Data[I];
end;

function maxvalue(const data : array of float) : float;

begin
   Result:=maxvalue(PFloat(@data[0]),High(Data)+1);
end;

function maxvalue(const data : PFloat; Const N : Integer) : float;

var
   i : longint;

begin
   { get an initial value }
   maxvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]>maxvalue then
       maxvalue:=data[i];
end;

function MaxValue(const Data: array of Integer): Integer;

begin
  Result:=MaxValue(PInteger(@Data[0]),High(Data)+1)
end;

function maxvalue(const data : PInteger; Const N : Integer) : Integer;

var
   i : longint;

begin
   { get an initial value }
   maxvalue:=data[0];
   for i:=1 to N-1 do
     if data[i]>maxvalue then
       maxvalue:=data[i];
end;


function Min(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Cardinal): Cardinal;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Cardinal): Cardinal;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Int64): Int64;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Int64): Int64;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Single): Single;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Single): Single;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Double): Double;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Double): Double;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Extended): Extended;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Extended): Extended;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

{ include cpu specific stuff }
{$i mathu.inc}

end.
{
  $Log$
  Revision 1.10  2003-04-24 09:21:59  florian
    + moved cpu dependend code to mathuh.inc and mathu.inc

  Revision 1.9  2003/01/03 20:34:02  peter
    * i386 fpu controlword functions added

  Revision 1.8  2002/09/07 21:06:12  carl
    * cleanup of parameters
    - remove assembler code

  Revision 1.7  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

}
