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

  About assembler usage:
  ----------------------
  I used as few as possible assembler to allow an easy port
  to other processors. Today, I think it's wasted time to write
  assembler because different versions of a family of processors
  need different implementations.

  To improve performance, I changed all integer arguments and
  functions results to longint, because 16 bit instructions are
  lethal for a modern intel processor.
                                                      (FK)

  What's to do:
    o a lot of function :), search for !!!!
    o some statistical functions
    o all financial functions
    o optimizations
}

unit math;
interface

{$MODE objfpc}

    uses
       sysutils;

    type
       { the original delphi functions use extended as argument, }
       { but I would prefer double, because 8 bytes is a very    }
       { natural size for the processor                          }
       float = extended;

       tpaymenttime = (ptendofperiod,ptstartofperiod);

       einvalidargument = class(ematherror);

{ Min/max determination }
function MinIntValue(const Data: array of Integer): Integer;
function MaxIntValue(const Data: array of Integer): Integer;

{ Extra, not present in Delphi, but used frequently  }
function Min(Int1,Int2:Integer):Integer;
function Min(Int1,Int2:Cardinal):Cardinal;
function Max(Int1,Int2:Integer):Integer;
function Max(Int1,Int2:Cardinal):Cardinal;

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
function lnxpi(x : float) : float;

{ exponential functions }

function power(base,exponent : float) : float;
{ base^exponent }
function intpower(base : float;exponent : longint) : float;

{ number converting }

{ rounds x towards positive infinity }
function ceil(x : float) : longint;
{ rounds x towards negative infinity }
function floor(x : float) : longint;

{ misc. functions }

{ splits x into mantissa and exponent (to base 2) }
procedure frexp(x : float;var mantissa,exponent : float);
{ returns x*(2^p) }
function ldexp(x : float;p : longint) : float;

{ statistical functions }

function mean(const data : array of float) : float;
function sum(const data : array of float) : float;
function sumofsquares(const data : array of float) : float;
{ calculates the sum and the sum of squares of data }
procedure sumsandsquares(const data : array of float;
  var sum,sumofsquares : float);
function minvalue(const data : array of float) : float;
function maxvalue(const data : array of float) : float;
{ calculates the standard deviation }
function stddev(const data : array of float) : float;
{ calculates the mean and stddev }
procedure meanandstddev(const data : array of float;
  var mean,stddev : float);
function variance(const data : array of float) : float;
function totalvariance(const data : array of float) : float;
{ returns random values with gaussian distribution }
function randg(mean,stddev : float) : float;

{ I don't know what the following functions do: }
function popnstddev(const data : array of float) : float;
function popnvariance(const data : array of float) : float;
procedure momentskewkurtosis(const data : array of float;
  var m1,m2,m3,m4,skew,kurtosis : float);

{ geometrical function }

{ returns the euclidean L2 norm }
function norm(const data : array of float) : float;

implementation

Procedure DoMathError(Const S : String);
begin
  writeln (StdErr,'Math Error : ',S);
end;

Procedure InvalidArgument;

begin
  DoMathError ('Invalid argument');
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
  {$ifndef i386}
  sinus:=sin(theta);
  cosinus:=cos(theta);
  {$else}
  asm
    fldl theta
    fsincos
    fwait
    movl cosinus,%eax
    fstpl (%eax)
    movl sinus,%eax
    fstpl (%eax)
  end;
  {$endif}
  end;

function arccos(x : float) : float;

{ There is some discussion as to what the correct formula is
  for arccos and arcsin is, but I take the one from my book...}

  begin
     ArcCos:=ArcTan2(Sqrt(1-x*x),x);
  end;

function arcsin(x : float) : float;

  begin
     ArcSin:=ArcTan2(x,Sqrt(1-x*x))
  end;

function arctan2( x,y : float) : float;

  begin
  {$ifndef i386}
  ArcTan2:=ArcTan(x/y);
  {$else}
    asm
    fldt X
    fldt Y
    fpatan
    leave
    ret $20
    end;
  {$endif}
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

Const MaxTanh=5000; { rather arbitrary, but more or less correct }

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
     arsinh:=Ln(x-Sqrt(1+x*x));
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

function lnxpi(x : float) : float;

  begin
     lnxpi:=ln(1+x);
  end;

function power(base,exponent : float) : float;

  begin
     Power:=exp(exponent * ln (base));
  end;

function intpower(base : float;exponent : longint) : float;

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

function ceil(x : float) : longint;

  begin
    Ceil:=Trunc(x);
    If Frac(x)>0 then
      Ceil:=Ceil+1;
  end;

function floor(x : float) : longint;

  begin
     Floor:=Trunc(x);
     If Frac(x)<0 then
       Floor := Floor-1;
  end;

procedure frexp(x : float;var mantissa,exponent : float);

  begin
     { !!!!!!! }
  end;

function ldexp(x : float;p : longint) : float;

  begin
     ldexp:=x*intpower(2.0,p);
  end;

function mean(const data : array of float) : float;

  begin
     mean:=sum(data);
     mean:=mean/(high(data)-low(data)+1);
  end;

function sum(const data : array of float) : float;

  var
     i : longint;

  begin
     sum:=0.0;
     for i:=low(data) to high(data) do
       sum:=sum+data[i];
  end;

function sumofsquares(const data : array of float) : float;

  var
     i : longint;

  begin
     sumofsquares:=0.0;
     for i:=low(data) to high(data) do
       sumofsquares:=sumofsquares+sqr(data[i]);
  end;

procedure sumsandsquares(const data : array of float;
  var sum,sumofsquares : float);

  var
     i : longint;
     temp : float;

  begin
     sumofsquares:=0.0;
     sum:=0.0;
     for i:=low(data) to high(data) do
       begin
          temp:=data[i];
          sumofsquares:=sumofsquares+sqr(temp);
          sum:=sum+temp;
       end;
  end;

function minvalue(const data : array of float) : float;

  var
     i : longint;

  begin
     { get an initial value }
     minvalue:=data[low(data)];
     for i:=low(data) to high(data) do
       if data[i]<minvalue then
         minvalue:=data[i];
  end;

function maxvalue(const data : array of float) : float;

  var
     i : longint;

  begin
     { get an initial value }
     maxvalue:=data[low(data)];
     for i:=low(data) to high(data) do
       if data[i]>maxvalue then
         maxvalue:=data[i];
  end;

function stddev(const data : array of float) : float;

  begin
     StdDev:=Sqrt(Variance(Data));
  end;

procedure meanandstddev(const data : array of float;
  var mean,stddev : float);

  begin

  end;

function variance(const data : array of float) : float;

  begin
     Variance:=TotalVariance(Data)/(High(Data)-Low(Data));
  end;

function totalvariance(const data : array of float) : float;

   var S,SS : Float;

  begin
     SumsAndSquares(Data,S,SS);
     TotalVariance := SS-Sqr(S)/(High(Data)-Low(Data));
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
     PopnStdDev:=Sqrt(PopnVariance(Data));
  end;

function popnvariance(const data : array of float) : float;

  begin
     PopnVariance:=TotalVariance(Data)/(High(Data)-Low(Data)+1);
  end;

procedure momentskewkurtosis(const data : array of float;
  var m1,m2,m3,m4,skew,kurtosis : float);

  Var S,SS,SC,SQ,invN,Acc,M1S,S2N,S3N,temp : Float;
      I : Longint;

  begin
     invN:=1.0/(High(Data)-Low(Data)+1);
     s:=0;
     ss:=0;
     sq:=0;
     sc:=0;
     for i:=Low(Data) to High(Data) do
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
     norm:=sqrt(sumofsquares(data));
  end;


function MinIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] < Result Then Result := Data[I];
end;

function MaxIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  For I := Succ(Low(Data)) To High(Data) Do
    If Data[I] > Result Then Result := Data[I];
end;

function Min(Int1,Int2:Integer):Integer;
begin
  If Int1 < Int2 Then Result := Int1
                 Else Result := Int2;
end;

function Min(Int1,Int2:Cardinal):Cardinal;
begin
  If Int1 < Int2 Then Result := Int1
                 Else Result := Int2;
end;

function Max(Int1,Int2:Integer):Integer;
begin
  If Int1 > Int2 Then Result := Int1
                 Else Result := Int2;
end;

function Max(Int1,Int2:Cardinal):Cardinal;
begin
  If Int1 > Int2 Then Result := Int1
                 Else Result := Int2;
end;


end.
{
    $Log$
    Revision 1.15  2000-02-09 16:59:32  peter
      * truncated log

    Revision 1.14  2000/01/11 21:07:33  marco
     * Changed some (%ebp) to real parameters

    Revision 1.13  2000/01/07 16:41:43  daniel
      * copyright 2000

    Revision 1.12  1999/09/21 20:47:05  florian
      * ceil and floor still had bugs :), hopefully it's the final fix now

}
