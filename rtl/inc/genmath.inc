{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Several contributors

    Generic mathemtical routines (on type real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  Credits                                                                }
{*************************************************************************}
{       Copyright Abandoned, 1987, Fred Fish                              }
{                                                                         }
{       This previously copyrighted work has been placed into the         }
{       public domain by the author (Fred Fish) and may be freely used    }
{       for any purpose, private or commercial.  I would appreciate       }
{       it, as a courtesy, if this notice is left in all copies and       }
{       derivative works.  Thank you, and enjoy...                        }
{                                                                         }
{       The author makes no warranty of any kind with respect to this     }
{       product and explicitly disclaims any implied warranties of        }
{       merchantability or fitness for any particular purpose.            }
{-------------------------------------------------------------------------}
{       Copyright (c) 1992 Odent Jean Philippe                            }
{                                                                         }
{       The source can be modified as long as my name appears and some    }
{       notes explaining the modifications done are included in the file. }
{-------------------------------------------------------------------------}
{       Copyright (c) 1997 Carl Eric Codere                               }
{-------------------------------------------------------------------------}

{$goto on}

type
    TabCoef = array[0..6] of Real;


const
      PIO2   =  1.57079632679489661923;       {  pi/2        }
      PIO4   =  7.85398163397448309616E-1;    {  pi/4        }
      SQRT2  =  1.41421356237309504880;       {  sqrt(2)     }
      SQRTH  =  7.07106781186547524401E-1;    {  sqrt(2)/2   }
      LOG2E  =  1.4426950408889634073599;     {  1/log(2)    }
      SQ2OPI =  7.9788456080286535587989E-1;  {  sqrt( 2/pi )}
      LOGE2  =  6.93147180559945309417E-1;    {  log(2)      }
      LOGSQ2 =  3.46573590279972654709E-1;    {  log(2)/2    }
      THPIO4 =  2.35619449019234492885;       {  3*pi/4      }
      TWOOPI =  6.36619772367581343075535E-1; {  2/pi        }
      lossth =  1.073741824e9;
      MAXLOG =  8.8029691931113054295988E1;    { log(2**127)  }
      MINLOG = -8.872283911167299960540E1;     { log(2**-128) }

      DP1 =   7.85398125648498535156E-1;
      DP2 =   3.77489470793079817668E-8;
      DP3 =   2.69515142907905952645E-15;

const sincof : TabCoef = (
                1.58962301576546568060E-10,
               -2.50507477628578072866E-8,
                2.75573136213857245213E-6,
               -1.98412698295895385996E-4,
                8.33333333332211858878E-3,
               -1.66666666666666307295E-1, 0);
      coscof : TabCoef = (
               -1.13585365213876817300E-11,
                2.08757008419747316778E-9,
               -2.75573141792967388112E-7,
                2.48015872888517045348E-5,
               -1.38888888888730564116E-3,
                4.16666666666665929218E-2, 0);



{ also necessary for Int() on systems with 64bit floats (JM) }
type
{$ifdef ENDIAN_LITTLE}
  float64 = packed record
    low: longint;
    high: longint;
  end;
{$else}
  float64 = packed record
    high: longint;
    low: longint;
  end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_TRUNC}
type
  float32 = longint;
  flag = byte;

  Function extractFloat64Frac0(const a: float64): longint;
    Begin
      extractFloat64Frac0 := a.high and $000FFFFF;
    End;


  Function extractFloat64Frac1(const a: float64): longint;
    Begin
      extractFloat64Frac1 := a.low;
    End;


  Function extractFloat64Exp(const a: float64): smallint;
    Begin
       extractFloat64Exp:= ( a.high shr 20 ) AND $7FF;
    End;


  Function extractFloat64Frac(const a: float64): int64;
    Begin
      extractFloat64Frac:=int64(a) and $000FFFFFFFFFFFFF;
    End;


  Function extractFloat64Sign(const a: float64) : flag;
    Begin
       extractFloat64Sign := a.high shr 31;
    End;


  Procedure shortShift64Left(a0:longint; a1:longint; count:smallint; VAR z0Ptr:longint; VAR z1Ptr:longint );
    Begin
        z1Ptr := a1 shl count;
        if count = 0 then
          z0Ptr := a0
        else
          z0Ptr := ( a0 shl count ) OR ( a1 shr ( ( - count ) AND 31 ) );
    End;

   function float64_to_int32_round_to_zero(a: float64 ): longint;
     Var
       aSign: flag;
       aExp, shiftCount: smallint;
       aSig0, aSig1, absZ, aSigExtra: longint;
       z: longint;
     Begin
       aSig1 := extractFloat64Frac1( a );
       aSig0 := extractFloat64Frac0( a );
       aExp := extractFloat64Exp( a );
       aSign := extractFloat64Sign( a );
       shiftCount := aExp - $413;
       if 0<=shiftCount then
       Begin
          if (aExp=$7FF)  and ((aSig0 or aSig1)<>0) then
            HandleError(207);
          shortShift64Left(aSig0 OR  $00100000, aSig1, shiftCount, absZ, aSigExtra );
       End
       else
       Begin
           if aExp<$3FF then
             begin
               float64_to_int32_round_to_zero := 0;
               exit;
             end;
           aSig0 := aSig0 or $00100000;
           aSigExtra := ( aSig0 shl ( shiftCount and 31 ) ) OR  aSig1;
           absZ := aSig0 shr ( - shiftCount );
       End;
       if aSign<>0 then
         z:=-absZ
       else
         z:=absZ;
       if ((aSign<>0) xor (z<0)) AND  (z<>0) then
         HandleError(207);
      float64_to_int32_round_to_zero := z;
     End;


   function float64_to_int64_round_to_zero(a : float64) : int64;
     var
       aSign : flag;
       aExp, shiftCount : smallint;
       aSig : int64;
       z : int64;
     begin
       aSig:=extractFloat64Frac(a);
       aExp:=extractFloat64Exp(a);
       aSign:=extractFloat64Sign(a);
       if aExp<>0 then
         aSig:=aSig or $0010000000000000;
       shiftCount:= aExp-$433;
       if 0<=shiftCount then
         begin
           if aExp>=$43e then
             begin
               if int64(a)<>$C3E0000000000000 then
                 HandleError(207);
               { pascal doesn't know Inf for int64 }
               HandleError(207);
             end;
           z:=aSig shl shiftCount;
         end
       else
         begin
           if aExp<$3fe then
             begin
               result:=0;
               exit;
             end;
           z:=aSig shr -shiftCount;
           {
           if (aSig shl (shiftCount and 63))<>0 then
             float_exception_flags |= float_flag_inexact;
           }
         end;
       if aSign<>0 then
         z:=-z;
       result:=z;
     end;

  Function ExtractFloat32Frac(a : Float32) : longint;
    Begin
       ExtractFloat32Frac := A AND $007FFFFF;
    End;


  Function extractFloat32Exp( a: float32 ): smallint;
    Begin
      extractFloat32Exp := (a shr 23) AND $FF;
    End;


  Function extractFloat32Sign( a: float32 ): Flag;
    Begin
      extractFloat32Sign := a shr 31;
    End;


  Function float32_to_int32_round_to_zero( a: Float32 ): longint;
    Var
       aSign : flag;
       aExp, shiftCount : smallint;
       aSig : longint;
       z : longint;
    Begin
       aSig := extractFloat32Frac( a );
       aExp := extractFloat32Exp( a );
       aSign := extractFloat32Sign( a );
       shiftCount := aExp - $9E;
       if ( 0 <= shiftCount ) then
         Begin
           if ( a <> Float32($CF000000) ) then
             Begin
               if ( (aSign=0) or ( ( aExp = $FF ) and (aSig<>0) ) ) then
                 Begin
                   HandleError(207);
                   exit;
                 end;
             End;
           HandleError(207);
           exit;
         End
       else
         if ( aExp <= $7E ) then
         Begin
           float32_to_int32_round_to_zero := 0;
           exit;
         End;
       aSig := ( aSig or $00800000 ) shl 8;
       z := aSig shr ( - shiftCount );
       if ( aSign<>0 ) then z := - z;
       float32_to_int32_round_to_zero := z;
    End;


{$ifdef INTERNCONSTINTF}
  function fpc_trunc_real(d : real) : int64;compilerproc;
{$else}
  function trunc(d : real) : int64;[internconst:fpc_in_const_trunc];
{$endif}
    var
{$ifdef cpuarm}
     l: longint;
{$endif cpuarm}
     f32 : float32;
     f64 : float64;
    Begin
     { in emulation mode the real is equal to a single }
     { otherwise in fpu mode, it is equal to a double  }
     { extended is not supported yet. }
     if sizeof(D) > 8 then
        HandleError(255);
     if sizeof(D)=8 then
       begin
         move(d,f64,sizeof(f64));
{$ifdef cpuarm}
         { the arm fpu has a strange opinion how a double has to be stored }
         l:=f64.low;
         f64.low:=f64.high;
         f64.high:=l;
{$endif cpuarm}
         result:=float64_to_int64_round_to_zero(f64);
       end
     else
       begin
         move(d,f32,sizeof(f32));
         result:=float32_to_int32_round_to_zero(f32);
       end;
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_INT}

{$ifdef SUPPORT_DOUBLE}

    { straight Pascal translation of the code for __trunc() in }
    { the file sysdeps/libm-ieee754/s_trunc.c of glibc (JM)    }
{$ifdef INTERNCONSTINTF}
    function fpc_int_real(d: double): double;compilerproc;
{$else}
    function int(d: double): double;[internconst:fpc_in_const_int];
{$endif}
      var
        i0, j0: longint;
        i1: cardinal;
        sx: longint;
        f64 : float64;
      begin
        f64:=float64(d);
{$ifdef cpuarm}
        { the arm fpu has a strange opinion how a double has to be stored }
        i0:=f64.low;
        f64.low:=f64.high;
        f64.high:=i0;
{$endif cpuarm}
        i0 := f64.high;
        i1 := cardinal(f64.low);
        sx := i0 and $80000000;
        j0 := ((i0 shr 20) and $7ff) - $3ff;
        if (j0 < 20) then
          begin
            if (j0 < 0) then
              begin
                { the magnitude of the number is < 1 so the result is +-0. }
                f64.high := sx;
                f64.low := 0;
              end
            else
              begin
                f64.high := sx or (i0 and not($fffff shr j0));
                f64.low := 0;
              end
          end
        else if (j0 > 51) then
          begin
            if (j0 = $400) then
              { d is inf or NaN }
              exit(d + d); { don't know why they do this (JM) }
          end
        else
          begin
            f64.high := i0;
            f64.low := longint(i1 and not(cardinal($ffffffff) shr (j0 - 20)));
          end;
{$ifdef cpuarm}
        { the arm fpu has a strange opinion how a double has to be stored }
        i0:=f64.low;
        f64.low:=f64.high;
        f64.high:=i0;
{$endif cpuarm}
        result:=double(f64);
      end;

{$else SUPPORT_DOUBLE}

{$ifdef INTERNCONSTINTF}
    function fpc_int_real(d : real) : real;compilerproc;
{$else}
    function int(d : real) : real;[internconst:fpc_in_const_int];
{$endif}
      begin
        { this will be correct since real = single in the case of }
        { the motorola version of the compiler...                 }
        result:=real(trunc(d));
      end;
{$endif SUPPORT_DOUBLE}

{$endif}


{$ifndef FPC_SYSTEM_HAS_ABS}

{$ifdef SUPPORT_DOUBLE}

  {$ifdef INTERNCONSTINTF}
    function fpc_abs_real(d : Double) : Double;compilerproc;
  {$else}
    function abs(d : Double) : Double;[public,alias:'FPC_ABS_REAL'];
  {$endif}
    begin
       if (d<0.0) then
         result := -d
      else
         result := d ;
    end;

{$else}

  {$ifdef INTERNCONSTINTF}
    function fpc_abs_real(d : Double) : Double;compilerproc;
  {$else}
    function abs(d : Real) : Real;[public,alias:'FPC_ABS_REAL'];
  {$endif}
    begin
       if (d<0.0) then
         result := -d
      else
         result := d ;
    end;

{$endif}

{$ifndef INTERNCONSTINTF}
  {$ifdef hascompilerproc}
    function fpc_abs_real(d:Real):Real;compilerproc; external name 'FPC_ABS_REAL';
  {$endif hascompilerproc}
{$endif}

{$endif not FPC_SYSTEM_HAS_ABS}


{$ifndef SYSTEM_HAS_FREXP}
    function frexp(x:Real; var e:Integer ):Real;
    {*  frexp() extracts the exponent from x.  It returns an integer     *}
    {*  power of two to expnt and the significand between 0.5 and 1      *}
    {*  to y.  Thus  x = y * 2**expn.                                    *}
    begin
      e :=0;
      if (abs(x)<0.5) then
       While (abs(x)<0.5) do
       begin
         x := x*2;
         Dec(e);
       end
      else
       While (abs(x)>1) do
       begin
         x := x/2;
         Inc(e);
       end;
      frexp := x;
    end;
{$endif not SYSTEM_HAS_FREXP}


{$ifndef SYSTEM_HAS_LDEXP}
    function ldexp( x: Real; N: Integer):Real;
    {* ldexp() multiplies x by 2**n.                                    *}
    var r : Real;
    begin
      R := 1;
      if N>0 then
         while N>0 do
         begin
            R:=R*2;
            Dec(N);
         end
      else
        while N<0 do
        begin
           R:=R/2;
           Inc(N);
        end;
      ldexp := x * R;
    end;
{$endif not SYSTEM_HAS_LDEXP}


    function polevl(var x:Real; var Coef:TabCoef; N:Integer):Real;
    {*****************************************************************}
    { Evaluate polynomial                                             }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    {  int N;                                                         }
    {  double x, y, coef[N+1], polevl[];                              }
    {                                                                 }
    {  y = polevl( x, coef, N );                                      }
    {                                                                 }
    {  DESCRIPTION:                                                   }
    {                                                                 }
    {     Evaluates polynomial of degree N:                           }
    {                                                                 }
    {                       2          N                              }
    {   y  =  C  + C x + C x  +...+ C x                               }
    {          0    1     2          N                                }
    {                                                                 }
    {   Coefficients are stored in reverse order:                     }
    {                                                                 }
    {   coef[0] = C  , ..., coef[N] = C  .                            }
    {              N                   0                              }
    {                                                                 }
    {   The function p1evl() assumes that coef[N] = 1.0 and is        }
    {   omitted from the array.  Its calling arguments are            }
    {   otherwise the same as polevl().                               }
    {                                                                 }
    {  SPEED:                                                         }
    {                                                                 }
    {   In the interest of speed, there are no checks for out         }
    {   of bounds arithmetic.  This routine is used by most of        }
    {   the functions in the library.  Depending on available         }
    {   equipment features, the user may wish to rewrite the          }
    {   program in microcode or assembly language.                    }
    {*****************************************************************}
    var ans : Real;
        i   : Integer;

    begin
      ans := Coef[0];
      for i:=1 to N do
        ans := ans * x + Coef[i];
      polevl:=ans;
    end;


    function p1evl(var x:Real; var Coef:TabCoef; N:Integer):Real;
    {                                                           }
    { Evaluate polynomial when coefficient of x  is 1.0.        }
    { Otherwise same as polevl.                                 }
    {                                                           }
    var
        ans : Real;
        i   : Integer;
    begin
      ans := x + Coef[0];
      for i:=1 to N-1 do
        ans := ans * x + Coef[i];
      p1evl := ans;
    end;


{$ifndef FPC_SYSTEM_HAS_SQR}
{$ifdef INTERNCONSTINTF}
    function fpc_sqr_real(d : Real) : Real;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
{$else}
    function sqr(d : Real) : Real;[internconst:fpc_in_const_sqr];
{$endif}
    begin
      result := d*d;
    end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_PI}
{$ifdef INTERNCONSTINTF}
    function fpc_pi_real : Real;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
{$else}
    function pi : Real;[internconst:fpc_in_const_pi];
{$endif}
    begin
      result := 3.1415926535897932385;
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_SQRT}
  {$ifdef INTERNCONSTINTF}
    function fpc_sqrt_real(d:Real):Real;compilerproc;
  {$else}
    {$ifdef hascompilerproc}
       function fpc_sqrt_real(d:Real):Real;compilerproc; external name 'FPC_SQRT_REAL';
    {$endif hascompilerproc}
    function sqrt(d:Real):Real;[internconst:fpc_in_const_sqrt];[public, alias: 'FPC_SQRT_REAL'];
  {$endif}
    {*****************************************************************}
    { Square root                                                     }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, sqrt();                                            }
    {                                                                 }
    { y = sqrt( x );                                                  }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Returns the square root of x.                                   }
    {                                                                 }
    { Range reduction involves isolating the power of two of the      }
    { argument and using a polynomial approximation to obtain         }
    { a rough value for the square root.  Then Heron's iteration      }
    { is used three times to converge to an accurate value.           }
    {*****************************************************************}
    var e   : Integer;
        w,z : Real;
    begin
       if( d <= 0.0 ) then
       begin
           if( d < 0.0 ) then
               HandleError(207);
           result := 0.0;
       end
     else
       begin
          w := d;
          { separate exponent and significand }
           z := frexp( d, e );

          {  approximate square root of number between 0.5 and 1  }
          {  relative error of approximation = 7.47e-3            }
          d := 4.173075996388649989089E-1 + 5.9016206709064458299663E-1 * z;

          { adjust for odd powers of 2 }
          if odd(e) then
             d := d*SQRT2;

          { re-insert exponent }
          d := ldexp( d, (e div 2) );

          { Newton iterations: }
          d := 0.5*(d + w/d);
          d := 0.5*(d + w/d);
          d := 0.5*(d + w/d);
          d := 0.5*(d + w/d);
          d := 0.5*(d + w/d);
          d := 0.5*(d + w/d);
          result := d;
       end;
    end;

{$endif}


{$ifndef FPC_SYSTEM_HAS_EXP}
  {$ifdef INTERNCONSTINTF}
    function fpc_exp_real(d:Real):Real;compilerproc;
  {$else}
    function Exp(d:Real):Real;[internconst:fpc_in_const_exp];
  {$endif}
    {*****************************************************************}
    { Exponential Function                                            }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, exp();                                             }
    {                                                                 }
    { y = exp( x );                                                   }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Returns e (2.71828...) raised to the x power.                   }
    {                                                                 }
    { Range reduction is accomplished by separating the argument      }
    { into an integer k and fraction f such that                      }
    {                                                                 }
    {     x    k  f                                                   }
    {    e  = 2  e.                                                   }
    {                                                                 }
    { A Pade' form of degree 2/3 is used to approximate exp(f)- 1     }
    { in the basic range [-0.5 ln 2, 0.5 ln 2].                       }
    {*****************************************************************}
    const  P : TabCoef = (
           1.26183092834458542160E-4,
           3.02996887658430129200E-2,
           1.00000000000000000000E0, 0, 0, 0, 0);
           Q : TabCoef = (
           3.00227947279887615146E-6,
           2.52453653553222894311E-3,
           2.27266044198352679519E-1,
           2.00000000000000000005E0, 0 ,0 ,0);

           C1 = 6.9335937500000000000E-1;
            C2 = 2.1219444005469058277E-4;
    var n : Integer;
        px, qx, xx : Real;
    begin
      if( d > MAXLOG) then
          HandleError(205)
      else
      if( d < MINLOG ) then
      begin
        HandleError(205);
      end
      else
      begin

     { Express e**x = e**g 2**n }
     {   = e**g e**( n loge(2) ) }
     {   = e**( g + n loge(2) )  }

        px := d * LOG2E;
        qx := Trunc( px + 0.5 ); { Trunc() truncates toward -infinity. }
        n  := Trunc(qx);
        d  := d - qx * C1;
        d  := d + qx * C2;

      { rational approximation for exponential  }
      { of the fractional part: }
      { e**x - 1  =  2x P(x**2)/( Q(x**2) - P(x**2) )  }
        xx := d * d;
        px := d * polevl( xx, P, 2 );
        d  :=  px/( polevl( xx, Q, 3 ) - px );
        d  := ldexp( d, 1 );
        d  :=  d + 1.0;
        d  := ldexp( d, n );
        result := d;
      end;
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_ROUND}
  {$ifdef INTERNCONSTINTF}
    function fpc_round_real(d : Real) : int64;compilerproc;
  {$else}
    {$ifdef hascompilerproc}
      function round(d : Real) : int64;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_round];{$endif} external name 'FPC_ROUND';
      function fpc_round(d : Real) : int64;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
   {$else}
      function round(d : Real) : int64;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_round];{$endif}
   {$endif hascompilerproc}
 {$endif}
     var
      fr: Real;
      tr: Int64;
    Begin
       fr := abs(Frac(d));
       tr := Trunc(d);
       if fr > 0.5 then
         if d >= 0 then
           result:=tr+1
         else
           result:=tr-1
       else
       if fr < 0.5 then
          result:=tr
       else { fr = 0.5 }
          { check sign to decide ... }
          { as in Turbo Pascal...    }
          if d >= 0.0 then
            result:=tr+1
          else
            result:=tr;
    end;
{$endif}


{$ifdef FPC_CURRENCY_IS_INT64}

    function trunc(c : currency) : int64;
      type
        tmyrec = record
          i: int64;
        end;
      begin
        result := int64(tmyrec(c)) div 10000
      end;


    function trunc(c : comp) : int64;
      begin
        result := c
      end;


    function round(c : currency) : int64;
      type
        tmyrec = record
          i: int64;
        end;
      var
        rem, absrem: longint;
      begin
        { (int64(tmyrec(c))(+/-)5000) div 10000 can overflow }
        result := int64(tmyrec(c)) div 10000;
        rem := int64(tmyrec(c)) - result * 10000;
        absrem := abs(rem);
        if (absrem > 5000) or
           ((absrem = 5000) and
            (rem > 0)) then
          if (rem > 0) then
            inc(result)
          else
            dec(result);
      end;


    function round(c : comp) : int64;
      begin
        result := c
      end;

{$endif FPC_CURRENCY_IS_INT64}



{$ifndef FPC_SYSTEM_HAS_LN}
  {$ifdef INTERNCONSTINTF}
    function fpc_ln_real(d:Real):Real;compilerproc;
  {$else}
    function Ln(d:Real):Real;[internconst:fpc_in_const_ln];
  {$endif}
    {*****************************************************************}
    { Natural Logarithm                                               }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, log();                                             }
    {                                                                 }
    { y = ln( x );                                                    }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Returns the base e (2.718...) logarithm of x.                   }
    {                                                                 }
    { The argument is separated into its exponent and fractional      }
    { parts.  If the exponent is between -1 and +1, the logarithm     }
    { of the fraction is approximated by                              }
    {                                                                 }
    {     log(1+x) = x - 0.5 x**2 + x**3 P(x)/Q(x).                   }
    {                                                                 }
    { Otherwise, setting  z = 2(x-1)/x+1),                            }
    {                                                                 }
    {     log(x) = z + z**3 P(z)/Q(z).                                }
    {                                                                 }
    {*****************************************************************}
    const  P : TabCoef = (
     {  Coefficients for log(1+x) = x - x**2/2 + x**3 P(x)/Q(x)
         1/sqrt(2) <= x < sqrt(2) }

           4.58482948458143443514E-5,
           4.98531067254050724270E-1,
           6.56312093769992875930E0,
           2.97877425097986925891E1,
           6.06127134467767258030E1,
           5.67349287391754285487E1,
           1.98892446572874072159E1);
       Q : TabCoef = (
           1.50314182634250003249E1,
           8.27410449222435217021E1,
           2.20664384982121929218E2,
           3.07254189979530058263E2,
           2.14955586696422947765E2,
           5.96677339718622216300E1, 0);

     { Coefficients for log(x) = z + z**3 P(z)/Q(z),
        where z = 2(x-1)/(x+1)
        1/sqrt(2) <= x < sqrt(2)  }

       R : TabCoef = (
           -7.89580278884799154124E-1,
            1.63866645699558079767E1,
           -6.41409952958715622951E1, 0, 0, 0, 0);
       S : TabCoef = (
           -3.56722798256324312549E1,
            3.12093766372244180303E2,
           -7.69691943550460008604E2, 0, 0, 0, 0);

    var e : Integer;
       z, y : Real;

    Label Ldone;
    begin
       if( d <= 0.0 ) then
          HandleError(207);
       d := frexp( d, e );

    { logarithm using log(x) = z + z**3 P(z)/Q(z),
      where z = 2(x-1)/x+1) }

       if( (e > 2) or (e < -2) ) then
       begin
         if( d < SQRTH ) then
         begin
           {  2( 2x-1 )/( 2x+1 ) }
          Dec(e, 1);
          z := d - 0.5;
          y := 0.5 * z + 0.5;
         end
         else
         begin
         {   2 (x-1)/(x+1)   }
           z := d - 0.5;
         z := z - 0.5;
         y := 0.5 * d  + 0.5;
         end;
         d := z / y;
         { /* rational form */ }
         z := d*d;
         z := d + d * ( z * polevl( z, R, 2 ) / p1evl( z, S, 3 ) );
         goto ldone;
       end;

    { logarithm using log(1+x) = x - .5x**2 + x**3 P(x)/Q(x) }

       if( d < SQRTH ) then
       begin
         Dec(e, 1);
         d := ldexp( d, 1 ) - 1.0; {  2x - 1  }
       end
       else
         d := d - 1.0;

       { rational form  }
       z := d*d;
       y := d * ( z * polevl( d, P, 6 ) / p1evl( d, Q, 6 ) );
       y := y - ldexp( z, -1 );   {  y - 0.5 * z  }
       z := d + y;

    ldone:
       { recombine with exponent term }
       if( e <> 0 ) then
       begin
         y := e;
         z := z - y * 2.121944400546905827679e-4;
         z := z + y * 0.693359375;
       end;

       result:= z;
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_SIN}
  {$ifdef INTERNCONSTINTF}
    function fpc_Sin_real(d:Real):Real;compilerproc;
  {$else}
    function Sin(d:Real):Real;[internconst:fpc_in_const_sin];
  {$endif}
    {*****************************************************************}
    { Circular Sine                                                   }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, sin();                                             }
    {                                                                 }
    { y = sin( x );                                                   }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Range reduction is into intervals of pi/4.  The reduction       }
    { error is nearly eliminated by contriving an extended            }
    { precision modular arithmetic.                                   }
    {                                                                 }
    { Two polynomial approximating functions are employed.            }
    { Between 0 and pi/4 the sine is approximated by                  }
    {      x  +  x**3 P(x**2).                                        }
    { Between pi/4 and pi/2 the cosine is represented as              }
    {      1  -  x**2 Q(x**2).                                        }
    {*****************************************************************}
    var  y, z, zz : Real;
         j, sign : Integer;

    begin
      { make argument positive but save the sign }
      sign := 1;
      if( d < 0 ) then
      begin
         d := -d;
         sign := -1;
      end;

      { above this value, approximate towards 0 }
      if( d > lossth ) then
      begin
        result := 0.0;
        exit;
      end;

      y := Trunc( d/PIO4 ); { integer part of x/PIO4 }

      { strip high bits of integer part to prevent integer overflow }
      z := ldexp( y, -4 );
      z := Trunc(z);           { integer part of y/8 }
      z := y - ldexp( z, 4 );  { y - 16 * (y/16) }

      j := Trunc(z); { convert to integer for tests on the phase angle }
      { map zeros to origin }
      { typecast is to avoid "can't determine which overloaded function }
      { to call"                                                        }
      if odd( longint(j) ) then
      begin
         inc(j);
         y := y + 1.0;
      end;
      j := j and 7; { octant modulo 360 degrees }
      { reflect in x axis }
      if( j > 3) then
      begin
        sign := -sign;
        dec(j, 4);
      end;

      { Extended precision modular arithmetic }
      z := ((d - y * DP1) - y * DP2) - y * DP3;

      zz := z * z;

      if( (j=1) or (j=2) ) then
        y := 1.0 - ldexp(zz,-1) + zz * zz * polevl( zz, coscof, 5 )
      else
      { y = z  +  z * (zz * polevl( zz, sincof, 5 )); }
        y := z  +  z * z * z * polevl( zz, sincof, 5 );

      if(sign < 0) then
      y := -y;
      result := y;
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_COS}
  {$ifdef INTERNCONSTINTF}
    function fpc_Cos_real(d:Real):Real;compilerproc;
  {$else}
    function Cos(d:Real):Real;[internconst:fpc_in_const_cos];
  {$endif}
    {*****************************************************************}
    { Circular cosine                                                 }
    {*****************************************************************}
    {                                                                 }
    { Circular cosine                                                 }
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, cos();                                             }
    {                                                                 }
    { y = cos( x );                                                   }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Range reduction is into intervals of pi/4.  The reduction       }
    { error is nearly eliminated by contriving an extended            }
    { precision modular arithmetic.                                   }
    {                                                                 }
    { Two polynomial approximating functions are employed.            }
    { Between 0 and pi/4 the cosine is approximated by                }
    {      1  -  x**2 Q(x**2).                                        }
    { Between pi/4 and pi/2 the sine is represented as                }
    {      x  +  x**3 P(x**2).                                        }
    {*****************************************************************}
    var  y, z, zz : Real;
         j, sign : Integer;
         i : LongInt;
    begin
    { make argument positive }
      sign := 1;
      if( d < 0 ) then
        d := -d;

      { above this value, round towards zero }
      if( d > lossth ) then
      begin
        result := 0.0;
        exit;
      end;

      y := Trunc( d/PIO4 );
      z := ldexp( y, -4 );
      z := Trunc(z);  { integer part of y/8 }
      z := y - ldexp( z, 4 );  { y - 16 * (y/16) }

      { integer and fractional part modulo one octant }
      i := Trunc(z);
      if odd( i ) then { map zeros to origin }
      begin
        inc(i);
        y := y + 1.0;
      end;
      j := i and 07;
      if( j > 3) then
      begin
        dec(j,4);
        sign := -sign;
      end;
      if( j > 1 ) then
        sign := -sign;

      { Extended precision modular arithmetic  }
      z := ((d - y * DP1) - y * DP2) - y * DP3;

      zz := z * z;

      if( (j=1) or (j=2) ) then
      { y = z  +  z * (zz * polevl( zz, sincof, 5 )); }
        y := z  +  z * z * z * polevl( zz, sincof, 5 )
      else
        y := 1.0 - ldexp(zz,-1) + zz * zz * polevl( zz, coscof, 5 );

      if(sign < 0) then
        y := -y;

      result := y ;
    end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_ARCTAN}
  {$ifdef INTERNCONSTINTF}
    function fpc_ArcTan_real(d:Real):Real;compilerproc;
  {$else}
    function ArcTan(d:Real):Real;[internconst:fpc_in_const_arctan];
  {$endif}
    {*****************************************************************}
    { Inverse circular tangent (arctangent)                           }
    {*****************************************************************}
    {                                                                 }
    { SYNOPSIS:                                                       }
    {                                                                 }
    { double x, y, atan();                                            }
    {                                                                 }
    { y = atan( x );                                                  }
    {                                                                 }
    { DESCRIPTION:                                                    }
    {                                                                 }
    { Returns radian angle between -pi/2 and +pi/2 whose tangent      }
    { is x.                                                           }
    {                                                                 }
    { Range reduction is from four intervals into the interval        }
    { from zero to  tan( pi/8 ).  The approximant uses a rational     }
    { function of degree 3/4 of the form x + x**3 P(x)/Q(x).          }
    {*****************************************************************}
    const P : TabCoef = (
          -8.40980878064499716001E-1,
          -8.83860837023772394279E0,
          -2.18476213081316705724E1,
          -1.48307050340438946993E1, 0, 0, 0);
          Q : TabCoef = (
          1.54974124675307267552E1,
          6.27906555762653017263E1,
          9.22381329856214406485E1,
          4.44921151021319438465E1, 0, 0, 0);

    { tan( 3*pi/8 ) }
    T3P8 = 2.41421356237309504880;
    { tan( pi/8 )   }
    TP8 = 0.41421356237309504880;

    var y,z  : Real;
        Sign : Integer;

    begin
      { make argument positive and save the sign }
      sign := 1;
      if( d < 0.0 ) then
      begin
       sign := -1;
       d := -d;
      end;

      { range reduction }
      if( d > T3P8 ) then
      begin
        y := PIO2;
        d := -( 1.0/d );
      end
      else if( d > TP8 ) then
      begin
        y := PIO4;
        d := (d-1.0)/(d+1.0);
      end
      else
       y := 0.0;

      { rational form in x**2 }

      z := d * d;
      y := y + ( polevl( z, P, 3 ) / p1evl( z, Q, 4 ) ) * z * d + d;

      if( sign < 0 ) then
        y := -y;
      result := y;
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FRAC}
  {$ifdef INTERNCONSTINTF}
    function fpc_frac_real(d : Real) : Real;compilerproc;
  {$else}
    function frac(d : Real) : Real;[internconst:fpc_in_const_frac];
  {$endif}
    begin
       result := d - Int(d);
    end;
{$endif}


{$ifdef FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

{$ifndef FPC_SYSTEM_HAS_QWORD_TO_DOUBLE}
function fpc_qword_to_double(q : qword): double; compilerproc;
  begin
     result:=dword(q and $ffffffff)+dword(q shr 32)*4294967296.0;
  end;
{$endif FPC_SYSTEM_HAS_INT64_TO_DOUBLE}


{$ifndef FPC_SYSTEM_HAS_INT64_TO_DOUBLE}
function fpc_int64_to_double(i : int64): double; compilerproc;
  begin
    if i<0 then
      result:=-double(qword(-i))
    else
      result:=qword(i);
  end;
{$endif FPC_SYSTEM_HAS_INT64_TO_DOUBLE}

{$endif FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}


{$ifdef SUPPORT_DOUBLE}
{****************************************************************************
                    Helper routines to support old TP styled reals
 ****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_REAL2DOUBLE}
    function real2double(r : real48) : double;

      var
         res : array[0..7] of byte;
         exponent : word;

      begin
         { copy mantissa }
         res[0]:=0;
         res[1]:=r[1] shl 5;
         res[2]:=(r[1] shr 3) or (r[2] shl 5);
         res[3]:=(r[2] shr 3) or (r[3] shl 5);
         res[4]:=(r[3] shr 3) or (r[4] shl 5);
         res[5]:=(r[4] shr 3) or (r[5] and $7f) shl 5;
         res[6]:=(r[5] and $7f) shr 3;

         { copy exponent }
         { correct exponent: }
         exponent:=(word(r[0])+(1023-129));
         res[6]:=res[6] or ((exponent and $f) shl 4);
         res[7]:=exponent shr 4;

         { set sign }
         res[7]:=res[7] or (r[5] and $80);
         real2double:=double(res);
      end;
{$endif FPC_SYSTEM_HAS_REAL2DOUBLE}
{$endif SUPPORT_DOUBLE}

{
  $Log: genmath.inc,v $
  Revision 1.32  2005/02/14 17:13:22  peter
    * truncate log

  Revision 1.31  2005/02/08 20:25:28  florian
    - killed power from system unit
    * move operator ** to math unit

}
