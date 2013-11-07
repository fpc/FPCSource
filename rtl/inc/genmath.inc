{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2007 by Several contributors

    Generic mathematical routines (on type real)

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
{ also necessary for Int() on systems with 64bit floats (JM) }
{$ifndef FPC_SYSTEM_HAS_float64}
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
{$endif FPC_SYSTEM_HAS_float64}


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

{$if not defined(FPC_SYSTEM_HAS_SIN) or not defined(FPC_SYSTEM_HAS_COS)}
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
{$endif}

{*
-------------------------------------------------------------------------------
Raises the exceptions specified by `flags'.  Floating-point traps can be
defined here if desired.  It is currently not possible for such a trap
to substitute a result value.  If traps are not implemented, this routine
should be simply `softfloat_exception_flags |= flags;'.
-------------------------------------------------------------------------------
*}
procedure float_raise(i: shortint);
var
  pflags: pbyte;
  unmasked_flags: byte;
Begin
  { taking address of threadvar produces somewhat more compact code }
  pflags := @softfloat_exception_flags;
  pflags^ := pflags^ or i;
  unmasked_flags := pflags^ and (not softfloat_exception_mask);
  if (unmasked_flags and float_flag_invalid) <> 0 then
     HandleError(207)
  else
  if (unmasked_flags and float_flag_divbyzero) <> 0 then
     HandleError(200)
  else
  if (unmasked_flags and float_flag_overflow) <> 0 then
     HandleError(205)
  else
  if (unmasked_flags and float_flag_underflow) <> 0 then
     HandleError(206)
  else
  if (unmasked_flags and float_flag_inexact) <> 0 then
     HandleError(207);
end;


{$ifndef FPC_SYSTEM_HAS_TRUNC}
{$ifndef FPC_SYSTEM_HAS_float32}
type
  float32 = longint;
{$endif FPC_SYSTEM_HAS_float32}
{$ifndef FPC_SYSTEM_HAS_flag}
type
  flag = byte;
{$endif FPC_SYSTEM_HAS_flag}

{$ifndef FPC_SYSTEM_HAS_extractFloat64Frac0}
  Function extractFloat64Frac0(const a: float64): longint;
    Begin
      extractFloat64Frac0 := a.high and $000FFFFF;
    End;
{$endif not FPC_SYSTEM_HAS_extractFloat64Frac0}


{$ifndef FPC_SYSTEM_HAS_extractFloat64Frac1}
  Function extractFloat64Frac1(const a: float64): longint;
    Begin
      extractFloat64Frac1 := a.low;
    End;
{$endif not FPC_SYSTEM_HAS_extractFloat64Frac1}


{$ifndef FPC_SYSTEM_HAS_extractFloat64Exp}
  Function extractFloat64Exp(const a: float64): smallint;
    Begin
       extractFloat64Exp:= ( a.high shr 20 ) AND $7FF;
    End;
{$endif not FPC_SYSTEM_HAS_extractFloat64Exp}


{$ifndef FPC_SYSTEM_HAS_extractFloat64Frac}
  Function extractFloat64Frac(const a: float64): int64;
    Begin
      extractFloat64Frac:=int64(a) and $000FFFFFFFFFFFFF;
    End;
{$endif not FPC_SYSTEM_HAS_extractFloat64Frac}


{$ifndef FPC_SYSTEM_HAS_extractFloat64Sign}
  Function extractFloat64Sign(const a: float64) : flag;
    Begin
       extractFloat64Sign := a.high shr 31;
    End;
{$endif not FPC_SYSTEM_HAS_extractFloat64Sign}


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
     label
       invalid;
     Begin
       aSig1 := extractFloat64Frac1( a );
       aSig0 := extractFloat64Frac0( a );
       aExp := extractFloat64Exp( a );
       aSign := extractFloat64Sign( a );
       shiftCount := aExp - $413;
       if 0<=shiftCount then
       Begin
          if (aExp=$7FF)  and ((aSig0 or aSig1)<>0) then
            goto invalid;
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
         begin
invalid:
           float_raise(float_flag_invalid);
           if (aSign <> 0) then
             float64_to_int32_round_to_zero:=longint($80000000)
           else
             float64_to_int32_round_to_zero:=$7FFFFFFF;
           exit;
         end;
       if ( aSigExtra <> 0) then
         float_raise(float_flag_inexact);

       float64_to_int32_round_to_zero := z;
     End;


   function genmath_float64_to_int64_round_to_zero(a : float64) : int64;
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
                 begin
                   float_raise(float_flag_invalid);
                   if (aSign=0) or ((aExp=$7FF) and
                      (aSig<>$0010000000000000 )) then
                     begin
                       result:=$7FFFFFFFFFFFFFFF;
                       exit;
                     end;
                 end;
               result:=$8000000000000000;
               exit;
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


  Function float32_to_int32_round_to_zero( a: Float32 ): longint;
    Var
       aSign : flag;
       aExp, shiftCount : smallint;
       aSig : longint;
       z : longint;
    Begin
       aSig := a and $007FFFFF;
       aExp := (a shr 23) and $FF;
       aSign := a shr 31;
       shiftCount := aExp - $9E;
       if ( 0 <= shiftCount ) then
         Begin
           if ( a <> Float32($CF000000) ) then
             Begin
               float_raise( float_flag_invalid );
               if ( (aSign=0) or ( ( aExp = $FF ) and (aSig<>0) ) ) then
                 Begin
                   float32_to_int32_round_to_zero:=$7fffffff;
                   exit;
                 end;
             End;
           float32_to_int32_round_to_zero:=longint($80000000);
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


  function fpc_trunc_real(d : ValReal) : int64;compilerproc;
    var
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
     l: longint;
{$endif FPC_DOUBLE_HILO_SWAPPED}
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
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
         { the arm fpu has a strange opinion how a double has to be stored }
         l:=f64.low;
         f64.low:=f64.high;
         f64.high:=l;
{$endif FPC_DOUBLE_HILO_SWAPPED}
         result:=genmath_float64_to_int64_round_to_zero(f64);
       end
     else
       begin
         move(d,f32,sizeof(f32));
         result:=float32_to_int32_round_to_zero(f32);
       end;
    end;
{$endif not FPC_SYSTEM_HAS_TRUNC}


{$ifndef FPC_SYSTEM_HAS_INT}

{$ifdef SUPPORT_DOUBLE}

    { straight Pascal translation of the code for __trunc() in }
    { the file sysdeps/libm-ieee754/s_trunc.c of glibc (JM)    }
    function fpc_int_real(d: ValReal): ValReal;compilerproc;
      var
        i0, j0: longint;
        i1: cardinal;
        sx: longint;
        f64 : float64;
      begin
        f64:=float64(d);
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
        { the arm fpu has a strange opinion how a double has to be stored }
        i0:=f64.low;
        f64.low:=f64.high;
        f64.high:=i0;
{$endif FPC_DOUBLE_HILO_SWAPPED}
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
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
        { the arm fpu has a strange opinion how a double has to be stored }
        i0:=f64.low;
        f64.low:=f64.high;
        f64.high:=i0;
{$endif FPC_DOUBLE_HILO_SWAPPED}
        result:=double(f64);
      end;

{$else SUPPORT_DOUBLE}

    function fpc_int_real(d : ValReal) : ValReal;compilerproc;
      begin
        { this will be correct since real = single in the case of }
        { the motorola version of the compiler...                 }
        result:=ValReal(trunc(d));
      end;
{$endif SUPPORT_DOUBLE}

{$endif not FPC_SYSTEM_HAS_INT}


{$ifndef FPC_SYSTEM_HAS_ABS}

    function fpc_abs_real(d : ValReal) : ValReal;compilerproc;
    begin
       if (d<0.0) then
         result := -d
      else
         result := d ;
    end;

{$endif not FPC_SYSTEM_HAS_ABS}


{$ifndef SYSTEM_HAS_FREXP}
    function frexp(x:Real; out e:Integer ):Real;
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
    function fpc_sqr_real(d : ValReal) : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
    begin
      result := d*d;
    end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_PI}
    function fpc_pi_real : ValReal;compilerproc;{$ifdef MATHINLINE}inline;{$endif}
    begin
      result := 3.1415926535897932385;
    end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d:ValReal):ValReal;compilerproc;
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
           if d < 0.0 then begin
             float_raise(float_flag_invalid);
             d := 0/0;
           end;
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
{$ifdef SUPPORT_DOUBLE}
    {
     This code was translated from uclib code, the original code
     had the following copyright notice:

     *
     * ====================================================
     * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
     *
     * Developed at SunPro, a Sun Microsystems, Inc. business.
     * Permission to use, copy, modify, and distribute this
     * software is freely granted, provided that this notice
     * is preserved.
     * ====================================================
     *}

    {*
     * Returns the exponential of x.
     *
     * Method
     *   1. Argument reduction:
     *      Reduce x to an r so that |r| <= 0.5*ln2 ~ 0.34658.
     *  Given x, find r and integer k such that
     *
     *               x = k*ln2 + r,  |r| <= 0.5*ln2.
     *
     *      Here r will be represented as r = hi-lo for better
     *  accuracy.
     *
     *   2. Approximation of exp(r) by a special rational function on
     *  the interval [0,0.34658]:
     *  Write
     *      R(r**2) = r*(exp(r)+1)/(exp(r)-1) = 2 + r*r/6 - r**4/360 + ...
     *      We use a special Reme algorithm on [0,0.34658] to generate
     *  a polynomial of degree 5 to approximate R. The maximum error
     *  of this polynomial approximation is bounded by 2**-59. In
     *  other words,
     *      R(z) ~ 2.0 + P1*z + P2*z**2 + P3*z**3 + P4*z**4 + P5*z**5
     *      (where z=r*r, and the values of P1 to P5 are listed below)
     *  and
     *      |                  5          |     -59
     *      | 2.0+P1*z+...+P5*z   -  R(z) | <= 2
     *      |                             |
     *  The computation of exp(r) thus becomes
     *                     2*r
     *      exp(r) = 1 + -------
     *                    R - r
     *                         r*R1(r)
     *             = 1 + r + ----------- (for better accuracy)
     *                        2 - R1(r)
     *  where
                         2       4             10
     *     R1(r) = r - (P1*r  + P2*r  + ... + P5*r   ).
     *
     *   3. Scale back to obtain exp(x):
     *  From step 1, we have
     *     exp(x) = 2^k * exp(r)
     *
     * Special cases:
     *  exp(INF) is INF, exp(NaN) is NaN;
     *  exp(-INF) is 0, and
     *  for finite argument, only exp(0)=1 is exact.
     *
     * Accuracy:
     *  according to an error analysis, the error is always less than
     *  1 ulp (unit in the last place).
     *
     * Misc. info.
     *  For IEEE double
     *      if x >  7.09782712893383973096e+02 then exp(x) overflow
     *      if x < -7.45133219101941108420e+02 then exp(x) underflow
     *
     * Constants:
     * The hexadecimal values are the intended ones for the following
     * constants. The decimal values may be used, provided that the
     * compiler will convert from decimal to binary accurately enough
     * to produce the hexadecimal values shown.
     *
    }
    function fpc_exp_real(d: ValReal):ValReal;compilerproc;
      const
        one: double = 1.0;
        halF : array[0..1] of double = (0.5,-0.5);
        huge: double = 1.0e+300;
        twom1000: double = 9.33263618503218878990e-302;     { 2**-1000=0x01700000,0}
        o_threshold: double =  7.09782712893383973096e+02;  { 0x40862E42, 0xFEFA39EF }
        u_threshold: double = -7.45133219101941108420e+02;  { 0xc0874910, 0xD52D3051 }
        ln2HI  : array[0..1] of double = ( 6.93147180369123816490e-01,  { 0x3fe62e42, 0xfee00000 }
             -6.93147180369123816490e-01); { 0xbfe62e42, 0xfee00000 }
        ln2LO : array[0..1] of double = (1.90821492927058770002e-10,  { 0x3dea39ef, 0x35793c76 }
             -1.90821492927058770002e-10); { 0xbdea39ef, 0x35793c76 }
        invln2: double =  1.44269504088896338700e+00; { 0x3ff71547, 0x652b82fe }
        P1: double   =  1.66666666666666019037e-01; { 0x3FC55555, 0x5555553E }
        P2: double   = -2.77777777770155933842e-03; { 0xBF66C16C, 0x16BEBD93 }
        P3: double   =  6.61375632143793436117e-05; { 0x3F11566A, 0xAF25DE2C }
        P4: double   = -1.65339022054652515390e-06; { 0xBEBBBD41, 0xC5D26BF1 }
        P5: double   =  4.13813679705723846039e-08; { 0x3E663769, 0x72BEA4D0 }
      var
        c,hi,lo,t,y : double;
        k,xsb : longint;
        hx,hy,lx : dword;
      begin
        hi:=0.0;
        lo:=0.0;
        k:=0;
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
        hx:=float64(d).low;
{$else}
        hx:=float64(d).high;
{$endif FPC_DOUBLE_HILO_SWAPPED}
        xsb := (hx shr 31) and 1;               { sign bit of d }
        hx := hx and $7fffffff;                 { high word of |d| }

        { filter out non-finite argument }
        if hx >= $40862E42 then
          begin                  { if |d|>=709.78... }
            if hx >= $7ff00000 then
              begin
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
                lx:=float64(d).high;
{$else}
                lx:=float64(d).low;
{$endif FPC_DOUBLE_HILO_SWAPPED}
                if ((hx and $fffff) or lx)<>0 then
                  begin
                    result:=d+d; { NaN }
                    exit;
                  end
                else
                  begin
                    if xsb=0 then
                      result:=d
                    else
                      result:=0.0;    { exp(+-inf)=(inf,0) }
                    exit;
                  end;
              end;
            if d > o_threshold then begin
              result:=huge*huge;  { overflow }
              exit;
            end;
            if d < u_threshold then begin
              result:=twom1000*twom1000; { underflow }
              exit;
            end;
          end;
        { argument reduction }
        if hx > $3fd62e42 then
          begin           { if  |d| > 0.5 ln2 }
            if hx < $3FF0A2B2 then { and |d| < 1.5 ln2 }
              begin
                hi := d-ln2HI[xsb];
                lo:=ln2LO[xsb];
                k := 1-xsb-xsb;
              end
            else
              begin
               k  := trunc(invln2*d+halF[xsb]);
               t  := k;
               hi := d - t*ln2HI[0];    { t*ln2HI is exact here }
               lo := t*ln2LO[0];
              end;
            d  := hi - lo;
          end
        else if hx < $3e300000 then
          begin     { when |d|<2**-28 }
            if huge+d>one then
              begin
                result:=one+d;{ trigger inexact }
                exit;
              end;
          end
        else
          k := 0;

        { d is now in primary range }
        t:=d*d;
        c:=d - t*(P1+t*(P2+t*(P3+t*(P4+t*P5))));
        if k=0 then
          begin
            result:=one-((d*c)/(c-2.0)-d);
            exit;
          end
        else
          y := one-((lo-(d*c)/(2.0-c))-hi);

        if k >= -1021 then
          begin
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
            hy:=float64(y).low;
            float64(y).low:=longint(hy)+(k shl 20);        { add k to y's exponent }
{$else}
            hy:=float64(y).high;
            float64(y).high:=longint(hy)+(k shl 20);        { add k to y's exponent }
{$endif FPC_DOUBLE_HILO_SWAPPED}
            result:=y;
          end
        else
          begin
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
            hy:=float64(y).low;
            float64(y).low:=longint(hy)+((k+1000) shl 20); { add k to y's exponent }
{$else}
            hy:=float64(y).high;
            float64(y).high:=longint(hy)+((k+1000) shl 20); { add k to y's exponent }
{$endif FPC_DOUBLE_HILO_SWAPPED}
            result:=y*twom1000;
          end;
      end;

{$else SUPPORT_DOUBLE}

    function fpc_exp_real(d: ValReal):ValReal;compilerproc;
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
          float_raise(float_flag_overflow)
      else
      if( d < MINLOG ) then
      begin
        float_raise(float_flag_underflow);
        result:=0; { Result if underflow masked }
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
{$endif SUPPORT_DOUBLE}

{$endif}


{$ifndef FPC_SYSTEM_HAS_ROUND}
    function fpc_round_real(d : ValReal) : int64;compilerproc;
     var
      fr: ValReal;
      tr: Int64;
    Begin
      fr := abs(Frac(d));
      tr := Trunc(d);
      result:=0;
      case softfloat_rounding_mode of
        float_round_nearest_even:
          begin
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
              begin
                if d >= 0.0 then
                  result:=tr+1
                else
                  result:=tr;
                { round to even }
                result:=result and not(1);
              end;
          end;
        float_round_down:
          if (d >= 0.0) or
             (fr = 0.0) then
            result:=tr
          else
            result:=tr-1;
        float_round_up:
          if (d >= 0.0) and
             (fr <> 0.0) then
            result:=tr+1
          else
            result:=tr;
        float_round_to_zero:
          result:=tr;
        else
          { needed for jvm: result must be initialized on all paths }
          result:=0;
      end;
    end;
{$endif FPC_SYSTEM_HAS_ROUND}


{$ifndef FPC_SYSTEM_HAS_LN}
    function fpc_ln_real(d:ValReal):ValReal;compilerproc;
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
         begin
           float_raise(float_flag_invalid);
           exit;
         end;
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
    function fpc_Sin_real(d:ValReal):ValReal;compilerproc;
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
    function fpc_Cos_real(d:ValReal):ValReal;compilerproc;
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
    function fpc_ArcTan_real(d:ValReal):ValReal;compilerproc;
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
    function fpc_frac_real(d : ValReal) : ValReal;compilerproc;
    begin
       result := d - Int(d);
    end;
{$endif}


{$ifdef FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

{$ifndef FPC_SYSTEM_HAS_QWORD_TO_DOUBLE}
function fpc_qword_to_double(q : qword): double; compilerproc;
  begin
     result:=dword(q and $ffffffff)+dword(q shr 32)*double(4294967296.0);
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
         { check for zero }
         if r[0]=0 then
           begin
             real2double:=0.0;
             exit;
           end;

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

{$ifdef SUPPORT_EXTENDED}
{ fast 10^n routine }
function FPower10(val: Extended; Power: Longint): Extended;
  const
    pow32 : array[0..31] of extended =
      (
        1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,
        1e11,1e12,1e13,1e14,1e15,1e16,1e17,1e18,1e19,1e20,
        1e21,1e22,1e23,1e24,1e25,1e26,1e27,1e28,1e29,1e30,
        1e31
      );
    pow512 : array[0..15] of extended =
      (
        1,1e32,1e64,1e96,1e128,1e160,1e192,1e224,
        1e256,1e288,1e320,1e352,1e384,1e416,1e448,
        1e480
      );
    pow4096 : array[0..9] of extended =
      (1,1e512,1e1024,1e1536,
       1e2048,1e2560,1e3072,1e3584,
       1e4096,1e4608
      );

    negpow32 : array[0..31] of extended =
      (
        1e-0,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10,
        1e-11,1e-12,1e-13,1e-14,1e-15,1e-16,1e-17,1e-18,1e-19,1e-20,
        1e-21,1e-22,1e-23,1e-24,1e-25,1e-26,1e-27,1e-28,1e-29,1e-30,
        1e-31
      );
    negpow512 : array[0..15] of extended =
      (
        0,1e-32,1e-64,1e-96,1e-128,1e-160,1e-192,1e-224,
        1e-256,1e-288,1e-320,1e-352,1e-384,1e-416,1e-448,
        1e-480
      );
    negpow4096 : array[0..9] of extended =
      (
        0,1e-512,1e-1024,1e-1536,
        1e-2048,1e-2560,1e-3072,1e-3584,
        1e-4096,1e-4608
      );

  begin
    if Power<0 then
      begin
        Power:=-Power;
        result:=val*negpow32[Power and $1f];
        power:=power shr 5;
        if power<>0 then
          begin
            result:=result*negpow512[Power and $f];
            power:=power shr 4;
            if power<>0 then
              begin
                if power<=9 then
                  result:=result*negpow4096[Power]
                else
                  result:=1.0/0.0;
              end;
          end;
      end
    else
      begin
        result:=val*pow32[Power and $1f];
        power:=power shr 5;
        if power<>0 then
          begin
            result:=result*pow512[Power and $f];
            power:=power shr 4;
            if power<>0 then
              begin
                if power<=9 then
                  result:=result*pow4096[Power]
                else
                  result:=1.0/0.0;
              end;
          end;
      end;
  end;
{$endif SUPPORT_EXTENDED}
