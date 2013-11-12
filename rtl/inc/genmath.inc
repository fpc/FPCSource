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
  float64 = record
{$ifndef FPC_DOUBLE_HILO_SWAPPED}
    low,high: longint;
{$else}
    high,low: longint;
{$endif FPC_DOUBLE_HILO_SWAPPED}
  end;
{$else}
  float64 = record
{$ifndef FPC_DOUBLE_HILO_SWAPPED}
    high,low: longint;
{$else}
    low,high: longint;
{$endif FPC_DOUBLE_HILO_SWAPPED}
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

{$ifdef SUPPORT_DOUBLE}
   { based on softfloat float64_to_int64_round_to_zero }
   function fpc_trunc_real(d : valreal) : int64; compilerproc;
     var
       aExp, shiftCount : smallint;
       aSig : int64;
       z : int64;
       a: float64 absolute d;
     begin
       aSig:=(int64(a.high and $000fffff) shl 32) or longword(a.low);
       aExp:=(a.high shr 20) and $7FF;
       if aExp<>0 then
         aSig:=aSig or $0010000000000000;
       shiftCount:= aExp-$433;
       if 0<=shiftCount then
         begin
           if aExp>=$43e then
             begin
               if (a.high<>$C3E00000) or (a.low<>0) then
                 begin
                   float_raise(float_flag_invalid);
                   if (a.high>=0) or ((aExp=$7FF) and
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
       if a.high<0 then
         z:=-z;
       result:=z;
     end;

{$else SUPPORT_DOUBLE}
  { based on softfloat float32_to_int64_round_to_zero }
  Function fpc_trunc_real( d: valreal ): int64; compilerproc;
    Var
       a : float32 absolute d;
       aExp, shiftCount : smallint;
       aSig : longint;
       aSig64, z : int64;
    Begin
       aSig := a and $007FFFFF;
       aExp := (a shr 23) and $FF;
       shiftCount := aExp - $BE;
       if ( 0 <= shiftCount ) then
         Begin
           if ( a <> Float32($DF000000) ) then
             Begin
               float_raise( float_flag_invalid );
               if ( (a>=0) or ( ( aExp = $FF ) and (aSig<>0) ) ) then
                 Begin
                   result:=$7fffffffffffffff;
                   exit;
                 end;
             End;
           result:=$8000000000000000;
           exit;
         End
       else
         if ( aExp <= $7E ) then
         Begin
           result := 0;
           exit;
         End;
       aSig64 := int64( aSig or $00800000 ) shl 40;
       z := aSig64 shr ( - shiftCount );
       if ( a<0 ) then z := - z;
       result := z;
    End;
{$endif SUPPORT_DOUBLE}

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
        hx:=float64(d).high;
        xsb := (hx shr 31) and 1;               { sign bit of d }
        hx := hx and $7fffffff;                 { high word of |d| }

        { filter out non-finite argument }
        if hx >= $40862E42 then
          begin                  { if |d|>=709.78... }
            if hx >= $7ff00000 then
              begin
                lx:=float64(d).low;
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
            hy:=float64(y).high;
            float64(y).high:=longint(hy)+(k shl 20);        { add k to y's exponent }
            result:=y;
          end
        else
          begin
            hy:=float64(y).high;
            float64(y).high:=longint(hy)+((k+1000) shl 20); { add k to y's exponent }
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
    {
     This code was translated from uclibc code, the original code
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

    {********************************************************************}
    { Inverse circular tangent (arctangent)                              }
    {********************************************************************}
    {                                                                    }
    { SYNOPSIS:                                                          }
    {                                                                    }
    { double x, y, atan();                                               }
    {                                                                    }
    { y = atan( x );                                                     }
    {                                                                    }
    { DESCRIPTION:                                                       }
    {                                                                    }
    { Returns radian angle between -pi/2 and +pi/2 whose tangent         }
    { is x.                                                              }
    {                                                                    }
    { Method                                                             }
    {   1. Reduce x to positive by atan(x) = -atan(-x).                  }
    {   2. According to the integer k=4t+0.25 chopped, t=x, the argument }
    {      is further reduced to one of the following intervals and the  }
    {      arctangent of t is evaluated by the corresponding formula:    }
    {                                                                    }
    {   [0,7/16]      atan(x) = t-t^3*(a1+t^2*(a2+...(a10+t^2*a11)...)   }
    {   [7/16,11/16]  atan(x) = atan(1/2) + atan( (t-0.5)/(1+t/2) )      }
    {   [11/16.19/16] atan(x) = atan( 1 ) + atan( (t-1)/(1+t) )          }
    {   [19/16,39/16] atan(x) = atan(3/2) + atan( (t-1.5)/(1+1.5t) )     }
    {   [39/16,INF]   atan(x) = atan(INF) + atan( -1/t )                 }
    {********************************************************************}
    const
      atanhi: array [0..3] of double = (
        4.63647609000806093515e-01, { atan(0.5)hi 0x3FDDAC67, 0x0561BB4F }
        7.85398163397448278999e-01, { atan(1.0)hi 0x3FE921FB, 0x54442D18 }
        9.82793723247329054082e-01, { atan(1.5)hi 0x3FEF730B, 0xD281F69B }
        1.57079632679489655800e+00  { atan(inf)hi 0x3FF921FB, 0x54442D18 }
      );

      atanlo: array [0..3] of double = (
        2.26987774529616870924e-17, { atan(0.5)lo 0x3C7A2B7F, 0x222F65E2 }
        3.06161699786838301793e-17, { atan(1.0)lo 0x3C81A626, 0x33145C07 }
        1.39033110312309984516e-17, { atan(1.5)lo 0x3C700788, 0x7AF0CBBD }
        6.12323399573676603587e-17  { atan(inf)lo 0x3C91A626, 0x33145C07 }
      );

      aT: array[0..10] of double = (
         3.33333333333329318027e-01,  { 0x3FD55555, 0x5555550D }
        -1.99999999998764832476e-01,  { 0xBFC99999, 0x9998EBC4 }
         1.42857142725034663711e-01,  { 0x3FC24924, 0x920083FF }
        -1.11111104054623557880e-01,  { 0xBFBC71C6, 0xFE231671 }
         9.09088713343650656196e-02,  { 0x3FB745CD, 0xC54C206E }
        -7.69187620504482999495e-02,  { 0xBFB3B0F2, 0xAF749A6D }
         6.66107313738753120669e-02,  { 0x3FB10D66, 0xA0D03D51 }
        -5.83357013379057348645e-02,  { 0xBFADDE2D, 0x52DEFD9A }
         4.97687799461593236017e-02,  { 0x3FA97B4B, 0x24760DEB }
        -3.65315727442169155270e-02,  { 0xBFA2B444, 0x2C6A6C2F }
         1.62858201153657823623e-02   { 0x3F90AD3A, 0xE322DA11 }
      );

      one:  double  = 1.0;
      huge: double  = 1.0e300;

    var
      w,s1,s2,z: double;
      ix,hx,id: longint;
      low: longword;
    begin
      hx:=float64(d).high;
      ix := hx and $7fffffff;
      if (ix>=$44100000) then   { if |x| >= 2^66 }
      begin
        low:=float64(d).low;
        if (ix > $7ff00000) or ((ix = $7ff00000) and (low<>0)) then
          exit(d+d);          { NaN }
        if (hx>0) then
          exit(atanhi[3]+atanlo[3])
        else
          exit(-atanhi[3]-atanlo[3]);
      end;
      if (ix < $3fdc0000) then  { |x| < 0.4375 }
      begin
        if (ix < $3e200000) then  { |x| < 2^-29 }
        begin
          if (huge+d>one) then exit(d);    { raise inexact }
        end;
        id := -1;
      end
      else
      begin
        d := abs(d);
        if (ix < $3ff30000) then    { |x| < 1.1875 }
        begin
          if (ix < $3fe60000) then  { 7/16 <=|x|<11/16 }
          begin
            id := 0; d := (2.0*d-one)/(2.0+d);
          end
          else                      { 11/16<=|x|< 19/16 }
          begin
            id := 1; d := (d-one)/(d+one);
          end
        end
        else
        begin
          if (ix < $40038000) then   { |x| < 2.4375 }
          begin
            id := 2; d := (d-1.5)/(one+1.5*d);
          end
          else                       { 2.4375 <= |x| < 2^66 }
          begin
            id := 3; d := -1.0/d;
          end;
        end;
      end;
    { end of argument reduction }
      z := d*d;
      w := z*z;
      { break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly }
      s1 := z*(aT[0]+w*(aT[2]+w*(aT[4]+w*(aT[6]+w*(aT[8]+w*aT[10])))));
      s2 := w*(aT[1]+w*(aT[3]+w*(aT[5]+w*(aT[7]+w*aT[9]))));
      if (id<0) then
        result := d - d*(s1+s2)
      else
      begin
        z := atanhi[id] - ((d*(s1+s2) - atanlo[id]) - d);
        if hx<0 then
          result := -z
        else
          result := z;
      end;
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
