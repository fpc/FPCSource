{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by several people
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  math.inc                                                               }
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
{                                                                         }
{*************************************************************************}
{  This is the Motorola 680x0 specific port of the math include.          }
{*************************************************************************}
{                                                                         }
{  o all reals are mapped to the single type under the motorola version   }
{                                                                         }
{   What is left to do:                                                   }
{    o add support for sqrt with fixed.                                   }

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




    function int(d : real) : real;
      begin
        { this will be correct since real = single in the case of }
        { the motorola version of the compiler...                 }
        int:=real(trunc(d));
      end;

    function trunc(d : real) : longint;
    var
     l: longint;
    Begin
     asm
        move.l   d,d0           { get number                        }
        move.l   d2,-(sp)       { save register                     }
        move.l   d0,d1
        swap     d1             { extract exp                       }
        move.w   d1,d2          { extract sign                      }
        bclr     #15,d1         { kill sign bit                     }
        lsr.w    #7,d1

        and.l    #$7fffff,d0    { remove exponent from mantissa     }
        bset     #23,d0         { restore implied leading "1"       }

        cmp.w    #BIAS4,d1      { check exponent                    }
        blt      @zero           { strictly factional, no integer part ?   }
        cmp.w    #BIAS4+32,d1   { is it too big to fit in a 32-bit integer ? }
        bgt      @toobig

        sub.w    #BIAS4+24,d1   { adjust exponent                   }
        bgt      @trunclab2     { shift up                          }
        beq      @trunclab7     { no shift (never too big)          }

        neg.w    d1
        lsr.l    d1,d0          { shift down to align radix point;  }
              { extra bits fall off the end (no rounding) }
        bra      @trunclab7      { never too big                     }
    @trunclab2:
        lsl.l   d1,d0           { shift up to align radix point     }
    @trunclab3:
        cmp.l   #$80000000,d0   { -2147483648 is a nasty evil special case }
        bne      @trunclab6
        tst.w    d2             { this had better be -2^31 and not 2^31    }
        bpl      @toobig
        bra      @trunclab8
    @trunclab6:
        tst.l   d0              { sign bit set ? (i.e. too big)     }
        bmi     @toobig
    @trunclab7:
        tst.w   d2              { is it negative ?                  }
        bpl     @trunclab8
        neg.l   d0              { negate                            }
        bra     @trunclab8
    @zero:
        clr.l   d0              { make the whole thing zero         }
        bra     @trunclab8
    @toobig:
        moveq   #-1,d0          { ugh. Should cause a trap here.    }
        bclr    #31,d0          { make it #0x7fffffff               }
    @trunclab8:
        move.l  (sp)+,d2
        move.l  d0,l
     end;
     if l = $7fffffff then
      RunError(207)
     else
       trunc := l
    end;





    function abs(d : Real) : Real;
    begin
       if( d < 0.0 ) then
         abs := -d
      else
         abs := d ;
    end;


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





    function sqr(d : Real) : Real;
    begin
      sqr := d*d;
    end;


    function pi : Real;
    begin
      pi := 3.1415926535897932385;
    end;


    function sqrt(d:Real):Real;
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
               RunError(207);
           sqrt := 0.0;
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
          sqrt := d;
       end;
    end;




    function Exp(d:Real):Real;
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
          RunError(205)
      else
      if( d < MINLOG ) then
      begin
        Runerror(205);
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
        Exp := d;
      end;
    end;


    function Round(d: Real): longint;
     var
      fr: Real;
      tr: Real;
    Begin
       fr := Frac(d);
       tr := Trunc(d);
       if fr > 0.5 then
          Round:=Trunc(d)+1
       else
       if fr < 0.5 then
          Round:=Trunc(d)
       else { fr = 0.5 }
          { check sign to decide ... }
          { as in Turbo Pascal...    }
          if d >= 0.0 then
            Round := Trunc(d)+1
          else
            Round := Trunc(d);
    end;


    function Ln(d:Real):Real;
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
          RunError(207);
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

       Ln:= z;
    end;



    function Sin(d:Real):Real;
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
        sin := 0.0;
        exit;
      end;

      y := Trunc( d/PIO4 ); { integer part of x/PIO4 }

      { strip high bits of integer part to prevent integer overflow }
      z := ldexp( y, -4 );
      z := Trunc(z);           { integer part of y/8 }
      z := y - ldexp( z, 4 );  { y - 16 * (y/16) }

      j := Trunc(z); { convert to integer for tests on the phase angle }
      { map zeros to origin }
      if odd( j ) then
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
      sin := y;
    end;




    function Cos(d:Real):Real;
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
        cos := 0.0;
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

      cos := y ;
    end;



    function ArcTan(d:Real):Real;
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
      Arctan := y;
    end;

    function frac(d : Real) : Real;
    begin
       frac := d - Int(d);
    end;

{$ifdef fixed}


    function sqrt(d : fixed) : fixed;
      begin
      end;

    function int(d : fixed) : fixed; assembler;
    {*****************************************************************}
    { Returns the integral part of d                                  }
    {*****************************************************************}
    asm
      move.l d,d0
      and.l  #$ffff0000,d0        { keep only upper bits   .. }
    end;


    function trunc(d : fixed) : longint;
    {*****************************************************************}
    { Returns the Truncated integral part of d                        }
    {*****************************************************************}
    begin
      trunc:=longint(integer(d shr 16));   { keep only upper 16 bits  }
    end;

    function frac(d : fixed) : fixed; assembler;
    {*****************************************************************}
    { Returns the Fractional part of d                                }
    {*****************************************************************}
    asm
      move.l d,d0
      and.l  #$ffff,d0           { keep only decimal parts - lower 16 bits }
    end;

    function abs(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute value of d                                 }
    {*****************************************************************}
    var
     w: integer;
    begin
     w:=integer(d shr 16);
     if w < 0 then
     begin
        w:=-w;                      { invert sign ...              }
        d:=d and $ffff;
        d:=d or (fixed(w) shl 16);  { add this to fixed number ... }
        abs:=d;
     end
     else
        abs:=d;                     { already positive... }
    end;


    function sqr(d : fixed) : fixed;
    {*****************************************************************}
    { Returns the Absolute squared value of d                         }
    {*****************************************************************}
    begin
            {16-bit precision needed, not 32 =)}
       sqr := d*d;
{       sqr := (d SHR 8 * d) SHR 8; }
    end;


    function Round(x: fixed): longint;
    {*****************************************************************}
    { Returns the Rounded value of d as a longint                     }
    {*****************************************************************}
    var
     lowf:integer;
     highf:integer;
    begin
      lowf:=x and $ffff;       { keep decimal part ... }
      highf :=integer(x shr 16);
      if lowf > 5 then
        highf:=highf+1
      else
      if lowf = 5 then
      begin
        { here we must check the sign ...       }
        { if greater or equal to zero, then     }
        { greater value will be found by adding }
        { one...                                }
         if highf >= 0 then
           Highf:=Highf+1;
      end;
      Round:= longint(highf);
    end;
{$endif fixed}

    function power(bas,expo : real) : real;
     begin
        if bas=0.0 then
          begin
            if expo<>0.0 then
              power:=0.0
            else
              HandleError(207);
          end
        else if expo=0.0 then
         power:=1
        else
        { bas < 0 is not allowed }
         if bas<0.0 then
          handleerror(207)
         else
          power:=exp(ln(bas)*expo);
     end;

   function power(bas,expo : longint) : longint;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
         begin
           if bas<0 then
            begin
              if odd(expo) then
               power:=-round(exp(ln(-bas)*expo))
              else
               power:=round(exp(ln(-bas)*expo));
            end
           else
            power:=round(exp(ln(bas)*expo));
         end;
     end;

