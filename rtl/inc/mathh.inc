{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { i386 FPU Controlword }

{$ifdef cpui386}
    const
      Default8087CW : word = $1332;

    procedure Set8087CW(cw:word);
    function Get8087CW:word;
{$endif cpui386}

   { declarations of the math routines }

    function abs(d : extended) : extended;
    function arctan(d : extended) : extended;
    function cos(d : extended) : extended;
    function exp(d : extended) : extended;
    function frac(d : extended) : extended;
    function int(d : extended) : extended;
    function ln(d : extended) : extended;
    function pi : extended;
    function round(d : extended) : int64;
    function sin(d : extended) : extended;
    function sqr(d : extended) : extended;
    function sqrt(d : extended) : extended;
    function trunc(d : extended) : int64;
    function power(bas,expo : extended) : extended;
    function power(bas,expo : int64) : int64;

    type
       real48 = array[0..5] of byte;

{$ifdef SUPPORT_DOUBLE}
    function Real2Double(r : real48) : double;
    operator := (b:real48) d:double;
{$endif}
{$ifdef SUPPORT_EXTENDED}
    operator := (b:real48) e:extended;
{$endif SUPPORT_EXTENDED}

{
  $Log$
  Revision 1.11  2003-01-15 00:40:18  peter
    * power returns int64

  Revision 1.10  2003/01/03 20:34:02  peter
    * i386 fpu controlword functions added

  Revision 1.9  2002/10/06 21:26:18  peter
    * round returns int64

  Revision 1.8  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/07/26 22:46:06  florian
    * interface of system unit for Linux/PowerPC compiles

}
