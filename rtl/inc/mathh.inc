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

   { declarations of the math routines }

    function abs(d : extended) : extended;
    function arctan(d : extended) : extended;
    function cos(d : extended) : extended;
    function exp(d : extended) : extended;
    function frac(d : extended) : extended;
    function int(d : extended) : extended;
    function ln(d : extended) : extended;
    function pi : extended;
    function round(d : extended) : longint;
    function sin(d : extended) : extended;
    function sqr(d : extended) : extended;
    function sqrt(d : extended) : extended;
    function trunc(d : extended) : int64;
    function power(bas,expo : extended) : extended;
    function power(bas,expo : longint) : longint;

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
  Revision 1.7  2002-07-26 22:46:06  florian
    * interface of system unit for Linux/PowerPC compiles

  Revision 1.6  2001/12/26 21:03:56  peter
    * merged fixes from 1.0.x

  Revision 1.5  2001/12/13 20:23:19  michael
  + Added double2real function from main branch

  Revision 1.4  2001/07/30 21:38:55  peter
    * m68k updates merged

}
