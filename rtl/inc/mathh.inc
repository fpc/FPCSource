{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klaempfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { declarations of the math routines }

    function abs(d : real) : real;
    function arctan(d : real) : real;
    function cos(d : real) : real;
    function exp(d : real) : real;
    function frac(d : real) : real;
    function int(d : real) : real;
    function ln(d : real) : real;
    function pi : real;
    function round(d : real) : longint;
    function sin(d : real) : real;
    function sqr(d : real) : real;
    function sqrt(d : real) : real;
    function trunc(d : real) : longint;
    function power(bas,expo : real) : real;
    function power(bas,expo : longint) : longint;
{$ifdef FIXED}
    function sqrt(d : fixed) : fixed;
    function Round(x: fixed): longint;
    function sqr(d : fixed) : fixed;
    function abs(d : fixed) : fixed;
    function frac(d : fixed) : fixed;
    function trunc(d : fixed) : longint;
    function int(d : fixed) : fixed;
{$endif FIXED}

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.7  1998/01/27 12:44:46  peter
    * removed comment level 2 warning

  Revision 1.6  1998/01/26 11:59:40  michael
  + Added log at the end


  
  Working file: rtl/inc/mathh.inc
  description:
  ----------------------------
  revision 1.5
  date: 1997/12/01 12:08:04;  author: michael;  state: Exp;  lines: +12 -4
  + added copyright reference header.
  ----------------------------
  revision 1.4
  date: 1997/11/28 23:26:45;  author: florian;  state: Exp;  lines: +3 -5
  $ifdef fixed added
  ----------------------------
  revision 1.3
  date: 1997/11/28 19:45:21;  author: pierre;  state: Exp;  lines: +35 -33
    * one more bug fix with namelength
    + fixed math in fixed_math define (does not compile yet)
  ----------------------------
  revision 1.2
  date: 1997/11/28 16:49:08;  author: carl;  state: Exp;  lines: +33 -26
  + added fixed point routines.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
