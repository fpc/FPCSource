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

{$ifdef DEFAULT_EXTENDED}
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
    function trunc(d : extended) : longint;
    function power(bas,expo : extended) : extended;
{$else DEFAULT_EXTENDED}
    function abs(d : real) : real;
    function arctan(d : real) : real;
    function cos(d : real) : real;
    function exp(d : real) : real;
    function frac(d : real) : real;
    function int(d : real) : real;
    function ln(d : real) : real;
    function round(d : real) : longint;
    function sin(d : real) : real;
    function sqr(d : real) : real;
    function sqrt(d : real) : real;
    function trunc(d : real) : longint;
    function power(bas,expo : real) : real;
    function pi : real;
{$endif DEFAULT_EXTENDED}

    function power(bas,expo : longint) : longint;

    type
       real48 = array[0..5] of byte;

    function Real2Double(r : real48) : double;

{$ifdef HASFIXED}
    function sqrt(d : fixed) : fixed;
    function Round(x: fixed): longint;
    function sqr(d : fixed) : fixed;
    function abs(d : fixed) : fixed;
    function frac(d : fixed) : fixed;
    function trunc(d : fixed) : longint;
    function int(d : fixed) : fixed;
{$endif HASFIXED}

{
  $Log$
  Revision 1.3  2000-10-21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.2  2000/07/13 11:33:44  michael
  + removed logs
 
}
