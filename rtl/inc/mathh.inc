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
  Revision 1.2  1998-05-12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
