{
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit pthreads;

interface

{$mode objfpc}
{$PACKRECORDS C}

{$if defined(BSD)}
  uses initc,BaseUnix, unixtype;
  {$i pthrbsd.inc}
{$elseif defined(linux)}
  uses initc, ctypes, unixtype;
  {$i pthrlinux.inc}
{$elseif defined(sunos)}
  uses initc, ctypes, unixtype;
  {$i pthrsnos.inc}
{$elseif defined(beos)}
  uses initc, ctypes, baseunix, unixtype;
  {$ifdef haiku}
    {$i pthrhaiku.inc}
  {$else}
    {$i pthrbeos.inc}
  {$endif}
{$elseif defined(aix)}
  uses initc, ctypes, baseunix, unixtype;
  {$i pthraix.inc}
{$else}
  {$error operating system not detected}
{$endif}

implementation

end.