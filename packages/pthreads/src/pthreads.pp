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

{$ifdef BSD}
uses initc,BaseUnix, unixtype;
{$i pthrbsd.inc}
{$else}
 {$ifdef linux}
 uses initc, ctypes, unixtype;
 {$i pthrlinux.inc}
 {$else}

  {$ifdef sunos}
  uses initc, ctypes, unixtype;
  {$i pthrsnos.inc}
  {$else}
   {$ifdef beos}
   uses initc, ctypes, baseunix, unixtype;
   {$i pthrbeos.inc}
   {$else}
    {$error operating system not detected}
   {$endif}
  {$endif}
 {$endif}
{$endif}

implementation

end.
