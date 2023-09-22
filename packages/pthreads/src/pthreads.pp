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

{$IFNDEF FPC_DOTTEDUNITS}
unit pthreads;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$mode objfpc}
{$PACKRECORDS C}

{$if defined(BSD)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC,UnixApi.Base, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc,BaseUnix, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$i pthrbsd.inc}
{$elseif defined(android)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC, System.CTypes, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc, ctypes, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$i pthrandroid.inc}
{$elseif defined(linux)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC, System.CTypes, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc, ctypes, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$i pthrlinux.inc}
{$elseif defined(sunos)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC, System.CTypes, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc, ctypes, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$i pthrsnos.inc}
{$elseif defined(beos)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC, System.CTypes, UnixApi.Base, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc, ctypes, baseunix, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$ifdef haiku}
    {$i pthrhaiku.inc}
  {$else}
    {$i pthrbeos.inc}
  {$endif}
{$elseif defined(aix)}
{$IFDEF FPC_DOTTEDUNITS}
  uses System.InitC, System.CTypes, UnixApi.Base, UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
  uses initc, ctypes, baseunix, unixtype;
{$ENDIF FPC_DOTTEDUNITS}
  {$i pthraix.inc}
{$else}
  {$error operating system not detected}
{$endif}

implementation

end.
