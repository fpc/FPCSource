{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Interface and OS-dependent part of variant support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE ObjFPC}
{$IFNDEF FPC_DOTTEDUNITS}
Unit varutils;
{$ENDIF}

Interface

{$IFDEF FPC_DOTTEDUNITS}
Uses System.SysUtils, System.Variants;
{$ELSE}
uses sysutils, variants;
{$ENDIF}
// Read definitions.

{$i varutilh.inc}

Implementation

// Code common to all platforms.

{$i cvarutil.inc}

// Code common to non-win32 platforms.

{$i varutils.inc}

end.
