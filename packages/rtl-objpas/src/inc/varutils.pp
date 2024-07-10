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

{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

{$if defined (win32) or defined (win64)}
{$define USE_WINDOWS_OLE_FUNCTIONS}
{$endif}

{$IFNDEF FPC_DOTTEDUNITS}
Unit varutils;
{$ENDIF}

Interface

{$IFDEF FPC_DOTTEDUNITS}
Uses
  System.SysUtils,
{$ifdef USE_WINDOWS_OLE_FUNCTIONS}
  System.Types;
{$else}
  System.Variants;
[$endif}
{$ELSE}
uses
  sysutils,
{$ifdef USE_WINDOWS_OLE_FUNCTIONS}
  Types;
{$else}
  variants;
[$endif}
{$ENDIF}
// Read definitions.

{$i varutilh.inc}

Implementation

// Code common to all platforms.

{$i cvarutil.inc}

{$ifdef USE_WINDOWS_OLE_FUNCTIONS}
// Code common to Windows OS platforms.

{$i wvarutil.inc}
{$else}
// Code common to other platforms.

{$i varutils.inc}
{$endif}

end.
