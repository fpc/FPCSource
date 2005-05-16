{
    *********************************************************************
    $Id: syswideh.inc,v 1.6 2005/03/12 14:56:22 florian Exp $
    Copyright (C) 2002 by Florian Klaempfl

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************
}

function WideUpperCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideLowerCase(const s : WideString) : WideString;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideCompareStr(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideSameStr(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideCompareText(const s1, s2 : WideString) : PtrInt;{$ifdef SYSUTILSINLINE}inline;{$endif}
function WideSameText(const s1, s2 : WideString) : Boolean;{$ifdef SYSUTILSINLINE}inline;{$endif}

Function WideFormat (Const Fmt : WideString; const Args : Array of const) : WideString;
Function WideFormatBuf (Var Buffer; BufLen : Cardinal;
                     Const Fmt; fmtLen : Cardinal;
                     Const Args : Array of const) : Cardinal;
Procedure WideFmtStr(Var Res: WideString; Const Fmt : WideString; Const args: Array of const);


{
  $Log: syswideh.inc,v $
  Revision 1.6  2005/03/12 14:56:22  florian
    + added Ansi* routines to widestring manager
    * made them using OS calls on windows

  Revision 1.5  2005/03/06 18:28:23  florian
    + WideFormatBuf and WideFmtStr implemented

  Revision 1.4  2005/02/26 15:00:14  florian
    + WideSameStr

  Revision 1.3  2005/02/26 10:21:17  florian
    + implemented WideFormat
    + some Widestring stuff implemented
    * some Widestring stuff fixed

  Revision 1.2  2005/02/03 18:40:02  florian
    * compilation with 1.0.x fixed
    + infrastructure for WideCompareText implemented

  Revision 1.1  2005/02/01 20:22:50  florian
    * improved widestring infrastructure manager
}
