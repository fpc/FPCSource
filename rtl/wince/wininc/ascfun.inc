{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Contains the Ascii functions for windows unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  ASCIIFunctions.h

  Declarations for all the Win32 ASCII Functions

  Copyright (C) 1996 Free Software Foundation, Inc.

  Author:  Scott Christley <scottc@net-community.com>

  This file is part of the Windows32 API Library.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  If you are interested in a warranty or support for this source code,
  contact Scott Christley <scottc@net-community.com> for more information.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.
  If not, write to the Free Software Foundation,
  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.



  *What should contain this file*
  
  Imported functions with header modified in order to be compatible with win32
  Ansi header equivalent like LoadLibraryA wich not exist under wince.
  if needed use header definition from rtl\win\wininc\ascfun.inc

  Changes :

  05/13/2006 update for wince4.2 port, orinaudo@gmail.com
}

{$ifdef read_interface}

function LoadLibraryA(lpLibFileName:LPCSTR):HINST;

{$endif read_interface}


{$ifdef read_implementation}

function LoadLibraryA(lpLibFileName:LPCSTR):HINST;
var  ws: PWideChar;
begin
  ws:=StringToPWideChar(lpLibFileName);
  try
   Result:=Windows.LoadLibrary(ws);
  finally FreeMem(ws); end;
end;

{$endif read_implementation}

