{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team.

    Contains missing wince functions present in win32 api

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  *What should contain this file*
  
  functions missing on wince in order to be compatible with win32

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

