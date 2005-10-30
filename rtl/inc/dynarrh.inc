{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    This file contains type declarations necessary for the dynamic
    array routine helpers in syshelp.inc

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


**********************************************************************}

type
  tdynarrayindex = sizeint;
  pdynarrayindex = ^tdynarrayindex;

  pdynarraytypeinfo = ^tdynarraytypeinfo;
  tdynarraytypeinfo = packed record
    kind : byte;
    namelen : byte;
    { here the chars follow, we've to skip them }
    elesize : sizeint;
    eletype : pdynarraytypeinfo;
    vartype : longint;
  end;
  
procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: SizeInt; lengthVec: PSizeInt);
