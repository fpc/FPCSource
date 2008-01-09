{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Common types used by group icon and group cursor resource types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit icocurtypes;

{$MODE OBJFPC}

interface

const
  RES_ICON   = 1;
  RES_CURSOR = 2;

type
  TNewHeader = packed record
    reserved : word;
    restype  : word;
    rescount : word;
  end;

  TIconDir = packed record
    width      : byte;
    height     : byte;
    colorcount : byte;
    reserved   : byte;
    planes     : word;
    bitcount   : word;
    bytesinres : longword;
    offsetId   : longword;
  end;

  TResCursorDir = packed record
    width      : word;
    height     : word;
    planes     : word;
    bitcount   : word;
    bytesinres : longword;
    cursorId   : word;
  end;

  TCurCursorDir = packed record
    width      : byte;
    height     : byte;
    reserved   : word;
    xhotspot   : word;
    yhotspot   : word;
    bytesincur : longword;
    curoffset  : longword;
  end;

implementation

end.
