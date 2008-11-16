{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    FileRec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the filerec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

const
  filerecnamelength = 255;
type
  FileRec = Packed Record
    Handle    : THandle;
    Mode      : longint;
    RecSize   : SizeInt;
    _private  : array[1..3 * SizeOf(SizeInt) + 5 * SizeOf (pointer)] of byte;
    UserData  : array[1..32] of byte;
    name      : array[0..filerecnamelength div SizeOf(RtlChar)] of RtlChar;
  End;

