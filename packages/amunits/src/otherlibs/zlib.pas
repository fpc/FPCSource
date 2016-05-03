{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.

  This is a unit for zlib.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{
  History:

  First version of this unit.
  17 Jan 2003.

  Changed cardinal > longword.
  Changed startcode for unit.
  12 Feb 2003.

  nils.sjoholm@mailbox.swipnet.se
}

UNIT ZLIB;

INTERFACE
USES Exec;

VAR ZLibBase : pLibrary = nil;

const
    ZLIBNAME : PChar = 'zlib.library';

  { Version 1.0  }
  { Compression strategy  }
     GZ_STRATEGY_DEFAULT = 0;
     GZ_STRATEGY_FILTERED = 1;
     GZ_STRATEGY_HUFFMAN = 2;
  { some often used compression levels  }
     GZ_COMPRESS_NO = 0;
     GZ_COMPRESS_FASTEST = 1;
     GZ_COMPRESS_DEFAULT = 6;
     GZ_COMPRESS_BEST = 9;


FUNCTION GZ_Close(handle : POINTER location 'a0') : LONGINT; syscall ZLibBase 042;
FUNCTION GZ_CompressMem(srcbuf : POINTER location 'a0'; srclen : longword location 'd0'; destbuf : POINTER location 'a1'; destlen : longword location 'd1'; strategy : longword location 'd2'; level : longword location 'd3'; VAR poutlen : longword location 'a2') : LONGINT; syscall ZLibBase 114;
FUNCTION GZ_DecompressMem(srcbuf : POINTER location 'a0'; srclen : longword location 'd0'; destbuf : POINTER location 'a1'; destlen : longword location 'd1') : LONGINT; syscall ZLibBase 120;
FUNCTION GZ_FGetC(handle : POINTER location 'a0') : pLONGINT; syscall ZLibBase 060;
FUNCTION GZ_FGetS(handle : POINTER location 'a0'; buf : pCHAR location 'a1'; len : longword location 'd0') : pCHAR; syscall ZLibBase 054;
FUNCTION GZ_FileLength(handle : POINTER location 'a0') : longword; syscall ZLibBase 138;
FUNCTION GZ_Open(filename : pCHAR location 'a0'; openmode : longword location 'd0'; strategy : longword location 'd1'; level : longword location 'd2') : POINTER; syscall ZLibBase 030;
FUNCTION GZ_OpenFromFH(fh : LONGINT location 'a0'; openmode : longword location 'd0'; strategy : longword location 'd1'; level : longword location 'd2') : POINTER; syscall ZLibBase 036;
FUNCTION GZ_Read(handle : POINTER location 'a0'; buf : POINTER location 'a1'; len : longword location 'd0') : LONGINT; syscall ZLibBase 048;
FUNCTION GZ_Write(handle : POINTER location 'a0'; buf : POINTER location 'a1'; len : longword location 'd0') : LONGINT; syscall ZLibBase 066;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  ZLibBase := OpenLibrary(ZLIBNAME,LIBVERSION);
finalization
  if Assigned(ZLibBase) then
    CloseLibrary(ZLibBase);
END. (* UNIT ZLIB *)



