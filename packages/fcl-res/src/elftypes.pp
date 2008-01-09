{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Types used by ELF resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit elftypes;

{$MODE OBJFPC}

interface

type
  TElfMagic = array[0..3] of char;

type
  TElfIdent = packed record
    Magic : TElfMagic;              // 0..3
    ElfClass : byte;                // 4
    ElfData : byte;                 // 5
    ElfVersion : byte;              // 6
    OsAbi : byte;                   // 7
    AbiVersion : byte;              // 8
    Padding : array[9..15] of byte; // 9..15
  end;

  //note: it doesn't include Ident block
  TElf32Hdr = packed record
    _Type : word;
    Machine : word;
    Version : longword;
    Entry : longword;
    ProgHdrOffset : longword;
    SectHdrOffset : longword;
    Flags : longword;
    HdrSize : word;
    ProgHdrEntrySize : word;
    ProgHdrNum : word;
    SectHdrEntrySize : word;
    SectHdrNum : word;
    NameTableIndex : word;
  end;

  //note: it doesn't include Ident block
  TElf64Hdr = packed record
    _Type : word;
    Machine : word;
    Version : longword;
    Entry : qword;
    ProgHdrOffset : qword;
    SectHdrOffset : qword;
    Flags : longword;
    HdrSize : word;
    ProgHdrEntrySize : word;
    ProgHdrNum : word;
    SectHdrEntrySize : word;
    SectHdrNum : word;
    NameTableIndex : word;
  end;

  TElf32SectHdr = packed record
    NameIdx : longword;
    _Type : longword;
    Flags : longword;
    Address : longword;
    Offset : longword;
    Size : longword;
    Link : longword;
    Info : longword;
    AddrAlign : longword;
    EntSize : longword;
  end;

  TElf64SectHdr = packed record
    NameIdx : longword;
    _Type : longword;
    Flags : qword;
    Address : qword;
    Offset : qword;
    Size : qword;
    Link : longword;
    Info : longword;
    AddrAlign : qword;
    EntSize : qword;
  end;
  PElf64SectHdr = ^TElf64SectHdr;
  
  TElf32Symbol = packed record
    Name : longword;
    Value : longword;
    Size : longword;
    Info : byte;
    Other : byte;
    SectIdx : word;
  end;
  
  TElf64Symbol = packed record
    Name : longword;
    Info : byte;
    Other : byte;
    SectIdx : word;
    Value : qword;
    Size : qword;
  end;
  PElf64Symbol = ^TElf64Symbol;

  TElf32Rel = packed record
    Offset : longword;
    Info   : longword;
  end;

  TElf32Rela = packed record
    Offset : longword;
    Info   : longword;
    addend : longint;
  end;
  PElf32Rela = ^TElf32Rela;

  TElf64Rel = packed record
    Offset : qword;
    Info   : qword;
  end;

  TElf64Rela = packed record
    Offset : qword;
    Info   : qword;
    addend : int64;
  end;
  PElf64Rela = ^TElf64Rela;

implementation

end.
