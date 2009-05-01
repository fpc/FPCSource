{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Types used by Mach-O resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit machotypes;

{$MODE OBJFPC}

interface

type
  TMachOMachineType = (mmtpowerpc, mmtpowerpc64, mmti386, mmtx86_64, mmtarm);
  TSegSectName = array[0..15] of char;

type
  TMachHdr = packed record
    magic : longword;
    cputype : longint;
    cpusubtype : longint;
    filetype : longword;
    ncmds : longword;
    sizeofcmds : longword;
    flags : longword;
  end;

  TLoadCommand = packed record
    cmd : longword;
    cmdsize : longword;
  end;

  //note: all commands don't include first two longwords

  TSegmentCommand32 = packed record
    name     : TSegSectName;
    vmaddr   : longword;
    vmsize   : longword;
    fileoff  : longword;
    filesize : longword;
    maxprot  : longint;
    initprot : longint;
    nsects   : longword;
    flags    : longword;
  end;

  TSegmentCommand64 = packed record
    name     : TSegSectName;
    vmaddr   : qword;
    vmsize   : qword;
    fileoff  : qword;
    filesize : qword;
    maxprot  : longint;
    initprot : longint;
    nsects   : longword;
    flags    : longword;
  end;
  
  TSection32 = packed record
    sectname : TSegSectName;
    segname  : TSegSectName;
    addr : longword;
    size : longword;
    offset : longword;
    align : longword;
    reloff : longword;
    nreloc : longword;
    flags : longword;
    reserved1 : longword;
    reserved2 : longword;
  end;

  TSection64 = packed record
    sectname : TSegSectName;
    segname  : TSegSectName;
    addr : qword;
    size : qword;
    offset : longword;
    align : longword;
    reloff : longword;
    nreloc : longword;
    flags : longword;
    reserved1 : longword;
    reserved2 : longword;
    reserved3 : longword;
  end;

  TSymtabCommand = packed record
    symoff : longword;
    nsyms : longword;
    stroff : longword;
    strsize : longword;
  end;
  
  TDySymtabCommand = packed record
    ilocalsym : longword;
    nlocalsym : longword;
    iextdefsym : longword;
    nextdefsym : longword;
    iundefsym : longword;
    nundefsym : longword;
    tocoff : longword;
    ntoc : longword;
    modtaboff : longword;
    nmodtab : longword;
    extrefsymoff : longword;
    nextrefsyms : longword;
    indirectsymoff : longword;
    nindirectsyms : longword;
    extreloff : longword;
    nextrel : longword;
    locreloff : longword;
    nlocrel : longword;
  end;
  
  TNList32 = packed record
    strx : longword;
    _type : byte;
    sect : byte;
    desc : word;
    value : longword;
  end;
  PNList32 = ^TNList32;
  
  TNList64 = packed record
    strx : longword;
    _type : byte;
    sect : byte;
    desc : word;
    value : qword;
  end;
  PNList64 = ^TNList64;

  TRelocationInfo = packed record
    address : longword;
    flags : longword;
  end;
  PRelocationInfo = ^TRelocationInfo;

implementation

end.
