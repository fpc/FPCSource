{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Peter Vreman

    Executable file reading functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit exeinfo;
interface

{$mode objfpc}
{$S-}

type
  TExeFile=record
    f : file;
    // cached filesize
    size      : int64;
    isopen    : boolean;
    nsects    : longint;
    sechdrofs,
    secstrofs : ptruint;
    processaddress : ptruint;
    FunctionRelative: boolean;
    filename  : string;
    // Allocate static buffer for reading data
    buf       : array[0..4095] of byte;
    bufsize,
    bufcnt    : longint;
  end;

function OpenExeFile(out e:TExeFile;const fn:string):boolean;
function FindExeSection(var e:TExeFile;const secname:string;out secofs,seclen:longint):boolean;
function CloseExeFile(var e:TExeFile):boolean;
function ReadDebugLink(var e:TExeFile;out dbgfn:string):boolean;


implementation

uses
  strings;


{****************************************************************************
                             Executable Loaders
****************************************************************************}

{$if defined(netbsd) or defined(freebsd) or defined(linux) or defined(sunos)}
  {$ifdef cpu64}
    {$define ELF64}
  {$else}
    {$define ELF32}
  {$endif}
{$endif}

{$if defined(win32) or defined(wince)}
  {$define PE32}
{$endif}

{$if defined(win64)}
  {$define PE32PLUS}
{$endif}

{$ifdef netwlibc}
  {$define netware}
{$endif}

{$IFDEF OS2}
  {$DEFINE EMX}
{$ENDIF OS2}

{$ifdef netware}

const SIZE_OF_NLM_INTERNAL_FIXED_HEADER = 130;
      SIZE_OF_NLM_INTERNAL_VERSION_HEADER = 32;
      SIZE_OF_NLM_INTERNAL_EXTENDED_HEADER = 124;

function loadNetwareNLM:boolean;
var valid : boolean;
    name  : string;
    StabLength,
    StabStrLength,
    alignAmount,
    hdrLength,
    dataOffset,
    dataLength : longint;

  function getByte:byte;
  begin
    BlockRead (f,getByte,1);
  end;

  procedure Skip (bytes : longint);
  var i : longint;
  begin
    for i := 1 to bytes do getbyte;
  end;

  function getLString : String;
  var Res:string;
  begin
    blockread (F, res, 1);
    if length (res) > 0 THEN
      blockread (F, res[1], length (res));
    getbyte;
    getLString := res;
  end;

  function getFixString (Len : byte) : string;
  var i : byte;
  begin
    getFixString := '';
    for I := 1 to Len do
      getFixString := getFixString + char (getbyte);
  end;

  function get0String : string;
  var c : char;
  begin
    get0String := '';
    c := char (getbyte);
    while (c <> #0) do
    begin
      get0String := get0String + c;
      c := char (getbyte);
    end;
  end;

  function getword : word;
  begin
    blockread (F, getword, 2);
  end;

  function getint32 : longint;
  begin
    blockread (F, getint32, 4);
  end;

begin
  processaddress := 0;
  LoadNetwareNLM:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  Skip (SIZE_OF_NLM_INTERNAL_FIXED_HEADER);
  getLString;  // NLM Description
  getInt32;    // Stacksize
  getInt32;    // Reserved
  skip(5);     // old Thread Name
  getLString;  // Screen Name
  getLString;  // Thread Name
  hdrLength := -1;
  dataOffset := -1;
  dataLength := -1;
  valid := true;
  repeat
    name := getFixString (8);
    if (name = 'VeRsIoN#') then
    begin
      Skip (SIZE_OF_NLM_INTERNAL_VERSION_HEADER-8);
    end else
    if (name = 'CoPyRiGh') then
    begin
      getword;     // T=
      getLString;  // Copyright String
    end else
    if (name = 'MeSsAgEs') then
    begin
      skip (SIZE_OF_NLM_INTERNAL_EXTENDED_HEADER - 8);
    end else
    if (name = 'CuStHeAd') then
    begin
      hdrLength := getInt32;
      dataOffset := getInt32;
      dataLength := getInt32;
      Skip (8); // dataStamp
      Valid := false;
    end else
      Valid := false;
  until not valid;
  if (hdrLength = -1) or (dataOffset = -1) or (dataLength = -1) then
    exit;
  (* The format of the section information is:
       null terminated section name
       zeroes to adjust to 4 byte boundary
       4 byte section data file pointer
       4 byte section size *)
  Seek (F, dataOffset);
  stabOfs := 0;
  stabStrOfs := 0;
  Repeat
    Name := Get0String;
    alignAmount := 4 - ((length (Name) + 1) MOD 4);
    Skip (alignAmount);
    if (Name = '.stab') then
    begin
      stabOfs := getInt32;
      stabLength := getInt32;
      stabcnt:=stabLength div sizeof(tstab);
    end else
    if (Name = '.stabstr') then
    begin
      stabStrOfs := getInt32;
      stabStrLength := getInt32;
    end else
      Skip (8);
  until (Name = '') or ((StabOfs <> 0) and (stabStrOfs <> 0));
  Seek (F,stabOfs);
  //if (StabOfs = 0) then __ConsolePrintf ('StabOfs = 0');
  //if (StabStrOfs = 0) then __ConsolePrintf ('StabStrOfs = 0');
  LoadNetwareNLM := ((stabOfs > 0) and (stabStrOfs > 0));
end;
{$endif}

{$ifdef go32v2}
function LoadGo32Coff:boolean;
type
  tcoffheader=packed record
    mach   : word;
    nsects : word;
    time   : longint;
    sympos : longint;
    syms   : longint;
    opthdr : word;
    flag   : word;
    other  : array[0..27] of byte;
  end;
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
var
  coffheader : tcoffheader;
  coffsec    : tcoffsechdr;
  i : longint;
begin
  processaddress := 0;
  LoadGo32Coff:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if e.size<2048+sizeof(tcoffheader) then
   exit;
  seek(f,2048);
  blockread(f,coffheader,sizeof(tcoffheader));
  if coffheader.mach<>$14c then
   exit;
  { read section info }
  for i:=1to coffheader.nSects do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if (coffsec.name[4]='b') and
        (coffsec.name[1]='s') and
        (coffsec.name[2]='t') then
      begin
        if (coffsec.name[5]='s') and
           (coffsec.name[6]='t') then
         stabstrofs:=coffsec.datapos+2048
        else
         begin
           stabofs:=coffsec.datapos+2048;
           stabcnt:=coffsec.datalen div sizeof(tstab);
         end;
      end;
   end;
  LoadGo32Coff:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif Go32v2}


{$ifdef PE32}
function OpenPeCoff(var e:TExeFile):boolean;
type
  tdosheader = packed record
     e_magic : word;
     e_cblp : word;
     e_cp : word;
     e_crlc : word;
     e_cparhdr : word;
     e_minalloc : word;
     e_maxalloc : word;
     e_ss : word;
     e_sp : word;
     e_csum : word;
     e_ip : word;
     e_cs : word;
     e_lfarlc : word;
     e_ovno : word;
     e_res : array[0..3] of word;
     e_oemid : word;
     e_oeminfo : word;
     e_res2 : array[0..9] of word;
     e_lfanew : longint;
  end;
  tpeheader = packed record
     PEMagic : longint;
     Machine : word;
     NumberOfSections : word;
     TimeDateStamp : longint;
     PointerToSymbolTable : longint;
     NumberOfSymbols : longint;
     SizeOfOptionalHeader : word;
     Characteristics : word;
     Magic : word;
     MajorLinkerVersion : byte;
     MinorLinkerVersion : byte;
     SizeOfCode : longint;
     SizeOfInitializedData : longint;
     SizeOfUninitializedData : longint;
     AddressOfEntryPoint : longint;
     BaseOfCode : longint;
     BaseOfData : longint;
     ImageBase : longint;
     SectionAlignment : longint;
     FileAlignment : longint;
     MajorOperatingSystemVersion : word;
     MinorOperatingSystemVersion : word;
     MajorImageVersion : word;
     MinorImageVersion : word;
     MajorSubsystemVersion : word;
     MinorSubsystemVersion : word;
     Reserved1 : longint;
     SizeOfImage : longint;
     SizeOfHeaders : longint;
     CheckSum : longint;
     Subsystem : word;
     DllCharacteristics : word;
     SizeOfStackReserve : longint;
     SizeOfStackCommit : longint;
     SizeOfHeapReserve : longint;
     SizeOfHeapCommit : longint;
     LoaderFlags : longint;
     NumberOfRvaAndSizes : longint;
     DataDirectory : array[1..$80] of byte;
  end;
  coffsymbol=packed record
    name    : array[0..3] of char; { real is [0..7], which overlaps the strofs ! }
    strofs  : longint;
    value   : longint;
    section : smallint;
    empty   : word;
    typ     : byte;
    aux     : byte;
  end;
var
  dosheader  : tdosheader;
  peheader   : tpeheader;
begin
  result:=false;
  { read and check header }
  if e.size<sizeof(dosheader) then
    exit;
  blockread(e.f,dosheader,sizeof(tdosheader));
  seek(e.f,dosheader.e_lfanew);
  blockread(e.f,peheader,sizeof(tpeheader));
  if peheader.pemagic<>$4550 then
    exit;
  e.sechdrofs:=filepos(e.f);
  e.nsects:=peheader.NumberOfSections;
  e.secstrofs:=peheader.PointerToSymbolTable+peheader.NumberOfSymbols*sizeof(coffsymbol)+4;
  if e.secstrofs>e.size then
    exit;
  result:=true;
end;
{$endif PE32}


{$if defined(PE32) or defined(PE32PLUS)}
function FindSectionPECoff(var e:TExeFile;const asecname:string;out secofs,seclen:longint):boolean;
type
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
var
  i : longint;
  sechdr     : tcoffsechdr;
  secname    : string;
  secnamebuf : array[0..255] of char;
  code,
  oldofs,
  bufsize    : longint;
  strofs     : cardinal;
begin
  result:=false;
  { read section info }
  seek(e.f,e.sechdrofs);
  for i:=1 to e.nsects do
   begin
     blockread(e.f,sechdr,sizeof(sechdr),bufsize);
     move(sechdr.name,secnamebuf,8);
     secnamebuf[8]:=#0;
     secname:=strpas(secnamebuf);
     if secname[1]='/' then
       begin
         Val(Copy(secname,2,8),strofs,code);
         if code=0 then
           begin
             fillchar(secnamebuf,sizeof(secnamebuf),0);
             oldofs:=filepos(e.f);
             seek(e.f,e.secstrofs+strofs);
             blockread(e.f,secnamebuf,sizeof(secnamebuf),bufsize);
             seek(e.f,oldofs);
             secname:=strpas(secnamebuf);
           end
         else
           secname:='';
       end;
     if asecname=secname then
       begin
         secofs:=sechdr.datapos;
         seclen:=sechdr.datalen;
         result:=true;
         exit;
       end;
   end;
end;
{$endif PE32 or PE32PLUS}


{$ifdef PE32PLUS}
function OpenPePlusCoff(var e:TExeFile):boolean;
type
  tdosheader = packed record
     e_magic : word;
     e_cblp : word;
     e_cp : word;
     e_crlc : word;
     e_cparhdr : word;
     e_minalloc : word;
     e_maxalloc : word;
     e_ss : word;
     e_sp : word;
     e_csum : word;
     e_ip : word;
     e_cs : word;
     e_lfarlc : word;
     e_ovno : word;
     e_res : array[0..3] of word;
     e_oemid : word;
     e_oeminfo : word;
     e_res2 : array[0..9] of word;
     e_lfanew : longint;
  end;
  tpeheader = packed record
     PEMagic : longint;
     Machine : word;
     NumberOfSections : word;
     TimeDateStamp : longint;
     PointerToSymbolTable : longint;
     NumberOfSymbols : longint;
     SizeOfOptionalHeader : word;
     Characteristics : word;
     Magic : word;
     MajorLinkerVersion : byte;
     MinorLinkerVersion : byte;
     SizeOfCode : longint;
     SizeOfInitializedData : longint;
     SizeOfUninitializedData : longint;
     AddressOfEntryPoint : longint;
     BaseOfCode : longint;
     BaseOfData : longint;
     ImageBase : longint;
     SectionAlignment : longint;
     FileAlignment : longint;
     MajorOperatingSystemVersion : word;
     MinorOperatingSystemVersion : word;
     MajorImageVersion : word;
     MinorImageVersion : word;
     MajorSubsystemVersion : word;
     MinorSubsystemVersion : word;
     Reserved1 : longint;
     SizeOfImage : longint;
     SizeOfHeaders : longint;
     CheckSum : longint;
     Subsystem : word;
     DllCharacteristics : word;
     SizeOfStackReserve : int64;
     SizeOfStackCommit : int64;
     SizeOfHeapReserve : int64;
     SizeOfHeapCommit : int64;
     LoaderFlags : longint;
     NumberOfRvaAndSizes : longint;
     DataDirectory : array[1..$80] of byte;
  end;
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
  coffsymbol=packed record
    name    : array[0..3] of char; { real is [0..7], which overlaps the strofs ! }
    strofs  : longint;
    value   : longint;
    section : smallint;
    empty   : word;
    typ     : byte;
    aux     : byte;
  end;
var
  dosheader  : tdosheader;
  peheader   : tpeheader;
begin
  result:=false;
  { read and check header }
  if E.Size<sizeof(dosheader) then
   exit;
  blockread(E.F,dosheader,sizeof(tdosheader));
  seek(E.F,dosheader.e_lfanew);
  blockread(E.F,peheader,sizeof(tpeheader));
  if peheader.pemagic<>$4550 then
   exit;
  e.sechdrofs:=filepos(e.f);
  e.nsects:=peheader.NumberOfSections;
  e.secstrofs:=peheader.PointerToSymbolTable+peheader.NumberOfSymbols*sizeof(coffsymbol)+4;
  if e.secstrofs>e.size then
    exit;
  result:=true;
end;
{$endif PE32PLUS}


{$IFDEF EMX}
type
  TDosHeader = packed record
     e_magic : word;
     e_cblp : word;
     e_cp : word;
     e_crlc : word;
     e_cparhdr : word;
     e_minalloc : word;
     e_maxalloc : word;
     e_ss : word;
     e_sp : word;
     e_csum : word;
     e_ip : word;
     e_cs : word;
     e_lfarlc : word;
     e_ovno : word;
     e_res : array[0..3] of word;
     e_oemid : word;
     e_oeminfo : word;
     e_res2 : array[0..9] of word;
     e_lfanew : longint;
  end;

  TEmxHeader = packed record
     Version: array [1..16] of char;
     Bound: word;
     AoutOfs: longint;
     Options: array [1..42] of char;
  end;

  TAoutHeader = packed record
     Magic: word;
     Machine: byte;
     Flags: byte;
     TextSize: longint;
     DataSize: longint;
     BssSize: longint;
     SymbSize: longint;
     EntryPoint: longint;
     TextRelocSize: longint;
     DataRelocSize: longint;
  end;

const
 StartPageSize = $1000;

var
 DosHeader: TDosHeader;
 EmxHeader: TEmxHeader;
 AoutHeader: TAoutHeader;
 StabOfs: PtrUInt;
 S4: string [4];

function OpenEMXaout (var E: TExeFile): boolean;
begin
 OpenEMXaout := false;
{ GDB after 4.18 uses offset to function begin
  in text section but OS/2 version still uses 4.16 PM }
 E.FunctionRelative := false;
{ read and check header }
 if E.Size > SizeOf (DosHeader) then
 begin
  BlockRead (E.F, DosHeader, SizeOf (TDosHeader));
  if E.Size > DosHeader.e_cparhdr shl 4 + SizeOf (TEmxHeader) then
  begin
   Seek (E.F, DosHeader.e_cparhdr shl 4);
   BlockRead (E.F, EmxHeader, SizeOf (TEmxHeader));
  S4 [0] := #4;
  Move (EmxHeader.Version, S4 [1], 4);
   if (S4 = 'emx ') and
                       (E.Size > EmxHeader.AoutOfs + SizeOf (TAoutHeader)) then
   begin
    Seek (E.F, EmxHeader.AoutOfs);
    BlockRead (E.F, AoutHeader, SizeOf (TAoutHeader));
   if AOutHeader.Magic = $10B then
     StabOfs := StartPageSize
   else
     StabOfs := EmxHeader.AoutOfs + SizeOf (TAoutHeader);
   StabOfs := StabOfs
                + AoutHeader.TextSize
                + AoutHeader.DataSize
                + AoutHeader.TextRelocSize
                + AoutHeader.DataRelocSize;
    if E.Size > StabOfs + AoutHeader.SymbSize then
     OpenEMXaout := true;
   end;
  end;
 end;
end;


function FindSectionEMXaout (var E: TExeFile; const ASecName: string;
                                         out SecOfs, SecLen: longint): boolean;
begin
 FindSectionEMXaout := false;
 if ASecName = '.stab' then
 begin
  SecOfs := StabOfs;
  SecLen := AoutHeader.SymbSize;
  FindSectionEMXaout := true;
 end else
 if ASecName = '.stabstr' then
 begin
  SecOfs := StabOfs + AoutHeader.SymbSize;
  SecLen := E.Size - Pred (SecOfs);
  FindSectionEMXaout := true;
 end;
end;
{$ENDIF EMX}


{$if defined(ELF32) or defined(ELF64)}
type
{$ifdef ELF32}
  telfheader=packed record
      magic0123         : longint;
      file_class        : byte;
      data_encoding     : byte;
      file_version      : byte;
      padding           : array[$07..$0f] of byte;
      e_type            : word;
      e_machine         : word;
      e_version         : longword;
      e_entry           : longword;                  // entrypoint
      e_phoff           : longword;                  // program header offset
      e_shoff           : longword;                  // sections header offset
      e_flags           : longword;
      e_ehsize          : word;             // elf header size in bytes
      e_phentsize       : word;             // size of an entry in the program header array
      e_phnum           : word;             // 0..e_phnum-1 of entrys
      e_shentsize       : word;             // size of an entry in sections header array
      e_shnum           : word;             // 0..e_shnum-1 of entrys
      e_shstrndx        : word;             // index of string section header
  end;
  telfsechdr=packed record
      sh_name           : longword;
      sh_type           : longword;
      sh_flags          : longword;
      sh_addr           : longword;
      sh_offset         : longword;
      sh_size           : longword;
      sh_link           : longword;
      sh_info           : longword;
      sh_addralign      : longword;
      sh_entsize        : longword;
    end;
{$endif ELF32}
{$ifdef ELF64}
  telfheader=packed record
      magic0123         : longint;
      file_class        : byte;
      data_encoding     : byte;
      file_version      : byte;
      padding           : array[$07..$0f] of byte;
      e_type            : word;
      e_machine         : word;
      e_version         : longword;
      e_entry           : int64;                  // entrypoint
      e_phoff           : int64;                  // program header offset
      e_shoff           : int64;                  // sections header offset
      e_flags           : longword;
      e_ehsize          : word;             // elf header size in bytes
      e_phentsize       : word;             // size of an entry in the program header array
      e_phnum           : word;             // 0..e_phnum-1 of entrys
      e_shentsize       : word;             // size of an entry in sections header array
      e_shnum           : word;             // 0..e_shnum-1 of entrys
      e_shstrndx        : word;             // index of string section header
  end;
  telfsechdr=packed record
      sh_name           : longword;
      sh_type           : longword;
      sh_flags          : int64;
      sh_addr           : int64;
      sh_offset         : int64;
      sh_size           : int64;
      sh_link           : longword;
      sh_info           : longword;
      sh_addralign      : int64;
      sh_entsize        : int64;
    end;
{$endif ELF64}

function OpenElf(var e:TExeFile):boolean;
var
  elfheader : telfheader;
  elfsec    : telfsechdr;
begin
  result:=false;
  { read and check header }
  if e.size<sizeof(telfheader) then
   exit;
  blockread(e.f,elfheader,sizeof(telfheader));
 if elfheader.magic0123<>{$ifdef ENDIAN_LITTLE}$464c457f{$else}$7f454c46{$endif} then
   exit;
  if elfheader.e_shentsize<>sizeof(telfsechdr) then
   exit;
  { read section names }
  seek(e.f,elfheader.e_shoff+elfheader.e_shstrndx*cardinal(sizeof(telfsechdr)));
  blockread(e.f,elfsec,sizeof(telfsechdr));
  e.secstrofs:=elfsec.sh_offset;
  e.sechdrofs:=elfheader.e_shoff;
  e.nsects:=elfheader.e_shnum;
  result:=true;
end;

function FindSectionElf(var e:TExeFile;const asecname:string;out secofs,seclen:longint):boolean;
var
  elfsec     : telfsechdr;
  secname    : string;
  secnamebuf : array[0..255] of char;
  oldofs,
  bufsize,i  : longint;
begin
  result:=false;
  seek(e.f,e.sechdrofs);
  for i:=1 to e.nsects do
   begin
     blockread(e.f,elfsec,sizeof(telfsechdr));
     fillchar(secnamebuf,sizeof(secnamebuf),0);
     oldofs:=filepos(e.f);
     seek(e.f,e.secstrofs+elfsec.sh_name);
     blockread(e.f,secnamebuf,sizeof(secnamebuf),bufsize);
     seek(e.f,oldofs);
     secname:=strpas(secnamebuf);
     if asecname=secname then
       begin
         secofs:=elfsec.sh_offset;
         seclen:=elfsec.sh_size;
         result:=true;
         exit;
       end;
   end;
end;
{$endif ELF32}



{$ifdef beos}

{$i ptypes.inc}

{ ------------------------- Images --------------------------- }

type
  // Descriptive formats
  status_t = Longint;
  team_id   = Longint;
  image_id = Longint;

    { image types }
const
   B_APP_IMAGE     = 1;
   B_LIBRARY_IMAGE = 2;
   B_ADD_ON_IMAGE  = 3;
   B_SYSTEM_IMAGE  = 4;

type
    image_info = packed record
     id      : image_id;
     _type   : longint;
     sequence: longint;
     init_order: longint;
     init_routine: pointer;
     term_routine: pointer;
     device: dev_t;
     node: ino_t;
     name: array[0..MAXPATHLEN-1] of char;
{     name: string[255];
     name2: string[255];
     name3: string[255];
     name4: string[255];
     name5: string[5];
}
     text: pointer;
     data: pointer;
     text_size: longint;
     data_size: longint;
    end;

function get_next_image_info(team: team_id; var cookie:longint; var info:image_info; size: size_t) : status_t;cdecl; external 'root' name '_get_next_image_info';

function LoadElf32Beos:boolean;
type
  telf32header=packed record
      magic0123         : longint;
      file_class        : byte;
      data_encoding     : byte;
      file_version      : byte;
      padding           : array[$07..$0f] of byte;
      e_type            : word;
      e_machine         : word;
      e_version         : longword;
      e_entry           : longword;                  // entrypoint
      e_phoff           : longword;                  // program header offset
      e_shoff           : longword;                  // sections header offset
      e_flags           : longword;
      e_ehsize          : word;             // elf header size in bytes
      e_phentsize       : word;             // size of an entry in the program header array
      e_phnum           : word;             // 0..e_phnum-1 of entrys
      e_shentsize       : word;             // size of an entry in sections header array
      e_shnum           : word;             // 0..e_shnum-1 of entrys
      e_shstrndx        : word;             // index of string section header
  end;
  telf32sechdr=packed record
      sh_name           : longword;
      sh_type           : longword;
      sh_flags          : longword;
      sh_addr           : longword;
      sh_offset         : longword;
      sh_size           : longword;
      sh_link           : longword;
      sh_info           : longword;
      sh_addralign      : longword;
      sh_entsize        : longword;
    end;
var
  elfheader : telf32header;
  elfsec    : telf32sechdr;
  secnames  : array[0..255] of char;
  pname     : pchar;
  i : longint;
  cookie    : longint;
  info      : image_info;
  result    : status_t;
begin
  cookie := 0;
  fillchar(info, sizeof(image_info), 0);
  get_next_image_info(0,cookie,info,sizeof(info));
  if (info._type = B_APP_IMAGE) then
     processaddress := cardinal(info.text)
  else
     processaddress := 0;
  LoadElf32Beos:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if E.Size<sizeof(telf32header) then
   exit;
  blockread(f,elfheader,sizeof(telf32header));
{$ifdef ENDIAN_LITTLE}
 if elfheader.magic0123<>$464c457f then
   exit;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
 if elfheader.magic0123<>$7f454c46 then
   exit;
{$endif ENDIAN_BIG}
  if elfheader.e_shentsize<>sizeof(telf32sechdr) then
   exit;
  { read section names }
  seek(f,elfheader.e_shoff+elfheader.e_shstrndx*cardinal(sizeof(telf32sechdr)));
  blockread(f,elfsec,sizeof(telf32sechdr));
  seek(f,elfsec.sh_offset);
  blockread(f,secnames,sizeof(secnames));
  { read section info }
  seek(f,elfheader.e_shoff);
  for i:=1to elfheader.e_shnum do
   begin
     blockread(f,elfsec,sizeof(telf32sechdr));
     pname:=@secnames[elfsec.sh_name];
     if (pname[4]='b') and
        (pname[1]='s') and
        (pname[2]='t') then
      begin
        if (pname[5]='s') and
           (pname[6]='t') then
         stabstrofs:=elfsec.sh_offset
        else
         begin
           stabofs:=elfsec.sh_offset;
           stabcnt:=elfsec.sh_size div sizeof(tstab);
         end;
      end;
   end;
  LoadElf32Beos:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif beos}

{$ifdef darwin}
type
MachoFatHeader=
packed record
    magic: longint;
    nfatarch: longint;
end;

MachoHeader=
packed record
     magic: longword;
     cpu_type_t: longint;
     cpu_subtype_t: longint;
     filetype: longint;
     ncmds: longint;
     sizeofcmds: longint;
     flags: longint;

end;

cmdblock=
packed record
   cmd: longint;
   cmdsize: longint;
end;

symbSeg=
packed record
 symoff :      longint;
 nsyms  :      longint;
 stroff :      longint;
 strsize:      longint;
end;


  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : dword;
  end;


function OpenMachO32PPC(var e:TExeFile):boolean;
var
   mh:MachoHeader;
begin
  result:= false;
  if e.size<sizeof(mh) then
    exit;
  blockread (e.f, mh, sizeof(mh));
  e.sechdrofs:=filepos(e.f);
  e.nsects:=mh.ncmds;
  result:=true;
end;


function FindSectionMachO32PPC(var e:TExeFile;const asecname:string;out secofs,seclen:longint):boolean;
var
   i: longint;
   block:cmdblock;
   symbolsSeg:  symbSeg;
begin
  seek(e.f,e.sechdrofs);
  for i:= 1 to e.nsects do
    begin
      blockread (e.f, block, sizeof(block));
      if block.cmd = $2   then
      begin
          blockread (e.f, symbolsSeg, sizeof(symbolsSeg));
          if asecname='.stab' then
            begin
              secofs:=symbolsSeg.symoff;
              { the caller will divide again by sizeof(tstab) }
              seclen:=symbolsSeg.nsyms*sizeof(tstab);
            end
          else if asecname='.stabstr' then
            begin
              secofs:=symbolsSeg.stroff;
              seclen:=symbolsSeg.strsize;
            end;
          result:=true;
          exit;
      end;
      Seek(e.f, FilePos (e.f) + block.cmdsize - sizeof(block));
    end;
end;
{$endif darwin}


{****************************************************************************
                                   CRC
****************************************************************************}

var
  Crc32Tbl : array[0..255] of cardinal;

procedure MakeCRC32Tbl;
var
  crc : cardinal;
  i,n : integer;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if (crc and 1)<>0 then
       crc:=(crc shr 1) xor cardinal($edb88320)
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


Function UpdateCrc32(InitCrc:cardinal;const InBuf;InLen:Integer):cardinal;
var
  i : integer;
  p : pchar;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  p:=@InBuf;
  result:=not InitCrc;
  for i:=1 to InLen do
   begin
     result:=Crc32Tbl[byte(result) xor byte(p^)] xor (result shr 8);
     inc(p);
   end;
  result:=not result;
end;


{****************************************************************************
                         Generic Executable Open/Close
****************************************************************************}

type
  TOpenProc=function(var e:TExeFile):boolean;
  TFindSectionProc=function(var e:TExeFile;const asecname:string;out secofs,seclen:longint):boolean;

  TExeProcRec=record
    openproc : TOpenProc;
    findproc : TFindSectionProc;
  end;

const
  ExeProcs : TExeProcRec = (
{$ifdef go32v2}
     openproc : @OpenGo32Coff;
     findproc : @FindSectionGo32Coff;
{$endif}
{$IFDEF EMX}
     openproc : @OpenEMXaout;
     findproc : @FindSectionEMXaout;
{$ENDIF EMX}
{$ifdef PE32}
     openproc : @OpenPeCoff;
     findproc : @FindSectionPeCoff;
{$endif}
{$ifdef PE32PLUS}
     openproc : @OpenPePlusCoff;
     findproc : @FindSectionPeCoff;
{$endif PE32PLUS}
{$if defined(ELF32) or defined(ELF64)}
     openproc : @OpenElf;
     findproc : @FindSectionElf;
{$endif}
{$ifdef Beos}
     openproc : @OpenElf32Beos;
     findproc : @FindSectionElf32Beos;
{$endif}
{$ifdef darwin}
     openproc : @OpenMachO32PPC;
     findproc : @FindSectionMachO32PPC;
{$endif darwin}
{$ifdef netware}
     openproc : @OpenNetwareNLM;
     findproc : @FindSectionNetwareNLM;
{$endif}
   );

function OpenExeFile(out e:TExeFile;const fn:string):boolean;
var
  ofm : word;
begin
  result:=false;
  fillchar(e,sizeof(e),0);
  e.bufsize:=sizeof(e.buf);
  e.filename:=fn;
  assign(e.f,fn);
  {$I-}
   ofm:=filemode;
   filemode:=$40;
   reset(e.f,1);
   filemode:=ofm;
  {$I+}
  if ioresult<>0 then
   exit;
  e.isopen:=true;
  // cache filesize
  e.size:=filesize(e.f);

  E.FunctionRelative := true;
  if ExeProcs.OpenProc<>nil then
    result:=ExeProcs.OpenProc(e);
end;


function CloseExeFile(var e:TExeFile):boolean;
begin
  result:=false;
  if not e.isopen then
    exit;
  e.isopen:=false;
  close(e.f);
  result:=true;
end;


function FindExeSection(var e:TExeFile;const secname:string;out secofs,seclen:longint):boolean;
begin
  result:=false;
  if not e.isopen then
    exit;
  if ExeProcs.FindProc<>nil then
    result:=ExeProcs.FindProc(e,secname,secofs,seclen);
end;



function CheckDbgFile(var e:TExeFile;const fn:string;dbgcrc:cardinal):boolean;
var
  c      : cardinal;
  ofm    : word;
  g      : file;
begin
  result:=false;
  assign(g,fn);
  {$I-}
   ofm:=filemode;
   filemode:=$40;
   reset(g,1);
   filemode:=ofm;
  {$I+}
  if ioresult<>0 then
   exit;
  { We reuse the buffer from e here to prevent too much stack allocation }
  c:=0;
  repeat
    blockread(g,e.buf,e.bufsize,e.bufcnt);
    c:=UpdateCrc32(c,e.buf,e.bufcnt);
  until e.bufcnt<e.bufsize;
  close(g);
  result:=(dbgcrc=c);
end;


function ReadDebugLink(var e:TExeFile;out dbgfn:string):boolean;
var
  dbglink : array[0..255] of char;
  i,
  dbglinklen,
  dbglinkofs : longint;
  dbgcrc     : cardinal;
begin
  result:=false;
  if not FindExeSection(e,'.gnu_debuglink',dbglinkofs,dbglinklen) then
    exit;
  if dbglinklen>sizeof(dbglink)-1 then
    exit;
  fillchar(dbglink,sizeof(dbglink),0);
  seek(e.f,dbglinkofs);
  blockread(e.f,dbglink,dbglinklen);
  dbgfn:=strpas(dbglink);
  if length(dbgfn)=0 then
    exit;
  i:=align(length(dbgfn)+1,4);
  if i>dbglinklen then
    exit;
  move(dbglink[i],dbgcrc,4);
  { current dir }
  if CheckDbgFile(e,dbgfn,dbgcrc) then
    begin
      result:=true;
      exit;
    end;
  { executable dir }
  i:=length(e.filename);
  while (i>0) and not(e.filename[i] in ['/','\']) do
    dec(i);
  if i>0 then
    begin
      dbgfn:=copy(e.filename,1,i)+dbgfn;
      if CheckDbgFile(e,dbgfn,dbgcrc) then
        begin
          result:=true;
          exit;
        end;
    end;
end;


end.
