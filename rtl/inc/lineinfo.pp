{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Peter Vreman

    Stabs Line Info Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit lineinfo;
interface

{$IFDEF OS2}
 {$DEFINE EMX} (* EMX is the only possibility under OS/2 at the moment *)
{$ENDIF OS2}

{$S-}

procedure GetLineInfo(addr:ptruint;var func,source:string;var line:longint);


implementation

uses
  strings;

const
  N_Function    = $24;
  N_TextLine    = $44;
  N_DataLine    = $46;
  N_BssLine     = $48;
  N_SourceFile  = $64;
  N_IncludeFile = $84;

  maxstabs = 40; { size of the stabs buffer }
  { GDB after 4.18 uses offset to function begin
    in text section but OS/2 version still uses 4.16 PM }
  StabsFunctionRelative : boolean = true;

type
  pstab=^tstab;
  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : dword;
  end;

{ We use static variable so almost no stack is required, and is thus
  more safe when an error has occured in the program }
var
  opened     : boolean; { set if the file is already open }
  f          : file;    { current file }
  stabcnt,              { amount of stabs }
  stabofs,              { absolute stab section offset in executable }
  stabstrofs : longint; { absolute stabstr section offset in executable }
  dirlength  : longint; { length of the dirctory part of the source file }
  stabs      : array[0..maxstabs-1] of tstab;  { buffer }
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
  dirstab,              { stab with current directory info }
  filestab   : tstab;   { stab with current file info }
  { value to subtract to addr parameter to get correct address on file }
  { this should be equal to the process start address in memory        }
  processaddress : ptruint;



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
  if filesize(f)<2048+sizeof(tcoffheader) then
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
function LoadPeCoff:boolean;
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
  dosheader  : tdosheader;
  peheader   : tpeheader;
  coffsec    : tcoffsechdr;
  i : longint;
begin
  processaddress := 0;
  LoadPeCoff:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if filesize(f)<sizeof(dosheader) then
   exit;
  blockread(f,dosheader,sizeof(tdosheader));
  seek(f,dosheader.e_lfanew);
  blockread(f,peheader,sizeof(tpeheader));
  if peheader.pemagic<>$4550 then
   exit;
  { read section info }
  for i:=1 to peheader.NumberOfSections do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if (coffsec.name[4]='b') and
        (coffsec.name[1]='s') and
        (coffsec.name[2]='t') then
      begin
        if (coffsec.name[5]='s') and
           (coffsec.name[6]='t') then
         stabstrofs:=coffsec.datapos
        else
         begin
           stabofs:=coffsec.datapos;
           stabcnt:=coffsec.datalen div sizeof(tstab);
         end;
      end;
   end;
  LoadPeCoff:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif PE32}


{$ifdef PE32PLUS}
function LoadPeCoff:boolean;
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
var
  dosheader  : tdosheader;
  peheader   : tpeheader;
  coffsec    : tcoffsechdr;
  i : longint;
begin
  processaddress := 0;
  LoadPeCoff:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if filesize(f)<sizeof(dosheader) then
   exit;
  blockread(f,dosheader,sizeof(tdosheader));
  seek(f,dosheader.e_lfanew);
  blockread(f,peheader,sizeof(tpeheader));
  if peheader.pemagic<>$4550 then
   exit;
  { read section info }
  for i:=1 to peheader.NumberOfSections do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if (coffsec.name[4]='b') and
        (coffsec.name[1]='s') and
        (coffsec.name[2]='t') then
      begin
        if (coffsec.name[5]='s') and
           (coffsec.name[6]='t') then
         stabstrofs:=coffsec.datapos
        else
         begin
           stabofs:=coffsec.datapos;
           stabcnt:=coffsec.datalen div sizeof(tstab);
         end;
      end;
   end;
  LoadPeCoff:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif PE32PLUS}


{$IFDEF EMX}
function LoadEMXaout: boolean;
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
 S4: string [4];
begin
 processaddress := 0;
 LoadEMXaout := false;
 StabOfs := -1;
 StabStrOfs := -1;
{ read and check header }
 if FileSize (F) > SizeOf (DosHeader) then
 begin
  BlockRead (F, DosHeader, SizeOf (TDosHeader));
  Seek (F, DosHeader.e_cparhdr shl 4);
  BlockRead (F, EmxHeader, SizeOf (TEmxHeader));
  S4 [0] := #4;
  Move (EmxHeader.Version, S4 [1], 4);
  if S4 = 'emx ' then
  begin
   Seek (F, EmxHeader.AoutOfs);
   BlockRead (F, AoutHeader, SizeOf (TAoutHeader));

   if AOutHeader.Magic=$10B then
     StabOfs :=   StartPageSize
   else
     StabOfs :=EmxHeader.AoutOfs + SizeOf (TAoutHeader);
   StabOfs :=   StabOfs
                + AoutHeader.TextSize
                + AoutHeader.DataSize
                + AoutHeader.TextRelocSize
                + AoutHeader.DataRelocSize;
   StabCnt := AoutHeader.SymbSize div SizeOf (TStab);
   StabStrOfs := StabOfs + AoutHeader.SymbSize;
   StabsFunctionRelative:=false;
   LoadEMXaout := (StabOfs <> -1) and (StabStrOfs <> -1);
  end;
 end;
end;
{$ENDIF EMX}


{$ifdef ELF32}
function LoadElf32:boolean;
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
begin
  processaddress := 0;
  LoadElf32:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if filesize(f)<sizeof(telf32header) then
   exit;
  blockread(f,elfheader,sizeof(telf32header));
{$ifdef ENDIAN_LITTLE}
 if elfheader.magic0123<>$464c457f then
   exit;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
 if elfheader.magic0123<>$7f454c46 then
   exit;
 { this seems to be at least the case for m68k cpu PM }
{$ifdef cpum68k}
 {StabsFunctionRelative:=false;}
{$endif cpum68k}
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
  LoadElf32:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif ELF32}


{$ifdef ELF64}
function LoadElf64:boolean;
type
  telf64header=packed record
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
  telf64sechdr=packed record
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
var
  elfheader : telf64header;
  elfsec    : telf64sechdr;
  secnames  : array[0..255] of char;
  pname     : pchar;
  i : longint;
begin
  processaddress := 0;
  LoadElf64:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if filesize(f)<sizeof(telf64header) then
   exit;
  blockread(f,elfheader,sizeof(telf64header));
{$ifdef ENDIAN_LITTLE}
 if elfheader.magic0123<>$464c457f then
   exit;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
 if elfheader.magic0123<>$7f454c46 then
   exit;
 { this seems to be at least the case for m68k cpu PM }
{$ifdef cpum68k}
 {StabsFunctionRelative:=false;}
{$endif cpum68k}
{$endif ENDIAN_BIG}
  if elfheader.e_shentsize<>sizeof(telf64sechdr) then
   exit;
  { read section names }
  seek(f,elfheader.e_shoff+elfheader.e_shstrndx*cardinal(sizeof(telf64sechdr)));
  blockread(f,elfsec,sizeof(telf64sechdr));
  seek(f,elfsec.sh_offset);
  blockread(f,secnames,sizeof(secnames));
  { read section info }
  seek(f,elfheader.e_shoff);
  for i:=1to elfheader.e_shnum do
   begin
     blockread(f,elfsec,sizeof(telf64sechdr));
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
  LoadElf64:=(stabofs<>-1) and (stabstrofs<>-1);
end;
{$endif ELF64}


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
  if filesize(f)<sizeof(telf32header) then
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


function readCommand: boolean;
var
    block:cmdblock;
    readMore :boolean;
    symbolsSeg:  symbSeg;

begin
    readCommand := false;
    readMore := true;
    blockread (f, block, sizeof(block));
    if block.cmd = $2   then
    begin
        blockread (f, symbolsSeg, sizeof(symbolsSeg));
        stabstrofs:=symbolsSeg.stroff;
        stabofs:=symbolsSeg.symoff;
        stabcnt:=symbolsSeg.nsyms;

        readMore := false;
        readCommand := true;
        exit;
    end;
    if readMore then
    begin
       Seek(f, FilePos (f) + block.cmdsize - sizeof(block));
    end;
end;

function LoadMachO32PPC:boolean;
var
   mh:MachoHeader;
   i: longint;
begin
  processaddress := 0;
  StabsFunctionRelative:=false;
  LoadMachO32PPC := false;
  blockread (f, mh, sizeof(mh));
  for i:= 1 to mh.ncmds do
  begin
     if readCommand then
     begin
         LoadMachO32PPC := true;
         exit;
     end;
  end;
end;
{$endif darwin}

{****************************************************************************
                          Executable Open/Close
****************************************************************************}

procedure CloseStabs;
begin
  close(f);
  opened:=false;
end;


function OpenStabs:boolean;
var
  ofm : word;
begin
  OpenStabs:=false;
  assign(f,paramstr(0));
  {$I-}
   ofm:=filemode;
   filemode:=$40;
   reset(f,1);
   filemode:=ofm;
  {$I+}
  if ioresult<>0 then
   exit;
  opened:=true;
{$ifdef go32v2}
  if LoadGo32Coff then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$IFDEF EMX}
  if LoadEMXaout then
   begin
     OpenStabs:=true;
     exit;
   end;
{$ENDIF EMX}
{$ifdef PE32}
  if LoadPECoff then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef PE32PLUS}
  if LoadPECoff then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif PE32PLUS}
{$ifdef ELF32}
  if LoadElf32 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef ELF64}
  if LoadElf64 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef Beos}
  if LoadElf32Beos then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef darwin}
  if LoadMachO32PPC then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif darwin}
{$ifdef netware}
  if LoadNetwareNLM then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
  CloseStabs;
end;


{$Q-}
{ this avoids problems with some targets PM }

procedure GetLineInfo(addr:ptruint;var func,source:string;var line:longint);
var
  res : {$ifdef tp}integer{$else}longint{$endif};
  stabsleft,
  stabscnt,i : longint;
  found : boolean;
  lastfunc : tstab;
begin
  fillchar(func,high(func)+1,0);
  fillchar(source,high(source)+1,0);
  line:=0;
  if not opened then
   begin
     if not OpenStabs then
      exit;
   end;
  { correct the value to the correct address in the file }
  { processaddress is set in OpenStabs                   }
  addr := addr - processaddress;
  //ScreenPrintfL1 (NWLoggerScreen,'addr: %x\n',addr);

  fillchar(funcstab,sizeof(tstab),0);
  fillchar(filestab,sizeof(tstab),0);
  fillchar(dirstab,sizeof(tstab),0);
  fillchar(linestab,sizeof(tstab),0);
  fillchar(lastfunc,sizeof(tstab),0);
  found:=false;
  seek(f,stabofs);
  stabsleft:=stabcnt;
  repeat
    if stabsleft>maxstabs then
     stabscnt:=maxstabs
    else
     stabscnt:=stabsleft;
    blockread(f,stabs,stabscnt*sizeof(tstab),res);
    stabscnt:=res div sizeof(tstab);
    for i:=0 to stabscnt-1 do
     begin
       case stabs[i].ntype of
         N_BssLine,
         N_DataLine,
         N_TextLine :
           begin
             if (stabs[i].ntype=N_TextLine) and StabsFunctionRelative then
               inc(stabs[i].nvalue,lastfunc.nvalue);
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>linestab.nvalue) then
              begin
                { if it's equal we can stop and take the last info }
                if stabs[i].nvalue=addr then
                 found:=true
                else
                 linestab:=stabs[i];
              end;
           end;
         N_Function :
           begin
             lastfunc:=stabs[i];
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>funcstab.nvalue) then
              begin
                funcstab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
              end;
           end;
         N_SourceFile,
         N_IncludeFile :
           begin
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>=filestab.nvalue) then
              begin
                { if same value and type then the first one
                  contained the directory PM }
                if (stabs[i].nvalue=filestab.nvalue) and
                   (stabs[i].ntype=filestab.ntype) then
                  dirstab:=filestab
                else
                  fillchar(dirstab,sizeof(tstab),0);
                filestab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
                { if new file then func is not valid anymore PM }
                if stabs[i].ntype=N_SourceFile then
                  begin
                    fillchar(funcstab,sizeof(tstab),0);
                    fillchar(lastfunc,sizeof(tstab),0);
                  end;
              end;
           end;
       end;
     end;
    dec(stabsleft,stabscnt);
  until found or (stabsleft=0);

{ get the line,source,function info }
  line:=linestab.ndesc;
  if dirstab.ntype<>0 then
   begin
     seek(f,stabstrofs+dirstab.strpos);
     blockread(f,source[1],high(source)-1,res);
     dirlength:=strlen(@source[1]);
     source[0]:=chr(dirlength);
   end
  else
   dirlength:=0;
  if filestab.ntype<>0 then
   begin
     seek(f,stabstrofs+filestab.strpos);
     blockread(f,source[dirlength+1],high(source)-(dirlength+1),res);
     source[0]:=chr(strlen(@source[1]));
   end;
  if funcstab.ntype<>0 then
   begin
     seek(f,stabstrofs+funcstab.strpos);
     blockread(f,func[1],high(func)-1,res);
     func[0]:=chr(strlen(@func[1]));
     i:=pos(':',func);
     if i>0 then
      Delete(func,i,255);
   end;
end;


function StabBackTraceStr(addr:Pointer):shortstring;
var
  func,
  source : string;
  hs     : string[32];
  line   : longint;
  Store  : TBackTraceStrFunc;
begin
  { reset to prevent infinite recursion if problems inside the code PM }
  {$ifdef netware}
  dec(addr,ptruint(system.NWGetCodeStart));  {we need addr relative to code start on netware}
  {$endif}
  Store:=BackTraceStrFunc;
  BackTraceStrFunc:=@SysBackTraceStr;
  GetLineInfo(ptruint(addr),func,source,line);
{ create string }
  {$ifdef netware}
  StabBackTraceStr:='  CodeStart + $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
  {$else}
  StabBackTraceStr:='  $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
  {$endif}
  if func<>'' then
   StabBackTraceStr:=StabBackTraceStr+'  '+func;
  if source<>'' then
   begin
     if func<>'' then
      StabBackTraceStr:=StabBackTraceStr+', ';
     if line<>0 then
      begin
        str(line,hs);
        StabBackTraceStr:=StabBackTraceStr+' line '+hs;
      end;
     StabBackTraceStr:=StabBackTraceStr+' of '+source;
   end;
  if Opened then
    BackTraceStrFunc:=Store;
end;


initialization
  BackTraceStrFunc:=@StabBackTraceStr;

finalization
  if opened then
   CloseStabs;

end.
