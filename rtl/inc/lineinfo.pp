{
    $Id$
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

procedure GetLineInfo(addr:dword;var func,source:string;var line:longint);


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
  processaddress : cardinal;



{****************************************************************************
                             Executable Loaders
****************************************************************************}

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


{$ifdef win32}
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
  for i:=1to peheader.NumberOfSections do
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
{$endif Win32}


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
(* I don't really know, where this "+ 4" comes from, *)
(* but it seems to be correct. :-) - TH              *)
(* Maybe not PM                                      *)
   StabCnt := AoutHeader.SymbSize div SizeOf (TStab);
   StabStrOfs := StabOfs + AoutHeader.SymbSize;
   StabsFunctionRelative:=false;
   LoadEMXaout := (StabOfs <> -1) and (StabStrOfs <> -1);
  end;
 end;
end;
{$ENDIF EMX}


{$ifdef HasUnix}
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
{$ifdef m68k}
 {StabsFunctionRelative:=false;}
{$endif m68k}
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
{$endif}

{$ifdef sunos}
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
{$endif sunos}



{$ifdef beos}

{$i osposixh.inc}
{$i syscall.inc}
{$i beos.inc}
function get_next_image_info(team: team_id; var cookie:longint; var info:image_info; size: size_t) : status_t;cdecl; external 'root' name '_get_next_image_info';

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
{$endif beos}


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
{$ifdef win32}
  if LoadPECoff then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef Unix}
  if LoadElf32 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef sunos}
  if LoadElf32 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef beos}
  if LoadElf32 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
  CloseStabs;
end;


{$Q-}
{ this avoids problems with some targets PM }

procedure GetLineInfo(addr:dword;var func,source:string;var line:longint);
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


function StabBackTraceStr(addr:longint):shortstring;
var
  func,
  source : string;
  hs     : string[32];
  line   : longint;
  Store  : TBackTraceStrFunc;
begin
  { reset to prevent infinite recursion if problems inside the code PM }
  Store:=BackTraceStrFunc;
  BackTraceStrFunc:=@SysBackTraceStr;
  GetLineInfo(dword(addr),func,source,line);
{ create string }
  StabBackTraceStr:='  0x'+HexStr(addr,8);
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
{
  $Log$
  Revision 1.12  2003-01-10 21:35:48  marco
   * hasunix fix (my first commit from beos :-)

  Revision 1.11  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.10  2002/09/07 11:09:40  carl
    * stack checking supported for all systems

  Revision 1.9  2002/05/31 13:37:24  marco
   * more Renamefest

}
