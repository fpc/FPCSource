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

type
  pstab=^tstab;
  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : longint;
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


{$ifdef linux}
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
  LoadElf32:=false;
  stabofs:=-1;
  stabstrofs:=-1;
  { read and check header }
  if filesize(f)<sizeof(telf32header) then
   exit;
  blockread(f,elfheader,sizeof(telf32header));
  if elfheader.magic0123<>$464c457f then
   exit;
  if elfheader.e_shentsize<>sizeof(telf32sechdr) then
   exit;
  { read section names }
  seek(f,elfheader.e_shoff+elfheader.e_shstrndx*sizeof(telf32sechdr));
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
{$endif linux}


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
{$ifdef win32}
  if LoadPECoff then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
{$ifdef linux}
  if LoadElf32 then
   begin
     OpenStabs:=true;
     exit;
   end;
{$endif}
  CloseStabs;
end;


procedure GetLineInfo(addr:longint;var func,source:string;var line:longint);
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
                { if same value then the first one
                  contained the directory PM }
                if stabs[i].nvalue=filestab.nvalue then
                  dirstab:=filestab
                else
                  fillchar(dirstab,sizeof(tstab),0);
                filestab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
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


function StabBackTraceStr(addr:longint):string;
var
  func,
  source : string;
  hs     : string[32];
  line   : longint;
begin
  GetLineInfo(addr,func,source,line);
{ if there was an error with opening reset the hook to the system default }
  if not Opened then
   BackTraceStrFunc:=@SysBackTraceStr;
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
end;


initialization
  BackTraceStrFunc:=@StabBackTraceStr;

finalization
  if opened then
   CloseStabs;

end.
{
  $Log$
  Revision 1.4  2000-02-08 15:23:02  pierre
   * fix for directories included in stabsinfo

  Revision 1.3  2000/02/06 22:13:42  florian
    * small typo for go32 fixed

  Revision 1.2  2000/02/06 19:14:22  peter
    * linux elf support

  Revision 1.1  2000/02/06 17:19:22  peter
    * lineinfo unit added which uses stabs to get lineinfo for backtraces

}