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
  stabs      : array[0..maxstabs-1] of tstab;  { buffer }
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
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
  LoadCoff:=false;
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
  LoadCoff:=(stabofs<>-1) and (stabstrofs<>-1);
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
                ((addr-stabs[i].nvalue)<(addr-linestab.nvalue)) then
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
                ((addr-stabs[i].nvalue)<(addr-funcstab.nvalue)) then
              begin
                funcstab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
              end;
           end;
         N_SourceFile,
         N_IncludeFile :
           begin
             if (stabs[i].nvalue<=addr) and
                ((addr-stabs[i].nvalue)<(addr-filestab.nvalue)) then
              begin
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
  if filestab.ntype<>0 then
   begin
     seek(f,stabstrofs+filestab.strpos);
     blockread(f,source[1],high(source)-1,res);
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
  Revision 1.1  2000-02-06 17:19:22  peter
    * lineinfo unit added which uses stabs to get lineinfo for backtraces

}
