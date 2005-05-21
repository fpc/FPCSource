{
    Copyright (c) 1998-2000 by Pavel Ozerski

    This program implements support post processing
    for the (i386) Win32 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
program postw32;
uses
{$ifdef fpc}
  strings
{$else}
  sysutils
{$endif}
  ;

const
  execinfo_f_cant_open_executable='Cannot open file ';
  execinfo_x_codesize='Code size: ';
  execinfo_x_initdatasize='Size of Initialized Data: ';
  execinfo_x_uninitdatasize='Size of Uninitialized Data: ';
  execinfo_f_cant_process_executable='Cannot process file ';
  execinfo_x_stackreserve='Size of Stack Reserve: ';
  execinfo_x_stackcommit='Size of Stack Commit: ';

type
  tapptype = (at_none,
    at_gui,at_cui
  );

var
  verbose:longbool;
  stacksize,
  ii,jj:longint;
  code:integer;
  DllVersion : sTring;
  Dllmajor,Dllminor : word;
  apptype : tapptype;

function tostr(i : longint) : string;
{
return string of value i
}
var
  hs : string;
begin
  str(i,hs);
  tostr:=hs;
end;

procedure Message1(const info,fn:string);
var
  e:longbool;
begin
  e:=pos('Cannot',info)=1;
  if verbose or e then
   writeln(info,fn);
  if e then
   halt(1);
end;


function postprocessexecutable(const fn : string;isdll:boolean):boolean;
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
     PEMagic : array[0..3] of char;
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
  psecfill=^tsecfill;
  tsecfill=record
    fillpos,
    fillsize : longint;
    next : psecfill;
  end;
var
  f : file;
  dosheader : tdosheader;
  peheader : tpeheader;
  firstsecpos,
  maxfillsize,
  l,peheaderpos : longint;
  coffsec : tcoffsechdr;
  secroot,hsecroot : psecfill;
  zerobuf : pointer;
begin
  postprocessexecutable:=false;
  { open file }
  assign(f,fn);
  {$I-}
   reset(f,1);
  if ioresult<>0 then
    Message1(execinfo_f_cant_open_executable,fn);
  { read headers }
  blockread(f,dosheader,sizeof(tdosheader));
  peheaderpos:=dosheader.e_lfanew;
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write info }
  Message1(execinfo_x_codesize,tostr(peheader.SizeOfCode));
  Message1(execinfo_x_initdatasize,tostr(peheader.SizeOfInitializedData));
  Message1(execinfo_x_uninitdatasize,tostr(peheader.SizeOfUninitializedData));
  { change stack size (PM) }
  { I am not sure that the default value is adequate !! }
  peheader.SizeOfStackReserve:=stacksize;
  { change the header }
  { sub system }
  { gui=2 }
  { cui=3 }
  if apptype=at_gui then
    peheader.Subsystem:=2
  else if apptype=at_cui then
    peheader.Subsystem:=3;
  if dllversion<>'' then
    begin
     peheader.MajorImageVersion:=dllmajor;
     peheader.MinorImageVersion:=dllminor;
    end;
  { reset timestamp }
  peheader.TimeDateStamp:=0;
  { write header back }
  seek(f,peheaderpos);
  blockwrite(f,peheader,sizeof(tpeheader));
  if ioresult<>0 then
    Message1(execinfo_f_cant_process_executable,fn);
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write the value after the change }
  Message1(execinfo_x_stackreserve,tostr(peheader.SizeOfStackReserve));
  Message1(execinfo_x_stackcommit,tostr(peheader.SizeOfStackCommit));
  { read section info }
  maxfillsize:=0;
  firstsecpos:=0;
  secroot:=nil;
  for l:=1to peheader.NumberOfSections do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if coffsec.datapos>0 then
      begin
        if secroot=nil then
         firstsecpos:=coffsec.datapos;
        new(hsecroot);
        hsecroot^.fillpos:=coffsec.datapos+coffsec.vsize;
        hsecroot^.fillsize:=coffsec.datalen-coffsec.vsize;
        hsecroot^.next:=secroot;
        secroot:=hsecroot;
        if secroot^.fillsize>maxfillsize then
         maxfillsize:=secroot^.fillsize;
      end;
   end;
  if firstsecpos>0 then
   begin
     l:=firstsecpos-filepos(f);
     if l>maxfillsize then
      maxfillsize:=l;
   end
  else
   l:=0;
  { get zero buffer }
  getmem(zerobuf,maxfillsize);
  fillchar(zerobuf^,maxfillsize,0);
  { zero from sectioninfo until first section }
  blockwrite(f,zerobuf^,l);
  { zero section alignments }
  while assigned(secroot) do
   begin
     seek(f,secroot^.fillpos);
     blockwrite(f,zerobuf^,secroot^.fillsize);
     hsecroot:=secroot;
     secroot:=secroot^.next;
     dispose(hsecroot);
   end;
  freemem(zerobuf,maxfillsize);
  close(f);
  {$I+}
  if ioresult<>0 then;
  postprocessexecutable:=true;
end;


var
  fn,s:string;
function GetSwitchValue(const key,shortkey,default:string;const PossibleValues:array of pchar):string;
var
  i,j,k:longint;
  x:double;
  s1,s2:string;
  code:integer;

  procedure Error;
  begin
   writeln('Error: unrecognized option ',paramstr(i),' ',s1);
   halt(1);
  end;

begin
  for i:=1 to paramcount do
   if(paramstr(i)=key)or(paramstr(i)=shortkey)then
    begin
     s1:=paramstr(succ(i));
     for j:=0 to high(PossibleValues)do
      begin
       s2:=strpas(PossibleValues[j]);
       if(length(s2)>1)and(s2[1]='*')then
        case s2[2]of
         'i':
          begin
           val(s1,k,code);
           if code<>0 then
            error;
           GetSwitchValue:=s1;
           exit;
          end;
         'r':
          begin
           val(s1,x,code);
           if code<>0 then
            error;
           GetSwitchValue:=s1;
           exit;
          end;
         's':
          begin
           GetSwitchValue:=s1;
           exit;
          end;
        end
       else if s1=s2 then
        begin
         GetSwitchValue:=s1;
         exit;
        end;
      end;
     error;
    end;
  GetSwitchValue:=default;
end;

procedure help_info;
begin
  fn:=paramstr(0);
  for jj:=length(fn)downto 1 do
   if fn[jj] in [':','\','/']then
    begin
     fn:=copy(fn,succ(jj),255);
     break;
    end;
  writeln('Usage: ',fn,' [options]');
  writeln('Options:');
  writeln('-i | --input <file>              - set input file;');
  writeln('-m | --subsystem <console | gui> - set Win32 subsystem;');
  writeln('-s | --stack <size>              - set stack size;');
  writeln('-V | --version <n.n>             - set image version;');
  writeln('-v | --verbose                   - show info while processing;');
  writeln('-h | --help | -?                 - show this screen');
  halt;
end;

begin
  verbose:=false;
  if paramcount=0 then
    help_info;
  for ii:=1 to paramcount do
    if(paramstr(ii)='-h')or(paramstr(ii)='--help')or(paramstr(ii)='-?')then
     help_info
    else if(paramstr(ii)='-v')or(paramstr(ii)='--verbose')then
     begin
      verbose:=true;
      break;
     end;
  fn:=GetSwitchValue('--input','-i','',['*s']);
  val(GetSwitchValue('--stack','-s','33554432',['*i']),stacksize,code);
  s:=GetSwitchValue('--subsystem','-m','console',['gui','console']);
  if s='gui' then
    apptype:=at_GUI
  else
    apptype:=at_cui;
  dllversion:=GetSwitchValue('--version','-V','1.0',['*r']);
  ii:=pos('.',dllversion);
  if ii=0 then
    begin
     ii:=succ(length(dllversion));
     dllversion:=dllversion+'.0';
    end
  else if ii=1 then
    begin
     ii:=2;
     dllversion:='0.'+dllversion;
    end;
  val(copy(dllversion,1,pred(ii)),dllmajor,code);
  val(copy(dllversion,succ(ii),length(dllversion)),dllminor,code);
  if verbose then
    writeln('Image Version: ',dllmajor,'.',dllminor);
  PostProcessExecutable(fn,false);
end.
