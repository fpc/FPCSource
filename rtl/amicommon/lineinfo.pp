{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Stabs Line Info Retriever, Amiga-NG version
    can parse relocatable ELF executables

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit lineinfo;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$S-}
{$Q-}

{$IF FPC_VERSION<3}
type
  CodePointer = Pointer;
{$ENDIF}

function GetLineInfo(addr:ptruint;var func,source:string;var line:longint) : boolean;
function StabBackTraceStr(addr:CodePointer):string;
procedure CloseStabs;

var
  // Allows more efficient operation by reusing previously loaded debug data
  // when the target module filename is the same. However, if an invalid memory
  // address is supplied then further calls may result in an undefined behaviour.
  // In summary: enable for speed, disable for resilience.
  AllowReuseOfLineInfoData: Boolean = True;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.ExeInfo,System.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses
  exeinfo,strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  N_Function    = $24;
  N_TextLine    = $44;
  N_DataLine    = $46;
  N_BssLine     = $48;
  N_SourceFile  = $64;
  N_IncludeFile = $84;

  maxstabs = 128; { size of the stabs buffer }
  maxstabsreloc = 128; { size of the stabs reloc buffer }

var
  { GDB after 4.18 uses offset to function begin
    in text section but OS/2 version still uses 4.16 PM }
  StabsFunctionRelative: boolean;
  StabsNeedsRelocation: boolean;

type
  pstab=^tstab;
  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : dword;
  end;

type
  pelf32_rela = ^telf32_rela;
  telf32_rela = packed record
    r_offset: pointer;
    r_info: dword;
    r_addend: longint;
  end;

type
  pelf32_rel = ^telf32_rel;
  telf32_rel = packed record
    r_offset: pointer;
    r_info: dword;
  end;

type
  pelf32_sym = ^telf32_sym;
  telf32_sym = packed record
    st_name: dword;
    st_addr: pointer;
    st_size: dword;
    st_info: byte;
    st_other: byte;
    st_shndx: word;
  end;

{$ifdef cpui386}
type
  pelf32_reloc = ^telf32_reloc;
  telf32_reloc = telf32_rel;
{$else}
type
  pelf32_reloc = ^telf32_reloc;
  telf32_reloc = telf32_rela;
{$endif}


{ We use static variable so almost no stack is required, and is thus
  more safe when an error has occurred in the program }
{$WARNING This code is not thread-safe, and needs improvement }  
var
  e          : TExeFile;
  stabcnt,              { amount of stabs }
  stablen,
  stabofs,              { absolute stab section offset in executable }
  stabstrlen,
  stabstrofs : longint; { absolute stabstr section offset in executable }
  dirlength  : longint; { length of the dirctory part of the source file }
  stabs      : array[0..maxstabs-1] of tstab;  { buffer }
  stabsreloc : array[0..maxstabsreloc-1] of telf32_reloc;
  textofs,
  textlen: longint;
  symtabofs,
  symtablen: longint;
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
  dirstab,              { stab with current directory info }
  filestab   : tstab;   { stab with current file info }
  filename,
  lastfilename,         { store last processed file }
  dbgfn : ansistring;
  lastopenstabs: Boolean; { store last result of processing a file }

  stabrelocofs,stabreloclen: longint;


function OpenStabs(addr : pointer) : boolean;
  var
    baseaddr : pointer;
begin
  // False by default
  OpenStabs:=false;

  // Empty so can test if GetModuleByAddr has worked
  filename := '';

  // Get filename by address using GetModuleByAddr
  GetModuleByAddr(addr,baseaddr,filename);
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,filename,' Baseaddr: ',hexstr(ptruint(baseaddr),sizeof(baseaddr)*2));
{$endif DEBUG_LINEINFO}

  // Check if GetModuleByAddr has worked
  if filename = '' then
    exit;

  // If target filename same as previous, then re-use previous result
  if AllowReuseOfLineInfoData and (filename = lastfilename) then
  begin
    {$ifdef DEBUG_LINEINFO}
    writeln(stderr,'Reusing debug data');
    {$endif DEBUG_LINEINFO}
    OpenStabs:=lastopenstabs;
    exit;
  end;

  // Close previously opened stabs
  CloseStabs;

  // Reset last open stabs result
  lastopenstabs := false;

  // Save newly processed filename
  lastfilename := filename;

  // Open exe file or debug link
  if not OpenExeFile(e,filename) then
    exit;
  if ReadDebugLink(e,dbgfn) then
    begin
      CloseExeFile(e);
      if not OpenExeFile(e,dbgfn) then
        exit;
    end;

  // Find stab section
{$ifdef BeOS}
  { Do not change ProcessAddress field for BeOS/Haiku
    if baseAddr is lower than ProcessAdress }
  if ptruint(baseaddr)>ptruint(e.processaddress) then
{$endif BeOS}
    e.processaddress:=ptruint(baseaddr)-e.processaddress;
  StabsFunctionRelative := E.FunctionRelative;
  if FindExeSection(e,'.text',textofs,textlen) and
     FindExeSection(e,'.stab',stabofs,stablen) and
     FindExeSection(e,'.stabstr',stabstrofs,stabstrlen) then
    begin
      stabcnt:=stablen div sizeof(tstab);
      lastopenstabs:=true;
      OpenStabs:=true;
    end
  else
    CloseExeFile(e);
end;


procedure CloseStabs;
begin
  if e.isopen then
    CloseExeFile(e);

  // Reset last processed filename
  lastfilename := '';
end;


var
  relocidx: longint;
  reloclen: longint;
  relocofs: longint;
  relocleft: longint;
  currentreloc: longint;

function InitRelocs: boolean;
var
  res: boolean;
begin
{$ifdef cpui386}
  res:=FindExeSection(e,'.rel.stab',stabrelocofs,stabreloclen);
  if res then
    res:=res and FindExeSection(e,'.symtab',symtabofs,symtablen);
{$else}
  res:=FindExeSection(e,'.rela.stab',stabrelocofs,stabreloclen);
{$endif}

  if res then
    begin
      reloclen:=maxstabsreloc;
      relocidx:=reloclen;
      relocofs:=stabrelocofs;
      relocleft:=stabreloclen;
      currentreloc:=-1;
    end;

  InitRelocs:=res;
end;

function min(a,b: longint): longint; inline;
begin
  if a<b then min:=a else min:=b;
end;

function GetNextReloc: boolean;
var
  origpos: longint;
  res: longint;
  readlen: longint;
begin
  GetNextReloc:=false;
  if relocleft <= 0 then
    exit;

  inc(relocidx);
  if relocidx >= reloclen then
    begin
      origpos:=filepos(e.f);
      seek(e.f,relocofs);
      readlen:=min(relocleft,maxstabsreloc*sizeof(telf32_reloc));
      blockread(e.f,stabsreloc,readlen,res);
      reloclen:=res div sizeof(telf32_reloc);
      dec(relocleft,res);
      if reloclen <= 0 then
        exit;
      relocofs:=filepos(e.f);
      relocidx:=0;
      seek(e.f,origpos);
    end;

  currentreloc:=relocidx;
  GetNextReloc:=true;
end;

function GetSym(symnr: longint): telf32_sym;
var
  origpos: longint;
begin
  origpos:=filepos(e.f);
  seek(e.f,symtabofs+(symnr*sizeof(telf32_sym)));
  blockread(e.f,GetSym,sizeof(telf32_sym));
  seek(e.f,origpos);
end;

procedure RelocStabsEntries(stab: pstab; stablen: longint);
const
  R_386_32 = 1;
var
  origpos: longint;
  intostabsofs: longint;
  j: longint;
  rel: pelf32_reloc;
  sym: telf32_sym;
begin
  origpos:=filepos(e.f);
  intostabsofs:=origpos-(stabofs+stablen*sizeof(tstab));

  j:=0;
  repeat
    rel:=@stabsreloc[currentreloc];
    while pointer(intostabsofs + (sizeof(tstab) * j) + 8) < rel^.r_offset do
      begin
        inc(j);
        if j >= stablen then exit;
      end;

    if (pointer(intostabsofs + (sizeof(tstab) * j) + 8) = rel^.r_offset) then
      begin
{$ifdef cpui386}
        if byte(rel^.r_info) = R_386_32 then
          begin
            sym:=GetSym(rel^.r_info shr 8);
            inc(stab[j].nvalue,ptruint(sym.st_addr));
          end;
{$endif}
{$ifdef cpupowerpc}
        inc(stab[j].nvalue,rel^.r_addend);
{$endif}
      end;
  until not GetNextReloc;
end;

function GetLineInfo(addr:ptruint;var func,source:string;var line:longint) : boolean;
var
  res,
  stabsleft,
  stabscnt,i : longint;
  found : boolean;
  lastfunc : tstab;
  lastline : tstab;

begin
  GetLineInfo:=false;
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'GetLineInfo called');
{$endif DEBUG_LINEINFO}
  fillchar(func,high(func)+1,0);
  fillchar(source,high(source)+1,0);
  line:=0;

  if not OpenStabs(pointer(addr)) then
    exit;
  
  { correct the value to the correct address in the file }
  { processaddress is set in OpenStabs                   }
  addr := dword(addr - e.processaddress);

  { if the address is outside our text segment, ignore it }
  if addr > textlen then
    exit;

  StabsNeedsRelocation:=InitRelocs and GetNextReloc;

{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'Addr: ',hexstr(addr,sizeof(addr)*2));
{$endif DEBUG_LINEINFO}
 
  fillchar(funcstab,sizeof(tstab),0);
  fillchar(filestab,sizeof(tstab),0);
  fillchar(dirstab,sizeof(tstab),0);
  fillchar(linestab,sizeof(tstab),0);
  fillchar(lastfunc,sizeof(tstab),0);
  found:=false;
  seek(e.f,stabofs);
  stabsleft:=stabcnt;
  repeat
    if stabsleft>maxstabs then
     stabscnt:=maxstabs
    else
     stabscnt:=stabsleft;
    blockread(e.f,stabs,stabscnt*sizeof(tstab),res);
    stabscnt:=res div sizeof(tstab);
    if StabsNeedsRelocation then
      relocstabsentries(@stabs,stabscnt);
    for i:=0 to stabscnt-1 do
     begin
       case stabs[i].ntype of
         N_BssLine,
         N_DataLine:
           begin
             // for code line info, we don't care about these
           end;
         N_TextLine :
           begin
             lastline:=stabs[i];
             if StabsFunctionRelative then
               inc(lastline.nvalue,lastfunc.nvalue);
             if (addr>=linestab.nvalue) and (addr<lastline.nvalue) then
               begin
                 found:=true;
                 break;
               end;
             linestab:=lastline;
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
     seek(e.f,stabstrofs+dirstab.strpos);
     blockread(e.f,source[1],high(source)-1,res);
     dirlength:=strlen(@source[1]);
     source[0]:=chr(dirlength);
   end
  else
   dirlength:=0;
  if filestab.ntype<>0 then
   begin
     seek(e.f,stabstrofs+filestab.strpos);
     blockread(e.f,source[dirlength+1],high(source)-(dirlength+1),res);
     source[0]:=chr(strlen(@source[1]));
   end;
  if funcstab.ntype<>0 then
   begin
     seek(e.f,stabstrofs+funcstab.strpos);
     blockread(e.f,func[1],high(func)-1,res);
     func[0]:=chr(strlen(@func[1]));
     i:=pos(':',func);
     if i>0 then
      Delete(func,i,255);
   end;

  if not AllowReuseOfLineInfoData then
    CloseStabs;

  GetLineInfo:=true;
end;


function StabBackTraceStr(addr:CodePointer):string;
var
  func,
  source : string;
  hs     : string;
  line   : longint;
  Store  : TBackTraceStrFunc;
  Success : boolean;
begin
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'StabBackTraceStr called');
{$endif DEBUG_LINEINFO}
  { reset to prevent infinite recursion if problems inside the code PM }
  Success:=false;
  Store:=BackTraceStrFunc;
  BackTraceStrFunc:=@SysBackTraceStr;

  { on most architectures, (but not everywhere, Sparc is a notable exception)
    for valid stacktraces you have to substract sizeof(pointer), or similar
    instruction length from the trace address otherwise the lineinfo might
    be off-by-one, because of course the backtrace addresses don't point to
    the jump instructions, but the following address, which might belong to
    a different source line entirely (KB) }
  Success:=GetLineInfo(ptruint(addr-sizeof(pointer)),func,source,line);

{ create string }
{$ifdef netware}
  { we need addr relative to code start on netware }
  dec(addr,ptruint(system.NWGetCodeStart));
  StabBackTraceStr:='  CodeStart + $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
{$else}
  if (addr<pointer(e.processaddress)) or (dword(addr-pointer(e.processaddress)) > textlen) then
    StabBackTraceStr:='  Addr $'+hexstr(addr)
  else
    StabBackTraceStr:='  Offs $'+hexstr(addr-e.processaddress);
{$endif}
  if Success then
  begin
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
  BackTraceStrFunc:=Store;
end;


initialization
  lastfilename := '';
  lastopenstabs := false;
  BackTraceStrFunc:=@StabBackTraceStr;

finalization
  CloseStabs;

end.
