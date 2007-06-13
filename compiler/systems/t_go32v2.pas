{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Go32v2 target

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
unit t_go32v2;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,
       fmodule,i_go32v2,
       link,ogcoff;

    type
      TInternalLinkerGo32v2=class(TInternallinker)
        constructor create;override;
        procedure DefaultLinkScript;override;
      end;

      TExternalLinkerGo32v2=class(texternallinker)
      private
         Function  WriteResponseFile(isdll:boolean) : Boolean;
         Function  WriteScript(isdll:boolean) : Boolean;
      public
         constructor Create;override;
         procedure SetDefaultInfo;override;
         function  MakeExecutable:boolean;override;
      end;


{****************************************************************************
                                  TCoffLinker
****************************************************************************}

    constructor TInternalLinkerGo32v2.Create;
      begin
        inherited Create;
        CExeoutput:=TDJCoffexeoutput;
        CObjInput:=TDJCoffObjInput;
      end;


    procedure TInternalLinkerGo32v2.DefaultLinkScript;
      begin
      end;


{****************************************************************************
                               TExternalLinkerGo32v2
****************************************************************************}

Constructor TExternalLinkerGo32v2.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TExternalLinkerGo32v2.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld $SCRIPT $OPT $STRIP -o $EXE $RES';
   end;
end;


Function TExternalLinkerGo32v2.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('-(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(GetShortName(s))
      end;
     LinkRes.Add('-)');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  linklibc:=false;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.GetFirst;
     if s<>'c' then
      begin
        i:=Pos(target_info.sharedlibext,S);
        if i>0 then
         Delete(S,i,255);
        LinkRes.Add('-l'+s);
      end
     else
      begin
        LinkRes.Add('-l'+s);
        linklibc:=true;
      end;
   end;
  { be sure that libc&libgcc is the last lib }
  if linklibc then
   begin
     LinkRes.Add('-lc');
     LinkRes.Add('-lgcc');
   end;

{ Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


Function TExternalLinkerGo32v2.WriteScript(isdll:boolean) : Boolean;
Var
  scriptres  : TLinkRes;
  HPath    : TCmdStrListItem;
  s        : string;
begin
  WriteScript:=False;

  { Open link.res file }
  ScriptRes:=TLinkRes.Create(outputexedir+Info.ScriptName);
  ScriptRes.Add('OUTPUT_FORMAT("coff-go32-exe")');
  ScriptRes.Add('ENTRY(start)');

  ScriptRes.Add('SECTIONS');
  ScriptRes.Add('{');
  ScriptRes.Add('  .text  0x1000+SIZEOF_HEADERS : {');
  ScriptRes.Add('  . = ALIGN(16);');
  { add objectfiles, start with prt0 always }
  ScriptRes.Add('  '+GetShortName(FindObjectFile('prt0','',false))+'(.text)');
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
       begin
          ScriptRes.Add('  . = ALIGN(16);');
          ScriptRes.Add('  '+GetShortName(s)+'(.text)');
       end;
   end;
  ScriptRes.Add('    *(.text)');
  ScriptRes.Add('    etext  =  . ; _etext = .;');
  ScriptRes.Add('    . = ALIGN(0x200);');
  ScriptRes.Add('  }');
  ScriptRes.Add('    .data  ALIGN(0x200) : {');
  ScriptRes.Add('      djgpp_first_ctor = . ;');
  ScriptRes.Add('      *(.ctor)');
  ScriptRes.Add('      djgpp_last_ctor = . ;');
  ScriptRes.Add('      djgpp_first_dtor = . ;');
  ScriptRes.Add('      *(.dtor)');
  ScriptRes.Add('      djgpp_last_dtor = . ;');
  ScriptRes.Add('      *(.data)');
  ScriptRes.Add('      *(.fpc*)');
  ScriptRes.Add('      *(.gcc_exc)');
  ScriptRes.Add('      ___EH_FRAME_BEGIN__ = . ;');
  ScriptRes.Add('      *(.eh_fram)');
  ScriptRes.Add('      ___EH_FRAME_END__ = . ;');
  ScriptRes.Add('      LONG(0)');
  ScriptRes.Add('       edata  =  . ; _edata = .;');
  ScriptRes.Add('       . = ALIGN(0x200);');
  ScriptRes.Add('    }');
  ScriptRes.Add('    .bss  SIZEOF(.data) + ADDR(.data) :');
  ScriptRes.Add('    {');
  ScriptRes.Add('      _object.2 = . ;');
  ScriptRes.Add('      . += 24 ;');
  ScriptRes.Add('      *(.bss)');
  ScriptRes.Add('      *(COMMON)');
  ScriptRes.Add('       end = . ; _end = .;');
  ScriptRes.Add('       . = ALIGN(0x200);');
  ScriptRes.Add('    }');
  ScriptRes.Add('  }');

  { Write path to search libraries }
  HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_DIR("'+GetShortName(HPath.Str)+'")');
     HPath:=TCmdStrListItem(HPath.Next);
   end;
  HPath:=TCmdStrListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_DIR("'+GetShortName(HPath.Str)+'")');
     HPath:=TCmdStrListItem(HPath.Next);
   end;

{ Write and Close response }
  ScriptRes.WriteToDisk;
  ScriptRes.Free;

  WriteScript:=True;
end;



function TExternalLinkerGo32v2.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StripStr : string[40];
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StripStr:='';
  if (cs_link_strip in current_settings.globalswitches) then
   StripStr:='-s';

  { Write used files and libraries and our own ld script }
  WriteScript(false);
  WriteResponsefile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES','@'+maybequoted(outputexedir+Info.ResName));
(* Potential issues with older ld version??? *)
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$SCRIPT','--script='+maybequoted(outputexedir+Info.ScriptName));
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   begin
     DeleteFile(outputexedir+Info.ResName);
     DeleteFile(outputexedir+Info.ScriptName);
   end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{$ifdef notnecessary}
procedure TExternalLinkerGo32v2.postprocessexecutable(const n : string);
type
  tcoffheader=packed record
    mach   : word;
    nsects : word;
    time   : longint;
    sympos : longint;
    syms   : longint;
    opthdr : word;
    flag   : word;
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
  psecfill=^TSecfill;
  TSecfill=record
    fillpos,
    fillsize : longint;
    next : psecfill;
  end;
var
  f : file;
  coffheader : tcoffheader;
  firstsecpos,
  maxfillsize,
  l : longint;
  coffsec : tcoffsechdr;
  secroot,hsecroot : psecfill;
  zerobuf : pointer;
begin
  { when -s is used quit, because there is no .exe }
  if cs_link_nolink in current_settings.globalswitches then
   exit;
  { open file }
  assign(f,n);
  {$I-}
   reset(f,1);
  if ioresult<>0 then
    Message1(execinfo_f_cant_open_executable,n);
  { read headers }
  seek(f,2048);
  blockread(f,coffheader,sizeof(tcoffheader));
  { read section info }
  maxfillsize:=0;
  firstsecpos:=0;
  secroot:=nil;
  for l:=1to coffheader.nSects do
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
  i:=ioresult;
  postprocessexecutable:=true;
end;
{$endif}


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_i386_go32v2_info,TExternalLinkerGo32v2);
//  RegisterInternalLinker(system_i386_go32v2_info,TInternalLinkerGo32v2);
  RegisterTarget(system_i386_go32v2_info);
end.
