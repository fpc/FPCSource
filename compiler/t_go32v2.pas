{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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

{$i defines.inc}

interface

  uses
    link;

  type
    tlinkergo32v2=class(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  WriteScript(isdll:boolean) : Boolean;
    public
       constructor Create;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
    end;


  implementation

    uses
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule;


{****************************************************************************
                               TLinkerGo32v2
****************************************************************************}

Constructor TLinkerGo32v2.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerGo32v2.SetDefaultInfo;
begin
  with Info do
   begin
      if cs_align in aktglobalswitches then
        ExeCmd[1]:='ld $SCRIPT $OPT $STRIP -o $EXE'
      else
        ExeCmd[1]:='ld -oformat coff-go32-exe $OPT $STRIP -o $EXE @$RES'
   end;
end;


Function TLinkerGo32v2.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TStringListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+GetShortName(HPath.Str));
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+GetShortName(HPath.Str));
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.AddFileName(GetShortName(FindObjectFile('prt0','')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(GetShortName(s));
   end;

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
        i:=Pos(target_os.sharedlibext,S);
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
  linkres.done;

  WriteResponseFile:=True;
end;

Function TLinkerGo32v2.WriteScript(isdll:boolean) : Boolean;
Var
  scriptres  : TLinkRes;
  i        : longint;
  s        : string;
  linklibc : boolean;
begin
  WriteScript:=False;

  { Open link.res file }
  ScriptRes.Init(outputexedir+Info.ResName);
  ScriptRes.Add('OUTPUT_FORMAT("coff-go32-exe")');
  ScriptRes.Add('ENTRY(start)');

{$ifdef dummy}
  { Write path to search libraries }
  HPath:=current_module.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_PATH("'+GetShortName(HPath^.Data^)+'")');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_PATH("'+GetShortName(HPath^.Data^)+'")');
     HPath:=HPath^.Next;
   end;
{$endif dummy}

  ScriptRes.Add('SECTIONS');
  ScriptRes.Add('{');
  ScriptRes.Add('  .text  0x1000+SIZEOF_HEADERS : {');
  ScriptRes.Add('  . = ALIGN(16);');
  { add objectfiles, start with prt0 always }
  ScriptRes.Add('  '+GetShortName(FindObjectFile('prt0',''))+'(.text)');
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

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     ScriptRes.Add('-(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        ScriptRes.AddFileName(GetShortName(s))
      end;
     ScriptRes.Add('-)');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  linklibc:=false;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.GetFirst;
     if s<>'c' then
      begin
        i:=Pos(target_os.sharedlibext,S);
        if i>0 then
         Delete(S,i,255);
        ScriptRes.Add('-l'+s);
      end
     else
      begin
        ScriptRes.Add('-l'+s);
        linklibc:=true;
      end;
   end;
  { be sure that libc&libgcc is the last lib }
  if linklibc then
   begin
     ScriptRes.Add('-lc');
     ScriptRes.Add('-lgcc');
   end;

{ Write and Close response }
  ScriptRes.WriteToDisk;
  ScriptRes.done;

  WriteScript:=True;
end;

function TLinkerGo32v2.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  StripStr : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StripStr:='';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

  if cs_align in aktglobalswitches then
    WriteScript(false)
  else
    { Write used files and libraries }
    WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$SCRIPT','--script='+outputexedir+Info.ResName);
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{$ifdef notnecessary}
procedure tlinkergo32v2.postprocessexecutable(const n : string);
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
  psecfill=^tsecfill;
  tsecfill=record
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
  if cs_link_extern in aktglobalswitches then
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

end.
{
  $Log$
  Revision 1.7  2001-01-27 21:29:35  florian
     * behavior -Oa optimized

  Revision 1.6  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/08/16 13:06:07  florian
    + support of 64 bit integer constants

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
