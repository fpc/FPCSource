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


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule;

  type
    tlinkergo32v2=class(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  WriteScript(isdll:boolean) : Boolean;
    public
       constructor Create;override;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
    end;


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
     ExeCmd[1]:='ld $SCRIPT $OPT $STRIP -o $EXE @$RES';
   end;
end;


Function TLinkerGo32v2.WriteResponseFile(isdll:boolean) : Boolean;
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


Function TLinkerGo32v2.WriteScript(isdll:boolean) : Boolean;
Var
  scriptres  : TLinkRes;
  HPath    : TStringListItem;
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

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_DIR("'+GetShortName(HPath.Str)+'")');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     ScriptRes.Add('SEARCH_DIR("'+GetShortName(HPath.Str)+'")');
     HPath:=TStringListItem(HPath.Next);
   end;

{ Write and Close response }
  ScriptRes.WriteToDisk;
  ScriptRes.Free;

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

  { Write used files and libraries and our own ld script }
  WriteScript(false);
  WriteResponsefile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$SCRIPT','--script='+maybequoted(outputexedir+Info.ScriptName));
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   begin
     RemoveFile(outputexedir+Info.ResName);
     RemoveFile(outputexedir+Info.ScriptName);
   end;

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


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
       target_i386_go32v2_info : ttargetinfo =
          (
            target       : target_i386_GO32V2;
            name         : 'GO32 V2 DOS extender';
            shortname    : 'Go32v2';
            flags        : [];
            cpu          : i386;
            unit_env     : 'GO32V2UNITS';
            extradefines : 'DPMI';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            assem        : as_i386_coff;
            assemextern  : as_i386_as;
            link         : ld_i386_go32v2;
            linkextern   : ld_i386_go32v2;
            ar           : ar_gnu_ar;
            res          : res_none;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 0;
                localalignmax   : 4;
                paraalign       : 2;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            size_of_longint : 4;
            heapsize     : 2048*1024;
            maxheapsize  : 32768*1024;
            stacksize    : 16384;
            DllScanSupported : false;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          );


initialization
  RegisterLinker(ld_i386_go32v2,TLinkerGo32v2);
  RegisterTarget(target_i386_go32v2_info);
end.
{
  $Log$
  Revision 1.16  2002-04-15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.15  2002/01/29 21:27:34  peter
    * default alignment changed to 4 bytes for locals and static const,var

  Revision 1.14  2001/09/18 11:32:00  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.13  2001/09/17 21:29:16  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.12  2001/08/30 20:08:23  peter
    * create script.res and use link.res for commandline

  Revision 1.11  2001/08/19 11:22:24  peter
    * palmos support from v10 merged

  Revision 1.10  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.9  2001/07/10 21:01:35  peter
    * fixed crash with writing of the linker script

  Revision 1.8  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.7  2001/06/28 19:46:25  peter
    * added override and virtual for constructors

  Revision 1.6  2001/06/18 20:36:26  peter
    * -Ur switch (merged)
    * masm fixes (merged)
    * quoted filenames for go32v2 and win32

  Revision 1.5  2001/06/03 15:15:31  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.4  2001/06/02 19:22:44  peter
    * extradefines field added

  Revision 1.3  2001/04/18 22:02:04  peter
    * registration of targets and assemblers

  Revision 1.2  2001/04/13 01:22:21  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.1  2001/02/26 19:43:11  peter
    * moved target units to subdir

  Revision 1.7  2001/01/27 21:29:35  florian
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
