{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) go32v1 target

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
unit t_go32v1;

{$i defines.inc}

interface

  uses
    link;

  type
    tlinkergo32v1=class(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
       constructor Create;override;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
    end;


  implementation

    uses
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule;


{****************************************************************************
                               TLinkergo32v1
****************************************************************************}

Constructor TLinkergo32v1.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkergo32v1.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld -oformat coff-go32 $OPT $STRIP -o $EXE @$RES';
     ExeCmd[2]:='aout2exe $EXE';
   end;
end;


Function TLinkergo32v1.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TStringListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.AddFileName(FindObjectFile('prt0',''));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('-(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(s)
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


function TLinkergo32v1.MakeExecutable:boolean;
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

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STRIP',StripStr);
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
       target_i386_go32v1_info : ttargetinfo =
          (
            target       : target_i386_GO32V1;
            name         : 'GO32 V1 DOS extender';
            shortname    : 'Go32v1';
            flags        : [];
            cpu          : i386;
            unit_env     : 'GO32V1UNITS';
            extradefines : 'DPMI';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';      { No .exe, the linker only output a.out ! }
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.pp1';
            unitlibext   : '.ppl';
            asmext       : '.s1';
            objext       : '.o1';
            resext       : '.res';
            resobjext    : '.o1r';
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
            assem        : as_i386_as;
            assemextern  : as_i386_as;
            link         : ld_i386_go32v1;
            linkextern   : ld_i386_go32v1;
            ar           : ar_gnu_ar;
            res          : res_none;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 0;
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
            size_of_pointer : 4;
            size_of_longint : 4;
            heapsize     : 2048*1024;
            maxheapsize  : 32768*1024;
            stacksize    : 16384;
            DllScanSupported:false;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          );


initialization
  RegisterLinker(ld_i386_go32v1,TLinkerGo32v1);
  RegisterTarget(target_i386_go32v1_info);
end.
{
  $Log$
  Revision 1.12  2002-01-29 21:27:34  peter
    * default alignment changed to 4 bytes for locals and static const,var

  Revision 1.11  2001/09/18 11:32:00  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.10  2001/09/17 21:29:16  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.9  2001/08/19 11:22:24  peter
    * palmos support from v10 merged

  Revision 1.8  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.7  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.6  2001/06/28 19:46:25  peter
    * added override and virtual for constructors

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

  Revision 1.5  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/09/24 15:06:30  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
