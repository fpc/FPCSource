{
    $Id$
    Copyright (c) 1998,99 by the FPC development team

    This unit handles the linker and binder calls for programs and
    libraries

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
Unit link;

Interface

{ Needed for LFN support in path to the executable }
{$ifdef GO32V2}
  {$define ALWAYSSHELL}
{$endif}

uses cobjects,files;

Type
    TLinkerInfo=record
      ExeCmd,
      DllCmd        : array[1..3] of string[80];
      ResName       : string[12];
      ExtraOptions  : string;
      DynamicLinker : string[80];
    end;

    PLinker=^TLinker;
    TLinker = Object
    public
       Info            : TLinkerInfo;
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles  : TStringContainer;
     { Methods }
       Constructor Init;
       Destructor Done;
       procedure AddModuleFiles(hp:pmodule);
       function  FindObjectFile(s : string) : string;
       function  FindLibraryFile(s:string;const ext:string) : string;
       Procedure AddObject(const S : String);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(S : String);
       Function  FindUtil(const s:string):String;
       Function  DoExec(const command,para:string;showinfo,useshell:boolean):boolean;
     { Virtuals }
       procedure SetDefaultInfo;virtual;
       Function  MakeExecutable:boolean;virtual;
       Function  MakeSharedLibrary:boolean;virtual;
       Function  MakeStaticLibrary(filescnt:longint):boolean;virtual;
     end;

Var
  Linker : PLinker;

procedure InitLinker;
procedure DoneLinker;


Implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  globtype,systems,
  script,globals,verbose,ppu
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETOS2}
    ,t_os2
  {$endif}
  {$ifndef NOTARGETWIN32}
    ,t_win32
  {$endif}
  {$ifndef NOTARGETGO32V1}
    ,t_go32v1
  {$endif}
  {$ifndef NOTARGETGO32V2}
    ,t_go32v2
  {$endif}
{$endif}
{$ifdef m68k}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
  ,gendef
  ;

{*****************************************************************************
                                   TLINKER
*****************************************************************************}

Constructor TLinker.Init;
begin
  ObjectFiles.Init_no_double;
  SharedLibFiles.Init_no_double;
  StaticLibFiles.Init_no_double;
{ set generic defaults }
  FillChar(Info,sizeof(Info),0);
  Info.ResName:='link.res';
{ set the linker specific defaults }
  SetDefaultInfo;
{ Allow Parameter overrides for linker info }
  with Info do
   begin
     if ParaLinkOptions<>'' then
      ExtraOptions:=ParaLinkOptions;
     if ParaDynamicLinker<>'' then
      DynamicLinker:=ParaDynamicLinker;
   end;
end;


Destructor TLinker.Done;
begin
  ObjectFiles.Done;
  SharedLibFiles.Done;
  StaticLibFiles.Done;
end;


Procedure TLinker.SetDefaultInfo;
begin
end;


procedure TLinker.AddModuleFiles(hp:pmodule);
var
  mask : longint;
begin
  with hp^ do
   begin
   { link unit files }
     if (flags and uf_no_link)=0 then
      begin
        { create mask which unit files need linking }
        mask:=link_allways;
        if hp^.is_unit then
         begin
           { static linking ? }
           if (cs_link_static in aktglobalswitches) then
            begin
              if (flags and uf_static_linked)=0 then
                Comment(V_Error,'unit '+modulename^+' can''t be static linked')
              else
                mask:=mask or link_static;
            end;
           { smart linking ? }
           if (cs_link_smart in aktglobalswitches) then
            begin
              if (flags and uf_smart_linked)=0 then
               begin
                 { if smart not avail then try static linking }
                 if (flags and uf_static_linked)<>0 then
                  begin
                    Comment(V_Warning,'unit '+modulename^+' can''t be smart linked, switching to static linking');
                    mask:=mask or link_static;
                  end
                 else
                  Comment(V_Error,'unit '+modulename^+' can''t be smart or static linked');
               end
              else
               mask:=mask or link_smart;
            end;
           { shared linking }
           if (cs_link_shared in aktglobalswitches) then
            begin
              if (flags and uf_shared_linked)=0 then
               begin
                 { if shared not avail then try static linking }
                 if (flags and uf_static_linked)<>0 then
                  begin
                    Comment(V_Warning,'unit '+modulename^+' can''t be shared linked, switching to static linking');
                    mask:=mask or link_static;
                  end
                 else
                  Comment(V_Error,'unit '+modulename^+' can''t be shared or static linked');
               end
              else
               mask:=mask or link_shared;
            end;
         end
        else
         begin
           { for programs link always static }
           mask:=mask or link_static;
         end;
        { unit files }
        while not linkunitofiles.empty do
         AddObject(linkunitofiles.getusemask(mask));
        while not linkunitstaticlibs.empty do
         AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
        while not linkunitsharedlibs.empty do
         AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
      end;
   { Other needed .o and libs, specified using $L,$LINKLIB,external }
     mask:=link_allways;
     while not linkotherofiles.empty do
      AddObject(linkotherofiles.Getusemask(mask));
     while not linkotherstaticlibs.empty do
      AddStaticLibrary(linkotherstaticlibs.Getusemask(mask));
     while not linkothersharedlibs.empty do
      AddSharedLibrary(linkothersharedlibs.Getusemask(mask));
   end;
end;


Function TLinker.FindUtil(const s:string):string;
var
  ldfound : boolean;
  LastBin : string;
begin
  LastBin:='';
  if utilsdirectory<>'' then
   LastBin:=Search(s+source_os.exeext,utilsdirectory,ldfound)+s+source_os.exeext;
  if LastBin='' then
   LastBin:=FindExe(s,ldfound);
  if (not ldfound) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_w_util_not_found,s);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  if ldfound then
   Message1(exec_t_using_util,LastBin);
  FindUtil:=LastBin;
end;


{ searches an object file }
function TLinker.FindObjectFile(s:string) : string;
var
  found : boolean;
begin
  findobjectfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s:=FixFileName(s);
  if FileExists(s) then
   begin
     Findobjectfile:=s;
     exit;
   end;
  { find object file
     1. cwd
     2. unit search path
     3. local object path
     4. global object path
     5. exepath }
  found:=false;
  findobjectfile:=search(s,'.',found)+s;
  if (not found) then
   findobjectfile:=search(s,unitsearchpath,found)+s;
  if (not found) and assigned(current_module^.localobjectsearchpath) then
   findobjectfile:=search(s,current_module^.localobjectsearchpath^,found)+s;
  if (not found) then
   findobjectfile:=search(s,objectsearchpath,found)+s;
  if (not found) then
   findobjectfile:=search(s,exepath,found)+s;
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_objfile_not_found,s);
end;


{ searches an library file }
function TLinker.FindLibraryFile(s:string;const ext:string) : string;
var
  found : boolean;
begin
  findlibraryfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+ext;
  if FileExists(s) then
   begin
     FindLibraryFile:=s;
     exit;
   end;
  { find libary
     1. cwd
     2. local libary dir
     3. global libary dir
     4. exe path of the compiler }
  found:=false;
  findlibraryfile:=search(s,'.',found)+s;
  if (not found) and assigned(current_module^.locallibrarysearchpath) then
   findlibraryfile:=search(s,current_module^.locallibrarysearchpath^,found)+s;
  if (not found) then
   findlibraryfile:=search(s,librarysearchpath,found)+s;
  if (not found) then
   findlibraryfile:=search(s,exepath,found)+s;
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_libfile_not_found,s);
end;


Procedure TLinker.AddObject(const S : String);
begin
  ObjectFiles.Insert(FindObjectFile(s));
end;


Procedure TLinker.AddSharedLibrary(S:String);
begin
{ remove prefix 'lib' }
  if Copy(s,1,length(target_os.libprefix))=target_os.libprefix then
   Delete(s,1,length(target_os.libprefix));
{ remove extension if any }
  if Copy(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext))=target_os.sharedlibext then
   Delete(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext)+1);
{ ready to be inserted }
  SharedLibFiles.Insert (S);
end;


Procedure TLinker.AddStaticLibrary(const S:String);
begin
  StaticLibFiles.Insert(FindLibraryFile(s,target_os.staticlibext));
end;


Function TLinker.DoExec(const command,para:string;showinfo,useshell:boolean):boolean;
begin
  DoExec:=true;
  if not(cs_link_extern in aktglobalswitches) then
   begin
     swapvectors;
{$ifdef ALWAYSSHELL}
     shell(command+' '+para);
{$else}
     if useshell then
      shell(command+' '+para)
     else
      exec(command,para);
{$endif}
     swapvectors;
     if (doserror<>0) then
      begin
         Message(exec_w_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message(exec_w_error_while_linking);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
        DoExec:=false;
       end;
   end;
{ Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
   begin
     if showinfo then
      AsmRes.AddLinkCommand(Command,Para,current_module^.exefilename^)
     else
      AsmRes.AddLinkCommand(Command,Para,'');
   end;
end;


function TLinker.MakeExecutable:boolean;
begin
  MakeExecutable:=false;
  Message(exec_e_exe_not_supported);
end;


Function TLinker.MakeSharedLibrary:boolean;
begin
  MakeSharedLibrary:=false;
  Message(exec_e_dll_not_supported);
end;


Function TLinker.MakeStaticLibrary(filescnt:longint):boolean;
{
  FilesCnt holds the amount of .o files created, if filescnt=0 then
  no smartlinking is used
}
var
  smartpath,
  cmdstr,
  binstr  : string;
  success : boolean;
  cnt     : longint;
begin
  MakeStaticLibrary:=false;

  smartpath:=current_module^.path^+FixPath(FixFileName(current_module^.modulename^)+target_info.smartext,false);
  SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
  Replace(cmdstr,'$LIB',current_module^.staticlibfilename^);
  if filescnt=0 then
    Replace(cmdstr,'$FILES',current_module^.objfilename^)
  else
    Replace(cmdstr,'$FILES',FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext));
  success:=DoExec(FindUtil(binstr),cmdstr,false,true);

{ Clean up }
  if not(cs_asm_leave in aktglobalswitches) then
   if not(cs_link_extern in aktglobalswitches) then
    begin
      if filescnt=0 then
       RemoveFile(current_module^.objfilename^)
      else
       begin
         for cnt:=1 to filescnt do
          if not RemoveFile(FixFileName(smartpath+current_module^.asmprefix^+tostr(cnt)+target_info.objext)) then
           RemoveFile(FixFileName(smartpath+current_module^.asmprefix^+'e'+tostr(cnt)+target_info.objext));
         RemoveDir(smartpath);
       end;
    end
   else
    begin
      if filescnt=0 then
       AsmRes.AddDeleteCommand(current_module^.objfilename^)
      else
       begin
         AsmRes.AddDeleteCommand(smartpath+current_module^.asmprefix^+'*'+target_info.objext);
         AsmRes.Add('rmdir '+smartpath);
       end;
    end;
  MakeStaticLibrary:=success;
end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure InitLinker;
begin
  case target_info.target of
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    target_i386_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
  {$ifndef NOTARGETWIN32}
    target_i386_Win32 :
      linker:=new(plinkerwin32,Init);
  {$endif}
  {$ifndef NOTARGETGO32V1}
    target_i386_Go32v1 :
      linker:=new(plinkergo32v1,Init);
  {$endif}
  {$ifndef NOTARGETGO32V2}
    target_i386_Go32v2 :
      linker:=new(plinkergo32v2,Init);
  {$endif}
  {$ifndef NOTARGETOS2}
    target_i386_os2 :
      linker:=new(plinkeros2,Init);
  {$endif}
{$endif i386}
{$ifdef m68k}
  {$ifndef NOTARGETPALMOS}
    target_m68k_palmos:
      linker:=new(plinker,Init);
  {$endif}
  {$ifndef NOTARGETLINUX}
    target_m68k_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
{$endif m68k}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    target_alpha_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
{$endif alpha}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    target_powerpc_linux :
      linker:=new(plinkerlinux,Init);
  {$endif}
{$endif powerpc}
    else
      linker:=new(plinker,Init);
  end;
end;


procedure DoneLinker;
begin
  if assigned(linker) then
   dispose(linker,done);
end;


end.
{
  $Log$
  Revision 1.75  1999-10-26 12:25:04  peter
    * fixed os2 linker

  Revision 1.74  1999/10/21 14:29:34  peter
    * redesigned linker object
    + library support for linux (only procedures can be exported)

  Revision 1.72  1999/09/16 23:05:52  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.71  1999/09/16 11:34:56  pierre
   * typo correction

  Revision 1.70  1999/09/15 22:09:16  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.69  1999/09/15 20:24:56  daniel
  + Dw switch now does something.

  Revision 1.68  1999/08/18 17:05:53  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.67  1999/08/16 15:35:23  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.66  1999/08/11 17:26:34  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.65  1999/08/10 12:51:16  pierre
    * bind_win32_dll removed (Relocsection used instead)
    * now relocsection is true by default ! (needs dlltool
      for DLL generation)

  Revision 1.64  1999/07/30 23:19:45  peter
    * fixed placing of dynamiclinker in link.res (should be the last after
      all other libraries)

  Revision 1.63  1999/07/29 01:31:39  peter
    * fixed shared library linking for glibc2 systems

  Revision 1.62  1999/07/27 11:05:51  peter
    * glibc 2.1.2 support

  Revision 1.61  1999/07/18 10:19:53  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.60  1999/07/07 20:33:53  peter
    * warning instead of error when switching to static linking

  Revision 1.59  1999/07/05 16:21:26  peter
    * fixed linking for units without linking necessary

  Revision 1.58  1999/07/03 00:29:51  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.57  1999/06/28 16:02:31  peter
    * merged

  Revision 1.54.2.3  1999/06/28 15:55:40  peter
    * also search path if not found in utilsdirectory

  Revision 1.54.2.2  1999/06/18 09:51:55  peter
    * always use shell() for go32v2 to support LFN

  Revision 1.54.2.1  1999/06/15 13:51:56  peter
    * also check ld-2.1.so for glibc 2.1, previous was only for 2.1.1

  Revision 1.54  1999/06/02 13:25:35  hajny
    * fixed stripping symbols for OS/2

  Revision 1.53  1999/05/04 21:44:44  florian
    * changes to compile it with Delphi 4.0

  Revision 1.52  1999/05/03 21:30:30  peter
    + glibc 2.1

  Revision 1.51  1999/04/28 23:42:33  pierre
   * removing of temporary directory with -s option

  Revision 1.50  1999/04/25 14:31:48  daniel
  * Bug fixed in linking: compiling files on another drive than the one you
  currently use you is done correctly.

  Revision 1.49  1999/03/25 16:55:30  peter
    + unitpath,librarypath,includepath,objectpath directives

  Revision 1.48  1999/03/23 16:22:43  peter
    * crtbegin/crtend only added if found

  Revision 1.47  1999/02/05 16:45:47  michael
  + Fixed gluing of options

  Revision 1.46  1999/02/05 08:54:26  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.45  1999/01/29 10:33:07  peter
    * objectsearchpath is now also searched if an object is not found

  Revision 1.44  1999/01/27 13:07:58  pierre
   * problem related with libc : go32v2 and linux differences

  Revision 1.43  1999/01/25 15:02:13  peter
    * link libc always as last

  Revision 1.42  1998/12/11 00:03:19  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.41  1998/12/01 23:39:46  pierre
   * postprocessexec for win32 changed

  Revision 1.40  1998/12/01 12:51:20  peter
    * fixed placing of ppas.sh and link.res when using -FE

  Revision 1.39  1998/11/30 13:26:23  pierre
    * the code for ordering the exported procs/vars was buggy
    + added -WB to force binding (Ozerski way of creating DLL)
      this is off by default as direct writing of .edata section seems
      OK

  Revision 1.38  1998/11/30 09:43:13  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.37  1998/10/26 22:23:31  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.36  1998/10/22 15:18:44  florian
    + switch -vx for win32 added

  Revision 1.35  1998/10/19 18:06:23  peter
    * use no_double

  Revision 1.34  1998/10/16 13:37:18  florian
    + switch -FD added to specify the path for utilities

  Revision 1.33  1998/10/14 13:38:22  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.32  1998/10/14 11:03:55  daniel
  * Forgot to dereference a pointer.

  Revision 1.31  1998/10/14 11:01:21  daniel
  * Staticlibfilename no longer not include a path. Correction when calling
  ar.

  Revision 1.30  1998/10/13 13:10:18  peter
    * new style for m68k/i386 infos and enums

  Revision 1.29  1998/10/13 08:19:34  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.28  1998/10/08 23:28:56  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.27  1998/10/06 17:16:52  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.26  1998/09/29 15:23:05  peter
    * remove also the end files for smartlinking

  Revision 1.25  1998/09/10 15:25:31  daniel
  + Added maxheapsize.
  * Corrected semi-bug in calling the assembler and the linker

  Revision 1.24  1998/09/07 18:32:45  peter
    * fixed for m68k

  Revision 1.23  1998/09/03 17:39:04  florian
    + better code for type conversation longint/dword to real type

  Revision 1.22  1998/09/01 09:01:00  peter
    + glibc2 support

  Revision 1.21  1998/08/31 12:26:26  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.20  1998/08/19 10:06:14  peter
    * fixed filenames and removedir which supports slash at the end

  Revision 1.19  1998/08/17 09:17:47  peter
    * static/shared linking updates

  Revision 1.18  1998/08/14 21:56:34  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.17  1998/08/14 18:16:08  peter
    * return after a failed call will now add it to ppas

  Revision 1.16  1998/08/12 19:28:15  peter
    * better libc support

  Revision 1.15  1998/08/10 14:50:02  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.14  1998/06/17 14:10:13  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.13  1998/06/08 22:59:46  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.12  1998/06/04 23:51:44  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.11  1998/05/27 00:20:31  peter
    * some scanner optimizes
    * automaticly aout2exe for go32v1
    * fixed dynamiclinker option which was added at the wrong place

  Revision 1.10  1998/05/22 12:32:47  peter
    * fixed -L on the commandline, Dos commandline is only 128 bytes

  Revision 1.9  1998/05/12 10:46:59  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.8  1998/05/11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.7  1998/05/08 09:21:20  michael
  * Added missing -Fl message to messages file.
  * Corrected mangling of file names when doing Linklib
  * -Fl now actually WORKS.
  * Librarysearchpath is now a field in linker object.

  Revision 1.6  1998/05/06 09:26:49  peter
    * fixed ld call with shell

  Revision 1.4  1998/05/04 17:54:25  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.3  1998/04/16 10:54:30  daniel
  * Fixed linking for OS/2.
}
