{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
       Function  MakeStaticLibrary:boolean;virtual;
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
               begin
                 { if smart not avail then try static linking }
                 if (flags and uf_static_linked)<>0 then
                  begin
                    Comment(V_Hint,'unit '+modulename^+' can''t be static linked, switching to smart linking');
                    mask:=mask or link_smart;
                  end
                 else
                  Comment(V_Error,'unit '+modulename^+' can''t be smart or static linked');
               end
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
                    Comment(V_Hint,'unit '+modulename^+' can''t be smart linked, switching to static linking');
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
                    Comment(V_Hint,'unit '+modulename^+' can''t be shared linked, switching to static linking');
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
   LastBin:=FindFile(s+source_os.exeext,utilsdirectory,ldfound)+s+source_os.exeext;
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
  findobjectfile:=FindFile(s,'.'+DirSep,found)+s;
  if (not found) then
   findobjectfile:=UnitSearchPath.FindFile(s,found)+s;
  if (not found) then
   findobjectfile:=current_module^.localobjectsearchpath.FindFile(s,found)+s;
  if (not found) then
   findobjectfile:=objectsearchpath.FindFile(s,found)+s;
  if (not found) then
   findobjectfile:=FindFile(s,exepath,found)+s;
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
  findlibraryfile:=FindFile(s,'.'+DirSep,found)+s;
  if (not found) then
   findlibraryfile:=current_module^.locallibrarysearchpath.FindFile(s,found)+s;
  if (not found) then
   findlibraryfile:=librarysearchpath.FindFile(s,found)+s;
  if (not found) then
   findlibraryfile:=FindFile(s,exepath,found)+s;
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
       begin
         if DLLsource then
           AsmRes.AddLinkCommand(Command,Para,current_module^.sharedlibfilename^)
         else
           AsmRes.AddLinkCommand(Command,Para,current_module^.exefilename^);
       end
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


Function TLinker.MakeStaticLibrary:boolean;
var
  smartpath,
  cmdstr,
  binstr  : string;
  success : boolean;
begin
  MakeStaticLibrary:=false;
{ remove the library, to be sure that it is rewritten }
  RemoveFile(current_module^.staticlibfilename^);
{ Call AR }
  smartpath:=current_module^.outputpath^+FixPath(FixFileName(current_module^.modulename^)+target_info.smartext,false);
  SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
  Replace(cmdstr,'$LIB',current_module^.staticlibfilename^);
  Replace(cmdstr,'$FILES',FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext));
  success:=DoExec(FindUtil(binstr),cmdstr,false,true);
{ Clean up }
  if not(cs_asm_leave in aktglobalswitches) then
   if not(cs_link_extern in aktglobalswitches) then
    begin
      while not SmartLinkOFiles.Empty do
       RemoveFile(SmartLinkOFiles.Get);
      RemoveDir(smartpath);
    end
   else
    begin
      AsmRes.AddDeleteCommand(FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext));
      AsmRes.Add('rmdir '+smartpath);
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
  Revision 1.84  2000-02-24 18:41:39  peter
    * removed warnings/notes

  Revision 1.83  2000/02/09 13:22:54  peter
    * log truncated

  Revision 1.82  2000/01/14 14:40:37  pierre
   * use ./ instead of . to look into startup dir

  Revision 1.81  2000/01/12 10:38:18  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.80  2000/01/11 09:52:06  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.79  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.78  1999/11/22 22:22:30  pierre
   * Give better info in script

  Revision 1.77  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.76  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.75  1999/10/26 12:25:04  peter
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

}
