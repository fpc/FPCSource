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
unit link;

{$i defines.inc}

{ Needed for LFN support in path to the executable }
{$ifdef GO32V2}
  { define ALWAYSSHELL, obsolete as go32v2 Dos.Exec
    now handles LFN names and converts them to SFN PM }
{$endif}

interface
uses
  cclasses,
  systems,
  fmodule;

Type
    TLinkerInfo=record
      ExeCmd,
      DllCmd        : array[1..3] of string[100];
      ResName       : string[12];
      ScriptName    : string[12];
      ExtraOptions  : string;
      DynamicLinker : string[100];
    end;

    TLinker = class
    public
       Info            : TLinkerInfo;
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles  : TStringList;
     { Methods }
       Constructor Create;virtual;
       Destructor Destroy;override;
       procedure AddModuleFiles(hp:tmodule);
       Procedure AddObject(const S,unitpath : String);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(S : String);
       Procedure AddStaticCLibrary(const S : String);
       Procedure AddSharedCLibrary(S : String);
       Function  FindUtil(const s:string):String;
       Function  DoExec(const command,para:string;showinfo,useshell:boolean):boolean;
     { Virtuals }
       procedure SetDefaultInfo;virtual;
       Function  MakeExecutable:boolean;virtual;
       Function  MakeSharedLibrary:boolean;virtual;
       Function  MakeStaticLibrary:boolean;virtual;
     end;

     TLinkerClass = class of TLinker;

var
  CLinker : array[tld] of TLinkerClass;
  Linker  : TLinker;

function FindObjectFile(s : string;const unitpath:string) : string;
function FindLibraryFile(s:string;const prefix,ext:string;var foundfile : string) : boolean;

procedure RegisterLinker(t:tld;c:TLinkerClass);
procedure InitLinker;
procedure DoneLinker;


Implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  cutils,globtype,
  script,globals,verbose,ppu;


{*****************************************************************************
                                   Helpers
*****************************************************************************}

{ searches an object file }
function FindObjectFile(s:string;const unitpath:string) : string;
var
  found : boolean;
  foundfile : string;
  s1 : string;
begin
  findobjectfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s1:=FixFileName(s);
  if FileExists(s1) then
   begin
     Findobjectfile:=ScriptFixFileName(s);
     exit;
   end;
  { find object file
     1. specified unit path (if specified)
     2. cwd
     3. unit search path
     4. local object path
     5. global object path
     6. exepath }
  found:=false;
  if unitpath<>'' then
   found:=FindFile(s,unitpath,foundfile);
  if (not found) then
   found:=FindFile(s,'.'+source_info.DirSep,foundfile);
  if (not found) then
   found:=UnitSearchPath.FindFile(s,foundfile);
  if (not found) then
   found:=current_module.localobjectsearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=objectsearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=FindFile(s,exepath,foundfile);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_objfile_not_found,s);
  findobjectfile:=ScriptFixFileName(foundfile);
end;


{ searches an library file }
function FindLibraryFile(s:string;const prefix,ext:string;var foundfile : string) : boolean;
var
  found : boolean;
  paths : string;
begin
  findlibraryfile:=false;
  foundfile:=s;
  if s='' then
   exit;
  { split path from filename }
  paths:=SplitPath(s);
  s:=SplitFileName(s);
  { add prefix 'lib' }
  if (prefix<>'') and (Copy(s,1,length(prefix))<>prefix) then
   s:=prefix+s;
  { add extension }
  if (ext<>'') and (Copy(s,length(s)-length(ext)+1,length(ext))<>ext) then
   s:=s+ext;
  { readd the split path }
  s:=paths+s;
  if FileExists(s) then
   begin
     foundfile:=ScriptFixFileName(s);
     FindLibraryFile:=true;
     exit;
   end;
  { find libary
     1. cwd
     2. local libary dir
     3. global libary dir
     4. exe path of the compiler }
  found:=FindFile(s,'.'+source_info.DirSep,foundfile);
  if (not found) then
   found:=current_module.locallibrarysearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=librarysearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=FindFile(s,exepath,foundfile);
  foundfile:=ScriptFixFileName(foundfile);
  findlibraryfile:=found;
end;


{*****************************************************************************
                                   TLINKER
*****************************************************************************}

Constructor TLinker.Create;
begin
  ObjectFiles:=TStringList.Create_no_double;
  SharedLibFiles:=TStringList.Create_no_double;
  StaticLibFiles:=TStringList.Create_no_double;
{ set generic defaults }
  FillChar(Info,sizeof(Info),0);
  Info.ResName:='link.res';
  Info.ScriptName:='script.res';
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


Destructor TLinker.Destroy;
begin
  ObjectFiles.Free;
  SharedLibFiles.Free;
  StaticLibFiles.Free;
end;


Procedure TLinker.SetDefaultInfo;
begin
end;


procedure TLinker.AddModuleFiles(hp:tmodule);
var
  mask : longint;
begin
  with hp do
   begin
   { link unit files }
     if (flags and uf_no_link)=0 then
      begin
        { create mask which unit files need linking }
        mask:=link_allways;
        { static linking ? }
        if (cs_link_static in aktglobalswitches) then
         begin
           if (flags and uf_static_linked)=0 then
            begin
              { if smart not avail then try static linking }
              if (flags and uf_smart_linked)<>0 then
               begin
                 Message1(exec_t_unit_not_static_linkable_switch_to_smart,modulename^);
                 mask:=mask or link_smart;
               end
              else
               Message1(exec_e_unit_not_smart_or_static_linkable,modulename^);
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
                 Message1(exec_t_unit_not_smart_linkable_switch_to_static,modulename^);
                 mask:=mask or link_static;
               end
              else
               Message1(exec_e_unit_not_smart_or_static_linkable,modulename^);
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
                 Message1(exec_t_unit_not_shared_linkable_switch_to_static,modulename^);
                 mask:=mask or link_static;
               end
              else
               Message1(exec_e_unit_not_shared_or_static_linkable,modulename^);
            end
           else
            mask:=mask or link_shared;
         end;
        { unit files }
        while not linkunitofiles.empty do
         AddObject(linkunitofiles.getusemask(mask),path^);
        while not linkunitstaticlibs.empty do
         AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
        while not linkunitsharedlibs.empty do
         AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
      end;
   { Other needed .o and libs, specified using $L,$LINKLIB,external }
     mask:=link_allways;
     while not linkotherofiles.empty do
      AddObject(linkotherofiles.Getusemask(mask),path^);
     while not linkotherstaticlibs.empty do
      AddStaticCLibrary(linkotherstaticlibs.Getusemask(mask));
     while not linkothersharedlibs.empty do
      AddSharedCLibrary(linkothersharedlibs.Getusemask(mask));
   end;
end;


Function TLinker.FindUtil(const s:string):string;
var
  Found    : boolean;
  FoundBin : string;
  UtilExe  : string;
begin
  if cs_link_on_target in aktglobalswitches then
    begin
      { If linking on target, don't add any path PM }
      FindUtil:=AddExtension(s,target_info.exeext);
      exit;
    end;
  UtilExe:=AddExtension(s,source_info.exeext);
  FoundBin:='';
  Found:=false;
  if utilsdirectory<>'' then
   Found:=FindFile(utilexe,utilsdirectory,Foundbin);
  if (not Found) then
   Found:=FindExe(utilexe,Foundbin);
  if (not Found) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_e_util_not_found,utilexe);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  if (FoundBin<>'') then
   Message1(exec_t_using_util,FoundBin);
  FindUtil:=FoundBin;
end;


Procedure TLinker.AddObject(const S,unitpath : String);
begin
  ObjectFiles.Concat(FindObjectFile(s,unitpath));
end;


Procedure TLinker.AddSharedLibrary(S:String);
begin
  if s='' then
   exit;
{ remove prefix 'lib' }
  if Copy(s,1,length(target_info.sharedlibprefix))=target_info.sharedlibprefix then
   Delete(s,1,length(target_info.sharedlibprefix));
{ remove extension if any }
  if Copy(s,length(s)-length(target_info.sharedlibext)+1,length(target_info.sharedlibext))=target_info.sharedlibext then
   Delete(s,length(s)-length(target_info.sharedlibext)+1,length(target_info.sharedlibext)+1);
{ ready to be added }
  SharedLibFiles.Concat(S);
end;


Procedure TLinker.AddStaticLibrary(const S:String);
var
  ns : string;
  found : boolean;
begin
  if s='' then
   exit;
  found:=FindLibraryFile(s,target_info.staticlibprefix,target_info.staticlibext,ns);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_libfile_not_found,s);
  StaticLibFiles.Concat(ns);
end;


Procedure TLinker.AddSharedCLibrary(S:String);
begin
  if s='' then
   exit;
{ remove prefix 'lib' }
  if Copy(s,1,length(target_info.sharedclibprefix))=target_info.sharedclibprefix then
   Delete(s,1,length(target_info.sharedclibprefix));
{ remove extension if any }
  if Copy(s,length(s)-length(target_info.sharedclibext)+1,length(target_info.sharedclibext))=target_info.sharedclibext then
   Delete(s,length(s)-length(target_info.sharedclibext)+1,length(target_info.sharedclibext)+1);
{ ready to be added }
  SharedLibFiles.Concat(S);
end;


Procedure TLinker.AddStaticCLibrary(const S:String);
var
  ns : string;
  found : boolean;
begin
  if s='' then
   exit;
  found:=FindLibraryFile(s,target_info.staticclibprefix,target_info.staticclibext,ns);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_libfile_not_found,s);
  StaticLibFiles.Concat(ns);
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
         Message(exec_e_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message(exec_e_error_while_linking);
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
           AsmRes.AddLinkCommand(Command,Para,current_module.sharedlibfilename^)
         else
           AsmRes.AddLinkCommand(Command,Para,current_module.exefilename^);
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
  RemoveFile(current_module.staticlibfilename^);
{ Call AR }
  smartpath:=current_module.outputpath^+FixPath(FixFileName(current_module.modulename^)+target_info.smartext,false);
  SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
  Replace(cmdstr,'$LIB',current_module.staticlibfilename^);
  Replace(cmdstr,'$FILES',ScriptFixFileName(smartpath+current_module.asmprefix^+'*'+target_info.objext));
  success:=DoExec(FindUtil(binstr),cmdstr,false,true);
{ Clean up }
  if not(cs_asm_leave in aktglobalswitches) then
   if not(cs_link_extern in aktglobalswitches) then
    begin
      while not SmartLinkOFiles.Empty do
       RemoveFile(SmartLinkOFiles.GetFirst);
      RemoveDir(smartpath);
    end
   else
    begin
      AsmRes.AddDeleteCommand(FixFileName(smartpath+current_module.asmprefix^+'*'+target_info.objext));
      AsmRes.Add('rmdir '+smartpath);
    end;
  MakeStaticLibrary:=success;
end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure RegisterLinker(t:tld;c:TLinkerClass);
begin
  CLinker[t]:=c;
end;


procedure InitLinker;
begin
  if assigned(CLinker[target_info.link]) then
   linker:=CLinker[target_info.link].Create
  else
   linker:=Tlinker.Create;
end;


procedure DoneLinker;
begin
  if assigned(linker) then
   Linker.Free;
end;


{*****************************************************************************
                                   Initialize
*****************************************************************************}

    const
      ar_gnu_ar_info : tarinfo =
          (
            id    : ar_gnu_ar;
            arcmd : 'ar rs $LIB $FILES'
          );

initialization
  RegisterAr(ar_gnu_ar_info);

end.
{
  $Log$
  Revision 1.25  2002-01-19 11:57:05  peter
    * fixed path appending for lib

  Revision 1.24  2001/09/18 11:30:48  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.23  2001/09/17 21:29:11  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.22  2001/08/30 20:13:53  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.21  2001/08/19 11:22:22  peter
    * palmos support from v10 merged

  Revision 1.20  2001/08/13 19:26:03  peter
    * fixed ordering of object and libraries

  Revision 1.19  2001/08/07 18:47:12  peter
    * merged netbsd start
    * profile for win32

  Revision 1.18  2001/06/28 19:46:25  peter
    * added override and virtual for constructors

  Revision 1.17  2001/06/03 15:15:31  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.16  2001/04/18 22:01:54  peter
    * registration of targets and assemblers

  Revision 1.15  2001/04/13 01:22:08  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.14  2001/02/26 19:44:52  peter
    * merged generic m68k updates from fixes branch

  Revision 1.13  2001/02/20 21:41:17  peter
    * new fixfilename, findfile for unix. Look first for lowercase, then
      NormalCase and last for UPPERCASE names.

  Revision 1.12  2001/01/12 19:19:44  peter
    * fixed searching for utils

  Revision 1.11  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/11/29 00:30:31  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/09/24 21:33:46  peter
    * message updates merges

  Revision 1.8  2000/09/24 15:06:18  peter
    * use defines.inc

  Revision 1.7  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.6  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.5  2000/09/04 09:40:23  michael
  + merged Patch from peter

  Revision 1.4  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/26 13:08:19  jonas
    * merged from fixes branch (v_hint to v_tried changed when attempting
      to smart/static/shared link)

  Revision 1.2  2000/07/13 11:32:43  michael
  + removed logs
}
