{
    Copyright (c) 1998-2002 by Peter Vreman

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

{$i fpcdefs.inc}

interface
uses
  cclasses,
  systems,
  fmodule,
  globtype;

Type
    TLinkerInfo=record
      ExeCmd,
      DllCmd        : array[1..3] of string;
      ResName       : string[100];
      ScriptName    : string[100];
      ExtraOptions  : string;
      DynamicLinker : string[100];
    end;

    TLinker = class(TAbstractLinker)
    public
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles  : TStringList;
       Constructor Create;virtual;
       Destructor Destroy;override;
       procedure AddModuleFiles(hp:tmodule);
       Procedure AddObject(const S,unitpath : String;isunit:boolean);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(S : String);
       Procedure AddStaticCLibrary(const S : String);
       Procedure AddSharedCLibrary(S : String);
       Function  MakeExecutable:boolean;virtual;
       Function  MakeSharedLibrary:boolean;virtual;
       Function  MakeStaticLibrary:boolean;virtual;
     end;

    TExternalLinker = class(TLinker)
    public
       Info : TLinkerInfo;
       Constructor Create;override;
       Destructor Destroy;override;
       Function  FindUtil(const s:string):String;
       Function  DoExec(const command:string; para:TCmdStr;showinfo,useshell:boolean):boolean;
       procedure SetDefaultInfo;virtual;
       Function  MakeStaticLibrary:boolean;override;
     end;

    TInternalLinker = class(TLinker)
    private
       procedure readobj(const fn:string);
    public
       Constructor Create;override;
       Destructor Destroy;override;
       Function  MakeExecutable:boolean;override;
     end;


var
  Linker  : TLinker;

function FindObjectFile(s : string;const unitpath:string;isunit:boolean) : string;
function FindLibraryFile(s:string;const prefix,ext:string;var foundfile : string) : boolean;

procedure InitLinker;
procedure DoneLinker;


Implementation

uses
{$IFDEF USE_SYSUTILS}
  SysUtils,
{$ELSE USE_SYSUTILS}
  dos,
{$ENDIF USE_SYSUTILS}
  cutils,
  script,globals,verbose,ppu,
  aasmbase,aasmtai,aasmcpu,
  ogbase,ogmap;

type
 TLinkerClass = class of Tlinker;

{*****************************************************************************
                                   Helpers
*****************************************************************************}

{ searches an object file }
function FindObjectFile(s:string;const unitpath:string;isunit:boolean) : string;
var
  found : boolean;
  foundfile : string;
begin
  findobjectfile:='';
  if s='' then
   exit;

  {When linking on target, the units has not been assembled yet,
   so there is no object files to look for at
   the host. Look for the corresponding assembler file instead,
   because it will be assembled to object file on the target.}
  if isunit and (cs_link_on_target in aktglobalswitches) then
    s:= ForceExtension(s,target_info.asmext);

  { when it does not belong to the unit then check if
    the specified file exists without searching any paths }
  if not isunit then
   begin
     if FileExists(FixFileName(s)) then
      begin
        foundfile:=ScriptFixFileName(s);
        found:=true;
      end;
   end;
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  { find object file
     1. output unit path
     2. output exe path
     3. specified unit path (if specified)
     4. cwd
     5. unit search path
     6. local object path
     7. global object path
     8. exepath (not when linking on target) }
  found:=false;
  if isunit and (OutputUnitDir<>'') then
    found:=FindFile(s,OutPutUnitDir,foundfile)
  else
    if OutputExeDir<>'' then
      found:=FindFile(s,OutPutExeDir,foundfile);
  if (not found) and (unitpath<>'') then
   found:=FindFile(s,unitpath,foundfile);
  if (not found) then
   found:=FindFile(s, CurDirRelPath(source_info), foundfile);
  if (not found) then
   found:=UnitSearchPath.FindFile(s,foundfile);
  if (not found) then
   found:=current_module.localobjectsearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=objectsearchpath.FindFile(s,foundfile);
  if not(cs_link_on_target in aktglobalswitches) and (not found) then
   found:=FindFile(s,exepath,foundfile);
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_objfile_not_found,s);

  {Restore file extension}
  if isunit and (cs_link_on_target in aktglobalswitches) then
    foundfile:= ForceExtension(foundfile,target_info.objext);

  findobjectfile:=ScriptFixFileName(foundfile);
end;


    { searches a (windows) DLL file }
    function FindDLL(const s:string;var founddll:string):boolean;
      var
        sysdir : string;
        Found : boolean;
      begin
        Found:=false;
        { Look for DLL in:
          1. Current dir
          2. Library Path
          3. windir,windir/system,windir/system32 }
        Found:=FindFile(s,'.'+source_info.DirSep,founddll);
        if (not found) then
         Found:=librarysearchpath.FindFile(s,founddll);
        if (not found) then
         begin
           {$IFDEF USE_SYSUTILS}
           sysdir:=FixPath(GetEnvironmentVariable('windir'),false);
           {$ELSE USE_SYSUTILS}
           sysdir:=FixPath(GetEnv('windir'),false);
           {$ENDIF USE_SYSUTILS}
           Found:=FindFile(s,sysdir+';'+sysdir+'system'+source_info.DirSep+';'+sysdir+'system32'+source_info.DirSep,founddll);
         end;
        if (not found) then
         begin
           message1(exec_w_libfile_not_found,s);
           FoundDll:=s;
         end;
        FindDll:=Found;
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
     4. exe path of the compiler (not when linking on target) }
  found:=FindFile(s, CurDirRelPath(source_info), foundfile);
  if (not found) and (current_module.outputpath^<>'') then
   found:=FindFile(s,current_module.outputpath^,foundfile);
  if (not found) then
   found:=current_module.locallibrarysearchpath.FindFile(s,foundfile);
  if (not found) then
   found:=librarysearchpath.FindFile(s,foundfile);
  if not(cs_link_on_target in aktglobalswitches) and (not found) then
   found:=FindFile(s,exepath,foundfile);
  foundfile:=ScriptFixFileName(foundfile);
  findlibraryfile:=found;
end;


{*****************************************************************************
                                   TLINKER
*****************************************************************************}

Constructor TLinker.Create;
begin
  Inherited Create;
  ObjectFiles:=TStringList.Create_no_double;
  SharedLibFiles:=TStringList.Create_no_double;
  StaticLibFiles:=TStringList.Create_no_double;
end;


Destructor TLinker.Destroy;
begin
  ObjectFiles.Free;
  SharedLibFiles.Free;
  StaticLibFiles.Free;
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
        begin
          AddObject(linkunitofiles.getusemask(mask),path^,true);
        end;
        while not linkunitstaticlibs.empty do
         AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
        while not linkunitsharedlibs.empty do
         AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
      end;
   { Other needed .o and libs, specified using $L,$LINKLIB,external }
     mask:=link_allways;
     while not linkotherofiles.empty do
      AddObject(linkotherofiles.Getusemask(mask),path^,false);
     while not linkotherstaticlibs.empty do
      AddStaticCLibrary(linkotherstaticlibs.Getusemask(mask));
     while not linkothersharedlibs.empty do
      AddSharedCLibrary(linkothersharedlibs.Getusemask(mask));
   end;
end;


Procedure TLinker.AddObject(const S,unitpath : String;isunit:boolean);
begin
  ObjectFiles.Concat(FindObjectFile(s,unitpath,isunit));
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
begin
  MakeStaticLibrary:=false;
  Message(exec_e_dll_not_supported);
end;


{*****************************************************************************
                              TEXTERNALLINKER
*****************************************************************************}

Constructor TExternalLinker.Create;
begin
  inherited Create;
  { set generic defaults }
  FillChar(Info,sizeof(Info),0);
  if cs_link_on_target in aktglobalswitches then
    begin
      Info.ResName:=outputexedir+inputfile+'_link.res';
      Info.ScriptName:=outputexedir+inputfile+'_script.res';
    end
  else
    begin
      Info.ResName:='link.res';
      Info.ScriptName:='script.res';
    end;
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


Destructor TExternalLinker.Destroy;
begin
  inherited destroy;
end;


Procedure TExternalLinker.SetDefaultInfo;
begin
end;


Function TExternalLinker.FindUtil(const s:string):string;
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


Function TExternalLinker.DoExec(const command:string; para:TCmdStr;showinfo,useshell:boolean):boolean;
var
  exitcode: longint;
begin
  DoExec:=true;
  if not(cs_link_extern in aktglobalswitches) then
   begin
     if useshell then
       exitcode := shell(maybequoted(command)+' '+para)
     else
{$IFDEF USE_SYSUTILS}
     try
       if ExecuteProcess(command,para) <> 0
       then begin
         Message(exec_e_error_while_linking);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
       end;
     except on E:EOSError do
       begin
         Message(exec_e_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
       end;
     end
   end;
{$ELSE USE_SYSUTILS}
       begin
         swapvectors;
         exec(command,para);
         swapvectors;
         exitcode := dosexitcode;
       end;
     if (doserror<>0) then
      begin
         Message(exec_e_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
      end
     else
      if (exitcode<>0) then
       begin
        Message(exec_e_error_while_linking);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
        DoExec:=false;
       end;
   end;
{$ENDIF USE_SYSUTILS}
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


Function TExternalLinker.MakeStaticLibrary:boolean;
var
  smartpath : TCmdStr;

  function GetNextFiles(const maxCmdLength : AInt; var item : TStringListItem) : string;
  begin
    result := '';
    while (assigned(item) and ((length(result) + length(item.str) + 1) < maxCmdLength)) do begin
      result := result + ' ' + item.str;
      item := TStringListItem(item.next);
    end;
  end;

var
  binstr  : string;
  success : boolean;
  cmdstr, nextcmd : TCmdStr;
  current : TStringListItem;
begin
  MakeStaticLibrary:=false;
{ remove the library, to be sure that it is rewritten }
  RemoveFile(current_module.staticlibfilename^);
{ Call AR }
  smartpath:=current_module.outputpath^+FixPath(lower(current_module.modulename^)+target_info.smartext,false);
  SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
  Replace(cmdstr,'$LIB',maybequoted(current_module.staticlibfilename^));
  { create AR commands }
  success := true;
  nextcmd := cmdstr;
  current := TStringListItem(SmartLinkOFiles.First);
  repeat
    Replace(nextcmd,'$FILES',GetNextFiles(240 - length(nextcmd), current));
    success:=DoExec(FindUtil(binstr),nextcmd,false,true);
    nextcmd := cmdstr;
  until (not assigned(current)) or (not success);

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
                              TINTERNALLINKER
*****************************************************************************}

Constructor TInternalLinker.Create;
begin
  inherited Create;
  exemap:=nil;
  exeoutput:=nil;
end;


Destructor TInternalLinker.Destroy;
begin
  exeoutput.free;
  exeoutput:=nil;
  inherited destroy;
end;


procedure TInternalLinker.readobj(const fn:string);
var
  objdata  : TAsmObjectData;
  objinput : tobjectinput;
begin
  Comment(V_Info,'Reading object '+fn);
  objinput:=exeoutput.newobjectinput;
  objdata:=objinput.newobjectdata(fn);
  if objinput.readobjectfile(fn,objdata) then
    exeoutput.addobjdata(objdata);
  { release input object }
  objinput.free;
end;


function TInternalLinker.MakeExecutable:boolean;
var
  s : string;
begin
  MakeExecutable:=false;

  { no support yet for libraries }
  if (not StaticLibFiles.Empty) or
     (not SharedLibFiles.Empty) then
   internalerror(123456789);

  if (cs_link_map in aktglobalswitches) then
   exemap:=texemap.create(current_module.mapfilename^);

  { read objects }
  readobj(FindObjectFile('prt0','',false));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      readobj(s);
   end;

  { generate executable }
  exeoutput.GenerateExecutable(current_module.exefilename^);

  { close map }
  if assigned(exemap) then
   begin
     exemap.free;
     exemap:=nil;
   end;

  MakeExecutable:=true;
end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure InitLinker;
var
 lk : TlinkerClass;
begin
  if (cs_link_internal in aktglobalswitches) and
     assigned(target_info.link) then
   begin
     lk:=TLinkerClass(target_info.link);
     linker:=lk.Create;
   end
  else if assigned(target_info.linkextern) then
   begin
     lk:=TlinkerClass(target_info.linkextern);
     linker:=lk.Create;
   end
  else
  begin
   linker:=Tlinker.Create;
  end;
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
