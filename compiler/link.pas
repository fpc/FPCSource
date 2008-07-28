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
      sysutils,
      cclasses,
      systems,
      fmodule,
      globtype,
      ogbase;

    Type
      TLinkerInfo=record
        ExeCmd,
        DllCmd,
        ExtDbgCmd     : array[1..3] of string;
        ResName       : string[100];
        ScriptName    : string[100];
        ExtraOptions  : TCmdStr;
        DynamicLinker : string[100];
      end;

      TLinker = class(TAbstractLinker)
      public
         HasResources,
         HasExports      : boolean;
         SysInitUnit     : string[20];
         ObjectFiles,
         SharedLibFiles,
         StaticLibFiles,
         FrameworkFiles  : TCmdStrList;
         Constructor Create;virtual;
         Destructor Destroy;override;
         procedure AddModuleFiles(hp:tmodule);
         Procedure AddObject(const S,unitpath : TCmdStr;isunit:boolean);
         Procedure AddStaticLibrary(const S : TCmdStr);
         Procedure AddSharedLibrary(S : TCmdStr);
         Procedure AddStaticCLibrary(const S : TCmdStr);
         Procedure AddSharedCLibrary(S : TCmdStr);
         Procedure AddFramework(S : TCmdStr);
         procedure AddImportSymbol(const libname,symname:TCmdStr;OrdNr: longint;isvar:boolean);virtual;
         Procedure InitSysInitUnitName;virtual;
         Function  MakeExecutable:boolean;virtual;
         Function  MakeSharedLibrary:boolean;virtual;
         Function  MakeStaticLibrary:boolean;virtual;
         procedure ExpandAndApplyOrder(var Src:TCmdStrList);
         procedure LoadPredefinedLibraryOrder;virtual;
         function  ReOrderEntries : boolean;
       end;

      TExternalLinker = class(TLinker)
      public
         Info : TLinkerInfo;
         Constructor Create;override;
         Destructor Destroy;override;
         Function  FindUtil(const s:TCmdStr):TCmdStr;
         Function  DoExec(const command:TCmdStr; para:TCmdStr;showinfo,useshell:boolean):boolean;
         procedure SetDefaultInfo;virtual;
         Function  MakeStaticLibrary:boolean;override;
       end;

      TInternalLinker = class(TLinker)
      private
         FCExeOutput : TExeOutputClass;
         FCObjInput  : TObjInputClass;
         { Libraries }
         FStaticLibraryList : TFPHashObjectList;
         FImportLibraryList : TFPHashObjectList;
         procedure Load_ReadObject(const para:TCmdStr);
         procedure Load_ReadStaticLibrary(const para:TCmdStr);
         procedure ParseScript_Load;
         procedure ParseScript_Order;
         procedure ParseScript_MemPos;
         procedure ParseScript_DataPos;
         procedure PrintLinkerScript;
         function  RunLinkScript(const outputname:TCmdStr):boolean;
      protected
         property CObjInput:TObjInputClass read FCObjInput write FCObjInput;
         property CExeOutput:TExeOutputClass read FCExeOutput write FCExeOutput;
         property StaticLibraryList:TFPHashObjectList read FStaticLibraryList;
         property ImportLibraryList:TFPHashObjectList read FImportLibraryList;
         procedure DefaultLinkScript;virtual;abstract;
         linkscript : TCmdStrList;
      public
         IsSharedLibrary : boolean;
         Constructor Create;override;
         Destructor Destroy;override;
         Function  MakeExecutable:boolean;override;
         Function  MakeSharedLibrary:boolean;override;
         procedure AddImportSymbol(const libname,symname:TCmdStr;OrdNr: longint;isvar:boolean);override;
       end;

    var
      Linker  : TLinker;

    function FindObjectFile(s : TCmdStr;const unitpath:TCmdStr;isunit:boolean) : TCmdStr;
    function FindLibraryFile(s:TCmdStr;const prefix,ext:TCmdStr;var foundfile : TCmdStr) : boolean;
    function FindDLL(const s:TCmdStr;var founddll:TCmdStr):boolean;

    procedure InitLinker;
    procedure DoneLinker;


Implementation

    uses
      cutils,cfileutl,cstreams,
      script,globals,verbose,comphook,ppu,fpccrc,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      owbase,owar,ogmap;

    type
     TLinkerClass = class of Tlinker;

{*****************************************************************************
                                   Helpers
*****************************************************************************}

    function GetFileCRC(const fn:string):cardinal;
      var
        fs : TCStream;
        bufcount,
        bufsize  : Integer;
        buf      : pbyte;
      begin
        result:=0;
        bufsize:=64*1024;
	      fs:=TCFileStream.Create(fn,fmOpenRead or fmShareDenyNone);
	      if CStreamError<>0 then
	        begin
	          fs.Free;
	          Comment(V_Error,'Can''t open file: '+fn);
	          exit;
	        end;
        getmem(buf,bufsize);
        repeat
          bufcount:=fs.Read(buf^,bufsize);
          result:=UpdateCrc32(result,buf^,bufcount);
        until bufcount<bufsize;
        freemem(buf);
        fs.Free;
      end;


    { searches an object file }
    function FindObjectFile(s:TCmdStr;const unitpath:TCmdStr;isunit:boolean) : TCmdStr;
      var
        found : boolean;
        foundfile : TCmdStr;
      begin
        findobjectfile:='';
        if s='' then
         exit;

        {When linking on target, the units has not been assembled yet,
         so there is no object files to look for at
         the host. Look for the corresponding assembler file instead,
         because it will be assembled to object file on the target.}
        if isunit and (cs_link_on_target in current_settings.globalswitches) then
          s:=ChangeFileExt(s,target_info.asmext);

        { when it does not belong to the unit then check if
          the specified file exists without searching any paths }
        if not isunit then
         begin
           if FileExists(FixFileName(s),false) then
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
           8. exepath (not when linking on target)
          for all finds don't use the directory caching }
        found:=false;
        if isunit and (OutputUnitDir<>'') then
          found:=FindFile(s,OutPutUnitDir,false,foundfile)
        else
          if OutputExeDir<>'' then
            found:=FindFile(s,OutPutExeDir,false,foundfile);
        if (not found) and (unitpath<>'') then
         found:=FindFile(s,unitpath,false,foundfile);
        if (not found) then
         found:=FindFile(s, CurDirRelPath(source_info),false,foundfile);
        if (not found) then
         found:=UnitSearchPath.FindFile(s,false,foundfile);
        if (not found) then
         found:=current_module.localobjectsearchpath.FindFile(s,false,foundfile);
        if (not found) then
         found:=objectsearchpath.FindFile(s,false,foundfile);
        if not(cs_link_on_target in current_settings.globalswitches) and (not found) then
         found:=FindFile(s,exepath,false,foundfile);
        if not(cs_link_nolink in current_settings.globalswitches) and (not found) then
         Message1(exec_w_objfile_not_found,s);

        {Restore file extension}
        if isunit and (cs_link_on_target in current_settings.globalswitches) then
          foundfile:= ChangeFileExt(foundfile,target_info.objext);

        findobjectfile:=ScriptFixFileName(foundfile);
      end;


    { searches a (windows) DLL file }
    function FindDLL(const s:TCmdStr;var founddll:TCmdStr):boolean;
      var
        sysdir : TCmdStr;
        Found : boolean;
      begin
        Found:=false;
        { Look for DLL in:
          1. Current dir
          2. Library Path
          3. windir,windir/system,windir/system32 }
        Found:=FindFile(s,'.'+source_info.DirSep,false,founddll);
        if (not found) then
         Found:=librarysearchpath.FindFile(s,false,founddll);
        if (not found) then
         begin
           sysdir:=FixPath(GetEnvironmentVariable('windir'),false);
           Found:=FindFile(s,sysdir+';'+sysdir+'system'+source_info.DirSep+';'+sysdir+'system32'+source_info.DirSep,false,founddll);
         end;
        if (not found) then
         begin
           message1(exec_w_libfile_not_found,s);
           FoundDll:=s;
         end;
        FindDll:=Found;
      end;


    { searches an library file }
    function FindLibraryFile(s:TCmdStr;const prefix,ext:TCmdStr;var foundfile : TCmdStr) : boolean;
      var
        found : boolean;
        paths : TCmdStr;
      begin
        findlibraryfile:=false;
        foundfile:=s;
        if s='' then
         exit;
        { split path from filename }
        paths:=ExtractFilePath(s);
        s:=ExtractFileName(s);
        { add prefix 'lib' }
        if (prefix<>'') and (Copy(s,1,length(prefix))<>prefix) then
         s:=prefix+s;
        { add extension }
        if (ext<>'') and (Copy(s,length(s)-length(ext)+1,length(ext))<>ext) then
         s:=s+ext;
        { readd the split path }
        s:=paths+s;
        if FileExists(s,false) then
         begin
           foundfile:=ScriptFixFileName(s);
           FindLibraryFile:=true;
           exit;
         end;
        { find libary
           1. cwd
           2. local libary dir
           3. global libary dir
           4. exe path of the compiler (not when linking on target)
          for all searches don't use the directory cache }
        found:=FindFile(s, CurDirRelPath(source_info), false,foundfile);
        if (not found) and (current_module.outputpath^<>'') then
         found:=FindFile(s,current_module.outputpath^,false,foundfile);
        if (not found) then
         found:=current_module.locallibrarysearchpath.FindFile(s,false,foundfile);
        if (not found) then
         found:=librarysearchpath.FindFile(s,false,foundfile);
        if not(cs_link_on_target in current_settings.globalswitches) and (not found) then
         found:=FindFile(s,exepath,false,foundfile);
        foundfile:=ScriptFixFileName(foundfile);
        findlibraryfile:=found;
      end;


{*****************************************************************************
                                   TLINKER
*****************************************************************************}

    Constructor TLinker.Create;
      begin
        Inherited Create;
        ObjectFiles:=TCmdStrList.Create_no_double;
        SharedLibFiles:=TCmdStrList.Create_no_double;
        StaticLibFiles:=TCmdStrList.Create_no_double;
        FrameworkFiles:=TCmdStrList.Create_no_double;
      end;


    Destructor TLinker.Destroy;
      begin
        ObjectFiles.Free;
        SharedLibFiles.Free;
        StaticLibFiles.Free;
        FrameworkFiles.Free;
      end;


    procedure TLinker.AddModuleFiles(hp:tmodule);
      var
        mask : longint;
        i,j  : longint;
        ImportLibrary : TImportLibrary;
        ImportSymbol  : TImportSymbol;
      begin
        with hp do
         begin
           if (flags and uf_has_resourcefiles)<>0 then
             HasResources:=true;
           if (flags and uf_has_exports)<>0 then
             HasExports:=true;
         { link unit files }
           if (flags and uf_no_link)=0 then
            begin
              { create mask which unit files need linking }
              mask:=link_always;
              { static linking ? }
              if (cs_link_static in current_settings.globalswitches) then
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
              if (cs_link_smart in current_settings.globalswitches) then
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
              if (cs_link_shared in current_settings.globalswitches) then
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
                AddObject(linkunitofiles.getusemask(mask),path^,true);
              while not linkunitstaticlibs.empty do
                AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
              while not linkunitsharedlibs.empty do
                AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
            end;
           { Other needed .o and libs, specified using $L,$LINKLIB,external }
           mask:=link_always;
           while not linkotherofiles.empty do
            AddObject(linkotherofiles.Getusemask(mask),path^,false);
           while not linkotherstaticlibs.empty do
            AddStaticCLibrary(linkotherstaticlibs.Getusemask(mask));
           while not linkothersharedlibs.empty do
            AddSharedCLibrary(linkothersharedlibs.Getusemask(mask));
           while not linkotherframeworks.empty do
             AddFramework(linkotherframeworks.Getusemask(mask));
           { Known Library/DLL Imports }
           for i:=0 to ImportLibraryList.Count-1 do
             begin
               ImportLibrary:=TImportLibrary(ImportLibraryList[i]);
               for j:=0 to ImportLibrary.ImportSymbolList.Count-1 do
                 begin
                   ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList[j]);
                   AddImportSymbol(ImportLibrary.Name,ImportSymbol.Name,ImportSymbol.OrdNr,ImportSymbol.IsVar);
                 end;
             end;
         end;
      end;


    procedure TLinker.AddImportSymbol(const libname,symname:TCmdStr;OrdNr: longint;isvar:boolean);
      begin
      end;


    Procedure TLinker.AddObject(const S,unitpath : TCmdStr;isunit:boolean);
      begin
        ObjectFiles.Concat(FindObjectFile(s,unitpath,isunit));
      end;


    Procedure TLinker.AddSharedLibrary(S:TCmdStr);
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


    Procedure TLinker.AddStaticLibrary(const S:TCmdStr);
      var
        ns : TCmdStr;
        found : boolean;
      begin
        if s='' then
          exit;
        found:=FindLibraryFile(s,target_info.staticlibprefix,target_info.staticlibext,ns);
        if not(cs_link_nolink in current_settings.globalswitches) and (not found) then
          Message1(exec_w_libfile_not_found,s);
        StaticLibFiles.Concat(ns);
      end;


    Procedure TLinker.AddSharedCLibrary(S:TCmdStr);
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


    Procedure TLinker.AddFramework(S:TCmdStr);
      begin
        if s='' then
          exit;
        { ready to be added }
        FrameworkFiles.Concat(S);
      end;


    Procedure TLinker.AddStaticCLibrary(const S:TCmdStr);
      var
        ns : TCmdStr;
        found : boolean;
      begin
        if s='' then
         exit;
        found:=FindLibraryFile(s,target_info.staticclibprefix,target_info.staticclibext,ns);
        if not(cs_link_nolink in current_settings.globalswitches) and (not found) then
         Message1(exec_w_libfile_not_found,s);
        StaticLibFiles.Concat(ns);
      end;


    procedure AddImportSymbol(const libname,symname:TCmdStr;OrdNr: longint;isvar:boolean);
      begin
      end;


    procedure TLinker.InitSysInitUnitName;
      begin
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


    Procedure TLinker.ExpandAndApplyOrder(var Src:TCmdStrList);
      var
        p : TLinkStrMap;
        i : longint;
      begin
        // call Virtual TLinker method to initialize
        LoadPredefinedLibraryOrder;

        // something to do?
        if (LinkLibraryAliases.count=0) and (LinkLibraryOrder.Count=0) Then
          exit;
        p:=TLinkStrMap.Create;

        // expand libaliases, clears src
        LinkLibraryAliases.expand(src,p);

        // writeln(src.count,' ',p.count,' ',linklibraryorder.count,' ',linklibraryaliases.count);
        // apply order
        p.UpdateWeights(LinkLibraryOrder);
        p.SortOnWeight;

        // put back in src
        for i:=0 to p.count-1 do
          src.insert(p[i].Key);
        p.free;
      end;


    procedure TLinker.LoadPredefinedLibraryOrder;
      begin
      end;


    function  TLinker.ReOrderEntries : boolean;
      begin
        result:=(LinkLibraryOrder.count>0) or (LinkLibraryAliases.count>0);
      end;


{*****************************************************************************
                              TEXTERNALLINKER
*****************************************************************************}

    Constructor TExternalLinker.Create;
      begin
        inherited Create;
        { set generic defaults }
        FillChar(Info,sizeof(Info),0);
        if cs_link_on_target in current_settings.globalswitches then
          begin
            Info.ResName:=outputexedir+ChangeFileExt(inputfilename,'_link.res');
            Info.ScriptName:=outputexedir+ChangeFileExt(inputfilename,'_script.res');
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


    Function TExternalLinker.FindUtil(const s:TCmdStr):TCmdStr;
      var
        Found    : boolean;
        FoundBin : TCmdStr;
        UtilExe  : TCmdStr;
      begin
        if cs_link_on_target in current_settings.globalswitches then
          begin
            { If linking on target, don't add any path PM }
            FindUtil:=ChangeFileExt(s,target_info.exeext);
            exit;
          end;
        UtilExe:=ChangeFileExt(s,source_info.exeext);
        FoundBin:='';
        Found:=false;
        if utilsdirectory<>'' then
         Found:=FindFile(utilexe,utilsdirectory,false,Foundbin);
        if (not Found) then
         Found:=FindExe(utilexe,false,Foundbin);
        if (not Found) and not(cs_link_nolink in current_settings.globalswitches) then
         begin
           Message1(exec_e_util_not_found,utilexe);
           current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
         end;
        if (FoundBin<>'') then
         Message1(exec_t_using_util,FoundBin);
        FindUtil:=FoundBin;
      end;


    Function TExternalLinker.DoExec(const command:TCmdStr; para:TCmdStr;showinfo,useshell:boolean):boolean;
      var
        exitcode: longint;
      begin
        DoExec:=true;
        if not(cs_link_nolink in current_settings.globalswitches) then
         begin
           FlushOutput;
           if useshell then
             exitcode:=shell(maybequoted(command)+' '+para)
           else
             try
               exitcode:=ExecuteProcess(command,para);
             except on E:EOSError do
               begin
                 Message(exec_e_cant_call_linker);
                 current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
                 DoExec:=false;
               end;
             end;
           if (exitcode<>0) then
             begin
               Message(exec_e_error_while_linking);
               current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
               DoExec:=false;
             end;
         end;
      { Update asmres when externmode is set }
        if cs_link_nolink in current_settings.globalswitches then
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

        function GetNextFiles(const maxCmdLength : AInt; var item : TCmdStrListItem) : TCmdStr;
          begin
            result := '';
            while (assigned(item) and ((length(result) + length(item.str) + 1) < maxCmdLength)) do begin
              result := result + ' ' + item.str;
              item := TCmdStrListItem(item.next);
            end;
          end;

      var
        binstr, scriptfile : TCmdStr;
        cmdstr, nextcmd, smartpath : TCmdStr;
        current : TCmdStrListItem;
        script: Text;
        scripted_ar : boolean;
        success : boolean;
      begin
        MakeStaticLibrary:=false;
      { remove the library, to be sure that it is rewritten }
        DeleteFile(current_module.staticlibfilename^);
      { Call AR }
        smartpath:=FixPath(ChangeFileExt(current_module.asmfilename^,target_info.smartext),false);
        SplitBinCmd(target_ar.arcmd,binstr,cmdstr);
        binstr := FindUtil(utilsprefix + binstr);


        scripted_ar:=target_ar.id=ar_gnu_ar_scripted;

        if scripted_ar then
          begin
            scriptfile := FixFileName(smartpath+'arscript.txt');
            Replace(cmdstr,'$SCRIPT',maybequoted(scriptfile));
            Assign(script, scriptfile);
            Rewrite(script);
            try
              writeln(script, 'CREATE ' + current_module.staticlibfilename^);
              current := TCmdStrListItem(SmartLinkOFiles.First);
              while current <> nil do
                begin
                  writeln(script, 'ADDMOD ' + current.str);
                  current := TCmdStrListItem(current.next);
                end;
              writeln(script, 'SAVE');
              writeln(script, 'END');
            finally
              Close(script);
            end;
            success:=DoExec(binstr,cmdstr,false,true);
          end
        else
          begin
            Replace(cmdstr,'$LIB',maybequoted(current_module.staticlibfilename^));
            { create AR commands }
            success := true;
            current := TCmdStrListItem(SmartLinkOFiles.First);
            repeat
              nextcmd := cmdstr;
              Replace(nextcmd,'$FILES',GetNextFiles(2047, current));
              success:=DoExec(binstr,nextcmd,false,true);
            until (not assigned(current)) or (not success);
          end;

        if (target_ar.arfinishcmd <> '') then
          begin
            SplitBinCmd(target_ar.arfinishcmd,binstr,cmdstr);
            binstr := FindUtil(utilsprefix + binstr);
            Replace(cmdstr,'$LIB',maybequoted(current_module.staticlibfilename^));
            success:=DoExec(binstr,cmdstr,false,true);
          end;

        { Clean up }
        if not(cs_asm_leave in current_settings.globalswitches) then
         if not(cs_link_nolink in current_settings.globalswitches) then
          begin
            while not SmartLinkOFiles.Empty do
              DeleteFile(SmartLinkOFiles.GetFirst);
            if scripted_ar then
              DeleteFile(scriptfile);
            RemoveDir(smartpath);
          end
         else
          begin
            AsmRes.AddDeleteCommand(FixFileName(smartpath+current_module.asmprefix^+'*'+target_info.objext));
            if scripted_ar then
              AsmRes.AddDeleteCommand(scriptfile);
            AsmRes.AddDeleteDirCommand(smartpath);
          end;
        MakeStaticLibrary:=success;
      end;


{*****************************************************************************
                              TINTERNALLINKER
*****************************************************************************}

    Constructor TInternalLinker.Create;
      begin
        inherited Create;
        linkscript:=TCmdStrList.Create;
        FStaticLibraryList:=TFPHashObjectList.Create(true);
        FImportLibraryList:=TFPHashObjectList.Create(true);
        exemap:=nil;
        exeoutput:=nil;
        CObjInput:=TObjInput;
      end;


    Destructor TInternalLinker.Destroy;
      begin
        linkscript.free;
        StaticLibraryList.Free;
        ImportLibraryList.Free;
        if assigned(exeoutput) then
          begin
            exeoutput.free;
            exeoutput:=nil;
          end;
        if assigned(exemap) then
          begin
            exemap.free;
            exemap:=nil;
          end;
        inherited destroy;
      end;


    procedure TInternalLinker.AddImportSymbol(const libname,symname:TCmdStr;OrdNr: longint;isvar:boolean);
      var
        ImportLibrary : TImportLibrary;
        ImportSymbol  : TFPHashObject;
      begin
        ImportLibrary:=TImportLibrary(ImportLibraryList.Find(libname));
        if not assigned(ImportLibrary) then
          ImportLibrary:=TImportLibrary.Create(ImportLibraryList,libname);
        ImportSymbol:=TFPHashObject(ImportLibrary.ImportSymbolList.Find(symname));
        if not assigned(ImportSymbol) then
          ImportSymbol:=TImportSymbol.Create(ImportLibrary.ImportSymbolList,symname,OrdNr,isvar);
      end;


    procedure TInternalLinker.Load_ReadObject(const para:TCmdStr);
      var
        objdata   : TObjData;
        objinput  : TObjinput;
        objreader : TObjectReader;
        fn        : TCmdStr;
      begin
        fn:=FindObjectFile(para,'',false);
        Comment(V_Tried,'Reading object '+fn);
        objinput:=CObjInput.Create;
        objdata:=objinput.newObjData(para);
        objreader:=TObjectreader.create;
        if objreader.openfile(fn) then
          begin
            if objinput.ReadObjData(objreader,objdata) then
              exeoutput.addobjdata(objdata);
          end;
        { release input object }
        objinput.free;
        objreader.free;
      end;


    procedure TInternalLinker.Load_ReadStaticLibrary(const para:TCmdStr);
      var
        objreader : TObjectReader;
      begin
{ TODO: Cleanup ignoring of   FPC generated libimp*.a files}
        { Don't load import libraries }
        if copy(ExtractFileName(para),1,6)='libimp' then
          exit;
        Comment(V_Tried,'Opening library '+para);
        objreader:=TArObjectreader.create(para);
        TStaticLibrary.Create(StaticLibraryList,para,objreader,CObjInput);
      end;


    procedure TInternalLinker.ParseScript_Load;
      var
        s,
        para,
        keyword : String;
        hp : TCmdStrListItem;
      begin
        exeoutput.Load_Start;
        hp:=TCmdStrListItem(linkscript.first);
        while assigned(hp) do
          begin
            s:=hp.str;
            if (s='') or (s[1]='#') then
              continue;
            keyword:=Upper(GetToken(s,' '));
            para:=GetToken(s,' ');
            if keyword='SYMBOL' then
              ExeOutput.Load_Symbol(para)
            else if keyword='ENTRYNAME' then
              ExeOutput.Load_EntryName(para)
            else if keyword='ISSHAREDLIBRARY' then
              ExeOutput.Load_IsSharedLibrary
            else if keyword='IMAGEBASE' then
              ExeOutput.Load_ImageBase(para)
            else if keyword='READOBJECT' then
              Load_ReadObject(para)
            else if keyword='READSTATICLIBRARY' then
              Load_ReadStaticLibrary(para);
            hp:=TCmdStrListItem(hp.next);
          end;
      end;


    procedure TInternalLinker.ParseScript_Order;
      var
        s,
        para,
        keyword : String;
        hp : TCmdStrListItem;
      begin
        exeoutput.Order_Start;
        hp:=TCmdStrListItem(linkscript.first);
        while assigned(hp) do
          begin
            s:=hp.str;
            if (s='') or (s[1]='#') then
              continue;
            keyword:=Upper(GetToken(s,' '));
            para:=GetToken(s,' ');
            if keyword='EXESECTION' then
              ExeOutput.Order_ExeSection(para)
            else if keyword='ENDEXESECTION' then
              ExeOutput.Order_EndExeSection
            else if keyword='OBJSECTION' then
              ExeOutput.Order_ObjSection(para)
            else if keyword='ZEROS' then
              ExeOutput.Order_Zeros(para)
            else if keyword='SYMBOL' then
              ExeOutput.Order_Symbol(para);
            hp:=TCmdStrListItem(hp.next);
          end;
        exeoutput.Order_End;
      end;


    procedure TInternalLinker.ParseScript_MemPos;
      var
        s,
        para,
        keyword : String;
        hp : TCmdStrListItem;
      begin
        exeoutput.MemPos_Start;
        hp:=TCmdStrListItem(linkscript.first);
        while assigned(hp) do
          begin
            s:=hp.str;
            if (s='') or (s[1]='#') then
              continue;
            keyword:=Upper(GetToken(s,' '));
            para:=GetToken(s,' ');
            if keyword='EXESECTION' then
              ExeOutput.MemPos_ExeSection(para)
            else if keyword='ENDEXESECTION' then
              ExeOutput.MemPos_EndExeSection
            else if keyword='HEADER' then
              ExeOutput.MemPos_Header;
            hp:=TCmdStrListItem(hp.next);
          end;
      end;


    procedure TInternalLinker.ParseScript_DataPos;
      var
        s,
        para,
        keyword : String;
        hp : TCmdStrListItem;
      begin
        exeoutput.DataPos_Start;
        hp:=TCmdStrListItem(linkscript.first);
        while assigned(hp) do
          begin
            s:=hp.str;
            if (s='') or (s[1]='#') then
              continue;
            keyword:=Upper(GetToken(s,' '));
            para:=GetToken(s,' ');
            if keyword='EXESECTION' then
              ExeOutput.DataPos_ExeSection(para)
            else if keyword='ENDEXESECTION' then
              ExeOutput.DataPos_EndExeSection
            else if keyword='HEADER' then
              ExeOutput.DataPos_Header
            else if keyword='SYMBOLS' then
              ExeOutput.DataPos_Symbols;
            hp:=TCmdStrListItem(hp.next);
          end;
      end;


    procedure TInternalLinker.PrintLinkerScript;
      var
        hp : TCmdStrListItem;
      begin
        if not assigned(exemap) then
          exit;
        exemap.Add('Used linker script');
        exemap.Add('');
        hp:=TCmdStrListItem(linkscript.first);
        while assigned(hp) do
          begin
            exemap.Add(hp.str);
            hp:=TCmdStrListItem(hp.next);
          end;
      end;


    function TInternalLinker.RunLinkScript(const outputname:TCmdStr):boolean;
      label
        myexit;
      var
        bsssize : aword;
        bsssec  : TExeSection;
        dbgname : TCmdStr;
      begin
        result:=false;

        Message1(exec_i_linking,outputname);
        FlushOutput;

{ TODO: Load custom linker script}
        DefaultLinkScript;

        exeoutput:=CExeOutput.Create;

        if (cs_link_map in current_settings.globalswitches) then
          exemap:=texemap.create(current_module.mapfilename^);

        PrintLinkerScript;

        { Load .o files and resolve symbols }
        ParseScript_Load;
        exeoutput.ResolveSymbols(StaticLibraryList);
        { Generate symbols and code to do the importing }
        exeoutput.GenerateLibraryImports(ImportLibraryList);
        { Fill external symbols data }
        exeoutput.FixupSymbols;
        if ErrorCount>0 then
          goto myexit;

        { Create .exe sections and add .o sections }
        ParseScript_Order;
        exeoutput.RemoveUnreferencedSections;
        exeoutput.MergeStabs;
        exeoutput.RemoveEmptySections;
        if ErrorCount>0 then
          goto myexit;

        { Calc positions in mem }
        ParseScript_MemPos;
        exeoutput.FixupRelocations;
        exeoutput.PrintMemoryMap;
        if ErrorCount>0 then
          goto myexit;

        if cs_link_separate_dbg_file in current_settings.globalswitches then
          begin
            { create debuginfo, which is an executable without data on disk }
            dbgname:=ChangeFileExt(outputname,'.dbg');
            exeoutput.ExeWriteMode:=ewm_dbgonly;
            ParseScript_DataPos;
            exeoutput.WriteExeFile(dbgname);
            { create executable with link to just created debuginfo file }
            exeoutput.ExeWriteMode:=ewm_exeonly;
            exeoutput.RemoveDebugInfo;
            exeoutput.GenerateDebugLink(dbgname,GetFileCRC(dbgname));
            ParseScript_MemPos;
            ParseScript_DataPos;
            exeoutput.WriteExeFile(outputname);
          end
        else
          begin
            exeoutput.ExeWriteMode:=ewm_exefull;
            ParseScript_DataPos;
            exeoutput.WriteExeFile(outputname);
          end;

{ TODO: fixed section names}
        status.codesize:=exeoutput.findexesection('.text').size;
        status.datasize:=exeoutput.findexesection('.data').size;
        bsssec:=exeoutput.findexesection('.bss');
        if assigned(bsssec) then
          bsssize:=bsssec.size
        else
          bsssize:=0;

        { Executable info }
        Message1(execinfo_x_codesize,tostr(status.codesize));
        Message1(execinfo_x_initdatasize,tostr(status.datasize));
        Message1(execinfo_x_uninitdatasize,tostr(bsssize));
        Message1(execinfo_x_stackreserve,tostr(stacksize));

      myexit:
        { close map }
        if assigned(exemap) then
          begin
            exemap.free;
            exemap:=nil;
          end;

        { close exe }
        exeoutput.free;
        exeoutput:=nil;

        result:=true;
      end;


    function TInternalLinker.MakeExecutable:boolean;
      begin
        IsSharedLibrary:=false;
        result:=RunLinkScript(current_module.exefilename^);
      end;


    function TInternalLinker.MakeSharedLibrary:boolean;
      begin
        IsSharedLibrary:=true;
        result:=RunLinkScript(current_module.sharedlibfilename^);
      end;


{*****************************************************************************
                                 Init/Done
*****************************************************************************}

    procedure InitLinker;
      var
        lk : TlinkerClass;
      begin
        if (cs_link_extern in current_settings.globalswitches) and
           assigned(target_info.linkextern) then
          begin
            lk:=TlinkerClass(target_info.linkextern);
            linker:=lk.Create;
          end
        else
          if assigned(target_info.link) then
            begin
              lk:=TLinkerClass(target_info.link);
              linker:=lk.Create;
            end
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
            id          : ar_gnu_ar;
            arcmd       : 'ar qS $LIB $FILES';
            arfinishcmd : 'ar s $LIB'
          );

      ar_gnu_ar_scripted_info : tarinfo =
          (
            id    : ar_gnu_ar_scripted;
            arcmd : 'ar -M < $SCRIPT';
            arfinishcmd : ''
          );

      ar_gnu_gar_info : tarinfo =
          ( id          : ar_gnu_gar;
            arcmd       : 'gar qS $LIB $FILES';
            arfinishcmd : 'gar s $LIB'
          );


initialization
  RegisterAr(ar_gnu_ar_info);
  RegisterAr(ar_gnu_ar_scripted_info);
  RegisterAr(ar_gnu_gar_info);
end.
