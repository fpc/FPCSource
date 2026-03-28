{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the first loading and searching of the modules

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
unit fppu;

{$i fpcdefs.inc}

{ $define DEBUG_UNIT_CRC_CHANGES}

{ close ppufiles on system that are
  short on file handles like DOS system PM }
{$ifdef GO32V2}
  {$define SHORT_ON_FILE_HANDLES}
{$endif GO32V2}
{$ifdef WATCOM}
  {$define SHORT_ON_FILE_HANDLES}
{$endif WATCOM}

interface

    uses
      cmsgs,verbose,
      cutils,cclasses,cstreams,
      globtype,globals,globstat,fpchash,finput,fmodule,compilerbase,
      symbase,ppu,symtype;

    type
       { tppumodule }
       TAvailableUnitFile = (auPPU,auSrc);
       TAvailableUnitFiles = set of TAvailableUnitFile;

       tppumodule = class(tmodule)
          ppufile    : tcompilerppufile; { the PPU file }
          sourcefn   : TPathStr; { Source specified with "uses .. in '..'" }
          comments   : TCmdStrList;
          nsprefix   : TCmdStr; { Namespace prefix the unit was found with }
          loadedfrommodule: tmodule;
{$ifdef Test_Double_checksum}
          interface_read_crc_index,
          interface_write_crc_index,
          indirect_read_crc_index,
          indirect_write_crc_index,
          implementation_read_crc_index,
          implementation_write_crc_index : cardinal;
          interface_crc_array,
          indirect_crc_array,
          implementation_crc_array  : pointer;
{$endif def Test_Double_checksum}
          constructor create(LoadedFrom:TModule;const amodulename: string; const afilename:TPathStr;_is_unit:boolean;acompiler:TCompilerBase);
          destructor destroy;override;
          function statestr: string; override;
          procedure checkstate; override;
          procedure reset(for_recompile: boolean);override;
          procedure re_resolve(loadfrom: tmodule);
          function  openppufile:boolean;
          function  openppustream(strm:TCStream):boolean;
          procedure getppucrc;
          procedure writeppu;
          function loadppu(from_module : tmodule) : boolean;
          function get_check_uses(out check_impl_uses, check_crc: boolean): boolean;
          function continueloadppu : boolean;
          function canreload(out firstwaiting: tmodule; ignore_do_reload: boolean): boolean;
          procedure reload;
          function ppuloadcancontinue(out firstwaiting: tmodule): boolean;
          function is_reload_needed(pu: tdependent_unit): boolean; override;
          procedure restore_state;
          procedure store_state;
          procedure recompile_from_sources;
          procedure check_sources_for_recompile;
          procedure post_load_or_compile(from_module : tmodule);
          procedure discardppu;
          procedure setdefgeneration;
          procedure end_of_parsing;override;
       private
          unitimportsymsderefs : tfplist;
         { Each time a unit's defs are (re)created, its defsgeneration is
           set to the value of a global counter, and the global counter is
           increased. We only reresolve its dependent units' defs in case
           they have been resolved only for an older generation, in order to
           avoid endless resolving loops in case of cyclic dependencies. }
          defsgeneration : longint;
          stored_state: tglobalstate;

          function check_loadfrompackage: boolean;
          function  openppu(ppufiletime:longint):boolean;
          procedure mark_recompile_needed(reason: trecompile_reason);
          function  search_unit_files(loaded_from : tmodule; onlysource:boolean):TAvailableUnitFiles;
          function  search_unit(loaded_from : tmodule; onlysource,shortname:boolean):TAvailableUnitFiles;
          function  loadfrompackage:boolean;
          procedure load_interface;
          procedure load_implementation;
          function load_usedunits: boolean;
          function load_usedunits_section: boolean;
          function ppu_check_used_crcs: boolean;
          procedure printcomments;
          procedure queuecomment(const s:TMsgStr;v,w:longint);
          procedure buildderefunitimportsyms;
          procedure derefunitimportsyms;
          procedure freederefunitimportsyms;
          procedure writesourcefiles;
          procedure writeusedunit(intf:boolean);
          procedure writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
          procedure writederefmap;
          procedure writederefdata;
          procedure writeImportSymbols;
          procedure writeResources;
          procedure writeOrderedSymbols;
          procedure writeunitimportsyms;
          procedure writeasmsyms(kind:tunitasmlisttype;list:tfphashobjectlist);
          procedure writeextraheader;
          procedure readsourcefiles;
          procedure readloadunit;
          procedure readlinkcontainer(var p:tlinkcontainer);
          procedure readderefmap;
          procedure readderefdata;
          procedure readImportSymbols;
          procedure readResources;
          procedure readOrderedSymbols;
          procedure readwpofile;
          procedure readunitimportsyms;
          procedure readasmsyms;
          procedure readextraheader;
{$IFDEF MACRO_DIFF_HINT}
          procedure writeusedmacro(p:TNamedIndexItem;arg:pointer);
          procedure writeusedmacros;
          procedure readusedmacros;
{$ENDIF}
       end;

    function registerunit(compiler:TCompilerBase;callermodule:tmodule;const s : TIDString;const fn:string; out is_new:boolean) : tppumodule;


implementation

uses
  SysUtils,
  cfileutl,
  systems,version,options,compiler,
  symtable, symsym,
  wpoinfo,
  scanner,
  aasmbase,ogbase,
  comphook,
  entfile,fpkg,fpcp;


var
  currentdefgeneration: longint;

{****************************************************************************
                                TPPUMODULE
 ****************************************************************************}

    constructor tppumodule.create(LoadedFrom:TModule;const amodulename: string; const afilename:TPathStr;_is_unit:boolean;acompiler:TCompilerBase);
      begin
        inherited create(LoadedFrom,amodulename,afilename,_is_unit,acompiler);
        loadedfrommodule:=LoadedFrom;
        ppufile:=nil;
        sourcefn:=afilename;
        unitimportsymsderefs:=tfplist.create;
      end;


    destructor tppumodule.destroy;
      begin
        discardppu;
        comments.free;
        comments:=nil;
        { all derefs allocated with new
          are dispose'd inside this method }
        freederefunitimportsyms;
        unitimportsymsderefs.free;
        unitimportsymsderefs:=nil;
        inherited Destroy;
      end;

    function tppumodule.statestr: string;
      begin
        Result:=inherited statestr;
        if state<>ms_load then exit;
        if ppu_waitingfor_crc then
          Result:=Result+',waitcrc'
        else if interface_compiled then
          Result:=Result+',interface_compiled'
        else
          Result:=Result+',waitintf';
      end;

    procedure tppumodule.checkstate;
      begin
        if state=ms_load then

        else
          inherited checkstate;
      end;

    procedure tppumodule.reset(for_recompile : boolean);
      begin
        loadedfrommodule:=nil;
        ppu_waitingfor_crc:=false;
        discardppu;
        freederefunitimportsyms;
        unitimportsymsderefs.free;
        unitimportsymsderefs:=tfplist.create;
        inherited reset(for_recompile);
      end;

    procedure tppumodule.re_resolve(loadfrom: tmodule);

      begin
        compiler.verbose.Message1(unit_u_reresolving_unit,modulename^);
        if tstoredsymtable(globalsymtable).is_deref_built then
          tstoredsymtable(globalsymtable).deref(false);
        if tstoredsymtable(globalsymtable).is_derefimpl_built then
          tstoredsymtable(globalsymtable).derefimpl(false);
        if assigned(localsymtable) then
          begin
            { we have only builderef(impl)'d the registered symbols of
              the localsymtable -> also only deref those again }
            if tstoredsymtable(localsymtable).is_deref_built then
              tstoredsymtable(localsymtable).deref(true);
            if tstoredsymtable(localsymtable).is_derefimpl_built then
              tstoredsymtable(localsymtable).derefimpl(true);
          end;
        derefimportedsymbols;
        if assigned(wpoinfo) then
          begin
            tunitwpoinfo(wpoinfo).deref;
            tunitwpoinfo(wpoinfo).derefimpl;
          end;

        { all dependent units were already flagged recursively for reload }
        defsgeneration:=currentdefgeneration;
      end;


    procedure tppumodule.queuecomment(const s:TMsgStr;v,w:longint);
    begin
      if comments = nil then
        comments := TCmdStrList.create;
      comments.insert(s);
    end;

    procedure tppumodule.printcomments;
    var
      comment: string;
    begin
      if comments = nil then
        exit;
      { comments are inserted in reverse order }
      repeat
        comment := comments.getlast;
        if length(comment) = 0 then
          exit;
        do_comment(v_normal, comment);
      until false;
    end;

    function tppumodule.openppufile:boolean;
      var
        ppufiletime : longint;
      begin
        openppufile:=false;
        compiler.verbose.Message1(unit_t_ppu_loading,ppufilename,@queuecomment);
      { Get ppufile time (also check if the file exists) }
        ppufiletime:=getnamedfiletime(ppufilename);
        if ppufiletime=-1 then
         exit;
      { Open the ppufile }
        compiler.verbose.Message1(unit_u_ppu_name,ppufilename);
        ppufile:=tcompilerppufile.create(ppufilename,compiler);
        if not ppufile.openfile then
         begin
           discardppu;
           compiler.verbose.Message(unit_u_ppu_file_too_short);
           exit;
         end;
        result:=openppu(ppufiletime);
      end;


    function tppumodule.openppustream(strm:TCStream):boolean;
      begin
        result:=false;
      { Open the ppufile }
        compiler.verbose.Message1(unit_u_ppu_name,ppufilename);
        ppufile:=tcompilerppufile.create(ppufilename,compiler);
        if not ppufile.openstream(strm) then
         begin
           discardppu;
           compiler.verbose.Message(unit_u_ppu_file_too_short);
           exit;
         end;
        result:=openppu(-1);
      end;


    function tppumodule.openppu(ppufiletime:longint):boolean;

      function checkheader: boolean;
        var
          psi: psysteminfo;
          system_name: shortstring;
        begin
          result:=false;
          { check for a valid PPU file }
            if not ppufile.CheckPPUId then
             begin
               compiler.verbose.Message(unit_u_ppu_invalid_header);
               exit;
             end;
          { check for allowed PPU versions }
            if not (ppufile.getversion = CurrentPPUVersion) then
             begin
               compiler.verbose.Message1(unit_u_ppu_invalid_version,tostr(ppufile.getversion),@queuecomment);
               exit;
             end;
          { check the target processor }
            if tsystemcpu(ppufile.header.common.cpu)<>compiler.target.cpu then
             begin
               compiler.verbose.Message1(unit_u_ppu_invalid_processor,cpu2str[tsystemcpu(ppufile.header.common.cpu)],@queuecomment);
               exit;
             end;
          { check target }
            if tsystem(ppufile.header.common.target)<>compiler.target.info.system then
             begin
               psi:=targetinfos[tsystem(ppufile.header.common.target)];
               if assigned(psi) then
                 system_name:=psi^.shortname
               else
                 system_name:='invalid ('+tostr(ppufile.header.common.target)+')';
               compiler.verbose.Message1(unit_u_ppu_invalid_target,system_name,@queuecomment);
               exit;
             end;
{$ifdef cpufpemu}
          { check if floating point emulation is on?
            fpu emulation isn't unit levelwise because it affects calling convention }
          if ((ppufile.header.common.flags and uf_fpu_emulation)<>0) <>
             (cs_fp_emulation in compiler.globals.current_settings.moduleswitches) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_fpumode,@queuecomment);
              exit;
            end;
{$endif cpufpemu}
           result:=true;
        end;

      function checkextraheader: boolean;
        begin
          result:=false;
          if ppufile.readentry<>ibextraheader then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_header);
              exit;
            end;
          readextraheader;
          if (longversion<>CurrentPPULongVersion) or
             not ppufile.EndOfEntry then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_header);
              exit;
            end;
{$ifdef i8086}
          { check i8086 memory model flags }
          if (mf_i8086_far_code in moduleflags) <>
             (compiler.globals.current_settings.x86memorymodel in [mm_medium,mm_large,mm_huge]) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_memory_model,@queuecomment);
              exit;
            end;
          if (mf_i8086_far_data in moduleflags) <>
             (compiler.globals.current_settings.x86memorymodel in [mm_compact,mm_large]) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_memory_model,@queuecomment);
              exit;
            end;
          if (mf_i8086_huge_data in moduleflags) <>
             (compiler.globals.current_settings.x86memorymodel=mm_huge) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_memory_model,@queuecomment);
              exit;
            end;
          if (mf_i8086_cs_equals_ds in moduleflags) <>
             (compiler.globals.current_settings.x86memorymodel=mm_tiny) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_memory_model,@queuecomment);
              exit;
            end;
          if (mf_i8086_ss_equals_ds in moduleflags) <>
             (compiler.globals.current_settings.x86memorymodel in [mm_tiny,mm_small,mm_medium]) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_memory_model,@queuecomment);
              exit;
            end;
{$endif i8086}
{$ifdef wasm}
          { check WebAssembly exceptions mode flag }
          if ((mf_wasm_no_exceptions in moduleflags) <>
              (ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches)) or
             ((mf_wasm_bf_exceptions in moduleflags) <>
              (ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches)) or
             ((mf_wasm_exnref_exceptions in moduleflags) <>
              (ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches)) or
             ((mf_wasm_native_exceptions in moduleflags) <>
              (ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches)) then
            begin
              compiler.verbose.Message(unit_u_ppu_invalid_wasm_exceptions_mode,@queuecomment);
              exit;
            end;
          if (mf_wasm_threads in moduleflags) <>
             (ts_wasm_threads in compiler.globals.current_settings.targetswitches) then
            begin
              compiler.verbose.Message(unit_u_ppu_wasm_threads_mismatch,@queuecomment);
              exit;
            end;
{$endif}
          if {$ifdef symansistr}not{$endif}(mf_symansistr in moduleflags) then
            begin
              compiler.verbose.Message(unit_u_ppu_symansistr_mismatch,@queuecomment);
              exit;
            end;
          if {$ifdef llvm}not{$endif}(mf_llvm in moduleflags) then
            begin
              compiler.verbose.Message(unit_u_ppu_llvm_mismatch,@queuecomment);
              exit;
            end;
          result:=true;
        end;

      begin
        openppu:=false;
        if not checkheader or
           not checkextraheader then
          begin
            discardppu;
            exit;
          end;

      { Load values for easier access }
        headerflags:=ppufile.header.common.flags;
        crc:=ppufile.header.checksum;
        crc_final:=true;
        interface_crc:=ppufile.header.interface_checksum;
        indirect_crc:=ppufile.header.indirect_checksum;
        change_endian:=ppufile.change_endian;
      { Show Debug info }
        if ppufiletime<>-1 then
          compiler.verbose.Message1(unit_u_ppu_time,compiler.time.filetimestring(ppufiletime))
        else
          compiler.verbose.Message1(unit_u_ppu_time,'unknown');
        compiler.verbose.Message1(unit_u_ppu_flags,tostr(headerflags));
        compiler.verbose.Message1(unit_u_ppu_crc,hexstr(ppufile.header.checksum,8));
        compiler.verbose.Message1(unit_u_ppu_crc,hexstr(ppufile.header.interface_checksum,8)+' (intfc)');
        compiler.verbose.Message1(unit_u_ppu_crc,hexstr(ppufile.header.indirect_checksum,8)+' (indc)');
        compiler.verbose.Comment(V_used,'Number of definitions: '+tostr(ppufile.header.deflistsize));
        compiler.verbose.Comment(V_used,'Number of symbols: '+tostr(ppufile.header.symlistsize));
        openppu:=true;
      end;


    function tppumodule.search_unit_files(loaded_from : tmodule; onlysource:boolean):TAvailableUnitFiles;
      var
        found : TAvailableUnitFiles;
      begin
        found:=search_unit(loaded_from,onlysource,false);
        if (found=[]) and
           (ft83 in AllowedFilenameTransFormations) and
           (length(modulename^)>8) then
           found:=search_unit(loaded_from,onlysource,true);
        search_unit_files:=found;
      end;

    function tppumodule.search_unit(loaded_from : tmodule; onlysource,shortname:boolean):TAvailableUnitFiles;
      var
         singlepathstring,
         filename : TCmdStr;

         Function UnitExists(const ext:string;var foundfile:TCmdStr;const prefix:TCmdStr):boolean;
         var
           s : tcmdstr;
         begin
           if compiler.verbose.CheckVerbosity(V_Tried) then
             compiler.verbose.Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           s:=FileName+ext;
           if prefix<>'' then
             s:=prefix+'.'+s;
           UnitExists:=FindFile(s,Singlepathstring,true,foundfile);
         end;

         Function PPUSearchPath(const s,prefix:TCmdStr):boolean;
         var
           found : boolean;
           hs,
           newname : TCmdStr;
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for PPU file }
           Found:=UnitExists(compiler.target.info.unitext,hs,prefix);
           if Found then
            Begin
              SetFileName(hs,false);
              if prefix<>'' then
                begin
                  newname:=prefix+'.'+realmodulename^;
                  stringdispose(realmodulename);
                  realmodulename:=stringdup(newname);
                  stringdispose(modulename);
                  modulename:=stringdup(upper(newname));
                end;
              Found:=openppufile;
            End;
           PPUSearchPath:=Found;
         end;

         Function SourceSearchPath(const s,prefix:TCmdStr):boolean;
         var
           found   : boolean;
           hs,
           newname : TCmdStr;
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for Sources }
           ppufile:=nil;
           recompile_reason:=rr_noppu;
         {Check for .pp file}
           Found:=UnitExists(sourceext,hs,prefix);
           if not Found then
            begin
              { Check for .pas }
              Found:=UnitExists(pasext,hs,prefix);
            end;
           if not Found and
              ((m_mac in compiler.globals.current_settings.modeswitches) or
               (tf_p_ext_support in compiler.target.info.flags)) then
            begin
              { Check for .p, if mode is macpas}
              Found:=UnitExists(pext,hs,prefix);
            end;
           mainsource:='';
           if Found then
            begin
              sources_avail:=true;
              { Load Filenames when found }
              mainsource:=hs;
              SetFileName(hs,false);
              if prefix<>'' then
                begin
                  newname:=prefix+'.'+realmodulename^;
                  stringdispose(realmodulename);
                  realmodulename:=stringdup(newname);
                  stringdispose(modulename);
                  modulename:=stringdup(upper(newname));
                end;
            end
           else
            sources_avail:=false;
           SourceSearchPath:=Found;
         end;

         Function SearchPath(const s,prefix:TCmdStr):TAvailableUnitFiles;
         var
           found : TAvailableUnitFiles;
         begin
           { First check for a ppu, then for the source }
           found:=[];
           if not onlysource then
             if PPUSearchPath(s,prefix) then
               Include(found,auPPU);
           if found=[] then
             if SourceSearchPath(s,prefix) then
              Include(found,auSrc);
           SearchPath:=found;
         end;

         Function SearchPathList(list:TSearchPathList;const prefix:TCmdStr):TAvailableUnitFiles;
         var
           hp : TCmdStrListItem;
           found : TAvailableUnitFiles;
         begin
           found:=[];
           hp:=TCmdStrListItem(list.First);
           while assigned(hp) do
            begin
              found:=SearchPath(hp.Str,prefix);
              if found<>[] then
               break;
              hp:=TCmdStrListItem(hp.next);
            end;
           SearchPathList:=found;
         end;

         function SearchPPUPaths(const prefix:TCmdStr):boolean;
         begin
           result:=PPUSearchPath('.',prefix);
           if (not result) and (outputpath<>'') then
            result:=PPUSearchPath(outputpath,prefix);
           if (not result) and Assigned(main_module) and (main_module.Path<>'')  then
            result:=PPUSearchPath(main_module.Path,prefix);
         end;

         function SearchSourcePaths(const prefix:TCmdStr):TAvailableUnitFiles;
         begin
           result:=[];
           if SourceSearchPath('.',prefix) then
              include(Result,auSrc);
           if (result=[]) and Assigned(main_module) and (main_module.Path<>'') then
             if SourceSearchPath(main_module.Path,prefix) then
              include(Result,auSrc);
           if (result=[]) and Assigned(loaded_from) then
             result:=SearchPathList(loaded_from.LocalUnitSearchPath,prefix);
           if (result=[]) then
             result:=SearchPathList(compiler.globals.UnitSearchPath,prefix);
         end;

         function SearchNamespaceList(const prefixes:TCmdStrList): TAvailableUnitFiles;
         var
           nsitem : TCmdStrListItem;
           res : TAvailableUnitFiles;
         begin
           res:=[];
           nsitem:=TCmdStrListItem(prefixes.first);
           while assigned(nsitem) do
             begin
               if not onlysource then
                 begin
                   if SearchPPUPaths(nsitem.str) then
                     Include(res,auPPU);
                   if res<>[] then
                     break;
                 end;
               res:=SearchSourcePaths(nsitem.str);
               if res<>[] then
                 break;
               nsitem:=TCmdStrListItem(nsitem.next);
             end;
           if assigned(nsitem) then
             nsprefix:=nsitem.str;
           result:=res;
         end;


       var
         fnd : TAvailableUnitFiles;
         hs : TPathStr;
       begin
         fnd:=[];
         if shortname then
           filename:=FixFileName(Copy(realmodulename^,1,8))
         else
           filename:=FixFileName(realmodulename^);
         { try to find unit
            1. look for ppu in cwd
            2. look for ppu in outputpath if set, this is tp7 compatible (PFV)
            3. look for ppu in maindir
            4. look for the specified source file (from the uses line)
            5. look for source in cwd
            6. look for source in maindir
            7. local unit pathlist
            8. global unit pathlist
            9. for each default namespace:
                  repeat 1 - 3 and 5 - 8 with namespace as prefix }
         if not onlysource then
           if SearchPPUPaths('') then
             include(fnd,auPPU);
         if (fnd=[]) and (sourcefn<>'') then
          begin
            { the full filename is specified so we can't use here the
              searchpath (PFV) }
            if compiler.verbose.CheckVerbosity(V_Tried) then
              compiler.verbose.Message1(unit_t_unitsearch,ChangeFileExt(sourcefn,sourceext));
            if FindFile(ChangeFileExt(sourcefn,sourceext),'',true,hs) then
              include(fnd,auSrc);
            if (fnd=[]) then
             begin
               if compiler.verbose.CheckVerbosity(V_Tried) then
                 compiler.verbose.Message1(unit_t_unitsearch,ChangeFileExt(sourcefn,pasext));
               if FindFile(ChangeFileExt(sourcefn,pasext),'',true,hs) then
                 include(fnd,auSrc);
             end;
            if (fnd=[]) and
               ((m_mac in compiler.globals.current_settings.modeswitches) or
                (tf_p_ext_support in compiler.target.info.flags)) then
             begin
               if compiler.verbose.CheckVerbosity(V_Tried) then
                 compiler.verbose.Message1(unit_t_unitsearch,ChangeFileExt(sourcefn,pext));
               if FindFile(ChangeFileExt(sourcefn,pext),'',true,hs) then
                include(fnd,auSrc)
             end;
            if [auSrc]=fnd then
             begin
               sources_avail:=true;
               {$IFDEF DEBUG_PPU_CYCLES}
               writeln('PPUALGO tppumodule.search_unit only sources, no ppu -> ',modulename^,' old=',state,' new=',ms_compile);
               {$ENDIF}
               state:=ms_compile;
               recompile_reason:=rr_noppu;
               mainsource:=hs;
               SetFileName(hs,false);
             end;
          end;
         if fnd=[] then
           begin
             fnd:=SearchSourcePaths('');
             // current_namespacelist is set to the current module's namespacelist.
             if (fnd=[]) and assigned(compiler.globals.current_namespacelist) and (compiler.globals.current_namespacelist.count>0) then
               fnd:=SearchNameSpaceList(compiler.globals.current_namespacelist);
             if (fnd=[]) and (compiler.globals.namespacelist.count>0) then
               fnd:=SearchNameSpaceList(compiler.globals.namespacelist);
           end;
         search_unit:=fnd;
      end;

    function tppumodule.loadfrompackage: boolean;
      (*var
        singlepathstring,
        filename : TCmdStr;

        Function UnitExists(const ext:string;var foundfile:TCmdStr):boolean;
          begin
            if CheckVerbosity(V_Tried) then
              compiler.verbose.Message1(unit_t_unitsearch,Singlepathstring+filename);
            UnitExists:=FindFile(FileName,Singlepathstring,true,foundfile);
          end;

        Function PPUSearchPath(const s:TCmdStr):boolean;
          var
            found : boolean;
            hs    : TCmdStr;
          begin
            Found:=false;
            singlepathstring:=FixPath(s,false);
          { Check for PPU file }
            Found:=UnitExists(compiler.target.info.unitext,hs);
            if Found then
             Begin
               SetFileName(hs,false);
               //Found:=OpenPPU;
             End;
            PPUSearchPath:=Found;
          end;

        Function SearchPathList(list:TSearchPathList):boolean;
          var
            hp : TCmdStrListItem;
            found : boolean;
          begin
            found:=false;
            hp:=TCmdStrListItem(list.First);
            while assigned(hp) do
             begin
               found:=PPUSearchPath(hp.Str);
               if found then
                break;
               hp:=TCmdStrListItem(hp.next);
             end;
            SearchPathList:=found;
          end;*)

      var
        pkg : ppackageentry;
        pkgunit : pcontainedunit;
        i,idx : longint;
        strm : TCStream;
      begin
        result:=false;
        for i:=0 to compiler.globals.packagelist.count-1 do
          begin
            pkg:=ppackageentry(compiler.globals.packagelist[i]);
            if not assigned(pkg^.package) then
              internalerror(2013053103);
            idx:=pkg^.package.containedmodules.FindIndexOf(modulename^);
            if idx>=0 then
              begin
                { the unit is part of this package }
                pkgunit:=pcontainedunit(pkg^.package.containedmodules[idx]);
                if not assigned(pkgunit^.module) then
                  pkgunit^.module:=self;
                { ToDo: check whether we really don't need this anymore }
                {filename:=pkgunit^.ppufile;
                if not SearchPathList(compiler.globals.unitsearchpath) then
                  exit};
                strm:=tpcppackage(pkg^.package).getmodulestream(self);
                if not assigned(strm) then
                  internalerror(2015103002);
                if not openppustream(strm) then
                  exit;
                package:=pkg^.package;
                compiler.verbose.Message2(unit_u_loading_from_package,modulename^,pkg^.package.packagename^);

                { now load the unit and all used units }
                load_interface;
                if not load_usedunits then
                  internalerror(2026020415);
                compiler.verbose.Message1(unit_u_finished_loading_unit,modulename^);

                result:=true;
                break;
              end;
          end;
      end;


    procedure tppumodule.buildderefunitimportsyms;
      { called by writeppu }
      var
        i : longint;
        deref : pderef;
        importsym: tunitimportsym;
      begin
        unitimportsymsderefs.capacity:=unitimportsymsderefs.count+unitimportsyms.count;
        for i:=0 to unitimportsyms.count-1 do
          begin
            importsym:=tunitimportsym(unitimportsyms[i]);
            new(deref);
            deref^.build(importsym.sym);
            unitimportsymsderefs.add(deref);
          end;
      end;


    procedure tppumodule.derefunitimportsyms;
      { called at end of loading a ppu }
      var
        i : longint;
        sym : tsym;
      begin
        unitimportsyms.capacity:=unitimportsyms.count+unitimportsymsderefs.count;
        for i:=0 to unitimportsymsderefs.count-1 do
          begin
            sym:=tsym(pderef(unitimportsymsderefs[i])^.resolve);
            addimportedsym(sym,false);
          end;
      end;

    procedure tppumodule.freederefunitimportsyms;
      var
        i : longint;
        deref : pderef;
      begin
        for i:=0 to unitimportsymsderefs.count-1 do
          begin
            deref:=pderef(unitimportsymsderefs[i]);
            system.dispose(deref);
          end;
      end;

{**********************************
    PPU Reading/Writing Helpers
***********************************}

{$IFDEF MACRO_DIFF_HINT}
    var
      is_initial: Boolean;

    procedure tppumodule.writeusedmacro(p:TNamedIndexItem;arg:pointer);
      begin
        if tmacro(p).is_used or is_initial then
          begin
            ppufile.putstring(p.name);
            ppufile.putboolean(is_initial);
            ppufile.putboolean(tmacro(p).is_used);
          end;
      end;

    procedure tppumodule.writeusedmacros;
      var
        oldcrc : boolean;
      begin
        oldcrc:=ppufile.do_crc;
        ppufile.do_crc:=false;
        is_initial:= true;
        initialmacrosymtable.foreach(@writeusedmacro,nil);
        is_initial:= false;
        if assigned(globalmacrosymtable) then
          globalmacrosymtable.foreach(@writeusedmacro,nil);
        localmacrosymtable.foreach(@writeusedmacro,nil);
        ppufile.writeentry(ibusedmacros);
        ppufile.do_crc:=oldcrc;
      end;
{$ENDIF}

    procedure tppumodule.writesourcefiles;
      var
        hp  : tinputfile;
        ifile : sizeint;
        oldcrc : boolean;
      begin
      { write the used source files }
        oldcrc:=ppufile.do_crc;
        ppufile.do_crc:=false;
      { write source files directly in good order }
        for ifile:=0 to sourcefiles.nfiles-1 do
          begin
            hp:=sourcefiles.files[ifile];
            ppufile.putstring(hp.inc_path+hp.name);
            ppufile.putlongint(hp.getfiletime);
         end;
        ppufile.writeentry(ibsourcefiles);
        ppufile.do_crc:=oldcrc;
      end;


    procedure tppumodule.writeusedunit(intf:boolean);
      var
        hp : tused_unit;
        oldcrc : boolean;
        u: tmodule;
      begin
        { write a reference for each used unit }
        {$IFDEF Debug_WaitCRC}
        writeln('tppumodule.writeusedunit START ',realmodulename^,' intf=',intf);
        {$ENDIF}
        {$IFDEF Debug_IndirectCRC}
        if intf then
          writeln('INDIRECT_CRC tppumodule.writeusedunit ',hexstr(ppufile.indirect_crc,8));
        {$ENDIF}
        hp:=tused_unit(used_units.first);
        while assigned(hp) do
         begin
           if hp.in_interface=intf then
             begin
               u:=hp.u;
               ppufile.putstring(u.realmodulename^);
               { the checksum should not affect the crc of this unit ! (PFV) }
               oldcrc:=ppufile.do_crc;
               ppufile.do_crc:=false;
               {$IFDEF Debug_WaitCRC}
               writeln('tppumodule.writeusedunit ',u.realmodulename^,' crc=',hexstr(u.crc,8),' interface_crc=',hexstr(u.interface_crc,8),' indirect_crc=',hexstr(u.indirect_crc,8));
               {$ENDIF}
               hp.checksum:=u.crc;
               hp.interface_checksum:=u.interface_crc;
               hp.indirect_checksum:=u.indirect_crc;
               ppufile.putlongint(longint(hp.checksum));
               ppufile.putlongint(longint(hp.interface_checksum));
               ppufile.putlongint(longint(hp.indirect_checksum));
               ppufile.do_crc:=oldcrc;
               { Combine all indirect checksums from units used by this unit.
                 The indirect_crc contains the classes+records of this unit as well. }
               if intf then
                 ppufile.indirect_crc:=UpdateCrc32(ppufile.indirect_crc,u.indirect_crc,sizeof(u.indirect_crc));
               {$IFDEF Debug_IndirectCRC}
               if intf then
                 writeln('INDIRECT_CRC tppumodule.writeusedunit ',hexstr(ppufile.indirect_crc,8),' ',u.modulename^,' ',hexstr(u.indirect_crc,8),' ');
               {$ENDIF}
             end;
           hp:=tused_unit(hp.next);
         end;
        ppufile.writeentry(ibloadunit);
      end;


    procedure tppumodule.writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
      var
        hcontainer : tlinkcontainer;
        s : TPathStr;
        mask : cardinal;
      begin
        hcontainer:=TLinkContainer.Create;
        while not p.empty do
         begin
           s:=p.get(mask);
           if strippath then
            ppufile.putstring(ExtractFileName(s))
           else
            ppufile.putstring(s);
           ppufile.putlongint(mask);
           hcontainer.add(s,mask);
         end;
        ppufile.writeentry(id);
        p.Free;
        p:=hcontainer;
      end;


    procedure tppumodule.writederefmap;
      var
        i : longint;
        oldcrc : boolean;
      begin
        { This does not influence crc }
        oldcrc:=ppufile.do_crc;
        ppufile.do_crc:=false;
        { The unit map used for resolving }
        ppufile.putlongint(derefmapcnt);
        for i:=0 to derefmapcnt-1 do
          begin
            if not assigned(derefmap[i].u) then
              internalerror(2005011512);
            ppufile.putstring(derefmap[i].u.modulename^)
          end;
        ppufile.writeentry(ibderefmap);
        ppufile.do_crc:=oldcrc;
      end;


    procedure tppumodule.writederefdata;
      var
        oldcrc : boolean;
        len,hlen : longint;
        buf : array[0..1023] of byte;
      begin
        if longword(derefdataintflen)>derefdata.size then
          internalerror(200310223);
        derefdata.seek(0);
        { Write interface data }
        len:=derefdataintflen;
        while (len>0) do
          begin
            if len>1024 then
              hlen:=1024
            else
              hlen:=len;
            derefdata.read(buf,hlen);
            ppufile.putdata(buf,hlen);
            dec(len,hlen);
          end;
        { Write implementation data, this does not influence crc }
        oldcrc:=ppufile.do_crc;
        ppufile.do_crc:=false;
        len:=derefdata.size-derefdataintflen;
        while (len>0) do
          begin
            if len>1024 then
              hlen:=1024
            else
              hlen:=len;
            derefdata.read(buf,hlen);
            ppufile.putdata(buf,hlen);
            dec(len,hlen);
          end;
        if derefdata.pos<>derefdata.size then
          internalerror(200310224);
        ppufile.do_crc:=oldcrc;
        ppufile.writeentry(ibderefdata);
      end;


    procedure tppumodule.writeImportSymbols;
      var
        i,j : longint;
        ImportLibrary : TImportLibrary;
        ImportSymbol  : TImportSymbol;
      begin
        for i:=0 to ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(ImportLibraryList[i]);
            ppufile.putstring(ImportLibrary.Name);
            ppufile.putlongint(ImportLibrary.ImportSymbolList.Count);
            for j:=0 to ImportLibrary.ImportSymbolList.Count-1 do
              begin
                ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList[j]);
                ppufile.putstring(ImportSymbol.Name);
                ppufile.putstring(ImportSymbol.MangledName);
                ppufile.putlongint(ImportSymbol.OrdNr);
                ppufile.putbyte(byte(ImportSymbol.IsVar));
              end;
          end;
        ppufile.writeentry(ibImportSymbols);
      end;


    procedure tppumodule.writeResources;
      var
        res : TCmdStrListItem;
      begin
        res:=TCmdStrListItem(ResourceFiles.First);
        while res<>nil do
          begin
            ppufile.putstring(res.FPStr);
            res:=TCmdStrListItem(res.Next);
          end;
        ppufile.writeentry(ibresources);
      end;


    procedure tppumodule.writeOrderedSymbols;
      var
        res : TCmdStrListItem;
      begin
        res:=TCmdStrListItem(linkorderedsymbols.First);
        while res<>nil do
          begin
            ppufile.putstring(res.FPStr);
            res:=TCmdStrListItem(res.Next);
          end;
        ppufile.writeentry(iborderedsymbols);
      end;


    procedure tppumodule.writeunitimportsyms;
      var
        i : longint;
      begin
        ppufile.putlongint(unitimportsymsderefs.count);
        for i:=0 to unitimportsymsderefs.count-1 do
          ppufile.putderef(pderef(unitimportsymsderefs[i])^);
        ppufile.writeentry(ibunitimportsyms);
      end;


    procedure tppumodule.writeasmsyms(kind:tunitasmlisttype;list:tfphashobjectlist);
      var
        i : longint;
        sym : TAsmSymbol;
      begin
        ppufile.putbyte(ord(kind));
        ppufile.putlongint(list.count);
        for i:=0 to list.count-1 do
          begin
            sym:=TAsmSymbol(list[i]);
            ppufile.putstring(sym.Name);
            ppufile.putbyte(ord(sym.bind));
            ppufile.putbyte(ord(sym.typ));
          end;
        ppufile.writeentry(ibasmsymbols);
      end;

    procedure tppumodule.writeextraheader;
      var
        old_docrc: boolean;
      begin
        { create unit flags }
        if compiler.globals.do_release then
          include(moduleflags,mf_release);
        if assigned(localsymtable) then
          include(moduleflags,mf_local_symtable);
        if cs_checkpointer_called in compiler.globals.current_settings.moduleswitches then
          include(moduleflags,mf_checkpointer_called);
        if cs_compilesystem in compiler.globals.current_settings.moduleswitches then
          include(moduleflags,mf_system_unit);
{$ifdef i8086}
        if compiler.globals.current_settings.x86memorymodel in [mm_medium,mm_large,mm_huge] then
          include(moduleflags,mf_i8086_far_code);
        if compiler.globals.current_settings.x86memorymodel in [mm_compact,mm_large] then
          include(moduleflags,mf_i8086_far_data);
        if compiler.globals.current_settings.x86memorymodel=mm_huge then
          include(moduleflags,mf_i8086_huge_data);
        if compiler.globals.current_settings.x86memorymodel=mm_tiny then
          include(moduleflags,mf_i8086_cs_equals_ds);
        if compiler.globals.current_settings.x86memorymodel in [mm_tiny,mm_small,mm_medium] then
          include(moduleflags,mf_i8086_ss_equals_ds);
{$endif i8086}
{$ifdef wasm}
        if ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches then
          include(moduleflags,mf_wasm_no_exceptions);
        if ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches then
          include(moduleflags,mf_wasm_exnref_exceptions);
        if ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches then
          include(moduleflags,mf_wasm_native_exceptions);
        if ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches then
          include(moduleflags,mf_wasm_bf_exceptions);
        if ts_wasm_threads in compiler.globals.current_settings.targetswitches then
          include(moduleflags,mf_wasm_threads);
{$endif wasm}
{$ifdef llvm}
        include(moduleflags,mf_llvm);
{$endif}
{$ifdef symansistr}
        include(moduleflags,mf_symansistr);
{$endif}

        old_docrc:=ppufile.do_crc;
        ppufile.do_crc:=false;
        ppufile.putlongint(longint(CurrentPPULongVersion));
        ppufile.putset(tppuset4(moduleflags));
        ppufile.writeentry(ibextraheader);
        ppufile.do_crc:=old_docrc;
      end;


{$IFDEF MACRO_DIFF_HINT}

{
  Define MACRO_DIFF_HINT for the whole compiler (and ppudump)
  to turn this facility on. Also the hint messages defined
  below must be commented in in the msg/errore.msg file.

  There is some problems with this, thats why it is shut off:

  At the first compilation, consider a macro which is not initially
  defined, but it is used (e g the check that it is undefined is true).
  Since it do not exist, there is no macro object where the is_used
  flag can be set. Later on when the macro is defined, and the ppu
  is opened, the check cannot detect this.

  Also, in which macro object should this flag be set ? It cant be set
  for macros in the initialmacrosymboltable since this table is shared
  between different files.
}

    procedure tppumodule.readusedmacros;
      var
        hs : string;
        mac : tmacro;
        was_initial,
        was_used : boolean;
      {Reads macros which was defined or used when the module was compiled.
       This is done when a ppu file is open, before it possibly is parsed.}
      begin
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           was_initial:=ppufile.getboolean;
           was_used:=ppufile.getboolean;
           mac:=tmacro(initialmacrosymtable.Find(hs));
           if assigned(mac) then
             begin
{$ifndef EXTDEBUG}
           { if we don't have the sources why tell }
              if sources_avail then
{$endif ndef EXTDEBUG}
               if (not was_initial) and was_used then
                compiler.verbose.Message2(unit_h_cond_not_set_in_last_compile,hs,mainsource^);
             end
           else { not assigned }
             if was_initial and
                was_used then
              compiler.verbose.Message2(unit_h_cond_set_in_last_compile,hs,mainsource^);
         end;
      end;
{$ENDIF}

    procedure tppumodule.readsourcefiles;
      var
        temp,hs       : string;
        inc_path      : string;
        temp_dir      : TCmdStr;
        main_dir      : TCmdStr;
        found,
        is_main       : boolean;
        orgfiletime,
        source_time   : longint;
        hp            : tinputfile;
      begin
        sources_avail:=not(mf_release in moduleflags);
        is_main:=true;
        main_dir:='';
        while not ppufile.endofentry do
         begin
           hs:=SetDirSeparators(ppufile.getstring);
           inc_path:=ExtractFilePath(hs);
           orgfiletime:=ppufile.getlongint;
           temp_dir:='';
           if sources_avail then
             begin
               if (headerflags and uf_in_library)<>0 then
                begin
                  sources_avail:=false;
                  temp:=' library';
                end
               else if pos('Macro ',hs)=1 then
                begin
                  { we don't want to find this file }
                  { but there is a problem with file indexing !! }
                  temp:='';
                end
               else
                begin
                  { check the date of the source files:
                     1 path of sourcefn
                     2 path of ppu
                     3 path of main source
                     4 current dir
                     5 include/unit path }
                  found:=false;
                  if sourcefn<>'' then
                  begin
                    temp_dir:=ExtractFilePath(SetDirSeparators(sourcefn));
                    Source_Time:=GetNamedFileTime(temp_dir+hs);
                    if Source_Time<>-1 then
                      hs:=temp_dir+hs;
                  end else
                    Source_Time:=-1;
                  if Source_Time=-1 then
                    begin
                      Source_Time:=GetNamedFileTime(path+hs);
                      if Source_Time<>-1 then
                        hs:=path+hs
                      else
                       if not(is_main) then
                        begin
                          Source_Time:=GetNamedFileTime(main_dir+hs);
                          if Source_Time<>-1 then
                            hs:=main_dir+hs;
                        end;
                    end;
                  if Source_Time=-1 then
                    Source_Time:=GetNamedFileTime(hs);
                  if (Source_Time=-1) then
                    begin
                      if is_main then
                        found:=compiler.globals.unitsearchpath.FindFile(hs,true,temp_dir)
                      else
                        found:=compiler.globals.includesearchpath.FindFile(hs,true,temp_dir);
                      if found then
                       begin
                         Source_Time:=GetNamedFileTime(temp_dir);
                         if Source_Time<>-1 then
                          hs:=temp_dir;
                       end;
                    end;
                  if Source_Time<>-1 then
                    begin
                      if is_main then
                        main_dir:=ExtractFilePath(hs);
                      temp:=' time '+compiler.time.filetimestring(source_time);
                      if (orgfiletime<>-1) and
                         (source_time<>orgfiletime) then
                        begin
                          {$IFDEF DEBUG_PPU_CYCLES}
                          writeln('PPUALGO tppumodule.readsourcefiles sourcechanged ',modulename^,' old=',state,' new=',ms_compile);
                          {$ENDIF}
                          state:=ms_compile;
                          recompile_reason:=rr_sourcenewer;
                          compiler.verbose.Message2(unit_u_source_modified,hs,ppufilename,@queuecomment);
                          temp:=temp+' *';
                        end;
                    end
                  else
                    begin
                      sources_avail:=false;
                      temp:=' not found';
                    end;
                  hp:=tdosinputfile.create(hs);
                  hp.inc_path:=inc_path;
                  { the indexing is wrong here PM }
                  sourcefiles.register_file(hp);
                end;
             end
           else
             begin
               { still register the source module for proper error messages
                 since source_avail for the module is still false, this should not hurt }
               sourcefiles.register_file(tdosinputfile.create(hs));

               temp:=' not available';
             end;
           if is_main then
             begin
               mainsource:=hs;
             end;
           compiler.verbose.Message1(unit_u_ppu_source,hs+temp,@queuecomment);
           is_main:=false;
         end;
      { check if we want to rebuild every unit, only if the sources are
        available }
        if compiler.globals.do_build and sources_avail then
          begin
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tppumodule.readsourcefiles do_build ',modulename^,' old=',state,' new=',ms_compile);
            {$ENDIF}
            state:=ms_compile;
            recompile_reason:=rr_build;
          end;
      end;


    procedure tppumodule.readloadunit;
      var
        hs : string;
        pu : tused_unit;
        hp : tppumodule;
        indchecksum,
        intfchecksum,
        checksum : cardinal;
        isnew : boolean;

      begin
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           checksum:=cardinal(ppufile.getlongint);
           intfchecksum:=cardinal(ppufile.getlongint);
           indchecksum:=cardinal(ppufile.getlongint);
           { set the state of this unit before registering, this is
             needed for a correct circular dependency check }
           hp:=registerunit(compiler,self,hs,'',isnew);
           if isnew then
             usedunits.Concat(tused_unit.create(hp,in_interface,true,nil));
           pu:=findusedunit(hp);
           if pu=nil then
             pu:=addusedunit(hp,false,nil);
           pu.checksum:=checksum;
           pu.interface_checksum:=intfchecksum;
           pu.indirect_checksum:=indchecksum;
         end;
        in_interface:=false;
      end;


    procedure tppumodule.readlinkcontainer(var p:tlinkcontainer);
      var
        s : string;
        m : longint;
      begin
        while not ppufile.endofentry do
         begin
           s:=ppufile.getstring;
           m:=ppufile.getlongint;
           p.add(s,m);
         end;
      end;


    procedure tppumodule.readderefmap;
      var
        i : longint;
      begin
        { Load unit map used for resolving }
        derefmapsize:=ppufile.getlongint;
        derefmapcnt:=derefmapsize;
        setlength(derefmap,derefmapsize);
        for i:=0 to derefmapsize-1 do
          derefmap[i].modulename:=ppufile.getpshortstring;
      end;


    procedure tppumodule.readderefdata;
      var
        len,hlen : longint;
        buf : array[0..1023] of byte;
      begin
        len:=ppufile.entrysize;
        while (len>0) do
          begin
            if len>1024 then
              hlen:=1024
            else
              hlen:=len;
            ppufile.getdata(buf,hlen);
            derefdata.write(buf,hlen);
            dec(len,hlen);
          end;
      end;


    procedure tppumodule.readImportSymbols;
      var
        j,
        extsymcnt   : longint;
        ImportLibrary  : TImportLibrary;
        extsymname  : string;
        extsymmangledname : string;
        extsymordnr : longint;
        extsymisvar : boolean;
      begin
        while not ppufile.endofentry do
          begin
            ImportLibrary:=TImportLibrary.Create(ImportLibraryList,ppufile.getstring);
            extsymcnt:=ppufile.getlongint;
            for j:=0 to extsymcnt-1 do
              begin
                extsymname:=ppufile.getstring;
                extsymmangledname:=ppufile.getstring;
                extsymordnr:=ppufile.getlongint;
                extsymisvar:=(ppufile.getbyte<>0);
                TImportSymbol.Create(ImportLibrary.ImportSymbolList,extsymname,
                  extsymmangledname,extsymordnr,extsymisvar);
              end;
          end;
      end;


    procedure tppumodule.readResources;
      begin
        while not ppufile.endofentry do
          resourcefiles.Insert(ppufile.getstring);
      end;


    procedure tppumodule.readOrderedSymbols;
      begin
        while not ppufile.endofentry do
          linkorderedsymbols.Concat(ppufile.getstring);
      end;


    procedure tppumodule.readwpofile;
      var
        orgwpofilename: string;
        orgwpofiletime: longint;
      begin
        { check whether we are using the same wpo feedback input file as when
          this unit was compiled (same file name and file date)
        }
        orgwpofilename:=ppufile.getstring;
        orgwpofiletime:=ppufile.getlongint;
        if (extractfilename(orgwpofilename)<>extractfilename(compiler.globals.wpofeedbackinput)) or
           (orgwpofiletime<>GetNamedFileTime(orgwpofilename)) then
          { make sure we don't throw away a precompiled unit if the user simply
            forgot to specify the right wpo feedback file
          }
          compiler.verbose.Message3(unit_e_different_wpo_file,ppufilename,orgwpofilename,compiler.time.filetimestring(orgwpofiletime));
      end;


    procedure tppumodule.readunitimportsyms;
      var
        c,i : longint;
        deref : pderef;
      begin
        c:=ppufile.getlongint;
        for i:=0 to c-1 do
          begin
            new(deref);
            ppufile.getderef(deref^);
            unitimportsymsderefs.add(deref);
          end;
      end;


    procedure tppumodule.readasmsyms;
      var
        c,i : longint;
        name : TSymStr;
        bind : TAsmsymbind;
        typ : TAsmsymtype;
        list : tfphashobjectlist;
      begin
        case tunitasmlisttype(ppufile.getbyte) of
          ualt_public:
            list:=publicasmsyms;
          ualt_extern:
            list:=externasmsyms;
        end;
        c:=ppufile.getlongint;
        for i:=0 to c-1 do
          begin
            name:=ppufile.getstring;
            bind:=TAsmsymbind(ppufile.getbyte);
            typ:=TAsmsymtype(ppufile.getbyte);
            TAsmSymbol.Create(list,name,bind,typ);
          end;
      end;


    procedure tppumodule.readextraheader;
      begin
        longversion:=cardinal(ppufile.getlongint);
        ppufile.getset(tppuset4(moduleflags));
      end;


    procedure tppumodule.load_interface;
      var
        b : byte;
        newmodulename : string;
      begin
       { read interface part }
         repeat
           b:=ppufile.readentry;
           case b of
             ibjvmnamespace :
               begin
                 namespace:=ppufile.getpshortstring;
               end;
             ibmodulename :
               begin
                 newmodulename:=ppufile.getstring;
                 if (cs_check_unit_name in compiler.globals.current_settings.globalswitches) and
                    (upper(newmodulename)<>modulename^) then
                   compiler.verbose.Message2(unit_f_unit_name_error,realmodulename^,newmodulename);
                 stringdispose(modulename);
                 stringdispose(realmodulename);
                 modulename:=stringdup(upper(newmodulename));
                 realmodulename:=stringdup(newmodulename);
               end;
             ibextraheader:
               begin
                 readextraheader;
               end;
             ibfeatures :
               begin
                 ppufile.getset(tppuset4(compiler.globals.features));
               end;
             ibmoduleoptions:
               begin
                 ppufile.getset(tppuset1(moduleoptions));
                 if mo_has_deprecated_msg in moduleoptions then
                   begin
                     stringdispose(deprecatedmsg);
                     deprecatedmsg:=ppufile.getpshortstring;
                   end;
               end;
             ibsourcefiles :
               readsourcefiles;
{$IFDEF MACRO_DIFF_HINT}
             ibusedmacros :
               readusedmacros;
{$ENDIF}
             ibloadunit :
               readloadunit;
             iblinkunitofiles :
               readlinkcontainer(LinkUnitOFiles);
             iblinkunitstaticlibs :
               readlinkcontainer(LinkUnitStaticLibs);
             iblinkunitsharedlibs :
               readlinkcontainer(LinkUnitSharedLibs);
             iblinkotherofiles :
               readlinkcontainer(LinkotherOFiles);
             iblinkotherstaticlibs :
               readlinkcontainer(LinkotherStaticLibs);
             iblinkothersharedlibs :
               readlinkcontainer(LinkotherSharedLibs);
             iblinkotherframeworks :
               readlinkcontainer(LinkOtherFrameworks);
             ibmainname:
               begin
                 mainname:=ppufile.getpshortstring;
                 if (compiler.globals.mainaliasname<>defaultmainaliasname) then
                   compiler.verbose.Message1(scan_w_multiple_main_name_overrides,compiler.globals.mainaliasname);
                 compiler.globals.mainaliasname:=mainname^;
               end;
             ibImportSymbols :
               readImportSymbols;
             ibderefmap :
               readderefmap;
             ibderefdata :
               readderefdata;
             ibresources:
               readResources;
             iborderedsymbols:
               readOrderedSymbols;
             ibwpofile:
               readwpofile;
             ibendinterface :
               break;
           else
             compiler.verbose.Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           { we can already stop when we know that we must recompile }
           if state=ms_compile then
             exit;
         until false;
      end;


    procedure tppumodule.load_implementation;
      var
        b : byte;
      begin
         { read implementation part }
         repeat
           b:=ppufile.readentry;
           case b of
             ibloadunit :
               readloadunit;
             ibasmsymbols :
               readasmsyms;
             ibunitimportsyms:
               readunitimportsyms;
             ibendimplementation :
               break;
           else
             compiler.verbose.Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;


    procedure tppumodule.writeppu;
      begin
         compiler.verbose.Message1(unit_u_ppu_write,realmodulename^);

         { create unit flags }
{$ifdef cpufpemu}
         if (cs_fp_emulation in compiler.globals.current_settings.moduleswitches) then
           headerflags:=headerflags or uf_fpu_emulation;
{$endif cpufpemu}
         { create new ppufile }
         ppufile:=tcompilerppufile.create(ppufilename,compiler);
         if not ppufile.createfile then
          compiler.verbose.Message(unit_f_ppu_cannot_write);

{$ifdef Test_Double_checksum_write}
         { Re-use the values collected in .INT part }
         if assigned(interface_crc_array) then
           begin
             ppufile.implementation_write_crc_index:=implementation_write_crc_index;
             ppufile.interface_write_crc_index:=interface_write_crc_index;
             ppufile.indirect_write_crc_index:=indirect_write_crc_index;
             if assigned(ppufile.interface_crc_array) then
               begin
                 dispose(ppufile.interface_crc_array);
                 ppufile.interface_crc_array:=interface_crc_array;
               end;
             if assigned(ppufile.implementation_crc_array) then
               begin
                 dispose(ppufile.implementation_crc_array);
                 ppufile.implementation_crc_array:=implementation_crc_array;
               end;
             if assigned(ppufile.indirect_crc_array) then
               begin
                 dispose(ppufile.indirect_crc_array);
                 ppufile.indirect_crc_array:=indirect_crc_array;
               end;
           end;
         if FileExists(ppufilename+'.IMP',false) then
           RenameFile(ppufilename+'.IMP',ppufilename+'.IMP-old');
         Assign(ppufile.CRCFile,ppufilename+'.IMP');
         Rewrite(ppufile.CRCFile);
         Writeln(ppufile.CRCFile,'CRC in writeppu method of implementation of ',ppufilename,' defsgeneration=',defsgeneration);
{$endif def Test_Double_checksum_write}

         { extra header (sub version, module flags) }
         writeextraheader;

         { first the (JVM) namespace }
         if assigned(namespace) then
           begin
             ppufile.putstring(namespace^);
             ppufile.writeentry(ibjvmnamespace);
           end;
         { the unitname }
         ppufile.putstring(realmodulename^);
         ppufile.writeentry(ibmodulename);

         ppufile.putset(tppuset1(moduleoptions));
         if mo_has_deprecated_msg in moduleoptions then
           ppufile.putstring(deprecatedmsg^);
         ppufile.writeentry(ibmoduleoptions);

         { write the alternate main procedure name if any }
         if assigned(mainname) then
           begin
             ppufile.putstring(mainname^);
             ppufile.writeentry(ibmainname);
           end;

         if cs_compilesystem in compiler.globals.current_settings.moduleswitches then
           begin
             ppufile.putset(tppuset4(compiler.globals.features));
             ppufile.writeentry(ibfeatures);
           end;

         writesourcefiles;
{$IFDEF MACRO_DIFF_HINT}
         writeusedmacros;
{$ENDIF}

         { write interface uses }
         writeusedunit(true);

         { write the objectfiles and libraries that come for this unit,
           preserve the containers because they are still needed to load
           the link.res.
            All doesn't depend on the crc! It doesn't matter
           if a unit is in a .o or .a file }
         ppufile.do_crc:=false;
         { write after source files, so that we know whether or not the compiler
           will recompile the unit when checking whether the correct wpo file is
           used (if it will recompile the unit anyway, it doesn't matter)
         }
         if (compiler.globals.wpofeedbackinput<>'') then
           begin
             ppufile.putstring(compiler.globals.wpofeedbackinput);
             ppufile.putlongint(getnamedfiletime(compiler.globals.wpofeedbackinput));
             ppufile.writeentry(ibwpofile);
           end;
         writelinkcontainer(linkunitofiles,iblinkunitofiles,true);
         writelinkcontainer(linkunitstaticlibs,iblinkunitstaticlibs,true);
         writelinkcontainer(linkunitsharedlibs,iblinkunitsharedlibs,true);
         writelinkcontainer(linkotherofiles,iblinkotherofiles,false);
         writelinkcontainer(linkotherstaticlibs,iblinkotherstaticlibs,true);
         writelinkcontainer(linkothersharedlibs,iblinkothersharedlibs,true);
         writelinkcontainer(linkotherframeworks,iblinkotherframeworks,true);
         writeImportSymbols;
         writeResources;
         writeOrderedSymbols;
         ppufile.do_crc:=true;

         { generate implementation deref data, the interface deref data is
           already generated when calculating the interface crc }
         if (cs_compilesystem in compiler.globals.current_settings.moduleswitches) then
           begin
             tstoredsymtable(globalsymtable).buildderef;
             derefdataintflen:=derefdata.size;
           end
         else
           { the unit may have been re-resolved, in which case the current
             position in derefdata is not necessarily at the end }
            derefdata.seek(derefdata.size);
         tstoredsymtable(globalsymtable).buildderefimpl;
         tunitwpoinfo(wpoinfo).buildderef;
         tunitwpoinfo(wpoinfo).buildderefimpl;

         if assigned(globalmacrosymtable) and (globalmacrosymtable.SymList.count > 0) then
            begin
              tstoredsymtable(globalmacrosymtable).buildderef;
              tstoredsymtable(globalmacrosymtable).buildderefimpl;
            end;

         if mf_local_symtable in moduleflags then
           tstoredsymtable(localsymtable).buildderef_registered;
         buildderefunitimportsyms;
         writederefmap;
         writederefdata;

         ppufile.writeentry(ibendinterface);

         { write the symtable entries }
         tstoredsymtable(globalsymtable).ppuwrite(ppufile);

         if assigned(globalmacrosymtable) and (globalmacrosymtable.SymList.count > 0) then
           begin
             ppufile.putbyte(byte(true));
             ppufile.writeentry(ibexportedmacros);
             tstoredsymtable(globalmacrosymtable).ppuwrite(ppufile);
           end
         else
           begin
             ppufile.putbyte(byte(false));
             ppufile.writeentry(ibexportedmacros);
           end;

         { everything after this doesn't affect the crc }
         ppufile.do_crc:=false;

         { write implementation uses }
         writeusedunit(false);

         { write all public assembler symbols }
         writeasmsyms(ualt_public,publicasmsyms);

         { write all external assembler symbols }
         writeasmsyms(ualt_extern,externasmsyms);

         { write all symbols imported from another unit }
         writeunitimportsyms;

         { end of implementation }
         ppufile.writeentry(ibendimplementation);

         { write static symtable
           needed for local debugging of unit functions }
         if mf_local_symtable in moduleflags then
           tstoredsymtable(localsymtable).ppuwrite(ppufile);

         { write whole program optimisation-related information }
         tunitwpoinfo(wpoinfo).ppuwrite(ppufile);

         { the last entry ibend is written automatically }

         { flush to be sure }
         ppufile.flush;

         { save crc in current module also }
         if not crc_final then
           begin
             crc_final:=true;
             crc:=ppufile.crc;
           end;
         {$IFDEF Debug_WaitCRC}
         writeln('tppumodule.writeppu ',modulename^,' crc=',hexstr(crc,8));
         {$ENDIF}

         { create and write header }
         { Note: the interface_crc and indirect_crc were computed in getppucrc
                 after the interface was compiled. The implementation must *not* effect them. }
         ppufile.header.common.size:=ppufile.size;
         ppufile.header.checksum:=crc;
         ppufile.header.interface_checksum:=interface_crc;
         ppufile.header.indirect_checksum:=indirect_crc;
         ppufile.header.common.compiler:=wordversion;
         ppufile.header.common.cpu:=word(compiler.target.cpu);
         ppufile.header.common.target:=word(compiler.target.info.system);
         ppufile.header.common.flags:=headerflags;
         ppufile.header.deflistsize:=current_module.deflist.count;
         ppufile.header.symlistsize:=current_module.symlist.count;
         ppufile.writeheader;

{$ifdef Test_Double_checksum_write}
         Writeln(ppufile.CRCFile,'End of implementation CRC in writeppu method of ',ppufilename,
                 ' implementation_crc=$',hexstr(ppufile.crc,8),
                 ' interface_crc=$',hexstr(ppufile.interface_crc,8),
                 ' indirect_crc=$',hexstr(ppufile.indirect_crc,8),
                 ' implementation_crc_size=',ppufile.implementation_read_crc_index,
                 ' interface_crc_size=',ppufile.interface_read_crc_index,
                 ' indirect_crc_size=',ppufile.indirect_read_crc_index,
                 ' defsgeneration=',defsgeneration);
         close(ppufile.CRCFile);
{$endif Test_Double_checksum_write}

         discardppu;
      end;


    procedure tppumodule.getppucrc;
      begin
         { create new ppufile }
         ppufile:=tcompilerppufile.create(ppufilename,compiler);
         ppufile.crc_only:=true;
         if not ppufile.createfile then
           compiler.verbose.Message(unit_f_ppu_cannot_write);

{$ifdef DEBUG_GENERATE_INTERFACE_PPU}
        if ppufile.writing_interface_ppu then
          ppufile.crc_only:=false;
{$endif DEBUG_GENERATE_INTERFACE_PPU}
{$ifdef Test_Double_checksum_write}
         if FileExists(ppufilename+'.INT',false) then
           RenameFile(ppufilename+'.INT',ppufilename+'.INT-old');
         Assign(ppufile.CRCFile,ppufilename+'.INT');
         Rewrite(ppufile.CRCFile);
         Writeln(ppufile.CRCFile,'CRC of getppucrc of ',ppufilename,
                 ' defsgeneration=',defsgeneration);
{$endif def Test_Double_checksum_write}
         { extra header (sub version, module flags) }
         writeextraheader;

         { first the (JVM) namespace }
         if assigned(namespace) then
           begin
             ppufile.putstring(namespace^);
             ppufile.writeentry(ibjvmnamespace);
           end;

         { the unitname }
         ppufile.putstring(realmodulename^);
         ppufile.writeentry(ibmodulename);

         ppufile.putset(tppuset1(moduleoptions));
         if mo_has_deprecated_msg in moduleoptions then
           ppufile.putstring(deprecatedmsg^);
         ppufile.writeentry(ibmoduleoptions);

         { the interface units affect the crc }
         writeusedunit(true);

         { deref data of interface that affect the crc }
         derefdata.reset;
         tstoredsymtable(globalsymtable).buildderef;
         derefdataintflen:=derefdata.size;
         writederefmap;
         writederefdata;

         ppufile.writeentry(ibendinterface);

         { write the symtable entries }
         tstoredsymtable(globalsymtable).ppuwrite(ppufile);

         if assigned(globalmacrosymtable) and (globalmacrosymtable.SymList.count > 0) then
           begin
             ppufile.putbyte(byte(true));
             ppufile.writeentry(ibexportedmacros);
             tstoredsymtable(globalmacrosymtable).ppuwrite(ppufile);
           end
         else
           begin
             ppufile.putbyte(byte(false));
             ppufile.writeentry(ibexportedmacros);
           end;

         { save crc  }
         crc:=ppufile.crc;
         if in_interface then
           begin
             { Note: the interface_crc and indirect_crc are not affected by the implementation }
             interface_crc:=ppufile.interface_crc;
             indirect_crc:=ppufile.indirect_crc;
           end;
         {$IFDEF Debug_WaitCRC}
         writeln('tppumodule.getppucrc ',realmodulename^,' in_interface=',in_interface,' crc=',hexstr(crc,8),' interface_crc=',hexstr(interface_crc,8),' indirect_crc=',hexstr(indirect_crc,8));
         {$ENDIF}

         { end of implementation, to generate a correct ppufile
           for ppudump when using DEBUG_GENERATE_INTERFACE_PPU define }
         ppufile.writeentry(ibendimplementation);

{$ifdef Test_Double_checksum_write}
         Writeln(ppufile.CRCFile,'End of CRC of getppucrc of ',ppufilename,
                 ' implementation_crc=$',hexstr(ppufile.crc,8),
                 ' interface_crc=$',hexstr(ppufile.interface_crc,8),
                 ' indirect_crc=$',hexstr(ppufile.indirect_crc,8),
                 ' implementation_crc_size=',ppufile.implementation_write_crc_index,
                 ' interface_crc_size=',ppufile.interface_write_crc_index,
                 ' indirect_crc_size=',ppufile.indirect_write_crc_index,
                 ' defsgeneration=',defsgeneration);
         close(ppufile.CRCFile);
         { Remember the values generated in .INT part }
          implementation_write_crc_index:=ppufile.implementation_write_crc_index;
          interface_write_crc_index:=ppufile.interface_write_crc_index;
          indirect_write_crc_index:=ppufile.indirect_write_crc_index;
          interface_crc_array:=ppufile.interface_crc_array;
          ppufile.interface_crc_array:=nil;
          implementation_crc_array:=ppufile.implementation_crc_array;
          ppufile.implementation_crc_array:=nil;
          indirect_crc_array:=ppufile.indirect_crc_array;
          ppufile.indirect_crc_array:=nil;
{$endif Test_Double_checksum_write}

         { create and write header, this will only be used
           for debugging purposes }
         ppufile.header.common.size:=ppufile.size;
         ppufile.header.checksum:=crc;
         ppufile.header.interface_checksum:=interface_crc;
         ppufile.header.indirect_checksum:=indirect_crc;
         ppufile.header.common.compiler:=wordversion;
         ppufile.header.common.cpu:=word(compiler.target.cpu);
         ppufile.header.common.target:=word(compiler.target.info.system);
         ppufile.header.common.flags:=headerflags;
         ppufile.writeheader;

         discardppu;
      end;

      function tppumodule.load_usedunits: boolean;
      { self is a ppu (or in a package) }
      begin
        Result:=true;
        if current_module<>self then
          internalerror(200212284);

        if not interface_compiled then
        begin
          { load the used units from interface }
          in_interface:=true;
          if not load_usedunits_section then
            exit(false); { e.g. fail or some used unit interface is not ready }
          if current_module<>self then
            internalerror(2026022317);
          { ok, now load the interface of this unit }
          if current_module<>self then
            internalerror(200208187);
          deflist.count:=ppufile.header.deflistsize;
          symlist.count:=ppufile.header.symlistsize;
          globalsymtable:=tglobalsymtable.create(realmodulename^,moduleid,compiler);
          tstoredsymtable(globalsymtable).ppuload(ppufile);

          if ppufile.readentry<>ibexportedmacros then
            compiler.verbose.Message(unit_f_ppu_read_error);
          if boolean(ppufile.getbyte) then
            begin
              globalmacrosymtable:=tmacrosymtable.Create(true,compiler);
              tstoredsymtable(globalmacrosymtable).ppuload(ppufile)
            end;

          interface_compiled:=true;

          { read the implementation part, containing
            the implementation uses and ObjData }
          in_interface:=false;
          load_implementation;
        end;

        { now only read the implementation uses }
        if not ppu_waitingfor_crc then
        begin
          if not load_usedunits_section then
            exit(false); { fail or some used unit interface is not ready }
          if current_module<>self then
            internalerror(2026022316);
        end;

        if not ppu_waitingfor_crc then
          begin
            ppu_waitingfor_crc:=true;

            { load implementation symtable }
            if mf_local_symtable in moduleflags then
              begin
                localsymtable:=tstaticsymtable.create(realmodulename^,moduleid,compiler);
                tstaticsymtable(localsymtable).ppuload(ppufile);
              end;

            { we can now dereference all pointers to the implementation parts }
            tstoredsymtable(globalsymtable).derefimpl(false);
            { we've just loaded the localsymtable from the ppu file, so everything
              in it was registered by definition (otherwise it wouldn't have been in
              there) }
            if assigned(localsymtable) then
              tstoredsymtable(localsymtable).derefimpl(false);

            remove_waitforunit_cycles;

          end;

        { the implementation uses were just connected,
          the scc_tree_crc_wait is outdated.
          If all used units are compiled, continue.
          otherwise some used units might still change }
        if find_used_unit_compiling<>nil then
          exit(false);

        { check that all used units have their crc and checksums match }
        if not ppu_check_used_crcs then
          exit(false);
        ppu_waitingfor_crc:=false;

        derefunitimportsyms;

        { read whole program optimisation-related information }
        wpoinfo:=tunitwpoinfo.ppuload(ppufile);
        tunitwpoinfo(wpoinfo).deref;
        tunitwpoinfo(wpoinfo).derefimpl;

        remove_all_waitsforthisunit;

        state:=ms_compiled;
      end;

    function tppumodule.load_usedunits_section: boolean;
      var
        pu: tused_unit;
        IntfCRCValid, CRCValid: Boolean;
      begin
        Result:=true;
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
        begin
          if pu.in_interface=in_interface then
          begin
            { adddependency before loadppu for invalid cycle test }
            if not pu.dependent_added then
            begin
              pu.u.adddependency(self,in_interface);
              pu.dependent_added:=true;
            end;

            tppumodule(pu.u).loadppu(self);
            { if this unit is scheduled for compilation or compiled we can stop }
            if state<>ms_load then
            begin
              {$IFDEF DEBUG_PPU_CYCLES}
              writeln('tppumodule.load_usedunits_section STOPPED ',modulename^,' ',statestr);
              {$ENDIF}
              exit(false);
            end;
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tppumodule.load_usedunits_section ',modulename^,' (',statestr,') ',BoolToStr(in_interface,'interface','implementation'),' uses "',pu.u.modulename^,'" state=',pu.u.statestr);
            {$ENDIF}

            { check crc(s) if recompile is needed.
              Currently ppus wait for a pas to be compiled, because a ppu cannot
              use only the interface of a pas.
              If an unit of a cycle is recompiled, the whole cycle is recompiled.

              If this ppu was compiled with -Ur only check interface_crc, not crc }
            CRCValid:=(not pu.u.do_reload) and pu.u.crc_final;
            IntfCRCValid:=(not pu.u.do_reload) and pu.u.interface_compiled;

            if (IntfCRCValid and
                     ((pu.u.interface_crc<>pu.interface_checksum) or
                      (pu.u.indirect_crc<>pu.indirect_checksum)))
                or (CRCValid and
                  {$IFNDEF EnableUrCRC}
                  (not (mf_release in moduleflags)) and
                  {$ENDIF}
                  (pu.u.crc<>pu.checksum)
                 ) then
            begin
              compiler.verbose.Message2(unit_u_recompile_crc_change,realmodulename^,pu.u.ppufilename,@queuecomment);
  {$ifdef DEBUG_UNIT_CRC_CHANGES}
              if (pu.u.interface_crc<>pu.interface_checksum) then
                compiler.verbose.Comment(V_Normal,'  intfcrc change: '+hexstr(pu.u.interface_crc,8)+' for '+pu.u.ppufilename+' <> '+hexstr(pu.interface_checksum,8)+' in unit '+realmodulename^)
              else if (pu.u.indirect_crc<>pu.indirect_checksum) then
                compiler.verbose.Comment(V_Normal,'  indcrc change: '+hexstr(pu.u.indirect_crc,8)+' for '+pu.u.ppufilename+' <> '+hexstr(pu.indirect_checksum,8)+' in unit '+realmodulename^)
              else
                compiler.verbose.Comment(V_Normal,'  implcrc change: '+hexstr(pu.u.crc,8)+' for '+pu.u.ppufilename+' <> '+hexstr(pu.checksum,8)+' in unit '+realmodulename^);
  {$endif DEBUG_UNIT_CRC_CHANGES}
              {$IFDEF DEBUG_PPU_CYCLES}
              writeln('PPUALGO tppumodule.load_usedunits_section ',modulename^,' ',BoolToStr(in_interface,'interface','implementation'),' uses "',pu.u.modulename^,'" old=',statestr,' new=',ms_compile);
              {$ENDIF}
              mark_recompile_needed(rr_crcchanged);
              exit(false);
            end;

            if pu.u.do_reload
                or (not pu.u.interface_compiled)
                or ctask_fast_backtrack then
            begin
              { this used unit is delayed
                Important: do not break, load the remaining uses section, so the scheduler
                           has more information about cycles }
              {$IFDEF DEBUG_PPU_CYCLES}
              writeln('PPUALGO tppumodule.load_usedunits_section ',modulename^,' ',BoolToStr(in_interface,'interface','implementation'),' uses "',pu.u.modulename^,'", state=',pu.u.statestr,', waiting ...');
              {$ENDIF}
              ctask_fast_backtrack:=true;
              Result:=false;
            end;
          end;
          pu:=tused_unit(pu.next);
        end;
      end;

    function tppumodule.ppu_check_used_crcs: boolean;
    // check crcs
    var
      pu: tused_unit;
    begin
      Result:=false;
      pu:=tused_unit(used_units.first);
      while assigned(pu) do
      begin
        if pu.u.crc_final then
        begin
          if (pu.u.interface_crc<>pu.interface_checksum)
              or (pu.u.indirect_crc<>pu.indirect_checksum)
              or (pu.u.crc<>pu.checksum) then
          begin
            {$ifdef DEBUG_UNIT_CRC_CHANGES}
            compiler.verbose.Comment(V_Normal,'  implcrc change: '+hexstr(pu.u.crc,8)+' for '+pu.u.ppufilename+' <> '+hexstr(pu.checksum,8)+' in unit '+realmodulename^);
            {$endif DEBUG_UNIT_CRC_CHANGES}
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tppumodule.ppu_check_used_crcs ',modulename^,' interface uses "',pu.u.modulename^,'" old=',statestr,' new=',ms_compile);
            {$ENDIF}
            mark_recompile_needed(rr_crcchanged);
            exit;
          end;
        end else begin
          { waiting for crc }
          exit;
        end;
        pu:=tused_unit(pu.next);
      end;

      Result:=true;
    end;

    function tppumodule.ppuloadcancontinue(out firstwaiting: tmodule): boolean;
    var
      pu: tused_unit;
    begin
      Result:=false;
      firstwaiting:=nil;
      if state<>ms_load then
        Internalerror(2026020610);

      if do_reload and not interface_compiled then
        exit(true);

      if ppu_waitingfor_crc and (scc_tree_crc_wait<>nil) and (scc_tree_crc_wait<>self) then
        exit; { the final load step needs all used units and their used units }

      pu:=tused_unit(used_units.first);
      while assigned(pu) do
      begin
        if pu.in_interface or interface_compiled then
        begin
          if pu.u.do_reload
              or not pu.u.interface_compiled
              or (ppu_waitingfor_crc and not pu.u.crc_final
                 {$IFNDEF EnableUrCRC}and not (mf_release in moduleflags){$ENDIF} ) then
          begin
            firstwaiting:=pu.u;
            exit;
          end;
        end;
        pu:=tused_unit(pu.next);
      end;
      Result:=true;
    end;

    function tppumodule.is_reload_needed(pu: tdependent_unit): boolean;
      begin
        if pu.u.state=ms_load then
          Result:=tppumodule(pu.u).ppu_waitingfor_crc
                or (pu.in_interface and pu.u.interface_compiled)
        else
          Result:=inherited is_reload_needed(pu);
      end;

    procedure tppumodule.restore_state;
      begin
        set_current_module(self);
        if stored_state<>nil then
          stored_state.restore;
      end;

    procedure tppumodule.store_state;
      begin
        if stored_state=nil then
          stored_state:=tglobalstate.Create(true,compiler)
        else
          stored_state.save(true);
      end;

    procedure tppumodule.setdefgeneration;
      begin
        defsgeneration:=currentdefgeneration;
        inc(currentdefgeneration);
      end;

    procedure tppumodule.end_of_parsing;
      begin
        { free ppu }
        discardppu;

        inherited end_of_parsing;
      end;

    { Returns true if the module was loaded from package }
    function tppumodule.check_loadfrompackage : boolean;

      begin
        { try to load it as a package unit first }
        Result:=(compiler.globals.packagelist.count>0) and loadfrompackage;
        if Result then
          begin
            do_reload:=false;
            state:=ms_compiled;
            { PPU is not needed anymore }
            if assigned(ppufile) then
             begin
               discardppu;
             end;
            { add the unit to the used units list of the program }
            usedunits.concat(tused_unit.create(self,true,false,nil));
          end;
      end;

    procedure tppumodule.recompile_from_sources;

      var
        was_interfaced_compiled: Boolean;
      begin
        set_current_module(self);
        check_sources_for_recompile;

        {$IFDEF DEBUG_PPU_CYCLES}
        writeln('PPUALGO tppumodule.recompile_from_sources ',modulename^,' old=',statestr,' new=',ms_compile);
        {$ENDIF}
        was_interfaced_compiled:=interface_compiled;
        { disconnect used modules }
        disconnect_depending_modules;
        if fromppu then
          ppu_discarded:=true;
        { Flag modules to reload }
        flagdependent;
        { Reset stack, parser, scanner, etc }
        if not fromppu then
          end_of_parsing;
        { Reset the module }
        reset(true);

        { mark this module for recompilation }
        state:=ms_compile;
        if was_interfaced_compiled then
          setdefgeneration;
      end;

    procedure tppumodule.check_sources_for_recompile;
      var
        pu: tused_unit;
      begin
        { recompile the unit or give a fatal error if sources not available }
        if not sources_avail then
         begin
           search_unit_files(loadedfrommodule,true);
           if not sources_avail then
            begin
              printcomments;
              if recompile_reason=rr_noppu then
                begin
                  pu:=tused_unit(loadedfrommodule.used_units.first);
                  while assigned(pu) do
                    begin
                      if pu.u=self then
                        break;
                      pu:=tused_unit(pu.next);
                    end;
                  if assigned(pu) and assigned(pu.unitsym) then
                    compiler.verbose.MessagePos2(pu.unitsym.fileinfo,unit_f_cant_find_ppu,realmodulename^,loadedfrommodule.realmodulename^)
                  else
                    compiler.verbose.Message2(unit_f_cant_find_ppu,realmodulename^,loadedfrommodule.realmodulename^);
                end
              else
                compiler.verbose.Message1(unit_f_cant_compile_unit,realmodulename^);
            end;
         end;
        { we found the sources, we do not need the verbose messages anymore }
        if comments <> nil then
        begin
          comments.free;
          comments:=nil;
        end;
      end;

    procedure tppumodule.mark_recompile_needed(reason: trecompile_reason);
      begin
        {$IFDEF DEBUG_PPU_CYCLES}
        writeln('PPUALGO tppumodule.mark_recompile_needed ',modulename^,' old=',statestr,' new=',ms_compile);
        {$ENDIF}
        recompile_reason:=reason;
        do_recompile:=true;
        do_reload:=true;
        state:=ms_compile;
      end;

    procedure tppumodule.post_load_or_compile(from_module : tmodule);
      begin
        if in_interface then
          internalerror(200212283);

        { reopen the old module }
  {$ifdef SHORT_ON_FILE_HANDLES}
        if from_module.is_unit and
            assigned(tppumodule(from_module).ppufile) then
           tppumodule(from_module).ppufile.tempopen;
  {$endif SHORT_ON_FILE_HANDLES}
      end;

    function tppumodule.loadppu(from_module : tmodule) : boolean;
      const
        ImplIntf : array[boolean] of string[15]=('implementation','interface');
      begin
        Result:=false;

        {$IFDEF DEBUG_PPU_CYCLES}
        writeln('PPUALGO tppumodule.loadppu START ',modulename^,' (',statestr,') used by "',from_module.modulename^,'" (',from_module.statestr,')');
        {$ENDIF}

        compiler.verbose.Message3(unit_u_load_unit,from_module.modulename^,
                 ImplIntf[from_module.in_interface],
                 modulename^);

        if do_reload then
          exit; { reload needed. see scheduler }

        if state>ms_registered then
          exit(interface_compiled); { loading has already started or is finished }

        if ppu_discarded then
          exit; { the ppu crc didn't match and this module was reset, don't load the ppu }

        if ctask_fast_backtrack then
          exit; { return to scheduler }

        loadedfrommodule:=from_module;

        set_current_module(self);

        if check_loadfrompackage then
        begin
          { No need to do anything, restore situation and exit. }
          set_current_module(from_module);
          {$IFDEF DEBUG_PPU_CYCLES}
          writeln('PPUALGO tppumodule.loadppu from package: ',modulename^,' (',statestr,') used by "',from_module.modulename^,'" (',from_module.statestr,')');
          {$ENDIF}
          exit(state in [ms_compiled,ms_processed]);
        end;

        { close old_current_ppu on system that are
          short on file handles like DOS PM }
{$ifdef SHORT_ON_FILE_HANDLES}
        if from_module.is_unit and
            assigned(tppumodule(from_module).ppufile) then
          tppumodule(from_module).ppufile.tempclose;
{$endif SHORT_ON_FILE_HANDLES}

        { search ppu file }
        compiler.verbose.Message1(unit_u_loading_unit,modulename^);
        if auPPU in search_unit_files(from_module,false) then
        begin
          state:=ms_load;
          fromppu:=true;
          load_interface;
        end
        else begin
          {$IFDEF DEBUG_PPU_CYCLES}
          writeln('PPUALGO tppumodule.try_load_ppufile ',modulename^,' no ppu found old=',statestr,' new=',ms_compile);
          {$ENDIF}
          { recompile_reason is already set by search_unit_files }
          state:=ms_compile;
        end;

        Result:=continueloadppu;

        set_current_module(from_module);
      end;

    function tppumodule.get_check_uses(out check_impl_uses, check_crc: boolean): boolean;
      begin
        check_impl_uses:=false;
        check_crc:=false;
        if not interface_compiled then
          exit(false);
        Result:=true;

        { if implementation was parsed then implementation uses must be checked too }
        if state=ms_load then
          check_impl_uses:=ppu_waitingfor_crc
        else if fromppu then
          check_impl_uses:=true
        else
          check_impl_uses:=state in [ms_compiling_waitfinish..ms_compiled,ms_processed];

        { if the crc(s) of used unit are known }
        check_crc:={$IFNDEF EnableUrCRC}not (mf_release in moduleflags) and{$ENDIF}
                   (fromppu or (state in [ms_load,ms_compiled,ms_processed]));
      end;

    function tppumodule.continueloadppu: boolean;
      var
        old_module: tmodule;
      begin
        Result:=false;
        old_module:=current_module;

        restore_state;

        if do_reload then
          Internalerror(2026021017);

        if state=ms_load then
        begin
          if load_usedunits then
          begin
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tppumodule.continueloadppu ',modulename^,' finished state=',statestr);
            {$ENDIF}
            compiler.verbose.Message1(unit_u_finished_loading_unit,modulename^);
          end else if state=ms_load then
          begin
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tppumodule.continueloadppu ',modulename^,' delay state=',statestr);
            {$ENDIF}
            store_state;
            { loading unfinished or reset, restore current_module }
            set_current_module(old_module);
            exit;
          end else if state<>ms_compile then
            internalerror(2026020510);
        end;

        { PPU is not needed anymore }
        if assigned(ppufile) then
          discardppu;

        if state=ms_compiled then
        begin
          Result:=true;
          post_load_or_compile(loadedfrommodule);
        end else if state=ms_compile then
          mark_recompile_needed(recompile_reason);

        { finished or recompile: no need to store state }
        FreeAndNil(stored_state);

        { we are back, restore current_module }
        set_current_module(old_module);
      end;

    function tppumodule.canreload(out firstwaiting: tmodule; ignore_do_reload: boolean): boolean;
      var
        check_impl_uses, check_crc: Boolean;
        pu: tused_unit;
      begin
        firstwaiting:=nil;
        if not get_check_uses(check_impl_uses, check_crc) then
          exit(true);

        pu:=tused_unit(used_units.first);
        while assigned(pu) do
        begin
          if pu.in_interface or check_impl_uses then
          begin
            if not pu.u.interface_compiled
                or (pu.u.do_reload and not ignore_do_reload) then
            begin
              firstwaiting:=pu.u;
              exit(false);
            end;
          end;
          pu:=tused_unit(pu.next);
        end;
        Result:=true;
      end;

    procedure tppumodule.reload;
      var
        firstwaiting: tmodule;
      begin
        if not do_reload then
          Internalerror(2026021015);

        if state in [ms_compiled,ms_processed] then
        begin
          writeln('tppumodule.reload_module ',modulename^,' ',statestr);
          Internalerror(2026022410);
        end;

        if not canreload(firstwaiting,true) then
        begin
          if do_reload then
          begin
            writeln('tppumodule.reload_module ',modulename^,' ',statestr,' RELOAD FAILED');
            Internalerror(2026022413);
          end;
          exit;
        end;

        {$IFDEF DEBUG_PPU_CYCLES}
        writeln('PPUALGO tppumodule.reload ',modulename^,' ',statestr,' RELOADING');
        {$ENDIF}
        compiler.verbose.Message(unit_u_forced_reload);
        do_reload:=false;
        set_current_module(self);
        re_resolve(loadedfrommodule);
      end;

    procedure tppumodule.discardppu;
      begin
        { PPU is not needed anymore }
        if not assigned(ppufile) then
          exit;
        ppufile.closefile;
        ppufile.free;
        ppufile:=nil;
      end;

{*****************************************************************************
                               RegisterUnit
*****************************************************************************}


    function registerunit(compiler:TCompilerBase;callermodule:tmodule;const s : TIDString;const fn:string; out is_new:boolean) : tppumodule;

          function FindCycle(aFile, SearchFor: tppumodule; var Cycle: TFPList): boolean;
          var
            aParent: tdependent_unit;
          begin
            { check already visited }
            if aFile.cycle_search_stamp=tppumodule.cycle_stamp then
              exit(false);
            aFile.cycle_search_stamp:=tppumodule.cycle_stamp; { mark visited }

            if aFile=SearchFor then
            begin
              { unit cycle found }
              if Cycle=nil then Cycle:=TFPList.Create;
              Cycle.Add(aFile);
              //Writeln('exit at ',aFile.modulename^,' callermodule=',callermodule.modulename^,' in_interface=',callermodule.in_interface);
              exit(true);
            end;

            aParent:=tdependent_unit(aFile.dependent_units.First);
            While Assigned(aParent) do
            begin
              //writeln('Registering ',Callermodule.modulename^,': checking cyclic dependency of ',aFile.modulename^, ' on ',aParent.u.modulename^);
              if aParent.in_interface then
              begin
                // writeln('Registering ',Callermodule.get_modulename,': checking cyclic dependency of ',aFile.get_modulename, ' on ',aparent.u.get_modulename);
                if FindCycle(tppumodule(aParent.u),SearchFor,Cycle) then
                begin
                  //Writeln('Cycle found, exit at ',aParent.u.modulename^,' uses ',aFile.modulename^);
                  Cycle.Add(aFile);
                  exit(true);
                end;
              end;
              aParent:=tdependent_unit(aParent.Next);
            end;
            Result:=false;
          end;

      var
        ups   : TIDString;
        hp    : tppumodule;
        cycle : TFPList;
{$IFDEF DEBUGCYCLE}
        cyclepath : ansistring;
        hp2   : tmodule;
{$ENDIF}

      begin
        { Info }
        ups:=upper(s);

        { search all loaded units, skip program/library }
        hp:=tppumodule(loaded_units.first);
        while assigned(hp) and ((hp.modulename^<>ups) or not hp.is_unit) do
          hp:=tppumodule(hp.next);

        is_new:=not assigned(hp);
        if is_new then
        begin
          { the unit is not in the loaded units,
            we create an entry and register the unit }
          compiler.verbose.Message1(unit_u_registering_new_unit,ups);
          hp:=tppumodule.create(callermodule,s,fn,true,compiler);
          addloadedunit(hp);
        end
        else if callermodule.in_interface then
        begin
          { check for a cycle }
          Cycle:=nil;
          try
            tmodule.increase_cycle_stamp;
            if FindCycle(tppumodule(CallerModule),hp,Cycle) then
            begin
              {$IFDEF DEBUGCYCLE}
              Writeln('Done cycle check');
              CyclePath:='';
              hp2:=TModule(Cycle[Cycle.Count-1]);
              for i:=0 to Cycle.Count-1 do begin
                if i>0 then CyclePath:=CyclePath+',';
                CyclePath:=CyclePath+TModule(Cycle[i]).realmodulename^;
              end;
              Writeln('Unit cycle detected: ',CyclePath);
              {$ENDIF}
              compiler.verbose.Message2(unit_f_circular_unit_reference,callermodule.realmodulename^,hp.realmodulename^);
            end;
          finally
            Cycle.Free;
          end;
        end;

        { return }
        registerunit:=hp;
      end;

end.
