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
unit fmodule;

{$i fpcdefs.inc}

{$ifdef go32v2}
  {$define shortasmprefix}
{$endif}
{$ifdef watcom}
  {$define shortasmprefix}
{$endif}
{$ifdef atari}
  {$define shortasmprefix}
{$endif}
{$ifdef OS2}
  { Although OS/2 supports long filenames I play it safe and
    use 8.3 filenames, because this allows the compiler to run
    on a FAT partition. (DM) }
  {$define shortasmprefix}
{$endif}

interface

    uses
       cutils,cclasses,cfileutl,
       globtype,finput,ogbase,fpkg,compilerbase,
       symbase,symsym,
       wpobase,
       aasmbase,aasmdata;


    const
      UNSPECIFIED_LIBRARY_NAME = '<none>';

    type
      trecompile_reason = (rr_unknown,
        rr_noppu,
        rr_sourcenewer,
        rr_build,
        rr_crcchanged,
        rr_buildcycle
      );

{$ifdef VER3_2}
      RTLString = ansistring;
{$endif VER3_2}

      { unit options }
      tmoduleoption = (mo_none,
        mo_hint_deprecated,
        mo_hint_platform,
        mo_hint_library,
        mo_hint_unimplemented,
        mo_hint_experimental,
        mo_has_deprecated_msg
      );
      tmoduleoptions = set of tmoduleoption;

      tlinkcontaineritem=class(tlinkedlistitem)
      public
         data : TPathStr;
         needlink : cardinal;
         constructor Create(const s:TPathStr;m:cardinal);
      end;

      tlinkcontainer=class(tlinkedlist)
         procedure add(const s : TPathStr;m:cardinal);
         function get(var m:cardinal) : TPathStr;
         function getusemask(mask:cardinal) : TPathStr;
         function find(const s:TPathStr):boolean;
      end;

      tmodule = class;
      tused_unit = class;

      tunitimportsym = class
        module: tmodule;
        symid: longint;
        sym: TSymEntry;
      end;

      tunitmaprec = record
        u        : tmodule;
        { number of references }
        refs     : longint;
        { index in the derefmap }
        derefidx : longint;
      end;
      tunitmaparray = array of tunitmaprec;

      tderefmaprec = record
        u           : tmodule;
        { modulename, used during ppu load }
        modulename  : pshortstring;
      end;
      tderefmaparray = array of tderefmaprec;

      tfinish_module_event = procedure(m: tmodule) of object;
      trename_module_event = procedure(m: tmodule; const oldname: TSymStr) of object;

      { tused_unit }

      tused_unit = class(tlinkedlistitem)
        checksum,
        interface_checksum,
        indirect_checksum: cardinal;
        in_uses,
        in_interface    : boolean;
        u               : tmodule;
        unitsym         : tunitsym;
        dependent_added : boolean;
        constructor create(_u : tmodule;intface,inuses:boolean;usym:tunitsym);
        procedure check_hints;
      end;

      tdependent_unit = class(tlinkedlistitem)
        u : tmodule;
        in_interface : boolean;
        constructor create(_u : tmodule; frominterface : boolean);
      end;

      { tmodule }

      tmodule = class(tmodulebase)
      private
        FImportLibraryList : TFPHashObjectList;
      public
        is_reset,                 { has reset been called ? }
        do_recompile,         { reset needed, done by ctask }
        do_reload,                { force reloading of the unit }
        fromppu: boolean;         { loaded from ppu }
        ppu_discarded: boolean;         { ppu was recompiled }
        ppu_waitingfor_crc: boolean;
        sources_avail,            { if all sources are reachable }
        interface_compiled,       { if the interface section has been parsed/compiled/loaded, interface_crc and indirect_crc are valid }
        is_dbginfo_written,
        is_unit,
        in_interface,             { processing the interface part? }
        { allow global settings }
        in_global     : boolean;
        { Whether a mode switch is still allowed at this point in the parsing.}
        mode_switch_allowed,
        { generate pic helper which loads eip in ecx (for leave procedures) }
        requires_ecx_pic_helper,
        { generate pic helper which loads eip in ebx (for non leave procedures) }
        requires_ebx_pic_helper : boolean;
        interface_only: boolean; { interface-only macpas unit; flag does not need saving/restoring to ppu }
        mainfilepos   : tfileposinfo;
        recompile_reason : trecompile_reason;  { the reason why the unit should be recompiled }
        crc_final: boolean;
        crc,                       { valid when crc_final=true }
        interface_crc,             { valid when interface_compiled=true }
        indirect_crc  : cardinal;  { valid when interface_compiled=true }
        headerflags   : cardinal;  { the PPU header flags }
        longversion   : cardinal;  { longer version than what fits in the ppu header }
        moduleflags   : tmoduleflags; { ppu flags that do not need to be known by just reading the ppu header }
        islibrary     : boolean;  { if it is a library (win32 dll) }
        IsPackage     : boolean;
        change_endian : boolean;  { if the unit is loaded on a system with a different endianess than it was compiled on }
        unitmap       : tunitmaparray; { mapping of all used units }
        unitmapsize   : longint;  { number of units in the map }
        derefmap      : tderefmaparray; { mapping of all units needed for deref }
        derefmapcnt   : longint;  { number of units in the map }
        derefmapsize  : longint;  { number of units in the map }
        derefdataintflen : longint;
        derefdata     : tdynamicarray;
        checkforwarddefs,
        deflist,
        symlist       : TFPObjectList;
        forwardgenericdefs : TFPHashObjectList; { contains a list of specializations of a forward declared generic (the key) }
        ptrdefs       : THashSet; { list of pointerdefs created in this module so we can reuse them (not saved/restored) }
        arraydefs     : THashSet; { list of single-element-arraydefs created in this module so we can reuse them (not saved/restored) }
        procaddrdefs  : THashSet; { list of procvardefs created when getting the address of a procdef (not saved/restored) }
{$ifdef llvm}
        llvmdefs      : THashSet; { defs added for llvm-specific reasons (not saved/restored) }
        llvmusedsyms  : TFPObjectList; { a list of asmsymbols and their defs that need to be added to llvm.used (so they're not removed by llvm optimisation passes nor by the linker) }
        llvmcompilerusedsyms : TFPObjectList; { a list of asmsymbols and their defs that need to be added to llvm.compiler.used (so they're not removed by llvm optimisation passes) }
        llvminitprocs,
        llvmfiniprocs : TFPList;
        llvmmetadatastrings: TFPHashList; { metadata strings (mapping string -> superregister) }
{$endif llvm}
        ansistrdef    : tobject; { an ansistring def redefined for the current module }
        wpoinfo       : tunitwpoinfobase; { whole program optimization-related information that is generated during the current run for this unit }
        globalsymtable,           { pointer to the global symtable of this unit }
        localsymtable : TSymtable;{ pointer to the local symtable of this unit }
        globalmacrosymtable,           { pointer to the global macro symtable of this unit }
        localmacrosymtable : TSymtable;{ pointer to the local macro symtable of this unit }
        mainscanner   : TObject;  { scanner object used }
        scanner       : TObject;  { scanner object used }
        procinfo      : TObject;  { current procedure being compiled }
        asmdata       : TObject;  { Assembler data }
        asmprefix     : pshortstring;  { prefix for the smartlink asmfiles }
        publicasmsyms : TFPHashObjectList; { contains the assembler symbols which need to be exported from a package }
        externasmsyms : TFPHashObjectList; { contains the assembler symbols which are imported from another unit }
        unitimportsyms : tfpobjectlist; { list of symbols (tunitimportsym) that are imported from other units }
        debuginfo     : TObject;
        _exports      : tlinkedlist;
        dllscannerinputlist : TFPHashList;
        localnamespacelist,
        resourcefiles,
        linkorderedsymbols : TCmdStrList;
        linkunitofiles,
        linkunitstaticlibs,
        linkunitsharedlibs,
        linkotherofiles,           { objects,libs loaded from the source }
        linkothersharedlibs,       { using $L or $LINKLIB or import lib (for linux) }
        linkotherstaticlibs,
        linkotherframeworks  : tlinkcontainer;
        mainname      : pshortstring; { alternate name for "main" procedure }
        package       : tpackage;

        used_units           : tlinkedlist; { list of tused_unit }
        dependent_units      : tlinkedlist;

        { circular unit groups = strongly connected components }
        scc_finished: boolean; { scc is compiled = this module and all used modules even indirectly are finished
                                 Note that in a cycle ms_processed can be reached while scc_finished is still false }
        scc_root: tmodule;     { valid if not scc_finished: all modules of a scc poins to their root module }
        scc_next: tmodule;     { next module in same scc }
        scc_index: integer;    { dont use. used in ttask_handler.update_circular_unit_groups }
        scc_lowindex: integer; { valid if >0 and scc_finished=false.
                                 lowest scc_index reachable through used_units,
                                 all circular connected modules have the same lowindex }
        scc_onstack: boolean;  { dont use. used in ttask_handler.update_circular_unit_groups }
        class var
          ctask_fast_backtrack: boolean; { true if some cycle was detected and returning fast to ctask scheduler }
          cycle_stamp: dword;
        var
        cycle_search_stamp: dword;
        scc_tree_unfinished: boolean; { only valid for scc roots }
        other_scc_unfinished: boolean; { only valid for scc roots }
        scc_tree_crc_wait: tmodule;

        task: TObject;         { ctask ttask }

        localunitsearchpath,           { local searchpaths }
        localobjectsearchpath,
        localincludesearchpath,
        locallibrarysearchpath,
        localframeworksearchpath : TSearchPathList;

        moduleoptions: tmoduleoptions;
        deprecatedmsg: pshortstring;
        compilecount : integer;
        consume_semicolon_after_uses : Boolean;
        initfinalchecked : boolean;
        functypechecked : boolean;

        { contains a list of types that are extended by helper types; the key is
          the full name of the type and the data is a TFPObjectList of
          tobjectdef instances (the helper defs) }
        extendeddefs: TFPHashObjectList;
        { contains a list of the current topmost non-generic symbol for a
          typename of which at least one generic exists; the key is the
          non-generic typename and the data is a TFPObjectList of tgenericdummyentry
          instances whereby the last one is the current top most one }
        genericdummysyms: TFPHashObjectList;
        { contains a list of specializations for which the method bodies need
          to be generated }
        pendingspecializations : TFPHashObjectList;
        { list of attributes that are used and thus need their construction
          functions generated }
        used_rtti_attrs: tfpobjectlist;

        { this contains a list of units (tmodule) that needs to be waited for until the
          unit can be finished (code generated, etc.); this is needed to handle
          specializations in circular unit usages correctly }
        waitingforunit: tfpobjectlist;
        { this contains a list of all units (tmodule) that are waiting for this unit to be
          finished }
        waitingunits: tfpobjectlist;

        finishstate: pointer;
        specializestate : pointer;

        namespace: pshortstring; { for JVM target: corresponds to Java package name }

        { for targets that initialise typed constants via explicit assignments
          instead of by generating an initialised data section (holds typed
          constant assignments at the module level; does not have to be saved
          into the ppu file, because translated into code during compilation)
           -- actual type: tnode (but fmodule should not depend on node) }
        tcinitcode     : tobject;

        { the current extended rtti directive }
        rtti_directive : trtti_directive;

        {create creates a new module which name is stored in 's'. LoadedFrom
        points to the module calling it. It is nil for the first compiled
        module. This allow inheritance of all path lists. MUST pay attention
        to that when creating link.res!!!!(mazen)}
        constructor create(LoadedFrom:TModule;const amodulename: string; const afilename:TPathStr;_is_unit:boolean;acompiler:TCompilerBase);
        destructor destroy;override;
        procedure reset(for_recompile: boolean);virtual;
        function statestr: string; virtual;
        procedure checkstate; virtual;
        procedure loadlocalnamespacelist;
        procedure adddependency(callermodule:tmodule; frominterface : boolean);
        procedure removedependency(callermodule:tmodule);
        function hasdependency(callermodule:tmodule): boolean;
        procedure flagdependent;
        class procedure increase_cycle_stamp;
        procedure disconnect_depending_modules; virtual;
        function is_reload_needed(du: tdependent_unit): boolean; virtual; // true if reload needed after self changed
        function are_all_used_units_compiled: boolean;
        class var finish_module: tfinish_module_event;
        procedure addimportedsym(sym:TSymEntry; check_if_exists: boolean = true);
        procedure derefimportedsymbols;
        function  addusedunit(hp:tmodule;inuses:boolean;usym:tunitsym):tused_unit;
        function  usesmodule_in_interface(m : tmodule) : boolean;
        function findusedunit(m : tmodule) : tused_unit;
        function usedunitsloaded(interface_units: boolean; out firstwaiting : tmodule): boolean;
        function nowaitingforunits(out firstwaiting : tmodule) : Boolean;
        function usedunitsfinalcrc(out firstwaiting : tmodule): boolean;
        procedure updatemaps;
        function  derefidx_unit(id:longint):longint;
        function  resolve_unit(id:longint):tmodule;
        procedure allunitsused;
        procedure end_of_parsing;virtual;
        procedure setmodulename(const s:string);
        procedure AddExternalImport(const libname,symname,symmangledname:string;OrdNr: longint;isvar:boolean;ImportByOrdinalOnly:boolean);
        procedure add_public_asmsym(sym:TAsmSymbol);
        procedure add_public_asmsym(const name:TSymStr;bind:TAsmsymbind;typ:Tasmsymtype);
        procedure add_extern_asmsym(sym:TAsmSymbol);
        procedure add_extern_asmsym(const name:TSymStr;bind:TAsmsymbind;typ:Tasmsymtype);
        procedure remove_waitforunit_cycles;
        procedure remove_all_waitsforthisunit;
        procedure remove_from_waitingforunits(amodule : tmodule);
        property ImportLibraryList : TFPHashObjectList read FImportLibraryList;
        function ToString: RTLString; override;
      end;

    var
       main_module       : tmodule;     { Main module of the program }
       current_module    : tmodule;     { Current module which is compiled or loaded }
       compiled_module   : tmodule;     { Current module which is compiled }
       usedunits         : tlinkedlist; { Used units for this program }
       loaded_units      : tlinkedlist; { All loaded units, excluding main_module }
       unloaded_units    : tlinkedlist; { Units removed from loaded_units, to be freed }
       all_modules: array of tmodule;   { modules by moduleid }
       SmartLinkOFiles   : TCmdStrList; { List of .o files which are generated,
                                          used to delete them after linking }


    procedure set_current_module(p:tmodule);
    function get_module(moduleindex : longint) : tmodule;
    function get_source_file(moduleindex,fileindex : longint) : tinputfile;
    procedure addloadedunit(hp:tmodule);
    function find_module_from_symtable(st:tsymtable):tmodule;


implementation

    uses
      SysUtils,globals,
      verbose,systems,compiler,
      scanner,dbgbase,
      procinfo,symdef,symtype;

{$ifdef MEMDEBUG}
    var
      memsymtable : TMemDebug;
{$endif}

{*****************************************************************************
                             Global Functions
*****************************************************************************}

    function find_module_from_symtable(st:tsymtable):tmodule;
      begin
        result:=get_module(st.moduleid);
      end;

    procedure set_current_module(p:tmodule);

      begin
        { save the state of the scanner }
        if assigned(current_scanner) then
          current_scanner.tempcloseinputfile;
        { set new module }
        current_module:=p;
        { restore previous module settings }
        Fillchar(current_filepos,sizeof(current_filepos),0);
        if assigned(current_module) then
          begin
            current_asmdata:=tasmdata(current_module.asmdata);
            current_debuginfo:=tdebuginfo(current_module.debuginfo);
            { restore scanner and file positions }
            set_current_scanner(tscannerfile(current_module.scanner));
            if assigned(current_scanner) then
              begin
                current_scanner.tempopeninputfile;
                current_scanner.gettokenpos;
                parser_current_file:=current_scanner.inputfile.name;
              end
            else
              begin
                current_filepos.moduleindex:=current_module.moduleid;
                parser_current_file:='';
              end;
          end
        else
          begin
            current_asmdata:=nil;
            set_current_scanner(nil);
            current_debuginfo:=nil;
          end;
      end;


    function get_module(moduleindex : longint) : tmodule;
      begin
        if moduleindex>=length(all_modules) then
          result:=nil
        else
          result:=all_modules[moduleindex];
      end;


    function get_source_file(moduleindex,fileindex : longint) : tinputfile;
      var
        hp : tmodule;
      begin
        hp:=get_module(moduleindex);
        if assigned(hp) then
          get_source_file:=hp.sourcefiles.get_file(fileindex)
        else
          get_source_file:=nil;
      end;


    procedure addloadedunit(hp:tmodule);
      begin
        loaded_units.concat(hp);
      end;


{****************************************************************************
                             TLinkContainerItem
 ****************************************************************************}

    constructor TLinkContainerItem.Create(const s:TPathStr;m:cardinal);
      begin
        inherited Create;
        data:=s;
        needlink:=m;
      end;


{****************************************************************************
                           TLinkContainer
 ****************************************************************************}

    procedure TLinkContainer.add(const s : TPathStr;m:cardinal);
      begin
        inherited concat(TLinkContainerItem.Create(s,m));
      end;


    function TLinkContainer.get(var m:cardinal) : TPathStr;
      var
        p : tlinkcontaineritem;
      begin
        p:=tlinkcontaineritem(inherited getfirst);
        if p=nil then
         begin
           get:='';
           m:=0;
         end
        else
         begin
           get:=p.data;
           m:=p.needlink;
           p.free;
           p := nil;
         end;
      end;


    function TLinkContainer.getusemask(mask:cardinal) : TPathStr;
      var
         p : tlinkcontaineritem;
         found : boolean;
      begin
        found:=false;
        repeat
          p:=tlinkcontaineritem(inherited getfirst);
          if p=nil then
           begin
             getusemask:='';
             exit;
           end;
          getusemask:=p.data;
          found:=(p.needlink and mask)<>0;
          p.free;
          p := nil;
        until found;
      end;


    function TLinkContainer.find(const s:TPathStr):boolean;
      var
        newnode : tlinkcontaineritem;
      begin
        find:=false;
        newnode:=tlinkcontaineritem(First);
        while assigned(newnode) do
         begin
           if newnode.data=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=tlinkcontaineritem(newnode.next);
         end;
      end;


{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

    constructor tused_unit.create(_u : tmodule;intface,inuses:boolean;usym:tunitsym);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=inuses;
        unitsym:=usym;
        if _u.state in [ms_load,ms_compiled_waitcrc,ms_compiled,ms_processed] then
          checksum:=u.crc
        else
          checksum:=0;
        if _u.interface_compiled then
         begin
           interface_checksum:=u.interface_crc;
           indirect_checksum:=u.indirect_crc;
         end
        else
         begin
           interface_checksum:=0;
           indirect_checksum:=0;
         end;
      end;


    procedure tused_unit.check_hints;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      var
        uname: pshortstring;
      begin
        uname:=u.realmodulename;
        if mo_hint_deprecated in u.moduleoptions then
          if (mo_has_deprecated_msg in u.moduleoptions) and (u.deprecatedmsg <> nil) then
            compiler.verbose.MessagePos2(unitsym.fileinfo,sym_w_deprecated_unit_with_msg,uname^,u.deprecatedmsg^)
          else
            compiler.verbose.MessagePos1(unitsym.fileinfo,sym_w_deprecated_unit,uname^);
        if mo_hint_experimental in u.moduleoptions then
          compiler.verbose.MessagePos1(unitsym.fileinfo,sym_w_experimental_unit,uname^);
        if mo_hint_platform in u.moduleoptions then
          compiler.verbose.MessagePos1(unitsym.fileinfo,sym_w_non_portable_unit,uname^);
        if mo_hint_library in u.moduleoptions then
          compiler.verbose.MessagePos1(unitsym.fileinfo,sym_w_library_unit,uname^);
        if mo_hint_unimplemented in u.moduleoptions then
          compiler.verbose.MessagePos1(unitsym.fileinfo,sym_w_non_implemented_unit,uname^);
      end;


{****************************************************************************
                            TDENPENDENT_UNIT
 ****************************************************************************}

    constructor tdependent_unit.create(_u: tmodule; frominterface: boolean);
      begin
         u:=_u;
         in_interface:=frominterface;
      end;


{****************************************************************************
                                  TMODULE
 ****************************************************************************}

    constructor tmodule.create(LoadedFrom:TModule;const amodulename: string; const afilename:TPathStr;_is_unit:boolean;acompiler:TCompilerBase);
      var
        n:string;
        fn:TPathStr;
        old_mod_cnt, i: SizeInt;
        new_mod_cnt: Integer;
      begin
        compiler:=acompiler;
        if amodulename='' then
          n:=ChangeFileExt(ExtractFileName(afilename),'')
        else
          n:=amodulename;
        if afilename='' then
          fn:=amodulename
        else
          fn:=afilename;
        { Programs have the name 'Program' to don't conflict with dup id's }
        if _is_unit then
         inherited create(amodulename,compiler)
        else
         inherited create('Program',compiler);
        mainsource:=fn;

        old_mod_cnt:=length(all_modules);
        if moduleid>=old_mod_cnt then
          begin
            if old_mod_cnt<32 then
              new_mod_cnt:=32
            else
              new_mod_cnt:=old_mod_cnt*2;
            setlength(all_modules,new_mod_cnt);
            for i:=old_mod_cnt to new_mod_cnt-1 do
              all_modules[i]:=nil;
          end;
        all_modules[moduleid]:=self;

        { Dos has the famous 8.3 limit :( }
{$ifdef shortasmprefix}
        asmprefix:=stringdup(FixFileName('as'));
{$else}
        asmprefix:=stringdup(FixFileName(n));
{$endif}
        setfilename(fn,true);
        localunitsearchpath:=TSearchPathList.Create;
        localobjectsearchpath:=TSearchPathList.Create;
        localincludesearchpath:=TSearchPathList.Create;
        locallibrarysearchpath:=TSearchPathList.Create;
        localframeworksearchpath:=TSearchPathList.Create;
        used_units:=TLinkedList.Create;
        dependent_units:=TLinkedList.Create;
        localnamespacelist:=TCmdStrList.Create;
        resourcefiles:=TCmdStrList.Create;
        linkorderedsymbols:=TCmdStrList.Create;
        linkunitofiles:=TLinkContainer.Create;
        linkunitstaticlibs:=TLinkContainer.Create;
        linkunitsharedlibs:=TLinkContainer.Create;
        linkotherofiles:=TLinkContainer.Create;
        linkotherstaticlibs:=TLinkContainer.Create;
        linkothersharedlibs:=TLinkContainer.Create;
        linkotherframeworks:=TLinkContainer.Create;
        mainname:=nil;
        FImportLibraryList:=TFPHashObjectList.Create(true);
        crc_final:=false;
        crc:=0;
        interface_crc:=0;
        indirect_crc:=0;
        headerflags:=0;
        longversion:=0;
        moduleflags:=[];
        scanner:=nil;
        unitmap:=nil;
        unitmapsize:=0;
        derefmap:=nil;
        derefmapsize:=0;
        derefmapcnt:=0;
        derefdata:=TDynamicArray.Create(1024);
        derefdataintflen:=0;
        deflist:=TFPObjectList.Create(false);
        symlist:=TFPObjectList.Create(false);
        ptrdefs:=THashSet.Create(64,true,false);
        arraydefs:=THashSet.Create(64,true,false);
        procaddrdefs:=THashSet.Create(64,true,false);
{$ifdef llvm}
        llvmdefs:=THashSet.Create(64,true,false);
        llvmusedsyms:=TFPObjectList.Create(true);
        llvmcompilerusedsyms:=TFPObjectList.Create(true);
        llvminitprocs:=TFPList.Create;
        llvmfiniprocs:=TFPList.Create;
        llvmmetadatastrings:=TFPHashList.Create;
{$endif llvm}
        ansistrdef:=nil;
        wpoinfo:=nil;
        checkforwarddefs:=TFPObjectList.Create(false);
        forwardgenericdefs:=TFPHashObjectList.Create(true);
        extendeddefs:=TFPHashObjectList.Create(true);
        genericdummysyms:=tfphashobjectlist.create(true);
        pendingspecializations:=tfphashobjectlist.create(false);
        waitingforunit:=tfpobjectlist.create(false);
        waitingunits:=tfpobjectlist.create(false);
        used_rtti_attrs:=tfpobjectlist.create(false);
        globalsymtable:=nil;
        localsymtable:=nil;
        globalmacrosymtable:=nil;
        localmacrosymtable:=nil;
        do_reload:=false;
        sources_avail:=true;
        mainfilepos.line:=0;
        mainfilepos.column:=0;
        mainfilepos.fileindex:=0;
        recompile_reason:=rr_unknown;
        in_interface:=true;
        in_global:=true;
        is_unit:=_is_unit;
        islibrary:=false;
        ispackage:=false;
        change_endian:=false;
        is_dbginfo_written:=false;
        mode_switch_allowed:= true;
        moduleoptions:=[];
        deprecatedmsg:=nil;
        namespace:=nil;
        tcinitcode:=nil;
        _exports:=TLinkedList.Create;
        dllscannerinputlist:=TFPHashList.Create;
        asmdata:=casmdata.create(modulename,compiler);
        unitimportsyms:=TFPObjectList.Create(true);
        publicasmsyms:=TFPHashObjectList.Create(true);
        externasmsyms:=TFPHashObjectList.Create(true);
        InitDebugInfo(self,false);
      end;


    destructor tmodule.destroy;
      var
        i : longint;
        current_debuginfo_reset : boolean;
      begin
        unitmap:=nil;
        if assigned(derefmap) then
          begin
            for i:=0 to derefmapcnt-1 do
              stringdispose(derefmap[i].modulename);
          end;
        derefmap:=nil;
        if assigned(_exports) then
          freeandnil(_exports);
        if assigned(dllscannerinputlist) then
          freeandnil(dllscannerinputlist);
        if assigned(localnamespacelist) then
          freeandnil(localnamespacelist);
        if assigned(scanner) then
          begin
            { also update current_scanner if it was pointing
              to this module }
            if current_scanner=tscannerfile(scanner) then
              set_current_scanner(nil);
            freeandnil(scanner);
          end;
        if assigned(asmdata) then
          begin
            if current_asmdata=asmdata then
              current_asmdata:=nil;
             asmdata.free;
             asmdata := nil;
          end;
        if assigned(procinfo) then
          begin
            if current_procinfo=tprocinfo(procinfo) then
              begin
                current_procinfo:=nil;
                current_structdef:=nil;
                current_genericdef:=nil;
                current_specializedef:=nil;
              end;
            { release procinfo tree }
            tprocinfo(procinfo).destroy_tree;
            procinfo := nil;
          end;
        DoneDebugInfo(self,current_debuginfo_reset);
        used_units.free;
        used_units := nil;
        dependent_units.free;
        dependent_units := nil;
        resourcefiles.Free;
        resourcefiles := nil;
        linkorderedsymbols.Free;
        linkorderedsymbols := nil;
        linkunitofiles.Free;
        linkunitofiles := nil;
        linkunitstaticlibs.Free;
        linkunitstaticlibs := nil;
        linkunitsharedlibs.Free;
        linkunitsharedlibs := nil;
        linkotherofiles.Free;
        linkotherofiles := nil;
        linkotherstaticlibs.Free;
        linkotherstaticlibs := nil;
        linkothersharedlibs.Free;
        linkothersharedlibs := nil;
        linkotherframeworks.Free;
        linkotherframeworks := nil;
        stringdispose(mainname);
        externasmsyms.Free;
        externasmsyms := nil;
        publicasmsyms.Free;
        publicasmsyms := nil;
        unitimportsyms.Free;
        unitimportsyms := nil;
        FImportLibraryList.Free;
        FImportLibraryList := nil;
        extendeddefs.Free;
        extendeddefs := nil;
        genericdummysyms.free;
        genericdummysyms := nil;
        pendingspecializations.free;
        pendingspecializations := nil;
        waitingforunit.free;
        waitingforunit := nil;
        waitingunits.free;
        waitingunits := nil;
        used_rtti_attrs.free;
        used_rtti_attrs := nil;
        stringdispose(asmprefix);
        stringdispose(deprecatedmsg);
        stringdispose(namespace);
        tcinitcode.free;
        tcinitcode := nil;
        localunitsearchpath.Free;
        localunitsearchpath := nil;
        localobjectsearchpath.free;
        localobjectsearchpath := nil;
        localincludesearchpath.free;
        localincludesearchpath := nil;
        locallibrarysearchpath.free;
        locallibrarysearchpath := nil;
        localframeworksearchpath.free;
        localframeworksearchpath := nil;
{$ifdef MEMDEBUG}
        memsymtable.start;
{$endif}
        derefdata.free;
        derefdata := nil;
        if assigned(deflist) then
          begin
            for i:=0 to deflist.Count-1 do
              if assigned(deflist[i]) and
                 (tdef(deflist[i]).registered_in_module=self) then
                tdef(deflist[i]).registered_in_module:=nil;
            deflist.free;
            deflist := nil;
          end;
        symlist.free;
        symlist := nil;
        ptrdefs.free;
        ptrdefs := nil;
        arraydefs.free;
        arraydefs := nil;
        procaddrdefs.free;
        procaddrdefs := nil;
{$ifdef llvm}
        llvmdefs.free;
        llvmdefs := nil;
        llvmusedsyms.free;
        llvmusedsyms := nil;
        llvmcompilerusedsyms.free;
        llvmcompilerusedsyms := nil;
        llvminitprocs.free;
        llvminitprocs := nil;
        llvmfiniprocs.free;
        llvmfiniprocs := nil;
        llvmmetadatastrings.free;
        llvmmetadatastrings := nil;
{$endif llvm}
        ansistrdef:=nil;
        wpoinfo.free;
        wpoinfo := nil;
        checkforwarddefs.free;
        checkforwarddefs := nil;
        forwardgenericdefs.free;
        forwardgenericdefs := nil;
        globalsymtable.free;
        globalsymtable := nil;
        localsymtable.free;
        localsymtable := nil;
        globalmacrosymtable.free;
        globalmacrosymtable := nil;
        localmacrosymtable.free;
        localmacrosymtable := nil;

        task:=nil;
        all_modules[moduleid]:=nil;
{$ifdef MEMDEBUG}
        memsymtable.stop;
{$endif}
        inherited Destroy;
      end;


    procedure tmodule.reset(for_recompile: boolean);
      var
        i   : longint;
        current_debuginfo_reset : boolean;
        m : tmodule;
      begin
        is_reset:=true;
        do_recompile:=false;
        fromppu:=false;
        if assigned(scanner) then
          begin
            { also update current_scanner if it was pointing
              to this module }
            if current_scanner=tscannerfile(scanner) then
              set_current_scanner(nil);
            freeandnil(scanner);
          end;
        if assigned(procinfo) then
          begin
            if current_procinfo=tprocinfo(procinfo) then
              begin
                current_procinfo:=nil;
                current_structdef:=nil;
                current_genericdef:=nil;
                current_specializedef:=nil;
              end;
            { release procinfo tree }
            tprocinfo(procinfo).destroy_tree;
          end;
        if assigned(asmdata) then
          begin
            if current_asmdata=asmdata then
              current_asmdata:=nil;
            asmdata.free;
            asmdata:=nil;
          end;
        DoneDebugInfo(self,current_debuginfo_reset);
        globalsymtable.free;
        globalsymtable:=nil;
        localsymtable.free;
        localsymtable:=nil;
        globalmacrosymtable.free;
        globalmacrosymtable:=nil;
        localmacrosymtable.free;
        localmacrosymtable:=nil;
        deflist.free;
        deflist:=TFPObjectList.Create(false);
        symlist.free;
        symlist:=TFPObjectList.Create(false);
        ptrdefs.free;
        ptrdefs:=THashSet.Create(64,true,false);
        arraydefs.free;
        arraydefs:=THashSet.Create(64,true,false);
        procaddrdefs.free;
        procaddrdefs:=THashSet.Create(64,true,false);
{$ifdef llvm}
        llvmdefs.free;
        llvmdefs:=THashSet.Create(64,true,false);
        llvmusedsyms.free;
        llvmusedsyms:=TFPObjectList.Create(true);
        llvmcompilerusedsyms.free;
        llvmcompilerusedsyms:=TFPObjectList.Create(true);
        llvminitprocs.free;
        llvminitprocs:=TFPList.Create;
        llvmfiniprocs.free;
        llvmfiniprocs:=TFPList.Create;
        llvmmetadatastrings.free;
        llvmmetadatastrings:=TFPHashList.Create;
{$endif llvm}
        ansistrdef:=nil;
        wpoinfo.free;
        wpoinfo:=nil;
        checkforwarddefs.free;
        checkforwarddefs:=TFPObjectList.Create(false);
        forwardgenericdefs.free;
        forwardgenericdefs:=TFPHashObjectList.Create(true);
        publicasmsyms.free;
        publicasmsyms:=TFPHashObjectList.Create(true);
        externasmsyms.free;
        externasmsyms:=TFPHashObjectList.Create(true);
        unitimportsyms.free;
        unitimportsyms:=TFPObjectList.Create(true);
        derefdata.free;
        derefdata:=TDynamicArray.Create(1024);
        unitmap:=nil;
        if assigned(derefmap) then
          begin
            for i:=0 to derefmapcnt-1 do
              stringdispose(derefmap[i].modulename);
          end;
        derefmap:=nil;
        unitmapsize:=0;
        derefmapsize:=0;
        derefmapcnt:=0;
        derefdataintflen:=0;
        sourcefiles.free;
        sourcefiles:=tinputfilemanager.create;
        asmdata:=casmdata.create(modulename,compiler);
        if current_module=self then
          current_asmdata:=TAsmData(asmdata);
        InitDebugInfo(self,current_debuginfo_reset);
        _exports.free;
        _exports:=tlinkedlist.create;
        dllscannerinputlist.free;
        dllscannerinputlist:=TFPHashList.create;
        { During reload, the list of used units cannot change.
          It can only change while recompiling.
          Because the used_units is used in loops in the load cycle(s) which
          can recurse into the same unit due to circular dependencies,
          we do not destroy the list, we only update the contents.
          As a result the loop variable does not get reset during the loop.
          For recompile, we recreate the list }
        if for_recompile then
          begin
          used_units.free;
          used_units:=TLinkedList.Create;
          end;
        // keep dependent_units
        resourcefiles.Free;
        resourcefiles:=TCmdStrList.Create;
        linkorderedsymbols.Free;
        linkorderedsymbols:=TCmdStrList.Create;
        pendingspecializations.free;
        pendingspecializations:=tfphashobjectlist.create(false);
        genericdummysyms.Free;
        genericdummysyms := tfphashobjectlist.create(true);
        extendeddefs.Free;
        extendeddefs:=TFPHashObjectList.Create(true);
        if assigned(waitingforunit) and
          (waitingforunit.count<>0) then
           begin
           Write(Self.modulename^, ' is reset while still waiting for units: ');
           for I:=0 to waitingforunit.Count-1 do
             begin
             M:=tmodule(waitingforunit.Items[i]);
             write(m.modulename^,' (state:',M.state,') ');
             end;
           Writeln;
           internalerror(2016070501);
           end;
        waitingforunit.free;
        waitingforunit:=tfpobjectlist.create(false);
        linkunitofiles.Free;
        linkunitofiles:=TLinkContainer.Create;
        linkunitstaticlibs.Free;
        linkunitstaticlibs:=TLinkContainer.Create;
        linkunitsharedlibs.Free;
        linkunitsharedlibs:=TLinkContainer.Create;
        linkotherofiles.Free;
        linkotherofiles:=TLinkContainer.Create;
        linkotherstaticlibs.Free;
        linkotherstaticlibs:=TLinkContainer.Create;
        linkothersharedlibs.Free;
        linkothersharedlibs:=TLinkContainer.Create;
        linkotherframeworks.Free;
        linkotherframeworks:=TLinkContainer.Create;
        stringdispose(mainname);
        FImportLibraryList.Free;
        FImportLibraryList:=TFPHashObjectList.Create;
        do_reload:=false;
        interface_compiled:=false;
        in_interface:=true;
        in_global:=true;
        mode_switch_allowed:=true;
        stringdispose(deprecatedmsg);
        stringdispose(namespace);
        tcinitcode.free;
        tcinitcode:=nil;
        localunitsearchpath.Free;
        localunitsearchpath:=TSearchPathList.Create;
        localobjectsearchpath.free;
        localobjectsearchpath:=TSearchPathList.Create;
        localincludesearchpath.free;
        localincludesearchpath:=TSearchPathList.Create;
        locallibrarysearchpath.free;
        locallibrarysearchpath:=TSearchPathList.Create;
        localframeworksearchpath.free;
        localframeworksearchpath:=TSearchPathList.Create;
        moduleoptions:=[];
        is_dbginfo_written:=false;
        crc_final:=false;
        crc:=0;
        interface_crc:=0;
        indirect_crc:=0;
        headerflags:=0;
        longversion:=0;
        moduleflags:=[];
        mainfilepos.line:=0;
        mainfilepos.column:=0;
        mainfilepos.fileindex:=0;
        recompile_reason:=rr_unknown;
        {
          The following fields should not
          be reset:
           mainsource
           state
           loaded_from
           ppu_discarded
           sources_avail
           compilecount
        }
      end;

    procedure tmodule.loadlocalnamespacelist;

      var
        nsitem : TCmdStrListItem;

      begin
        // Copying local namespace list
        if premodule_namespacelist.Count>0 then
          begin
          nsitem:=TCmdStrListItem(premodule_namespacelist.First);
          while assigned(nsItem) do
            begin
            localnamespacelist.Concat(nsitem.Str);
            nsItem:=TCmdStrListItem(nsitem.Next);
            end;
          premodule_namespacelist.Clear;
          end;
        current_namespacelist:=localnamespacelist;
      end;


    procedure tmodule.adddependency(callermodule: tmodule; frominterface: boolean);
      begin
        if hasdependency(callermodule) then exit;
        if is_unit then
          compiler.verbose.Message2(unit_u_add_depend_to,callermodule.modulename^,modulename^);

        { dependent_units is needed by the invalid cycle test,
          using the program is an invalid cycle as well }
        dependent_units.concat(tdependent_unit.create(callermodule,frominterface));

        if callermodule.scc_finished then
          Internalerror(2026022202);
      end;

    procedure tmodule.removedependency(callermodule: tmodule);
      var
        du, nextdu: tdependent_unit;
      begin
        du:=tdependent_unit(dependent_units.First);
        while Assigned(du) do
        begin
          nextdu:=tdependent_unit(du.Next);
          if du.u=callermodule then
            dependent_units.Remove(du);
          du:=nextdu;
        end;
      end;

    function tmodule.hasdependency(callermodule: tmodule): boolean;
      var
        du: tdependent_unit;
      begin
        du:=tdependent_unit(dependent_units.First);
        while Assigned(du) do
        begin
          if du.u=callermodule then
            exit(true);
          du:=tdependent_unit(du.Next);
        end;
        Result:=false;
      end;

    procedure tmodule.flagdependent;
      var
        dm : tdependent_unit;
        m : tmodule;

      begin
        { flag all units that depend on this unit for reloading }
        if not interface_compiled then exit;

        dm:=tdependent_unit(dependent_units.first);
        while assigned(dm) do
        begin
          { We do not have to reload the unit that wants to load
            this unit, unless this unit is already compiled during
            the loading }
          m:=dm.u;
          if m.state in [ms_compiled,ms_processed] then
          begin
            writeln('tmodule.flagdependent ',modulename^,' state=',statestr,', is used by ',BoolToStr(dm.in_interface,'interface','implementation'),' of ',m.modulename^,' ',m.statestr);
            Internalerror(2026022510);
          end;
          if not m.do_reload and is_reload_needed(dm) then
          begin
            {$IFDEF DEBUG_PPU_CYCLES}
            writeln('PPUALGO tmodule.flagdependent ',modulename^,' state=',statestr,', is used by ',BoolToStr(dm.in_interface,'interface','implementation'),' of ',m.modulename^,' ',m.statestr);
            {$ENDIF}
            m.do_reload:=true;
            compiler.verbose.Message1(unit_u_flag_for_reload,m.modulename^);
            { We have to flag the units that depend on this unit even
              though it didn't change, because they might also
              indirectly depend on the unit that did change (e.g.,
              in case rgobj, rgx86 and rgcpu have been compiled
              already, and then rgobj is recompiled for some reason
              -> rgx86 is re-reresolved, but the vmtentries of trgcpu
              must also be re-resolved, because they will also contain
              pointers to procdefs in the old trgobj (in case of a
              recompile, all old defs are freed) }
            m.flagdependent;
          end;
          dm:=tdependent_unit(dm.next);
        end;
      end;

    class procedure tmodule.increase_cycle_stamp;
      begin
        if cycle_stamp=high(integer) then
          Internalerror(2026022203);
        inc(cycle_stamp);
      end;

    function tmodule.statestr: string;
      begin
        str(state,Result);
        if do_recompile then
          Result:='do_recompile,'+Result;
        if do_reload then
          Result:='do_reload,'+Result;
      end;

    procedure tmodule.checkstate;
      begin
        // Note: ms_load is checked in tppumodule.checkstate

        if interface_compiled then
        begin
          if state in [ms_registered,ms_compile,ms_compiling_wait,ms_compiling_waitintf] then
          begin
            writeln('tmodule.checkstate ',modulename^,' ',statestr,' interface_compiled=true');
            Internalerror(2026021912);
          end;
        end else begin
          if state in [ms_compiling_waitimpl,ms_compiling_waitfinish,ms_compiled_waitcrc,ms_compiled,ms_processed] then
          begin
            writeln('tmodule.checkstate ',modulename^,' ',statestr,' interface_compiled=false');
            Internalerror(2026021911);
          end;
        end;

        if crc_final then
        begin
          if state in [ms_registered,ms_compile,ms_compiling_wait,ms_compiling_waitintf,
            ms_compiling_waitimpl,ms_compiling_waitfinish] then
          begin
            writeln('tmodule.checkstate ',modulename^,' ',statestr,' crc_final=true');
            Internalerror(2026021910);
          end;
        end else begin
          if state in [ms_compiled_waitcrc,ms_compiled,ms_processed] then
          begin
            writeln('tmodule.checkstate ',modulename^,' ',statestr,' crc_final=false');
            Internalerror(2026021909);
          end;
        end;
      end;

    procedure tmodule.disconnect_depending_modules;
      var
        uu: tused_unit;
      begin
        uu:=tused_unit(used_units.first);
        while assigned(uu) do
          begin
            uu.u.removedependency(self);
            uu.dependent_added:=false;
            uu:=tused_unit(uu.next);
          end;
      end;

    function tmodule.is_reload_needed(du: tdependent_unit): boolean;
      { du.u is a module using this unit }
      begin
        Result:=(du.u.state in [ms_compiling_waitfinish,ms_compiled_waitcrc,ms_compiled,ms_processed])
             or (du.in_interface and du.u.interface_compiled);
        { Note: see also the override in fppu.tppumodule }
      end;

    function tmodule.are_all_used_units_compiled: boolean;
      var
        uu: tused_unit;
      begin
        uu:=tused_unit(used_units.First);
        while assigned(uu) do
          begin
            if not (uu.u.state in [ms_compiled,ms_processed]) then
              exit(false);
            uu:=tused_unit(uu.Next);
          end;
        Result:=true;
      end;

    procedure tmodule.addimportedsym(sym: TSymEntry; check_if_exists: boolean);
      var
        importsym: tunitimportsym;
        module: tmodule;
        asymtable: TSymtable;
        i: Integer;
      begin
        if check_if_exists then
        begin
          for i:=0 to unitimportsyms.Count-1 do
            if tunitimportsym(unitimportsyms[i]).sym=sym then
              exit;
        end;

        asymtable:=sym.owner;
        module:=get_module(asymtable.moduleid);
        if module=nil then
        begin
          writeln('tmodule.find_unitimportsymbol missing moduleid=',asymtable.moduleid);
          Internalerror(2026022622);
        end;

        if (sym.SymId>=module.symlist.Count) then
          Internalerror(2026022617);
        if sym<>TSymEntry(module.symlist[sym.SymId]) then
        begin
          writeln('tmodule.addimportedsym ',modulename^,' ',statestr,' ',Sym.RealName,' ',Sym.SymId,' MISMATCH');
          Internalerror(2026022611);
        end;

        importsym:=tunitimportsym.Create;
        importsym.sym:=sym;
        importsym.module:=module;
        importsym.symid:=sym.SymId;
        unitimportsyms.Add(importsym);
      end;

    procedure tmodule.derefimportedsymbols;
      var
        i: Integer;
        importsym: tunitimportsym;
        module: tmodule;
      begin
        for i:=0 to unitimportsyms.Count-1 do
          begin
            importsym:=tunitimportsym(unitimportsyms[i]);
            module:=importsym.module;
            if (importsym.SymId>=module.symlist.Count) then
              Internalerror(2026022618);
            importsym.sym:=TSymEntry(module.symlist[importsym.symid]);
          end;
      end;

    function tmodule.addusedunit(hp:tmodule;inuses:boolean;usym:tunitsym):tused_unit;
      var
        pu : tused_unit;
      begin
        pu:=tused_unit.create(hp,in_interface,inuses,usym);
        used_units.concat(pu);
        addusedunit:=pu;
      end;

    function tmodule.usedunitsloaded(interface_units : boolean; out firstwaiting : tmodule): boolean;
      var
        uu: tused_unit;
        ok: Boolean;
      begin
        Result:=true;
        firstwaiting:=nil;
        uu:=tused_unit(used_units.First);
        while assigned(uu) do
        begin
          if uu.in_interface=interface_units then
          begin
            ok:=uu.u.interface_compiled and not uu.u.do_reload;
            {$IFDEF DEBUG_CTASK_VERBOSE}
            writeln('  ',ToString,' checking state of ', uu.u.ToString,' : ',uu.u.statestr,' : ',ok);
            uu.u.checkstate;
            {$ENDIF}
            if not ok then
            begin
              Result:=false;
              firstwaiting:=uu.u;
              {$IFNDEF DEBUG_CTASK_VERBOSE}
              break;
              {$ENDIF}
            end;
          end;
          uu:=tused_unit(uu.Next);
        end;
      end;

    function tmodule.nowaitingforunits(out firstwaiting : tmodule): Boolean;

      begin
        firstwaiting:=nil;
        Result:=waitingforunit.count=0;
        If not Result then
          firstwaiting:=tmodule(waitingforunit[0]);
      end;

    function tmodule.usedunitsfinalcrc(out firstwaiting: tmodule): boolean;

    var
      uu: tused_unit;

    begin
      firstwaiting:=scc_tree_crc_wait;
      if (firstwaiting<>nil) and (firstwaiting<>self) then
        exit(false);

      uu:=tused_unit(used_units.First);
      while assigned(uu) do
        begin
        if uu.u.do_reload
            or not uu.u.interface_compiled
            or not uu.u.crc_final then
          begin
          firstwaiting:=uu.u;
          exit(false);
          end;
        uu:=tused_unit(uu.Next);
        end;

      firstwaiting:=nil;
      Result:=True;
    end;

    function tmodule.usesmodule_in_interface(m: tmodule): boolean;

      var
        u : tused_unit;

      begin
        result:=False;
        u:=tused_unit(used_units.First);
        while assigned(u) do
          begin
          if (u.u=m) then
            exit(u.in_interface) ;
          u:=tused_unit(u.next);
          end;
      end;

    function tmodule.findusedunit(m: tmodule): tused_unit;
    var
      u : tused_unit;

    begin
      result:=nil;
      u:=tused_unit(used_units.First);
      while assigned(u) do
        begin
        if u.u=m then
          exit(u);
        u:=tused_unit(u.next);
        end;
    end;

    procedure tmodule.updatemaps;
      var
        oldmapsize : longint;
        hp  : tmodule;
        i   : longint;
      begin
        { Extend unitmap }
        oldmapsize:=unitmapsize;
        unitmapsize:=loaded_units.count+1;
        setlength(unitmap,unitmapsize);

        { Extend Derefmap }
        oldmapsize:=derefmapsize;
        derefmapsize:=loaded_units.count+1;
        setlength(derefmap,derefmapsize);
        { Add all units to unitmap }
        hp:=tmodule(loaded_units.first);
        if hp=nil then exit;
        i:=hp.moduleid;
        while assigned(hp) do
          begin
            if hp.moduleid>=unitmapsize then
              internalerror(2005011513);
            { Verify old entries }
            if (i<oldmapsize) then
              begin
                if (hp.moduleid<>i) or
                   (unitmap[hp.moduleid].u<>hp) then
                  internalerror(200501156);
              end
            else
              begin
                unitmap[hp.moduleid].u:=hp;
                unitmap[hp.moduleid].derefidx:=-1;
              end;
            inc(i);
            hp:=tmodule(hp.next);
          end;
      end;



    function tmodule.derefidx_unit(id:longint):longint;
      begin
        if id>=unitmapsize then
          internalerror(2005011511);
        if unitmap[id].derefidx=-1 then
          begin
            unitmap[id].derefidx:=derefmapcnt;
            inc(derefmapcnt);
            derefmap[unitmap[id].derefidx].u:=unitmap[id].u;
          end;
        if unitmap[id].derefidx>=derefmapsize then
          internalerror(2005011514);
        result:=unitmap[id].derefidx;
      end;


    function tmodule.resolve_unit(id:longint):tmodule;
      var
        hp : tmodule;
      begin
        if id>=derefmapsize then
          begin
            writeln('tmodule.resolve_unit ',modulename^,' ',statestr,' id=',id,' derefmapsize=',derefmapsize);
            internalerror(200306231);
          end;
        result:=derefmap[id].u;
        if not assigned(result) then
          begin
            if not assigned(derefmap[id].modulename) or
               (derefmap[id].modulename^='') then
              internalerror(200501159);
            hp:=tmodule(loaded_units.first);
            while assigned(hp) do
              begin
                { only check for units. The main program is also
                  as a unit in the loaded_units list. We simply need
                  to ignore this entry (PFV) }
                if hp.is_unit and
                   (hp.modulename^=derefmap[id].modulename^) then
                  break;
                hp:=tmodule(hp.next);
              end;
            if not assigned(hp) then
              internalerror(2005011510);
            derefmap[id].u:=hp;
            result:=hp;
          end;
      end;


    procedure tmodule.allunitsused;
      var
        pu : tused_unit;
      begin
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
          begin
            if assigned(pu.u.globalsymtable) then
              begin
                if unitmap[pu.u.moduleid].u<>pu.u then
                  internalerror(200501157);
                { Give a note when the unit is not referenced, skip
                  this is for units with an initialization/finalization }
                if (unitmap[pu.u.moduleid].refs=0) and
                   pu.in_uses and
                   ((pu.u.moduleflags * [mf_init,mf_finalize])=[]) then
                  CGMessagePos2(pu.unitsym.fileinfo,sym_n_unit_not_used,pu.u.realmodulename^,realmodulename^);
              end;
            pu:=tused_unit(pu.next);
          end;
      end;

    procedure tmodule.end_of_parsing;
      begin
        { free asmdata }
        if assigned(asmdata) then
          begin
            asmdata.free;
            asmdata:=nil;
          end;

        { free scanner }
        if assigned(scanner) then
          begin
            if current_scanner=tscannerfile(scanner) then
              set_current_scanner(nil);
            FreeAndNil(scanner);
            scanner:=nil;
          end;

        { free symtable stack }
        if assigned(compiler.symtablestack) then
          begin
            compiler.symtablestack.free;
            tcompiler(compiler).symtablestack:=nil;
          end;
        if assigned(compiler.macrosymtablestack) then
          begin
            compiler.macrosymtablestack.free;
            tcompiler(compiler).macrosymtablestack:=nil;
          end;
        waitingforunit.free;
        waitingforunit:=nil;
        localmacrosymtable.free;
        localmacrosymtable:=nil;
        ptrdefs.free;
        ptrdefs:=nil;
        arraydefs.free;
        arraydefs:=nil;
        procaddrdefs.free;
        procaddrdefs:=nil;
{$ifdef llvm}
        llvmdefs.free;
        llvmdefs:=nil;
{$endif llvm}
        checkforwarddefs.free;
        checkforwarddefs:=nil;
        tcinitcode.free;
        tcinitcode:=nil;
        localunitsearchpath.free;
        localunitsearchpath:=nil;
        localobjectsearchpath.free;
        localobjectsearchpath:=nil;
        localincludesearchpath.free;
        localincludesearchpath:=nil;
        locallibrarysearchpath.free;
        locallibrarysearchpath:=nil;
        localframeworksearchpath.free;
        localframeworksearchpath:=nil;
      end;


    procedure tmodule.setmodulename(const s:string);
      begin
        stringdispose(modulename);
        stringdispose(realmodulename);
        modulename:=stringdup(upper(s));
        realmodulename:=stringdup(s);
        { also update asmlibrary names }
        current_asmdata.name:=modulename;
      end;


    procedure tmodule.AddExternalImport(const libname, symname, symmangledname: string; OrdNr: longint; isvar: boolean;
      ImportByOrdinalOnly: boolean);
      var
        ImportLibrary,OtherIL : TImportLibrary;
        ImportSymbol  : TImportSymbol;
        i : longint;
      begin
        ImportLibrary:=TImportLibrary(ImportLibraryList.Find(libname));
        if not assigned(ImportLibrary) then
          ImportLibrary:=TImportLibrary.Create(ImportLibraryList,libname);
        ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList.Find(symname));
        if not assigned(ImportSymbol) then
          begin
            { Check that the same name does not exist in another library }
            { If it does and the same mangled name is used, issue a warning }
            if ImportLibraryList.Count>1 then
              for i:=0 To ImportLibraryList.Count-1 do
                begin
                  OtherIL:=TImportLibrary(ImportLibraryList.Items[i]);
                  ImportSymbol:=TImportSymbol(OtherIL.ImportSymbolList.Find(symname));
                  if assigned(ImportSymbol) then
                    begin
                      if ImportSymbol.MangledName=symmangledname then
                        begin
                          CGMessage3(sym_w_library_overload,symname,libname,OtherIL.Name);
                          break;
                        end;
                    end;
                end;
            if not ImportByOrdinalOnly then
              { negative ordinal number indicates import by name with ordinal number as hint }
              OrdNr:=-OrdNr;
            ImportSymbol:=TImportSymbol.Create(ImportLibrary.ImportSymbolList,
              symname,symmangledname,OrdNr,isvar);
          end;
      end;


    procedure tmodule.add_public_asmsym(sym:TAsmSymbol);
      begin
        add_public_asmsym(sym.name,sym.bind,sym.typ);
      end;


    procedure tmodule.add_public_asmsym(const name:TSymStr;bind:TAsmsymbind;typ:Tasmsymtype);
      var
        sym : tasmsymbol;
      begin
        { ToDo: check for AB_GLOBAL, AB_EXTERNAL? }
        sym:=tasmsymbol(publicasmsyms.find(name));
        if assigned(sym) then
          begin
            if (sym.bind<>bind) or (sym.typ<>typ) then
              internalerror(2016070101);
            exit;
          end;
        tasmsymbol.create(publicasmsyms,name,bind,typ);
      end;


    procedure tmodule.add_extern_asmsym(sym:TAsmSymbol);
      begin
        add_extern_asmsym(sym.name,sym.bind,sym.typ);
      end;


    procedure tmodule.add_extern_asmsym(const name:TSymStr;bind:TAsmsymbind;typ:Tasmsymtype);
      var
        sym : tasmsymbol;
      begin
        { ToDo: check for AB_EXTERNAL? }
        sym:=tasmsymbol(externasmsyms.find(name));
        if assigned(sym) then
          begin
            if (sym.bind<>bind) or (sym.typ<>typ) then
              internalerror(2016070102);
            exit;
          end;
        tasmsymbol.create(externasmsyms,name,bind,typ);
      end;

    procedure tmodule.remove_waitforunit_cycles;

        function search(m: tmodule): boolean;
        var
          i: Integer;
        begin
          Result:=false;
          if m=self then exit(true);
          if m.cycle_search_stamp=tmodule.cycle_stamp then exit;
          m.cycle_search_stamp:=tmodule.cycle_stamp;
          if not Assigned(m.waitingunits) then exit;
          for i:=m.waitingunits.Count-1 downto 0 do
            if search(tmodule(m.waitingunits[i])) then
              exit(true);
        end;

      var
        waitingmodule: tmodule;
        i: Integer;
      begin
        if not Assigned(waitingunits) then exit;
        tmodule.increase_cycle_stamp;
        for i:=waitingunits.Count-1 downto 0 do
          begin
            waitingmodule:=tmodule(waitingunits[i]);
            if search(waitingmodule) then
              begin
                waitingunits.delete(i);
                waitingmodule.remove_from_waitingforunits(self);
                tmodule.increase_cycle_stamp;
              end;
          end;
      end;

    procedure tmodule.remove_all_waitsforthisunit;
      var
        i: Integer;
        waitingmodule: tmodule;
      begin
        if not assigned(waitingunits) then exit;
        for i:=0 to waitingunits.count-1 do
          begin
            waitingmodule:=tmodule(waitingunits[i]);
            waitingmodule.remove_from_waitingforunits(self);
          end;
        waitingunits.Clear;
      end;

    procedure tmodule.remove_from_waitingforunits(amodule: tmodule);
    begin
      // It can be nil after when this is called after end_of_parsing was called.
      if assigned(waitingforunit) then
        waitingforunit.remove(amodule);
    end;

    function tmodule.ToString: RTLString;
      begin
        // Assigned self so we can detect nil.
        if assigned(modulename) then
          Result:='('+ModuleName^+')'
        else
         Result:='(<'+inttostr(ptrint(self))+'>)';
        // Possibly add some state ?
      end;


initialization
{$ifdef MEMDEBUG}
  memsymtable:=TMemDebug.create('Symtables');
  memsymtable.stop;
{$endif MEMDEBUG}

finalization
{$ifdef MEMDEBUG}
  memsymtable.free;
  memsymtable := nil;
{$endif MEMDEBUG}

end.
