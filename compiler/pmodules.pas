{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Handles the parsing and loading of the modules (ppufiles)

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
unit pmodules;

{$i fpcdefs.inc}

interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


implementation

    uses
       SysUtils,
       globtype,version,systems,tokens,
       cutils,cfileutils,cclasses,comphook,
       globals,verbose,fmodule,finput,fppu,
       symconst,symbase,symtype,symdef,symsym,symtable,
       aasmtai,aasmdata,aasmcpu,aasmbase,
       cgbase,cgobj,
       nbas,ncgutil,
       link,assemble,import,export,gendef,ppu,comprsrc,dbgbase,
       cresstr,procinfo,
       pexports,
       scanner,pbase,pexpr,psystem,psub,pdecsub,ptype;


    procedure create_objectfile;
      var
        DLLScanner      : TDLLScanner;
        s               : string;
        KeepShared      : TCmdStrList;
      begin
        { try to create import entries from system dlls }
        if (tf_has_dllscanner in target_info.flags) and
           (not current_module.linkOtherSharedLibs.Empty) then
         begin
           { Init DLLScanner }
           if assigned(CDLLScanner[target_info.system]) then
            DLLScanner:=CDLLScanner[target_info.system].Create
           else
            internalerror(200104121);
           KeepShared:=TCmdStrList.Create;
           { Walk all shared libs }
           While not current_module.linkOtherSharedLibs.Empty do
            begin
              S:=current_module.linkOtherSharedLibs.Getusemask(link_always);
              if not DLLScanner.scan(s) then
               KeepShared.Concat(s);
            end;
           DLLscanner.Free;
           { Recreate import section }
           if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            begin
              if assigned(current_asmdata.asmlists[al_imports]) then
               current_asmdata.asmlists[al_imports].clear
              else
               current_asmdata.asmlists[al_imports]:=TAsmList.Create;
              importlib.generatelib;
            end;
           { Readd the not processed files }
           while not KeepShared.Empty do
            begin
              s:=KeepShared.GetFirst;
              current_module.linkOtherSharedLibs.add(s,link_always);
            end;
           KeepShared.Free;
         end;

        { Start and end module debuginfo, at least required for stabs
          to insert n_sourcefile lines }
        if (cs_debuginfo in current_settings.moduleswitches) or
           (cs_use_lineinfo in current_settings.globalswitches) then
          current_debuginfo.insertmoduleinfo;

        { create the .s file and assemble it }
        GenerateAsm(false);

        { Also create a smartlinked version ? }
        if create_smartlink_library then
         begin
           GenerateAsm(true);
           if (af_needar in target_asm.flags) then
             Linker.MakeStaticLibrary;
         end;

        { resource files }
        CompileResourceFiles;
      end;


    procedure insertobjectfile;
    { Insert the used object file for this unit in the used list for this unit }
      begin
        current_module.linkunitofiles.add(current_module.objfilename^,link_static);
        current_module.flags:=current_module.flags or uf_static_linked;

        if create_smartlink_library then
         begin
           current_module.linkunitstaticlibs.add(current_module.staticlibfilename^,link_smart);
           current_module.flags:=current_module.flags or uf_smart_linked;
         end;
      end;


    procedure create_dwarf;
      begin
        { Dwarf conflicts with smartlinking in separate .a files }
        if create_smartlink_library then
          exit;
        { Call frame information }
        if (tf_needs_dwarf_cfi in target_info.flags) and
           (af_supports_dwarf in target_asm.flags) then
          begin
            current_asmdata.asmlists[al_dwarf]:=TAsmList.create;
            current_asmdata.asmcfi.generate_code(current_asmdata.asmlists[al_dwarf]);
          end;
      end;


    procedure InsertThreadvarTablesTable;
      var
        hp : tused_unit;
        ltvTables : TAsmList;
        count : longint;
      begin
        if (tf_section_threadvars in target_info.flags) then
          exit;
        ltvTables:=TAsmList.Create;
        count:=0;
        hp:=tused_unit(usedunits.first);
        while assigned(hp) do
         begin
           If (hp.u.flags and uf_threadvars)=uf_threadvars then
            begin
              ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',hp.u.globalsymtable,''),0));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program threadvars, if any }
        If (current_module.flags and uf_threadvars)=uf_threadvars then
         begin
           ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',current_module.localsymtable,''),0));
           inc(count);
         end;
        { Insert TableCount at start }
        ltvTables.insert(Tai_const.Create_32bit(count));
        { insert in data segment }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_data,'FPC_THREADVARTABLES',sizeof(aint));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('FPC_THREADVARTABLES',AT_DATA,0));
        current_asmdata.asmlists[al_globals].concatlist(ltvTables);
        current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname('FPC_THREADVARTABLES'));
        ltvTables.free;
      end;

    procedure AddToThreadvarList(p:TObject;arg:pointer);
      var
        ltvTable : TAsmList;
      begin
        ltvTable:=TAsmList(arg);
        if (tsym(p).typ=staticvarsym) and
           (vo_is_thread_var in tstaticvarsym(p).varoptions) then
         begin
           { address of threadvar }
           ltvTable.concat(tai_const.Createname(tstaticvarsym(p).mangledname,0));
           { size of threadvar }
           ltvTable.concat(tai_const.create_32bit(tstaticvarsym(p).getsize));
         end;
      end;


    procedure InsertThreadvars;
      var
        s : string;
        ltvTable : TAsmList;
      begin
         if (tf_section_threadvars in target_info.flags) then
           exit;
         ltvTable:=TAsmList.create;
         if assigned(current_module.globalsymtable) then
           current_module.globalsymtable.SymList.ForEachCall(@AddToThreadvarList,ltvTable);
         current_module.localsymtable.SymList.ForEachCall(@AddToThreadvarList,ltvTable);
         if ltvTable.first<>nil then
          begin
            s:=make_mangledname('THREADVARLIST',current_module.localsymtable,'');
            { end of the list marker }
            ltvTable.concat(tai_const.create_sym(nil));
            { add to datasegment }
            maybe_new_object_file(current_asmdata.asmlists[al_globals]);
            new_section(current_asmdata.asmlists[al_globals],sec_data,s,sizeof(aint));
            current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
            current_asmdata.asmlists[al_globals].concatlist(ltvTable);
            current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(s));
            current_module.flags:=current_module.flags or uf_threadvars;
          end;
         ltvTable.Free;
      end;


    Procedure InsertResourceInfo;

    var
      hp           : tused_unit;
      found        : Boolean;
      I            : Integer;
      ResourceInfo : TAsmList;

    begin
      if target_res.id=res_elf then
        begin
        hp:=tused_unit(usedunits.first);
        found:=false;
        Found:=((current_module.flags and uf_has_resourcefiles)=uf_has_resourcefiles);
        If not found then
          While Assigned(hp) and not Found do
            begin
            Found:=((hp.u.flags and uf_has_resourcefiles)=uf_has_resourcefiles);
            hp:=tused_unit(hp.next);
            end;
        ResourceInfo:=TAsmList.Create;
        if found then
          begin
          { Valid pointer to resource information }
          ResourceInfo.concat(Tai_symbol.Createname_global('FPC_RESLOCATION',AT_DATA,0));
          ResourceInfo.concat(Tai_const.Createname('FPC_RESSYMBOL',0));
{$ifdef EXTERNALRESPTRS}
          current_module.linkotherofiles.add('resptrs.o',link_always);
{$else EXTERNALRESPTRS}
          new_section(ResourceInfo,sec_fpc,'resptrs',4);
          ResourceInfo.concat(Tai_symbol.Createname_global('FPC_RESSYMBOL',AT_DATA,0));
          For I:=1 to 32 do
            ResourceInfo.Concat(Tai_const.Create_32bit(0));
{$endif EXTERNALRESPTRS}
          end
        else
          begin
          { Nil pointer to resource information }
          ResourceInfo.concat(Tai_symbol.Createname_global('FPC_RESLOCATION',AT_DATA,0));
          ResourceInfo.Concat(Tai_const.Create_32bit(0));
          end;
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        current_asmdata.asmlists[al_globals].concatlist(ResourceInfo);
        ResourceInfo.free;
        end;
    end;


    Procedure InsertResourceTablesTable;
      var
        hp : tmodule;
        ResourceStringTables : tasmlist;
        count : longint;
      begin
        ResourceStringTables:=tasmlist.Create;
        count:=0;
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            If (hp.flags and uf_has_resourcestrings)=uf_has_resourcestrings then
              begin
                ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESSTR',hp.localsymtable,'START'),0));
                ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESSTR',hp.localsymtable,'END'),0));
                inc(count);
              end;
            hp:=tmodule(hp.next);
          end;
        { Insert TableCount at start }
        ResourceStringTables.insert(Tai_const.Create_aint(count));
        { Add to data segment }
        maybe_new_object_file(current_asmdata.AsmLists[al_globals]);
        new_section(current_asmdata.AsmLists[al_globals],sec_data,'FPC_RESOURCESTRINGTABLES',sizeof(aint));
        current_asmdata.AsmLists[al_globals].concat(Tai_symbol.Createname_global('FPC_RESOURCESTRINGTABLES',AT_DATA,0));
        current_asmdata.AsmLists[al_globals].concatlist(ResourceStringTables);
        current_asmdata.AsmLists[al_globals].concat(Tai_symbol_end.Createname('FPC_RESOURCESTRINGTABLES'));
        ResourceStringTables.free;
      end;


    procedure InsertInitFinalTable;
      var
        hp : tused_unit;
        unitinits : TAsmList;
        count : longint;
      begin
        unitinits:=TAsmList.Create;
        count:=0;
        hp:=tused_unit(usedunits.first);
        while assigned(hp) do
         begin
           { call the unit init code and make it external }
           if (hp.u.flags and (uf_init or uf_finalize))<>0 then
            begin
              if (hp.u.flags and uf_init)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',hp.u.globalsymtable,''),0))
              else
               unitinits.concat(Tai_const.Create_sym(nil));
              if (hp.u.flags and uf_finalize)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',hp.u.globalsymtable,''),0))
              else
               unitinits.concat(Tai_const.Create_sym(nil));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Insert initialization/finalization of the program }
        if (current_module.flags and (uf_init or uf_finalize))<>0 then
         begin
           if (current_module.flags and uf_init)<>0 then
            unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',current_module.localsymtable,''),0))
           else
            unitinits.concat(Tai_const.Create_sym(nil));
           if (current_module.flags and uf_finalize)<>0 then
            unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',current_module.localsymtable,''),0))
           else
            unitinits.concat(Tai_const.Create_sym(nil));
           inc(count);
         end;
        { Insert TableCount,InitCount at start }
        unitinits.insert(Tai_const.Create_32bit(0));
        unitinits.insert(Tai_const.Create_32bit(count));
        { Add to data segment }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_data,'INITFINAL',sizeof(aint));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('INITFINAL',AT_DATA,0));
        current_asmdata.asmlists[al_globals].concatlist(unitinits);
        current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname('INITFINAL'));
        unitinits.free;
      end;


    procedure insertmemorysizes;
{$IFDEF POWERPC}
      var
        stkcookie: string;
{$ENDIF POWERPC}
      begin
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        { Insert Ident of the compiler in the .fpc.version section }
        current_asmdata.asmlists[al_globals].concat(Tai_section.create(sec_fpc,'version',0));
        current_asmdata.asmlists[al_globals].concat(Tai_align.Create(const_align(32)));
        current_asmdata.asmlists[al_globals].concat(Tai_string.Create('FPC '+full_version_string+
          ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname));
        if not(tf_no_generic_stackcheck in target_info.flags) then
          begin
            { stacksize can be specified and is now simulated }
            new_section(current_asmdata.asmlists[al_globals],sec_data,'__stklen', sizeof(aint));
            current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__stklen',AT_DATA,sizeof(aint)));
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(stacksize));
          end;
{$IFDEF POWERPC}
        { AmigaOS4 "stack cookie" support }
        if ( target_info.system = system_powerpc_amiga ) then
         begin
           { this symbol is needed to ignite powerpc amigaos' }
           { stack allocation magic for us with the given stack size. }
           { note: won't work for m68k amigaos or morphos. (KB) }
           str(stacksize,stkcookie);
           stkcookie:='$STACK: '+stkcookie+#0;
           maybe_new_object_file(current_asmdata.asmlists[al_globals]);
           new_section(current_asmdata.asmlists[al_globals],sec_data,'__stack_cookie',length(stkcookie));
           current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__stack_cookie',AT_DATA,length(stkcookie)));
           current_asmdata.asmlists[al_globals].concat(Tai_string.Create(stkcookie));
         end;
{$ENDIF POWERPC}
        { Initial heapsize }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_data,'__heapsize',sizeof(aint));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__heapsize',AT_DATA,sizeof(aint)));
        current_asmdata.asmlists[al_globals].concat(Tai_const.Create_aint(heapsize));
        { Initial heapsize }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_data,'__fpc_valgrind',sizeof(boolean));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__fpc_valgrind',AT_DATA,sizeof(boolean)));
        current_asmdata.asmlists[al_globals].concat(Tai_const.create_8bit(byte(cs_gdb_valgrind in current_settings.globalswitches)));
      end;


    procedure AddUnit(const s:string);
      var
        hp : tppumodule;
        unitsym : tunitsym;
      begin
        { load unit }
        hp:=registerunit(current_module,s,'');
        hp.loadppu;
        hp.adddependency(current_module);
        { add to symtable stack }
        symtablestack.push(hp.globalsymtable);
        if (m_mac in current_settings.modeswitches) and
           assigned(hp.globalmacrosymtable) then
          macrosymtablestack.push(hp.globalmacrosymtable);
        { insert unitsym }
        unitsym:=tunitsym.create(s,hp);
        inc(unitsym.refs);
        current_module.localsymtable.insert(unitsym);
        { add to used units }
        current_module.addusedunit(hp,false,unitsym);
      end;


    procedure maybeloadvariantsunit;
      var
        hp : tmodule;
      begin
        { Do we need the variants unit? Skip this
          for VarUtils unit for bootstrapping }
        if (current_module.flags and uf_uses_variants=0) or
           (current_module.modulename^='VARUTILS') then
          exit;
        { Variants unit already loaded? }
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            if hp.modulename^='VARIANTS' then
              exit;
            hp:=tmodule(hp.next);
          end;
        { Variants unit is not loaded yet, load it now }
        Message(parser_w_implicit_uses_of_variants_unit);
        AddUnit('variants');
      end;


    procedure loaddefaultunits;
      begin
        { we are going to rebuild the symtablestack, clear it first }
        symtablestack.clear;
        macrosymtablestack.clear;

        { macro symtable }
        macrosymtablestack.push(initialmacrosymtable);

        { are we compiling the system unit? }
        if (cs_compilesystem in current_settings.moduleswitches) then
         begin
           systemunit:=tglobalsymtable(current_module.localsymtable);
           { create system defines }
           create_intern_symbols;
           create_intern_types;
           { Set the owner of errorsym and errortype to symtable to
             prevent crashes when accessing .owner }
           generrorsym.owner:=systemunit;
           generrordef.owner:=systemunit;
           exit;
         end;

        { insert the system unit, it is allways the first. Load also the
          internal types from the system unit }
        AddUnit('system');
        systemunit:=tglobalsymtable(symtablestack.top);
        load_intern_types;

        { Set the owner of errorsym and errortype to symtable to
          prevent crashes when accessing .owner }
        generrorsym.owner:=systemunit;
        generrordef.owner:=systemunit;

        { Units only required for main module }
        if not(current_module.is_unit) then
         begin
           { Heaptrc unit, load heaptrace before any other units especially objpas }
           if (cs_use_heaptrc in current_settings.globalswitches) then
             AddUnit('heaptrc');
           { Lineinfo unit }
           if (cs_use_lineinfo in current_settings.globalswitches) then begin
             if (paratargetdbg = dbg_stabs) then
               AddUnit('lineinfo')
             else
               AddUnit('lnfodwrf');
           end;
           { Valgrind requires c memory manager }
           if (cs_gdb_valgrind in current_settings.globalswitches) then
             AddUnit('cmem');
{$ifdef cpufpemu}
           { Floating point emulation unit?
             softfpu must be in the system unit anyways (FK)
           if (cs_fp_emulation in current_settings.moduleswitches) and not(target_info.system in system_wince) then
             AddUnit('softfpu');
           }
{$endif cpufpemu}
         end;
        { Objpas unit? }
        if m_objpas in current_settings.modeswitches then
          AddUnit('objpas');
        { Macpas unit? }
        if m_mac in current_settings.modeswitches then
          AddUnit('macpas');
        { Profile unit? Needed for go32v2 only }
        if (cs_profile in current_settings.moduleswitches) and
           (target_info.system in [system_i386_go32v2,system_i386_watcom]) then
          AddUnit('profile');
        if (cs_load_fpcylix_unit in current_settings.globalswitches) then
          begin
            AddUnit('fpcylix');
            AddUnit('dynlibs');
          end;
      end;


    procedure loadautounits;
      var
        hs,s : string;
      begin
        hs:=autoloadunits;
        repeat
          s:=GetToken(hs,',');
          if s='' then
            break;
          AddUnit(s);
        until false;
      end;


    procedure loadunits;
      var
         s,sorg  : TIDString;
         fn      : string;
         pu      : tused_unit;
         hp2     : tmodule;
         unitsym : tunitsym;
      begin
         consume(_USES);
         repeat
           s:=pattern;
           sorg:=orgpattern;
           consume(_ID);
           { support "<unit> in '<file>'" construct, but not for tp7 }
           fn:='';
           if not(m_tp7 in current_settings.modeswitches) and
              try_to_consume(_OP_IN) then
             fn:=FixFileName(get_stringconst);
           { Give a warning if lineinfo is loaded }
           if s='LINEINFO' then begin
            Message(parser_w_no_lineinfo_use_switch);
            if (paratargetdbg in [dbg_dwarf2, dbg_dwarf3]) then
              s := 'LNFODWRF';
            sorg := s;
           end;
           { Give a warning if objpas is loaded }
           if s='OBJPAS' then
            Message(parser_w_no_objpas_use_mode);
           { Using the unit itself is not possible }
           if (s<>current_module.modulename^) then
            begin
              { check if the unit is already used }
              hp2:=nil;
              pu:=tused_unit(current_module.used_units.first);
              while assigned(pu) do
               begin
                 if (pu.u.modulename^=s) then
                  begin
                    hp2:=pu.u;
                    break;
                  end;
                 pu:=tused_unit(pu.next);
               end;
              if not assigned(hp2) then
                hp2:=registerunit(current_module,sorg,fn)
              else
                Message1(sym_e_duplicate_id,s);
              { Create unitsym, we need to use the name as specified, we
                can not use the modulename because that can be different
                when -Un is used }
              unitsym:=tunitsym.create(sorg,nil);
              current_module.localsymtable.insert(unitsym);
              { the current module uses the unit hp2 }
              current_module.addusedunit(hp2,true,unitsym);
            end
           else
            Message1(sym_e_duplicate_id,s);
           if token=_COMMA then
            begin
              pattern:='';
              consume(_COMMA);
            end
           else
            break;
         until false;

         { Load the units }
         pu:=tused_unit(current_module.used_units.first);
         while assigned(pu) do
          begin
            { Only load the units that are in the current
              (interface/implementation) uses clause }
            if pu.in_uses and
               (pu.in_interface=current_module.in_interface) then
             begin
               tppumodule(pu.u).loadppu;
               { is our module compiled? then we can stop }
               if current_module.state=ms_compiled then
                exit;
               { add this unit to the dependencies }
               pu.u.adddependency(current_module);
               { save crc values }
               pu.checksum:=pu.u.crc;
               pu.interface_checksum:=pu.u.interface_crc;
               { connect unitsym to the module }
               pu.unitsym.module:=pu.u;
               { add to symtable stack }
               symtablestack.push(pu.u.globalsymtable);
               if (m_mac in current_settings.modeswitches) and
                  assigned(pu.u.globalmacrosymtable) then
                 macrosymtablestack.push(pu.u.globalmacrosymtable);
             end;
            pu:=tused_unit(pu.next);
          end;

         consume(_SEMICOLON);
      end;


     procedure reset_all_defs;

       procedure reset_used_unit_defs(hp:tmodule);
         var
           pu : tused_unit;
         begin
           pu:=tused_unit(hp.used_units.first);
           while assigned(pu) do
             begin
               if not pu.u.is_reset then
                 begin
                   { prevent infinte loop for circular dependencies }
                   pu.u.is_reset:=true;
                   if assigned(pu.u.globalsymtable) then
                     begin
                       tglobalsymtable(pu.u.globalsymtable).reset_all_defs;
                       reset_used_unit_defs(pu.u);
                     end;
                 end;
               pu:=tused_unit(pu.next);
             end;
         end;

       var
         hp2 : tmodule;
       begin
         hp2:=tmodule(loaded_units.first);
         while assigned(hp2) do
           begin
             hp2.is_reset:=false;
             hp2:=tmodule(hp2.next);
           end;
         reset_used_unit_defs(current_module);
       end;


    procedure free_localsymtables(st:TSymtable);
      var
        i   : longint;
        def : tstoreddef;
        pd  : tprocdef;
      begin
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tstoreddef(st.DefList[i]);
            if def.typ=procdef then
              begin
                pd:=tprocdef(def);
                if assigned(pd.localst) and
                   (pd.localst.symtabletype<>staticsymtable) and
                   not(po_inline in pd.procoptions) then
                  begin
                    free_localsymtables(pd.localst);
                    pd.localst.free;
                    pd.localst:=nil;
                  end;
              end;
          end;
      end;


    procedure parse_implementation_uses;
      begin
         if token=_USES then
           loadunits;
      end;


    procedure setupglobalswitches;
      begin
        if (cs_create_pic in current_settings.moduleswitches) then
          begin
            def_system_macro('FPC_PIC');
            def_system_macro('PIC');
          end;
      end;


    function create_main_proc(const name:string;potype:tproctypeoption;st:TSymtable):tcgprocinfo;
      var
        ps  : tprocsym;
        pd  : tprocdef;
      begin
        { there should be no current_procinfo available }
        if assigned(current_procinfo) then
         internalerror(200304275);
        {Generate a procsym for main}
        ps:=tprocsym.create('$'+name);
        { main are allways used }
        inc(ps.refs);
        st.insert(ps);
        pd:=tprocdef.create(main_program_level);
        include(pd.procoptions,po_global);
        pd.procsym:=ps;
        ps.ProcdefList.Add(pd);
        { set procdef options }
        pd.proctypeoption:=potype;
        pd.proccalloption:=pocall_default;
        include(pd.procoptions,po_hascallingconvention);
        pd.forwarddef:=false;
        pd.setmangledname(target_info.cprefix+name);
        pd.aliasnames.insert(pd.mangledname);
        handle_calling_convention(pd);
        { We don't need is a local symtable. Change it into the static
          symtable }
        pd.localst.free;
        pd.localst:=st;
        { set procinfo and current_procinfo.procdef }
        result:=tcgprocinfo(cprocinfo.create(nil));
        result.procdef:=pd;
        { main proc does always a call e.g. to init system unit }
        include(result.flags,pi_do_call);
      end;


    procedure release_main_proc(pi:tcgprocinfo);
      begin
        { remove localst as it was replaced by staticsymtable }
        pi.procdef.localst:=nil;
        { remove procinfo }
        current_module.procinfo:=nil;
        pi.free;
        pi:=nil;
      end;


    function gen_implicit_initfinal(flag:word;st:TSymtable):tcgprocinfo;
      begin
        { update module flags }
        current_module.flags:=current_module.flags or flag;
        { create procdef }
        case flag of
          uf_init :
            begin
              result:=create_main_proc(make_mangledname('',current_module.localsymtable,'init_implicit'),potype_unitinit,st);
              result.procdef.aliasnames.insert(make_mangledname('INIT$',current_module.localsymtable,''));
            end;
          uf_finalize :
            begin
              result:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize_implicit'),potype_unitfinalize,st);
              result.procdef.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
              if (not current_module.is_unit) then
                result.procdef.aliasnames.insert('PASCALFINALIZE');
            end;
          else
            internalerror(200304253);
        end;
        result.code:=cnothingnode.create;
      end;


    procedure copy_macro(p:TObject; arg:pointer);
      begin
        current_module.globalmacrosymtable.insert(tmacro(p).getcopy);
      end;


    procedure proc_unit;

      function is_assembler_generated:boolean;
      var
        hal : tasmlisttype;
      begin
        result:=false;
        if Errorcount=0 then
          begin
            for hal:=low(TasmlistType) to high(TasmlistType) do
              if not current_asmdata.asmlists[hal].empty then
                begin
                  result:=true;
                  exit;
                end;
          end;
      end;

      var
         main_file: tinputfile;
{$ifdef EXTDEBUG}
         store_crc,
{$endif EXTDEBUG}
         store_interface_crc : cardinal;
         s1,s2  : ^string; {Saves stack space}
         force_init_final : boolean;
         init_procinfo,
         finalize_procinfo : tcgprocinfo;
         unitname8 : string[8];
         has_impl,ag: boolean;
{$ifdef i386}
         gotvarsym : tstaticvarsym;
{$endif i386}
      begin
         init_procinfo:=nil;
         finalize_procinfo:=nil;

         if m_mac in current_settings.modeswitches then
           current_module.mode_switch_allowed:= false;

         consume(_UNIT);
         if compile_level=1 then
          Status.IsExe:=false;

         if token=_ID then
          begin
             { create filenames and unit name }
             main_file := current_scanner.inputfile;
             while assigned(main_file.next) do
               main_file := main_file.next;

             new(s1);
             s1^:=current_module.modulename^;
             current_module.SetFileName(main_file.path^+main_file.name^,true);
             current_module.SetModuleName(orgpattern);

             { check for system unit }
             new(s2);
             s2^:=upper(ChangeFileExt(ExtractFileName(main_file.name^),''));
             unitname8:=copy(current_module.modulename^,1,8);
             if (cs_check_unit_name in current_settings.globalswitches) and
                (
                 not(
                     (current_module.modulename^=s2^) or
                     (
                      (length(current_module.modulename^)>8) and
                      (unitname8=s2^)
                     )
                    )
                 or
                 (
                  (length(s1^)>8) and
                  (s1^<>current_module.modulename^)
                 )
                ) then
              Message1(unit_e_illegal_unit_name,current_module.realmodulename^);
             if (current_module.modulename^='SYSTEM') then
              include(current_settings.moduleswitches,cs_compilesystem);
             dispose(s2);
             dispose(s1);
          end;

         if (target_info.system in system_unit_program_exports) then
           exportlib.preparelib(current_module.realmodulename^);

         consume(_ID);
         consume(_SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { handle the global switches }
         setupglobalswitches;

         message1(unit_u_loading_interface_units,current_module.modulename^);

         { update status }
         status.currentmodule:=current_module.realmodulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (current_module.modulename^='OBJPAS') then
           exclude(current_settings.modeswitches,m_objpas);

         { maybe turn off m_mac if we are compiling macpas }
         if (current_module.modulename^='MACPAS') then
           exclude(current_settings.modeswitches,m_mac);

         parse_only:=true;

         { generate now the global symboltable,
           define first as local to overcome dependency conflicts }
         current_module.localsymtable:=tglobalsymtable.create(current_module.modulename^,current_module.moduleid);

         { insert unitsym of this unit to prevent other units having
           the same name }
         current_module.localsymtable.insert(tunitsym.create(current_module.realmodulename^,current_module));

         { load default units, like the system unit }
         loaddefaultunits;

         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in current_settings.moduleswitches) and
            (token=_USES) then
           begin
             loadunits;
             { has it been compiled at a higher level ?}
             if current_module.state=ms_compiled then
               exit;
           end;

         { move the global symtable from the temporary local to global }
         current_module.globalsymtable:=current_module.localsymtable;
         current_module.localsymtable:=nil;

         reset_all_defs;

         { number all units, so we know if a unit is used by this unit or
           needs to be added implicitly }
         current_module.updatemaps;

         { ... parse the declarations }
         Message1(parser_u_parsing_interface,current_module.realmodulename^);
         symtablestack.push(current_module.globalsymtable);
         read_interface_declarations;
         symtablestack.pop(current_module.globalsymtable);

         { Export macros defined in the interface for macpas. The macros
           are put in the globalmacrosymtable that will only be used by other
           units. The current unit continues to use the localmacrosymtable }
         if (m_mac in current_settings.modeswitches) then
          begin
            current_module.globalmacrosymtable:=tmacrosymtable.create(true);
            current_module.localmacrosymtable.SymList.ForEachCall(@copy_macro,nil);
          end;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         { Our interface is compiled, generate CRC and switch to implementation }
         if not(cs_compilesystem in current_settings.moduleswitches) and
            (Errorcount=0) then
           tppumodule(current_module).getppucrc;
         current_module.in_interface:=false;
         current_module.interface_compiled:=true;

         { First reload all units depending on our interface, we need to do this
           in the implementation part to prevent errorneous circular references }
         reload_flagged_units;

         { Parse the implementation section }
         if (m_mac in current_settings.modeswitches) and try_to_consume(_END) then
           has_impl:= false
         else
           has_impl:= true;

         parse_only:=false;

         { generates static symbol table }
         current_module.localsymtable:=tstaticsymtable.create(current_module.modulename^,current_module.moduleid);

{$ifdef i386}
         if cs_create_pic in current_settings.moduleswitches then
           begin
             { insert symbol for got access in assembler code}
             gotvarsym:=tstaticvarsym.create('_GLOBAL_OFFSET_TABLE_',vs_value,voidpointertype,[vo_is_external]);
             gotvarsym.set_mangledname('_GLOBAL_OFFSET_TABLE_');
             current_module.localsymtable.insert(gotvarsym);
             { avoid unnecessary warnings }
             gotvarsym.varstate:=vs_read;
             gotvarsym.refs:=1;
           end;
{$endif i386}

         if has_impl then
           begin
             consume(_IMPLEMENTATION);
             Message1(unit_u_loading_implementation_units,current_module.modulename^);
             { Read the implementation units }
             parse_implementation_uses;
           end;

         if current_module.state=ms_compiled then
           exit;

         { reset ranges/stabs in exported definitions }
         reset_all_defs;

         { All units are read, now give them a number }
         current_module.updatemaps;

         symtablestack.push(current_module.globalsymtable);
         symtablestack.push(current_module.localsymtable);

         if has_impl then
           begin
             Message1(parser_u_parsing_implementation,current_module.modulename^);
             if current_module.in_interface then
               internalerror(200212285);

             { Compile the unit }
             init_procinfo:=create_main_proc(make_mangledname('',current_module.localsymtable,'init'),potype_unitinit,current_module.localsymtable);
             init_procinfo.procdef.aliasnames.insert(make_mangledname('INIT$',current_module.localsymtable,''));
             init_procinfo.parse_body;
             { save file pos for debuginfo }
             current_module.mainfilepos:=init_procinfo.entrypos;
           end;

         { Generate specializations of objectdefs methods }
         generate_specialization_procs;

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=tglobalsymtable(current_module.globalsymtable).needs_init_final or
                           tstaticsymtable(current_module.localsymtable).needs_init_final;

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         if force_init_final and ((current_module.flags and uf_init)=0) then
           begin
             { first release the not used init procinfo }
             if assigned(init_procinfo) then
               release_main_proc(init_procinfo);
             init_procinfo:=gen_implicit_initfinal(uf_init,current_module.localsymtable);
           end;
         { finalize? }
         if has_impl and (token=_FINALIZATION) then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              finalize_procinfo:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize'),potype_unitfinalize,current_module.localsymtable);
              finalize_procinfo.procdef.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
              finalize_procinfo.parse_body;
           end
         else if force_init_final then
           finalize_procinfo:=gen_implicit_initfinal(uf_finalize,current_module.localsymtable);

         { Now both init and finalize bodies are read and it is known
           which variables are used in both init and finalize we can now
           generate the code. This is required to prevent putting a variable in
           a register that is also used in the finalize body (PFV) }
         if assigned(init_procinfo) then
           begin
             init_procinfo.generate_code;
             init_procinfo.resetprocdef;
             release_main_proc(init_procinfo);
           end;
         if assigned(finalize_procinfo) then
           begin
             finalize_procinfo.generate_code;
             finalize_procinfo.resetprocdef;
             release_main_proc(finalize_procinfo);
           end;

         symtablestack.pop(current_module.localsymtable);
         symtablestack.pop(current_module.globalsymtable);

         { the last char should always be a point }
         consume(_POINT);

         if (Errorcount=0) then
           begin
             { tests, if all (interface) forwards are resolved }
             tstoredsymtable(current_module.globalsymtable).check_forwards;
             { check if all private fields are used }
             tstoredsymtable(current_module.globalsymtable).allprivatesused;
             { remove cross unit overloads }
             tstoredsymtable(current_module.globalsymtable).unchain_overloaded;

             { test static symtable }
             tstoredsymtable(current_module.localsymtable).allsymbolsused;
             tstoredsymtable(current_module.localsymtable).allprivatesused;
             tstoredsymtable(current_module.localsymtable).check_forwards;
             tstoredsymtable(current_module.localsymtable).checklabels;
             tstoredsymtable(current_module.localsymtable).unchain_overloaded;

             { used units }
             current_module.allunitsused;
           end;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         { do we need to add the variants unit? }
         maybeloadvariantsunit;

         { generate wrappers for interfaces }
         gen_intf_wrappers(current_asmdata.asmlists[al_procedures],current_module.globalsymtable);
         gen_intf_wrappers(current_asmdata.asmlists[al_procedures],current_module.localsymtable);

         { generate pic helpers to load eip if necessary }
         gen_pic_helpers(current_asmdata.asmlists[al_procedures]);

         { generate rtti/init tables }
         write_persistent_type_info(current_module.globalsymtable);
         write_persistent_type_info(current_module.localsymtable);

         { Tables }
         insertThreadVars;

         { Resource strings }
         GenerateResourceStrings;

         { generate debuginfo }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.inserttypeinfo;

         { generate imports }
         if current_module.ImportLibraryList.Count>0 then
           importlib.generatelib;

         { insert own objectfile, or say that it's in a library
           (no check for an .o when loading) }
         ag:=is_assembler_generated;
         if ag then
           insertobjectfile
         else
           begin
             current_module.flags:=current_module.flags or uf_no_link;
             current_module.flags:=current_module.flags and not uf_has_debuginfo;
           end;

         if ag then
          begin
            { create dwarf debuginfo }
            create_dwarf;
            { assemble }
            create_objectfile;
          end;

         { Write out the ppufile after the object file has been created }
         store_interface_crc:=current_module.interface_crc;
{$ifdef EXTDEBUG}
         store_crc:=current_module.crc;
{$endif EXTDEBUG}
         if (Errorcount=0) then
           tppumodule(current_module).writeppu;

         if not(cs_compilesystem in current_settings.moduleswitches) then
           if store_interface_crc<>current_module.interface_crc then
             Message1(unit_u_interface_crc_changed,current_module.ppufilename^);
{$ifdef EXTDEBUG}
         if not(cs_compilesystem in current_settings.moduleswitches) then
           if (store_crc<>current_module.crc) and simplify_ppu then
             Message1(unit_u_implementation_crc_changed,current_module.ppufilename^);
{$endif EXTDEBUG}

         { release all overload references and local symtables that
           are not needed anymore }
         tstoredsymtable(current_module.localsymtable).unchain_overloaded;
         tstoredsymtable(current_module.globalsymtable).unchain_overloaded;
         free_localsymtables(current_module.globalsymtable);
         free_localsymtables(current_module.localsymtable);

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

        Message1(unit_u_finished_compiling,current_module.modulename^);
      end;


    procedure proc_program(islibrary : boolean);
      var
         main_file : tinputfile;
         hp,hp2    : tmodule;
         finalize_procinfo,
         init_procinfo,
         main_procinfo : tcgprocinfo;
         force_init_final : boolean;
      begin
         DLLsource:=islibrary;
         Status.IsLibrary:=IsLibrary;
         Status.IsExe:=true;
         parse_only:=false;
         main_procinfo:=nil;
         init_procinfo:=nil;
         finalize_procinfo:=nil;

         { DLL defaults to create reloc info }
         if islibrary then
           begin
             if not RelocSectionSetExplicitly then
               RelocSection:=true;
           end;

         { Relocation works only without stabs under Windows when }
         { external linker (LD) is used.  LD generates relocs for }
         { stab sections which is not loaded in memory. It causes }
         { AV error when DLL is loaded and relocation is needed.  }
         { Internal linker does not have this problem.            }
         if RelocSection and
            (target_info.system in system_all_windows+[system_i386_wdosx]) and
            (cs_link_extern in current_settings.globalswitches) then
           begin
              include(current_settings.globalswitches,cs_link_strip);
              { Warning stabs info does not work with reloc section !! }
              if cs_debuginfo in current_settings.moduleswitches then
                begin
                  Message1(parser_w_parser_reloc_no_debug,current_module.mainsource^);
                  Message(parser_w_parser_win32_debug_needs_WN);
                  exclude(current_settings.moduleswitches,cs_debuginfo);
                end;
           end;
         { get correct output names }
         main_file := current_scanner.inputfile;
         while assigned(main_file.next) do
           main_file := main_file.next;

         current_module.SetFileName(main_file.path^+main_file.name^,true);

         if islibrary then
           begin
              consume(_LIBRARY);
              current_module.setmodulename(orgpattern);
              current_module.islibrary:=true;
              exportlib.preparelib(orgpattern);

              if tf_library_needs_pic in target_info.flags then
                include(current_settings.moduleswitches,cs_create_pic);

              consume(_ID);
              consume(_SEMICOLON);
           end
         else
           { is there an program head ? }
           if token=_PROGRAM then
            begin
              consume(_PROGRAM);
              current_module.setmodulename(orgpattern);
              if (target_info.system in system_unit_program_exports) then
                exportlib.preparelib(orgpattern);
              consume(_ID);
              if token=_LKLAMMER then
                begin
                   consume(_LKLAMMER);
                   repeat
                     consume(_ID);
                   until not try_to_consume(_COMMA);
                   consume(_RKLAMMER);
                end;
              consume(_SEMICOLON);
            end
         else if (target_info.system in system_unit_program_exports) then
           exportlib.preparelib(current_module.realmodulename^);

         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { setup things using the switches }
         setupglobalswitches;

         { set implementation flag }
         current_module.in_interface:=false;
         current_module.interface_compiled:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         current_module.localsymtable:=tstaticsymtable.create(current_module.modulename^,current_module.moduleid);

         { load standard units (system,objpas,profile unit) }
         loaddefaultunits;

         { Load units provided on the command line }
         loadautounits;

         {Load the units used by the program we compile.}
         if token=_USES then
           loadunits;

         { reset ranges/stabs in exported definitions }
         reset_all_defs;

         { All units are read, now give them a number }
         current_module.updatemaps;

         {Insert the name of the main program into the symbol table.}
         if current_module.realmodulename^<>'' then
           current_module.localsymtable.insert(tunitsym.create(current_module.realmodulename^,current_module));

         Message1(parser_u_parsing_implementation,current_module.mainsource^);

         symtablestack.push(current_module.localsymtable);

         { The program intialization needs an alias, so it can be called
           from the bootstrap code.}
         if islibrary then
          begin
            main_procinfo:=create_main_proc(make_mangledname('',current_module.localsymtable,mainaliasname),potype_proginit,current_module.localsymtable);
            { Win32 startup code needs a single name }
            if not(target_info.system in systems_darwin) then
              main_procinfo.procdef.aliasnames.insert('PASCALMAIN')
            else
              main_procinfo.procdef.aliasnames.insert(target_info.Cprefix+'PASCALMAIN')
          end
         else if (target_info.system in ([system_i386_netware,system_i386_netwlibc,system_powerpc_macos]+systems_darwin)) then
           begin
             main_procinfo:=create_main_proc('PASCALMAIN',potype_proginit,current_module.localsymtable);
           end
         else
           begin
             main_procinfo:=create_main_proc(mainaliasname,potype_proginit,current_module.localsymtable);
             main_procinfo.procdef.aliasnames.insert('PASCALMAIN');
           end;
         main_procinfo.parse_body;
         { save file pos for debuginfo }
         current_module.mainfilepos:=main_procinfo.entrypos;

         { Generate specializations of objectdefs methods }
         generate_specialization_procs;

         { should we force unit initialization? }
         force_init_final:=tstaticsymtable(current_module.localsymtable).needs_init_final;
         if force_init_final then
           init_procinfo:=gen_implicit_initfinal(uf_init,current_module.localsymtable);

         { Add symbol to the exports section for win32 so smartlinking a
           DLL will include the edata section }
         if assigned(exportlib) and
            (target_info.system in [system_i386_win32,system_i386_wdosx]) and
            ((current_module.flags and uf_has_exports)<>0) then
           current_asmdata.asmlists[al_procedures].concat(tai_const.createname(make_mangledname('EDATA',current_module.localsymtable,''),0));

         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;
              { Parse the finalize }
              finalize_procinfo:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize'),potype_unitfinalize,current_module.localsymtable);
              finalize_procinfo.procdef.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
              finalize_procinfo.procdef.aliasnames.insert('PASCALFINALIZE');
              finalize_procinfo.parse_body;
           end
         else
           if force_init_final then
             finalize_procinfo:=gen_implicit_initfinal(uf_finalize,current_module.localsymtable);

          { the finalization routine of libraries is generic (and all libraries need to }
          { be finalized, so they can finalize any units they use                       }
          if (islibrary) then
            exportlib.setfininame(current_asmdata.asmlists[al_procedures],'FPC_LIB_EXIT');

         { all labels must be defined before generating code }
         if Errorcount=0 then
           tstoredsymtable(current_module.localsymtable).checklabels;

         { See remark in unit init/final }
         main_procinfo.generate_code;
         main_procinfo.resetprocdef;
         release_main_proc(main_procinfo);
         if assigned(init_procinfo) then
           begin
             init_procinfo.generate_code;
             init_procinfo.resetprocdef;
             release_main_proc(init_procinfo);
           end;
         if assigned(finalize_procinfo) then
           begin
             finalize_procinfo.generate_code;
             finalize_procinfo.resetprocdef;
             release_main_proc(finalize_procinfo);
           end;

         symtablestack.pop(current_module.localsymtable);

         { consume the last point }
         consume(_POINT);

         if (Errorcount=0) then
           begin
             { test static symtable }
             tstoredsymtable(current_module.localsymtable).allsymbolsused;
             tstoredsymtable(current_module.localsymtable).allprivatesused;
             tstoredsymtable(current_module.localsymtable).check_forwards;
             tstoredsymtable(current_module.localsymtable).unchain_overloaded;

             current_module.allunitsused;
           end;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
           begin
             Message1(unit_f_errors_in_unit,tostr(Errorcount));
             status.skip_error:=true;
             exit;
           end;

         { remove all unused units, this happends when units are removed
           from the uses clause in the source and the ppu was already being loaded }
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
          begin
            hp2:=hp;
            hp:=tmodule(hp.next);
            if hp2.is_unit and
               not assigned(hp2.globalsymtable) then
              loaded_units.remove(hp2);
          end;

         { do we need to add the variants unit? }
         maybeloadvariantsunit;

         linker.initsysinitunitname;
         if target_info.system in system_internal_sysinit then
         begin
           { add start/halt unit }
           AddUnit(linker.sysinitunit);
         end;

{$ifdef arm}
         { Insert .pdata section for arm-wince.
           It is needed for exception handling. }
         if target_info.system in [system_arm_wince] then
           InsertPData;
{$endif arm}

         InsertThreadvars;

         { generate pic helpers to load eip if necessary }
         gen_pic_helpers(current_asmdata.asmlists[al_procedures]);

         { generate rtti/init tables }
         write_persistent_type_info(current_module.localsymtable);

         { generate wrappers for interfaces }
         gen_intf_wrappers(current_asmdata.asmlists[al_procedures],current_module.localsymtable);

         { generate imports }
         if current_module.ImportLibraryList.Count>0 then
           importlib.generatelib;

         { generate debuginfo }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.inserttypeinfo;

         if islibrary or (target_info.system in system_unit_program_exports) then
           exportlib.generatelib;

         { Reference all DEBUGINFO sections from the main .fpc section }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.referencesections(current_asmdata.asmlists[al_procedures]);

         { Resource strings }
         GenerateResourceStrings;

         { insert Tables and StackLength }
         insertinitfinaltable;
         InsertThreadvarTablesTable;
         InsertResourceTablesTable;
         insertmemorysizes;

         { Insert symbol to resource info }
         InsertResourceInfo;

         { create dwarf debuginfo }
         create_dwarf;

         { insert own objectfile }
         insertobjectfile;

         { assemble and link }
         create_objectfile;

         { We might need the symbols info if not using
           the default do_extractsymbolinfo
           which is a dummy function PM }
         needsymbolinfo:=do_extractsymbolinfo<>@def_extractsymbolinfo;
         { release all local symtables that are not needed anymore }
         if (not needsymbolinfo) then
           free_localsymtables(current_module.localsymtable);

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         if (not current_module.is_unit) then
           begin
             { create the executable when we are at level 1 }
             if (compile_level=1) then
               begin
                 { create global resource file by collecting all resource files }
                 CollectResourceFiles;
                 { write .def file }
                 if (cs_link_deffile in current_settings.globalswitches) then
                  deffile.writefile;
                 { insert all .o files from all loaded units and
                   unload the units, we don't need them anymore.
                   Keep the current_module because that is still needed }
                 hp:=tmodule(loaded_units.first);
                 while assigned(hp) do
                  begin
                    linker.AddModuleFiles(hp);
                    hp2:=tmodule(hp.next);
                    if (hp<>current_module) and
                       (not needsymbolinfo) then
                      begin
                        loaded_units.remove(hp);
                        hp.free;
                      end;
                    hp:=hp2;
                  end;
                 { finally we can create a executable }
                 if DLLSource then
                   linker.MakeSharedLibrary
                 else
                   linker.MakeExecutable;
               end;

             { Give Fatal with error count for linker errors }
             if (Errorcount>0) and not status.skip_error then
              begin
                Message1(unit_f_errors_in_unit,tostr(Errorcount));
                status.skip_error:=true;
              end;
          end;
      end;

end.
