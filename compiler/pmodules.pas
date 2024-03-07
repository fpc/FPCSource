{
    Copyright (c) 1998-2008 by Florian Klaempfl

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

uses fmodule;

    function proc_unit(curr: tmodule):boolean;
    function parse_unit_interface_declarations(curr : tmodule) : boolean;
    function proc_unit_implementation(curr: tmodule):boolean;
    function proc_package(curr: tmodule) : boolean;
    function proc_program(curr: tmodule; islibrary : boolean) : boolean;
    function proc_program_declarations(curr : tmodule; islibrary : boolean) : boolean;
    procedure finish_unit(module:tmodule);

implementation

    uses
       SysUtils,
       globtype,systems,tokens,
       cutils,cfileutl,cclasses,comphook,
       globals,verbose,finput,fppu,globstat,fpcp,fpkg,
       symconst,symbase,symtype,symdef,symsym,symtable,defutil,symcreat,
       wpoinfo,
       aasmtai,aasmdata,aasmbase,aasmcpu,
       cgbase,ngenutil,
       nbas,nutils,ncgutil,
       link,assemble,import,export,gendef,ppu,comprsrc,dbgbase,
       cresstr,procinfo,
       objcgutl,
       pkgutil,
       wpobase,
       scanner,pbase,pexpr,psystem,psub,pgenutil,pparautl,ncgvmt,ncgrtti,
       ctask,
       cpuinfo;


    procedure create_objectfile(curr : tmodule);
      var
        DLLScanner      : TDLLScanner;
        s               : string;
        KeepShared      : TCmdStrList;
      begin
        { try to create import entries from system dlls }
        if (tf_has_dllscanner in target_info.flags) and
           (not curr.linkOtherSharedLibs.Empty) then
         begin
           { Init DLLScanner }
           if assigned(CDLLScanner[target_info.system]) then
            DLLScanner:=CDLLScanner[target_info.system].Create
           else
            internalerror(200104121);
           KeepShared:=TCmdStrList.Create;
           { Walk all shared libs }
           While not curr.linkOtherSharedLibs.Empty do
            begin
              S:=curr.linkOtherSharedLibs.Getusemask(link_always);
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
              curr.linkOtherSharedLibs.add(s,link_always);
            end;
           KeepShared.Free;
         end;

        { allow a target-specific pass over all assembler code (used by LLVM
          to insert type definitions }
        cnodeutils.InsertObjectInfo;

        { Start and end module debuginfo, at least required for stabs
          to insert n_sourcefile lines }
        if (cs_debuginfo in current_settings.moduleswitches) or
           (cs_use_lineinfo in current_settings.globalswitches) then
          current_debuginfo.insertmoduleinfo;

        { create the .s file and assemble it }
        if not(create_smartlink_library) or not(tf_no_objectfiles_when_smartlinking in target_info.flags) then
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


    procedure insertobjectfile(curr : tmodule);
    { Insert the used object file for this unit in the used list for this unit }
      begin
        curr.linkunitofiles.add(curr.objfilename,link_static);
        curr.headerflags:=curr.headerflags or uf_static_linked;

        if create_smartlink_library then
          begin
            curr.linkunitstaticlibs.add(curr.staticlibfilename ,link_smart);
            curr.headerflags:=curr.headerflags or uf_smart_linked;
          end;
        if cs_lto in current_settings.moduleswitches then
          begin
            curr.linkunitofiles.add(ChangeFileExt(curr.objfilename,LTOExt),link_lto);
            curr.headerflags:=curr.headerflags or uf_lto_linked;
          end;
      end;


    procedure create_dwarf_frame;
      begin
        { Dwarf conflicts with smartlinking in separate .a files }
        if create_smartlink_library then
          exit;
        { Call frame information }
        { MWE: we write our own info, so dwarf asm support is not really needed }
        { if (af_supports_dwarf in target_asm.flags) and }
        { CFI is currently broken for Darwin }
        if not(target_info.system in systems_darwin) and
           (
            (tf_needs_dwarf_cfi in target_info.flags) or
            (target_dbg.id in [dbg_dwarf2, dbg_dwarf3])
           ) then
          begin
            current_asmdata.asmlists[al_dwarf_frame].Free;
            current_asmdata.asmlists[al_dwarf_frame] := TAsmList.create;
            current_asmdata.asmcfi.generate_code(current_asmdata.asmlists[al_dwarf_frame]);
          end;
      end;

    Function CheckResourcesUsed(curr : tmodule) : boolean;
      var
        hp           : tused_unit;
        found        : Boolean;
      begin
        CheckResourcesUsed:=tf_has_winlike_resources in target_info.flags;
        if not CheckResourcesUsed then exit;

        hp:=tused_unit(usedunits.first);
        found:=mf_has_resourcefiles in curr.moduleflags;
        while Assigned(hp) and not found do
          begin
            found:=mf_has_resourcefiles in hp.u.moduleflags;
            hp:=tused_unit(hp.next);
          end;
        CheckResourcesUsed:=found;
      end;

    function AddUnit(curr : tmodule; const s:string;addasused:boolean): tppumodule;
      var
        hp : tppumodule;
        unitsym : tunitsym;
        isnew,load_ok : boolean;

      begin
        { load unit }
        hp:=registerunit(curr,s,'',isnew);
        if isnew then
          usedunits.concat(tused_unit.create(hp,true,addasused,nil));
        load_ok:=hp.loadppu(curr);
        hp.adddependency(curr,curr.in_interface);
        if not load_ok then
          { We must schedule a compile. }
          task_handler.addmodule(hp);
        { add to symtable stack }
        symtablestack.push(hp.globalsymtable);
        if (m_mac in current_settings.modeswitches) and
            assigned(hp.globalmacrosymtable) then
           macrosymtablestack.push(hp.globalmacrosymtable);
        { insert unitsym }
        unitsym:=cunitsym.create(hp.modulename^,hp);
        inc(unitsym.refs);
        tabstractunitsymtable(curr.localsymtable).insertunit(unitsym);
        if addasused then
          { add to used units }
          curr.addusedunit(hp,false,unitsym);
        result:=hp;
      end;


    function AddUnit(curr :tmodule; const s:string):tppumodule;
      begin
        result:=AddUnit(curr,s,true);
      end;


    function maybeloadvariantsunit(curr : tmodule) : boolean;
      var
        hp : tmodule;
        addsystemnamespace : Boolean;
      begin
        result:=true;
        { Do we need the variants unit? Skip this
          for VarUtils unit for bootstrapping }
        if not(mf_uses_variants in curr.moduleflags) or
           (curr.modulename^='VARUTILS') or
           (curr.modulename^='SYSTEM.VARUTILS') then
          exit;
        { Variants unit already loaded? }
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            if (hp.modulename^='VARIANTS') or (hp.modulename^='SYSTEM.VARIANTS') then
              exit;
            hp:=tmodule(hp.next);
          end;
        { Variants unit is not loaded yet, load it now }
        Message(parser_w_implicit_uses_of_variants_unit);
        addsystemnamespace:=namespacelist.Find('System')=Nil;
        if addsystemnamespace then
          namespacelist.concat('System');
        result:=AddUnit(curr,'variants').state in [ms_compiled,ms_processed];
        if addsystemnamespace then
          namespacelist.Remove('System');
      end;


    function MaybeRemoveResUnit(curr : tmodule) : boolean;
      var
        resources_used : boolean;
        hp : tmodule;
        uu : tused_unit;
        unitname : shortstring;
      begin
        { We simply remove the unit from:
           - usedunit list, so that things like init/finalization table won't
              contain references to this unit
           - loaded_units list, so that the unit object file doesn't get linked
             with the executable. }
        { Note: on windows we always need resources! }
        resources_used:=(target_info.system in systems_all_windows)
                         or CheckResourcesUsed(curr);
        if (not resources_used) and (tf_has_winlike_resources in target_info.flags) then
          begin
            { resources aren't used, so we don't need this unit }
            if target_res.id=res_ext then
              unitname:='FPEXTRES'
            else
              unitname:='FPINTRES';
            Message1(unit_u_unload_resunit,unitname);
            { find the module }
            hp:=tmodule(loaded_units.first);
            while assigned(hp) do
              begin
                if hp.is_unit and (hp.modulename^=unitname) then break;
                hp:=tmodule(hp.next);
              end;
            if not assigned(hp) then
              internalerror(200801071);
            { find its tused_unit in the global list }
            uu:=tused_unit(usedunits.first);
            while assigned(uu) do
              begin
                if uu.u=hp then break;
                uu:=tused_unit(uu.next);
              end;
            if not assigned(uu) then
              internalerror(200801072);
           { remove the tused_unit }
            usedunits.Remove(uu);
            uu.Free;
            // Remove from local list
            uu:=tused_unit(curr.used_units.first);
            while assigned(uu) do
              begin
                if uu.u=hp then break;
                uu:=tused_unit(uu.next);
              end;
            if not assigned(uu) then
              internalerror(2024020701);
            curr.used_units.Remove(uu);
            uu.Free;
           { remove the module }
            loaded_units.Remove(hp);
            unloaded_units.Concat(hp);
          end;
        MaybeRemoveResUnit:=resources_used;
      end;


    function loadsystemunit(curr : tmodule) : boolean;
      var
        state: tglobalstate;
        sys : tmodule;

      begin
        Result:=False;
        { we are going to rebuild the symtablestack, clear it first }
        symtablestack.clear;
        macrosymtablestack.clear;

        { macro symtable }
        macrosymtablestack.push(initialmacrosymtable);

        { are we compiling the system unit? }
        if (cs_compilesystem in current_settings.moduleswitches) then
         begin
           systemunit:=tglobalsymtable(curr.localsymtable);
           { create system defines }
           create_intern_types;
           create_intern_symbols;
           { Set the owner of errorsym and errortype to symtable to
             prevent crashes when accessing .owner }
           generrorsym.owner:=systemunit;
           generrordef.owner:=systemunit;
           exit;
         end;

        { insert the system unit, it is allways the first. Load also the
          internal types from the system unit }
        Sys:=AddUnit(curr,'system');
        Result:=Assigned(Sys) and (Sys.State in [ms_processed,ms_compiled]);
        systemunit:=tglobalsymtable(symtablestack.top);

        { load_intern_types resets the scanner... }
        current_scanner.tempcloseinputfile;
        state:=tglobalstate.create(true);
        load_intern_types;
        state.restore(true);
        FreeAndNil(state);
        current_scanner.tempopeninputfile;

        { Set the owner of errorsym and errortype to symtable to
          prevent crashes when accessing .owner }
        generrorsym.owner:=systemunit;
        generrordef.owner:=systemunit;
        // Implicitly enable unicode strings in unicode RTL in modes objfpc/delphi.
        { TODO: Check if we should also do this for mode macpas }
        if not (cs_compilesystem in current_settings.moduleswitches) then
          if ([m_objfpc,m_delphi] * current_settings.modeswitches)<>[] then
            if is_systemunit_unicode then
              Include(current_settings.modeswitches,m_default_unicodestring);
      end;


    { Return true if all units were loaded, no recompilation needed. }
    function loaddefaultunits(curr :tmodule) : boolean;

      Procedure CheckAddUnit(s: string);

        var
          OK : boolean;
          m : TModule;

        begin
          m:=AddUnit(curr,s,true);
          OK:=assigned(m) and (m.state in [ms_processed,ms_compiled]);
          Result:=ok and Result;
        end;

      begin
        Result:=True;
        { Units only required for main module }
        if not(curr.is_unit) then
         begin
           { Heaptrc unit, load heaptrace before any other units especially objpas }
           if (cs_use_heaptrc in current_settings.globalswitches) then
             CheckAddUnit('heaptrc');
           { Valgrind requires c memory manager }
           if (cs_gdb_valgrind in current_settings.globalswitches) or
              (([cs_sanitize_address]*current_settings.moduleswitches)<>[]) then
             CheckAddUnit('cmem');
           { Lineinfo unit }
           if (cs_use_lineinfo in current_settings.globalswitches) then begin
             case target_dbg.id of
               dbg_stabs:
                 CheckAddUnit('lineinfo');
               dbg_stabx:
                 CheckAddUnit('lnfogdb');
               else
                 CheckAddUnit('lnfodwrf');
             end;
           end;
{$ifdef cpufpemu}
           { Floating point emulation unit?
             softfpu must be in the system unit anyways (FK)
           if (cs_fp_emulation in current_settings.moduleswitches) and not(target_info.system in system_wince) then
             CheckAddUnit('softfpu');
           }
{$endif cpufpemu}
           { Which kind of resource support?
             Note: if resources aren't used this unit will be removed later,
             otherwise we need it here since it must be loaded quite early }
           if (tf_has_winlike_resources in target_info.flags) then
             if target_res.id=res_ext then
               CheckAddUnit('fpextres')
             else
               CheckAddUnit('fpintres');
         end
        else if (cs_checkpointer in current_settings.localswitches) then
          CheckAddUnit('heaptrc');
        { Objpas unit? }
        if m_objpas in current_settings.modeswitches then
          CheckAddUnit('objpas');

        { Macpas unit? }
        if m_mac in current_settings.modeswitches then
          CheckAddUnit('macpas');

        if m_iso in current_settings.modeswitches then
          CheckAddUnit('iso7185');

        if m_extpas in current_settings.modeswitches then
          begin
            { basic procedures for Extended Pascal are for now provided by the iso unit }
            CheckAddUnit('iso7185');
            CheckAddUnit('extpas');
          end;

        { blocks support? }
        if m_blocks in current_settings.modeswitches then
          CheckAddUnit('blockrtl');

        { Determine char size. }

        // Ansi RTL ?
        if not is_systemunit_unicode then
          begin
          if m_default_unicodestring in current_settings.modeswitches then
            CheckAddUnit('uuchar'); // redefines char as widechar
          end
        else
          begin
          // Unicode RTL
          if not (m_default_ansistring in current_settings.modeswitches) then
            if not (curr.modulename^<>'UACHAR') then
              CheckAddUnit('uachar'); // redefines char as ansichar
          end;

        { Objective-C support unit? }
        if (m_objectivec1 in current_settings.modeswitches) then
          begin
            { interface to Objective-C run time }
            CheckAddUnit('objc');
            loadobjctypes;
            { NSObject }
            if not(curr.is_unit) or
               (curr.modulename^<>'OBJCBASE') then
              CheckAddUnit('objcbase');
          end;
        { Profile unit? Needed for go32v2 only }
        if (cs_profile in current_settings.moduleswitches) and
           (target_info.system in [system_i386_go32v2,system_i386_watcom]) then
          CheckAddUnit('profile');
        if (cs_load_fpcylix_unit in current_settings.globalswitches) then
          begin
            CheckAddUnit('fpcylix');
            CheckAddUnit('dynlibs');
          end;
{$push}
{$warn 6018 off} { Unreachable code due to compile time evaluation }
        { CPU targets with microcontroller support can add a controller specific unit }
        if ControllerSupport and (target_info.system in (systems_embedded+systems_freertos)) and
          (current_settings.controllertype<>ct_none) and
          (embedded_controllers[current_settings.controllertype].controllerunitstr<>'') and
          (embedded_controllers[current_settings.controllertype].controllerunitstr<>curr.modulename^) then
          CheckAddUnit(embedded_controllers[current_settings.controllertype].controllerunitstr);
{$pop}
{$ifdef XTENSA}
        if not(curr.is_unit) and (target_info.system=system_xtensa_freertos) then
          if (current_settings.controllertype=ct_esp32) then
            begin
              if (idf_version>=40100) and (idf_version<40200) then
                CheckAddUnit('espidf_40100')
              else if (idf_version>=40200) and (idf_version<40400) then
                CheckAddUnit('espidf_40200')
              else if idf_version>=40400 then
                CheckAddUnit('espidf_40400')
              else
                Comment(V_Warning, 'Unsupported esp-idf version');
            end
          else if (current_settings.controllertype=ct_esp8266) then
            begin
              if (idf_version>=30300) and (idf_version<30400) then
                CheckAddUnit('esp8266rtos_30300')
              else if idf_version>=30400 then
                CheckAddUnit('esp8266rtos_30400')
              else
                Comment(V_Warning, 'Unsupported esp-rtos version');
            end;
{$endif XTENSA}
      end;


    { Return true if all units were loaded, no recompilation needed. }
    function loadautounits(curr: tmodule) : boolean;

      Procedure CheckAddUnit(s: string);

        var
          OK : boolean;
          m : TModule;

        begin
          m:=AddUnit(curr,s,true);
          OK:=assigned(m) and (m.state in [ms_compiled,ms_processed]);
          Result:=ok and Result;
        end;

      var
        hs,s : string;
      begin
        Result:=True;
        hs:=autoloadunits;
        repeat
          s:=GetToken(hs,',');
          if s='' then
            break;
          CheckAddUnit(s);
        until false;
      end;

    procedure parseusesclause(curr: tmodule);

      var
         s,sorg  : ansistring;
         fn      : string;
         pu  : tused_unit;
         hp2     : tmodule;
         unitsym : tunitsym;
         filepos : tfileposinfo;
         isnew : boolean;

      begin
        consume(_USES);
        repeat
          s:=pattern;
          sorg:=orgpattern;
          filepos:=current_tokenpos;
          consume(_ID);
          while token=_POINT do
            begin
              consume(_POINT);
              s:=s+'.'+pattern;
              sorg:=sorg+'.'+orgpattern;
              consume(_ID);
            end;
          { support "<unit> in '<file>'" construct, but not for tp7 }
          fn:='';
          if not(m_tp7 in current_settings.modeswitches) and
             try_to_consume(_OP_IN) then
            fn:=FixFileName(get_stringconst);
          { Give a warning if lineinfo is loaded }
          if s='LINEINFO' then
            begin
              Message(parser_w_no_lineinfo_use_switch);
              if (target_dbg.id in [dbg_dwarf2, dbg_dwarf3]) then
               s := 'LNFODWRF';
             sorg := s;
            end;
          { Give a warning if objpas is loaded }
          if s='OBJPAS' then
           Message(parser_w_no_objpas_use_mode);
          { Using the unit itself is not possible }
          if (s<>curr.modulename^) then
           begin
             { check if the unit is already used }
             hp2:=nil;
             pu:=tused_unit(curr.used_units.first);
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
               begin
               hp2:=registerunit(curr,sorg,fn,isnew);
               if isnew then
                 usedunits.concat(tused_unit.create(hp2,curr.in_interface,true,nil));
               end
             else
               Message1(sym_e_duplicate_id,s);
             { Create unitsym, we need to use the name as specified, we
               can not use the modulename because that can be different
               when -Un is used }
             current_tokenpos:=filepos;
             unitsym:=cunitsym.create(sorg,nil);
             { the current module uses the unit hp2 }
             curr.addusedunit(hp2,true,unitsym);
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
      end;

    function loadunits(curr: tmodule; frominterface : boolean) : boolean;

      var
         s  : ansistring;
         pu  : tused_unit;
         state: tglobalstate;
         isLoaded : Boolean;
         mwait : tmodule;

         procedure restorestate;

         begin
           state.restore(true);
           if assigned(current_scanner) and (current_module.scanner=current_scanner) then
              begin
              if assigned(current_scanner.inputfile) then
                current_scanner.tempopeninputfile;
              end;

           state.free;
         end;

      begin
        Result:=true;
        mwait:=nil;
        current_scanner.tempcloseinputfile;
        state:=tglobalstate.create(true);
         { Load the units }
         pu:=tused_unit(curr.used_units.first);
         while assigned(pu) do
          begin
            { Only load the units that are in the current
              (interface/implementation) uses clause }
            if pu.in_uses and
               (pu.in_interface=frominterface) then
             begin
               if (pu.u.state in [ms_processed, ms_compiled,ms_compiling_waitimpl]) then
                 isLoaded:=true
               else if (pu.u.state=ms_registered) then
                  // try to load
                 isLoaded:=tppumodule(pu.u).loadppu(curr)
               else
                 isLoaded:=False;
               isLoaded:=IsLoaded and not pu.u.is_reset;
               if not IsLoaded then
                 begin
                   if mwait=nil then
                     mwait:=pu.u;
                   // In case of is_reset, the task handler will discard the state if the module was already there
                   task_handler.addmodule(pu.u);
                 end;
               Result:=Result and IsLoaded;
               { is our module compiled? then we can stop }
               if curr.state in [ms_compiled,ms_processed] then
                 begin
                 Restorestate;
                 exit;
                 end;
               { add this unit to the dependencies }
               pu.u.adddependency(curr,frominterface);
               { check hints }
               pu.check_hints;
             end;
            pu:=tused_unit(pu.next);
          end;

         Restorestate;
      end;

     {
       Connect loaded units: check crc and add to symbol tables.
       this can only be called after all units were actually loaded!
     }

     procedure connect_loaded_units(_module : tmodule; preservest:tsymtable);

     var
       pu  : tused_unit;
       sorg   : ansistring;
       unitsymtable: tabstractunitsymtable;

     begin
       // writeln(_module.get_modulename,': Connecting units');
       pu:=tused_unit(_module.used_units.first);
       while assigned(pu) do
         begin
         {
         Writeln('Connect : ',Assigned(_module.modulename), ' ', assigned(pu.u), ' ' ,assigned(pu.u.modulename));
         if assigned(pu.u) then
           begin
             if assigned(pu.u.modulename) then
               Writeln(_module.modulename^,': Examining connect of file ',pu._fn,' (',pu.u.modulename^,')')
             else
               Writeln(_module.modulename^,': Examining connect of file ',pu._fn);

           end
         else
           Writeln(_module.modulename^,': Examining unit without module... ');
         }
         if not (pu.in_uses and
            (pu.in_interface=_module.in_interface)) then
           begin
//           writeln('Must not connect ',pu.u.modulename^,' (pu.in_interface: ',pu.in_interface,' <> module.in_interface',_module.in_interface,')');
           end
         else
           begin
//           writeln('Must connect ',pu.u.modulename^,'(sym: ',pu.unitsym.realname,')');
           { save crc values }
           pu.checksum:=pu.u.crc;
           pu.interface_checksum:=pu.u.interface_crc;
           pu.indirect_checksum:=pu.u.indirect_crc;
           if tppumodule(pu.u).nsprefix<>'' then
             begin
               { use the name as declared in the uses section for -Un }
               sorg:=tppumodule(pu.u).nsprefix+'.'+pu.unitsym.realname;
               { update unitsym now that we have access to the full name }
               pu.unitsym.free;
               pu.unitsym:=cunitsym.create(sorg,pu.u);
             end
           else
             begin
               { connect unitsym to the module }
               pu.unitsym.module:=pu.u;
               pu.unitsym.register_sym;
             end;
           {
             Add the unit symbol in the current symtable.
             localsymtable will be nil after the interface uses clause is parsed and the local symtable
             is moved to the global.
           }
           if assigned(_module.localsymtable) then
             unitsymtable:=tabstractunitsymtable(_module.localsymtable)
           else
             unitsymtable:=tabstractunitsymtable(_module.globalsymtable);
           // Writeln('Adding used unit sym ',pu.unitsym.realName,' to table ',unitsymtable.get_name);
           unitsymtable.insertunit(pu.unitsym);
           { add to symtable stack }
           // Writeln('Adding used unit symtable ',pu.u.globalsymtable.name^,' (',pu.u.globalsymtable.DefList.Count, ' defs) to stack');
           if assigned(preservest) then
             symtablestack.pushafter(pu.u.globalsymtable,preservest)
           else
             symtablestack.push(pu.u.globalsymtable);
           if (m_mac in current_settings.modeswitches) and
              assigned(pu.u.globalmacrosymtable) then
             macrosymtablestack.push(pu.u.globalmacrosymtable);

           end;
         pu:=tused_unit(pu.next);
         end;
       // writeln(_module.get_modulename,': Done Connecting units');
     end;



     procedure reset_all_defs(curr: tmodule);
       begin
         if assigned(curr.wpoinfo) then
           curr.wpoinfo.resetdefs;
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
                pd.freeimplprocdefinfo;
                pd.done_paraloc_info(calleeside);
              end;
          end;
      end;


    procedure free_unregistered_localsymtable_elements(curr : tmodule);
      var
        i: longint;
        def: tdef;
        sym: tsym;
      begin
        for i:=curr.localsymtable.deflist.count-1 downto 0 do
          begin
            def:=tdef(curr.localsymtable.deflist[i]);
            { since commit 48986 deflist might have NIL entries }
            if not assigned(def) then
              continue;
            { this also frees def, as the defs are owned by the symtable }
            if not def.is_registered and
               not(df_not_registered_no_free in def.defoptions) then
              begin
                { if it's a procdef, unregister it from its procsym first,
                  unless that sym hasn't been registered either (it's possible
                  to have one overload in the interface and another in the
                  implementation) }
                if (def.typ=procdef) and
                   tprocdef(def).procsym.is_registered then
                 tprocsym(tprocdef(def).procsym).ProcdefList.Remove(def);
                curr.localsymtable.deletedef(def);
              end;
          end;
        { from high to low so we hopefully have moves of less data }
        for i:=curr.localsymtable.symlist.count-1 downto 0 do
          begin
            sym:=tsym(curr.localsymtable.symlist[i]);
            { this also frees sym, as the symbols are owned by the symtable }
            if not sym.is_registered then
              curr.localsymtable.DeleteSym(sym);
          end;
      end;


    procedure setupglobalswitches;
      begin
        if (cs_create_pic in current_settings.moduleswitches) then
          begin
            def_system_macro('FPC_PIC');
            def_system_macro('PIC');
          end;
      end;


    function create_main_proc(const name:TSymStr;potype:tproctypeoption;st:TSymtable):tcgprocinfo;
      var
        ps  : tprocsym;
        pd  : tprocdef;
      begin
        { there should be no current_procinfo available }
        if assigned(current_procinfo) then
         internalerror(200304275);
        {Generate a procsym for main}
        ps:=cprocsym.create('$'+name);
        { always register the symbol }
        ps.register_sym;
        { main are allways used }
        inc(ps.refs);
        st.insertsym(ps);
        pd:=tprocdef(cnodeutils.create_main_procdef(target_info.cprefix+name,potype,ps));
        { We don't need a local symtable, change it into the static symtable }
        if not (potype in [potype_mainstub,potype_pkgstub,potype_libmainstub]) then
          begin
            pd.localst.free;
            pd.localst:=st;
          end
        else if (potype=potype_pkgstub) and
            (target_info.system in systems_all_windows+systems_nativent) then
          pd.proccalloption:=pocall_stdcall
        else
          pd.proccalloption:=pocall_cdecl;
        handle_calling_convention(pd,hcc_default_actions_impl);
        { set procinfo and current_procinfo.procdef }
        result:=tcgprocinfo(cprocinfo.create(nil));
        result.procdef:=pd;
        { main proc does always a call e.g. to init system unit }
        if potype<>potype_pkgstub then
          include(result.flags,pi_do_call);
      end;


    procedure release_main_proc(curr: tmodule; pi:tcgprocinfo);
      begin
        { remove localst as it was replaced by staticsymtable }
        pi.procdef.localst:=nil;
        { remove procinfo }
        curr.procinfo:=nil;
        pi.free;
        pi:=nil;
      end;



    { Insert _GLOBAL_OFFSET_TABLE_ symbol if system uses it }

    procedure maybe_load_got(curr: tmodule);
{$if defined(i386) or defined (sparcgen)}
       var
         gotvarsym : tstaticvarsym;
{$endif i386 or sparcgen}
      begin
{$if defined(i386) or defined(sparcgen)}
         if (cs_create_pic in current_settings.moduleswitches) and
            (tf_pic_uses_got in target_info.flags) then
           begin
             { insert symbol for got access in assembler code}
             gotvarsym:=cstaticvarsym.create('_GLOBAL_OFFSET_TABLE_',
                          vs_value,voidpointertype,[vo_is_external]);
             gotvarsym.set_mangledname('_GLOBAL_OFFSET_TABLE_');
             curr.localsymtable.insertsym(gotvarsym);
             { avoid unnecessary warnings }
             gotvarsym.varstate:=vs_read;
             gotvarsym.refs:=1;
           end;
{$endif i386 or sparcgen}
      end;

    function gen_implicit_initfinal(curr: tmodule; flag:tmoduleflag;st:TSymtable):tcgprocinfo;
      begin
        { create procdef }
        case flag of
          mf_init :
            begin
              result:=create_main_proc(make_mangledname('',curr.localsymtable,'init_implicit$'),potype_unitinit,st);
              result.procdef.aliasnames.concat(make_mangledname('INIT$',curr.localsymtable,''));
            end;
          mf_finalize :
            begin
              result:=create_main_proc(make_mangledname('',curr.localsymtable,'finalize_implicit$'),potype_unitfinalize,st);
              result.procdef.aliasnames.concat(make_mangledname('FINALIZE$',curr.localsymtable,''));
              if (not curr.is_unit) then
                result.procdef.aliasnames.concat('PASCALFINALIZE');
            end;
          else
            internalerror(200304253);
        end;
        result.code:=cnothingnode.create;
      end;


    procedure copy_macro(p:TObject; arg:pointer);
      begin
        TModule(arg).globalmacrosymtable.insertsym(tmacro(p).getcopy);
      end;

    function try_consume_hintdirective(var moduleopt:tmoduleoptions; var deprecatedmsg:pshortstring):boolean;
      var
        deprecated_seen,
        last_is_deprecated:boolean;
      begin
        try_consume_hintdirective:=false;
        deprecated_seen:=false;
        repeat
          last_is_deprecated:=false;
          case idtoken of
            _LIBRARY :
              begin
                include(moduleopt,mo_hint_library);
                try_consume_hintdirective:=true;
              end;
            _DEPRECATED :
              begin
                { allow deprecated only once }
                if deprecated_seen then
                  break;
                include(moduleopt,mo_hint_deprecated);
                try_consume_hintdirective:=true;
                last_is_deprecated:=true;
                deprecated_seen:=true;
              end;
            _EXPERIMENTAL :
              begin
                include(moduleopt,mo_hint_experimental);
                try_consume_hintdirective:=true;
              end;
            _PLATFORM :
              begin
                include(moduleopt,mo_hint_platform);
                try_consume_hintdirective:=true;
              end;
            _UNIMPLEMENTED :
              begin
                include(moduleopt,mo_hint_unimplemented);
                try_consume_hintdirective:=true;
              end;
            else
              break;
          end;
          consume(Token);
          { handle deprecated message }
          if ((token=_CSTRING) or (token=_CCHAR)) and last_is_deprecated then
            begin
              if deprecatedmsg<>nil then
                internalerror(201001221);
              if token=_CSTRING then
                deprecatedmsg:=stringdup(cstringpattern)
              else
                deprecatedmsg:=stringdup(pattern);
              consume(token);
              include(moduleopt,mo_has_deprecated_msg);
            end;
        until false;
      end;


{$ifdef jvm}
      procedure addmoduleclass(curr : tmodule);
        var
          def: tobjectdef;
          typesym: ttypesym;
        begin
          { java_jlobject may not have been parsed yet (system unit); in any
            case, we only use this to refer to the class type, so inheritance
            does not matter }
          def:=cobjectdef.create(odt_javaclass,'__FPC_JVM_Module_Class_Alias$',nil,true);
          include(def.objectoptions,oo_is_external);
          include(def.objectoptions,oo_is_sealed);
          def.objextname:=stringdup(curr.realmodulename^);
          typesym:=ctypesym.create('__FPC_JVM_Module_Class_Alias$',def);
          symtablestack.top.insertsym(typesym);
        end;
{$endif jvm}

type
    tfinishstate=record
      init_procinfo:tcgprocinfo;
      finalize_procinfo:tcgprocinfo;
    end;
    pfinishstate=^tfinishstate;



    function proc_unit_implementation(curr: tmodule):boolean;

      var
        init_procinfo,
        finalize_procinfo : tcgprocinfo;
        i,j : integer;
        finishstate:pfinishstate;


      begin
        result:=true;
        init_procinfo:=nil;
        finalize_procinfo:=nil;
        finishstate:=nil;

        set_current_module(curr);

        { We get here only after used modules were loaded }
        connect_loaded_units(curr,curr.globalsymtable);

        { All units are read, now give them a number }
        curr.updatemaps;

        { further, changing the globalsymtable is not allowed anymore }
        curr.globalsymtable.sealed:=true;
        symtablestack.push(curr.localsymtable);

        if not curr.interface_only then
          begin
            if (curr.modulename^='FMX.UTILS') then
              Writeln('Here');
            Message1(parser_u_parsing_implementation,curr.modulename^);
            if curr.in_interface then
              internalerror(200212285);

            { Compile the unit }
            init_procinfo:=create_main_proc(make_mangledname('',curr.localsymtable,'init$'),potype_unitinit,curr.localsymtable);
            init_procinfo.procdef.aliasnames.concat(make_mangledname('INIT$',curr.localsymtable,''));
            init_procinfo.parse_body;
            { save file pos for debuginfo }
            curr.mainfilepos:=init_procinfo.entrypos;

            { parse finalization section }
            if token=_FINALIZATION then
              begin
                { Compile the finalize }
                finalize_procinfo:=create_main_proc(make_mangledname('',curr.localsymtable,'finalize$'),potype_unitfinalize,curr.localsymtable);
                finalize_procinfo.procdef.aliasnames.concat(make_mangledname('FINALIZE$',curr.localsymtable,''));
                finalize_procinfo.parse_body;
              end
          end;

        { remove all units that we are waiting for that are already waiting for
          us => breaking up circles }
        for i:=0 to curr.waitingunits.count-1 do
          for j:=curr.waitingforunit.count-1 downto 0 do
            if curr.waitingunits[i]=curr.waitingforunit[j] then
              curr.waitingforunit.delete(j);

    {$ifdef DEBUG_UNITWAITING}
        Writeln('Units waiting for ', curr.modulename^, ': ',
          curr.waitingforunit.Count);
    {$endif}
        result:=curr.waitingforunit.count=0;

        { save all information that is needed for finishing the unit }
        New(finishstate);
        finishstate^.init_procinfo:=init_procinfo;
        finishstate^.finalize_procinfo:=finalize_procinfo;
        curr.finishstate:=finishstate;

        if result then
          finish_unit(curr)
        else
          curr.state:=ms_compiling_waitfinish;
      end;

    function parse_unit_interface_declarations(curr : tmodule) : boolean;

      begin
        result:=true;
        set_current_module(curr);

        { update the symtable }
        connect_loaded_units(curr,nil);

        { We must do this again, because units can have been added to the list while another task was being handled }
        curr.updatemaps;

        { consume the semicolon after maps have been updated else conditional compiling expressions
          might cause internal errors, see tw8611 }

        if curr.consume_semicolon_after_uses then
          consume(_SEMICOLON);

        { now push our own symtable }
        symtablestack.push(curr.globalsymtable);
        { Dump stack
          Write(curr.modulename^);
          symtablestack.dump;
          }

        { ... parse the declarations }
        Message1(parser_u_parsing_interface,curr.realmodulename^);

{$ifdef jvm}
         { fake classdef to represent the class corresponding to the unit }
         addmoduleclass(curr);
{$endif}
        read_interface_declarations;


        { Export macros defined in the interface for macpas. The macros
          are put in the globalmacrosymtable that will only be used by other
          units. The current unit continues to use the localmacrosymtable }

        if (m_mac in current_settings.modeswitches) then
          begin
            curr.globalmacrosymtable:=tmacrosymtable.create(true);
            curr.localmacrosymtable.SymList.ForEachCall(@copy_macro,curr);
          end;

        { leave when we got an error }
        if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            symtablestack.pop(curr.globalsymtable);

{$ifdef DEBUG_NODE_XML}
            XMLFinalizeNodeFile('unit');
{$endif DEBUG_NODE_XML}
            exit;
          end;

        { Our interface is compiled, generate CRC and switch to implementation }
        if not(cs_compilesystem in current_settings.moduleswitches) and
          (Errorcount=0) then
           tppumodule(curr).getppucrc;
        curr.in_interface:=false;
        curr.interface_compiled:=true;

        { First reload all units depending on our interface, we need to do this
          in the implementation part to prevent erroneous circular references }
        tppumodule(curr).setdefgeneration;
        tppumodule(curr).reload_flagged_units;

        { Parse the implementation section }
        if (m_mac in current_settings.modeswitches) and try_to_consume(_END) then
          curr.interface_only:=true
        else
          curr.interface_only:=false;

        parse_only:=false;

        { create static symbol table }
        curr.localsymtable:=tstaticsymtable.create(curr.modulename^,curr.moduleid);


        { Insert _GLOBAL_OFFSET_TABLE_ symbol if system uses it }
        maybe_load_got(curr);
        if not curr.interface_only then
          begin
            consume(_IMPLEMENTATION);
            Message1(unit_u_loading_implementation_units,curr.modulename^);
            { Read the implementation units }
            if token=_USES then
              begin
              parseusesclause(curr);
              if not loadunits(curr,false) then
                 curr.state:=ms_compiling_waitimpl;
               consume(_SEMICOLON);
              end;
          end;

        if curr.state in [ms_compiled,ms_processed] then
           begin
           // Writeln('Popping global symtable ?');
           symtablestack.pop(curr.globalsymtable);
           end;

        { Can we continue compiling ? }
        result:=curr.state<>ms_compiling_waitimpl;
        if result then
          result:=proc_unit_implementation(curr)
      end;

    function proc_unit(curr: tmodule):boolean;
      var
         main_file: tinputfile;
         s1,s2  : ^string; {Saves stack space}
         unitname : ansistring;
         unitname8 : string[8];
         consume_semicolon_after_uses:boolean;
         feature : tfeature;
         load_ok : boolean;

      begin
         result:=true;

         if m_mac in current_settings.modeswitches then
           curr.mode_switch_allowed:= false;

         consume(_UNIT);
         if curr.is_initial then
          Status.IsExe:=false;

         unitname:=orgpattern;
         consume(_ID);
         while token=_POINT do
           begin
             consume(_POINT);
             unitname:=unitname+'.'+orgpattern;
             consume(_ID);
           end;

         { create filenames and unit name }
         main_file := current_scanner.inputfile;
         while assigned(main_file.next) do
           main_file := main_file.next;

         new(s1);
         s1^:=curr.modulename^;
         curr.SetFileName(main_file.path+main_file.name,true);
         curr.SetModuleName(unitname);

{$ifdef DEBUG_NODE_XML}
         XMLInitializeNodeFile('unit', unitname);
{$endif DEBUG_NODE_XML}

         { check for system unit }
         new(s2);
         s2^:=upper(ChangeFileExt(ExtractFileName(main_file.name),''));
         unitname8:=copy(curr.modulename^,1,8);
         if (cs_check_unit_name in current_settings.globalswitches) and
            (
             not(
                 (curr.modulename^=s2^) or
                 (
                  (length(curr.modulename^)>8) and
                  (unitname8=s2^)
                 )
                )
             or
             (
              (length(s1^)>8) and
              (s1^<>curr.modulename^)
             )
            ) then
           Message2(unit_e_illegal_unit_name,curr.realmodulename^,s1^);
         if (curr.modulename^='SYSTEM') then
          include(current_settings.moduleswitches,cs_compilesystem);
         dispose(s2);
         dispose(s1);

         if (target_info.system in systems_unit_program_exports) then
           exportlib.preparelib(curr.realmodulename^);

         { parse hint directives }
         try_consume_hintdirective(curr.moduleoptions, curr.deprecatedmsg);

         consume(_SEMICOLON);

         { handle the global switches, do this before interface, because after interface has been
           read, all following directives are parsed as well }
         setupglobalswitches;

         { generate now the global symboltable,
           define first as local to overcome dependency conflicts }
         curr.localsymtable:=tglobalsymtable.create(curr.modulename^,curr.moduleid);

         { insert unitsym of this unit to prevent other units having
           the same name }
         tabstractunitsymtable(curr.localsymtable).insertunit(cunitsym.create(curr.realmodulename^,curr));

         { load default system unit, it must be loaded before interface is parsed
           else we cannot use e.g. feature switches before the next real token }
         load_ok:=loadsystemunit(curr);

         { system unit is loaded, now insert feature defines }
         for feature:=low(tfeature) to high(tfeature) do
           if feature in features then
             def_system_macro('FPC_HAS_FEATURE_'+featurestr[feature]);

         consume(_INTERFACE);

         { global switches are read, so further changes aren't allowed  }
         curr.in_global:=false;

         message1(unit_u_loading_interface_units,curr.modulename^);

         { update status }
         status.currentmodule:=curr.realmodulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (curr.modulename^='OBJPAS') then
           exclude(current_settings.modeswitches,m_objpas);

         { maybe turn off m_mac if we are compiling macpas }
         if (curr.modulename^='MACPAS') then
           exclude(current_settings.modeswitches,m_mac);

         parse_only:=true;

         { load default units, like language mode units }
         if not(cs_compilesystem in current_settings.moduleswitches) then
           load_ok:=loaddefaultunits(curr) and load_ok;

         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in current_settings.moduleswitches) and
            (token=_USES) then
           begin
             // We do this as late as possible.
             if Assigned(curr) then
               curr.Loadlocalnamespacelist
             else
               current_namespacelist:=Nil;
             parseusesclause(curr);
             load_ok:=loadunits(curr,true) and load_ok;
             { has it been compiled at a higher level ?}
             if curr.state in [ms_compiled,ms_processed] then
               begin
                 Message1(parser_u_already_compiled,curr.realmodulename^);
                 exit;
               end;

             consume_semicolon_after_uses:=true;
           end
         else
           consume_semicolon_after_uses:=false;

         { we need to store this in case compilation is transferred to another unit }
         curr.consume_semicolon_after_uses:=consume_semicolon_after_uses;

         { move the global symtable from the temporary local to global }
         current_module.globalsymtable:=current_module.localsymtable;
         current_module.localsymtable:=nil;

         { Now we check if we can continue. }

         if not load_ok then
           curr.state:=ms_compiling_waitintf;

         { create whole program optimisation information (may already be
           updated in the interface, e.g., in case of classrefdef typed
           constants }
         curr.wpoinfo:=tunitwpoinfo.create;

         { Can we continue compiling ? }
         result:=curr.state<>ms_compiling_waitintf;
         if result then
           result:=parse_unit_interface_declarations(curr);
      end;

    procedure finish_unit(module:tmodule);

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

      procedure module_is_done(curr: tmodule);inline;
        begin
          dispose(pfinishstate(curr.finishstate));
          curr.finishstate:=nil;
        end;

      var
{$ifdef EXTDEBUG}
        store_crc,
{$endif EXTDEBUG}
        store_interface_crc,
        store_indirect_crc: cardinal;
        force_init_final : boolean;
        init_procinfo,
        finalize_procinfo : tcgprocinfo;
        i : longint;
        ag : boolean;
        finishstate : tfinishstate;
        waitingmodule : tmodule;
      begin
         { curr is now module }

         if not assigned(module.finishstate) then
           internalerror(2012091801);
         finishstate:=pfinishstate(module.finishstate)^;

         finalize_procinfo:=finishstate.finalize_procinfo;
         init_procinfo:=finishstate.init_procinfo;

         { Generate specializations of objectdefs methods }
         generate_specialization_procs;

         // This needs to be done before we generate the VMTs
         if (target_cpu=tsystemcpu.cpu_wasm32) then
           begin
           add_synthetic_interface_classes_for_st(module.globalsymtable);
           add_synthetic_interface_classes_for_st(module.localsymtable);
           end;

         { generate construction functions for all attributes in the unit:
           this must be done before writing the VMTs because
           during VMT writing  the extended field info is written }

         generate_attr_constrs(current_module.used_rtti_attrs);

         { Generate VMTs }
         if Errorcount=0 then
           begin
             write_vmts(module.globalsymtable,true);
             write_vmts(module.localsymtable,false);
           end;

         { add implementations for synthetic method declarations added by
           the compiler }
         add_synthetic_method_implementations(module.globalsymtable);
         add_synthetic_method_implementations(module.localsymtable);

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=tglobalsymtable(module.globalsymtable).needs_init_final or
                           tstaticsymtable(module.localsymtable).needs_init_final;

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         { Now the sole purpose of this is to change 'init' to 'init_implicit',
           is it needed at all? (Sergei) }
         { it's needed in case cnodeutils.force_init = true }
         if (force_init_final or cnodeutils.force_init) and
            (
              not assigned(init_procinfo) or
              has_no_code(init_procinfo.code)
            ) then
           begin
             { first release the not used init procinfo }
             if assigned(init_procinfo) then
               begin
                 release_proc_symbol(init_procinfo.procdef);
                 release_main_proc(module,init_procinfo);
               end;
             init_procinfo:=gen_implicit_initfinal(module,mf_init,module.localsymtable);
           end;
         if (force_init_final or cnodeutils.force_final) and
            (
              not assigned(finalize_procinfo) or
              has_no_code(finalize_procinfo.code)
            ) then
           begin
             { first release the not used finalize procinfo }
             if assigned(finalize_procinfo) then
               begin
                 release_proc_symbol(finalize_procinfo.procdef);
                 release_main_proc(module,finalize_procinfo);
               end;
             finalize_procinfo:=gen_implicit_initfinal(module,mf_finalize,module.localsymtable);
           end;

         { Now both init and finalize bodies are read and it is known
           which variables are used in both init and finalize we can now
           generate the code. This is required to prevent putting a variable in
           a register that is also used in the finalize body (PFV) }
         if assigned(init_procinfo) then
           begin
             if (force_init_final or cnodeutils.force_init) or
                not(has_no_code(init_procinfo.code)) then
               begin
                 init_procinfo.code:=cnodeutils.wrap_proc_body(init_procinfo.procdef,init_procinfo.code);
                 init_procinfo.generate_code_tree;
                 include(module.moduleflags,mf_init);
               end
             else
               release_proc_symbol(init_procinfo.procdef);
             init_procinfo.resetprocdef;
             release_main_proc(module,init_procinfo);
           end;
         if assigned(finalize_procinfo) then
           begin
             if force_init_final or
                cnodeutils.force_init or
                not(has_no_code(finalize_procinfo.code)) then
               begin
                 finalize_procinfo.code:=cnodeutils.wrap_proc_body(finalize_procinfo.procdef,finalize_procinfo.code);
                 finalize_procinfo.generate_code_tree;
                 include(module.moduleflags,mf_finalize);
               end
             else
               release_proc_symbol(finalize_procinfo.procdef);
             finalize_procinfo.resetprocdef;
             release_main_proc(module,finalize_procinfo);
           end;

         symtablestack.pop(module.localsymtable);
         symtablestack.pop(module.globalsymtable);

         { the last char should always be a point }
         { Do not attempt to read next token after dot,
           there may be a #0 when the unit was finished in a separate stage }
         consume_last_dot;

         { reset wpo flags for all defs }
         reset_all_defs(module);

         if (Errorcount=0) then
           begin
             { tests, if all (interface) forwards are resolved }
             tstoredsymtable(module.globalsymtable).check_forwards;
             { check if all private fields are used }
             tstoredsymtable(module.globalsymtable).allprivatesused;

             { test static symtable }
             tstoredsymtable(module.localsymtable).allsymbolsused;
             tstoredsymtable(module.localsymtable).allprivatesused;
             tstoredsymtable(module.localsymtable).check_forwards;
             tstoredsymtable(module.localsymtable).checklabels;

             { used units }
             module.allunitsused;
           end;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            module_is_done(module);
{$ifdef DEBUG_NODE_XML}
            XMLFinalizeNodeFile('unit');
{$endif DEBUG_NODE_XML}
            exit;
          end;

         { if an Objective-C module, generate rtti and module info }
         MaybeGenerateObjectiveCImageInfo(module.globalsymtable,module.localsymtable);

         { do we need to add the variants unit? }
         maybeloadvariantsunit(module);

         { generate rtti/init tables }
         write_persistent_type_info(module.globalsymtable,true);
         write_persistent_type_info(module.localsymtable,false);

         { Tables }
         cnodeutils.InsertThreadvars;

         { Resource strings }
         GenerateResourceStrings;

         { Widestring typed constants }
         cnodeutils.InsertWideInits;

         { Resourcestring references }
         cnodeutils.InsertResStrInits;

         { generate debuginfo }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.inserttypeinfo;

         { generate imports }
         if module.ImportLibraryList.Count>0 then
           importlib.generatelib;

         { insert own objectfile, or say that it's in a library
           (no check for an .o when loading) }
         ag:=is_assembler_generated;
         if ag then
           insertobjectfile(module)
         else
           begin
             module.headerflags:=module.headerflags or uf_no_link;
             exclude(module.moduleflags,mf_has_stabs_debuginfo);
             exclude(module.moduleflags,mf_has_dwarf_debuginfo);
           end;

         if ag then
          begin
            { create callframe info }
            create_dwarf_frame;
            { assemble }
            create_objectfile(module);
          end;

         { Write out the ppufile after the object file has been created }
         store_interface_crc:=module.interface_crc;
         store_indirect_crc:=module.indirect_crc;
{$ifdef EXTDEBUG}
         store_crc:=module.crc;
{$endif EXTDEBUG}
         if (Errorcount=0) then
           tppumodule(module).writeppu;

         if not(cs_compilesystem in current_settings.moduleswitches) then
           begin
             if store_interface_crc<>module.interface_crc then
               Message1(unit_u_interface_crc_changed,module.ppufilename);
             if store_indirect_crc<>module.indirect_crc then
               Message1(unit_u_indirect_crc_changed,module.ppufilename);
           end;
{$ifdef EXTDEBUG}
         if not(cs_compilesystem in current_settings.moduleswitches) then
           if (store_crc<>module.crc) then
             Message1(unit_u_implementation_crc_changed,module.ppufilename);
{$endif EXTDEBUG}

         { release unregistered defs/syms from the localsymtable }
         free_unregistered_localsymtable_elements(module);
         { release local symtables that are not needed anymore }
         free_localsymtables(module.globalsymtable);
         free_localsymtables(module.localsymtable);

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            module_is_done(module);
{$ifdef DEBUG_NODE_XML}
            XMLFinalizeNodeFile('unit');
{$endif DEBUG_NODE_XML}
            exit;
          end;

{$ifdef debug_devirt}
         { print out all instantiated class/object types }
         writeln('constructed object/class/classreftypes in ',module.realmodulename^);
         for i := 0 to module.wpoinfo.createdobjtypes.count-1 do
           begin
             write('  ',tdef(module.wpoinfo.createdobjtypes[i]).GetTypeName);
             case tdef(module.wpoinfo.createdobjtypes[i]).typ of
               objectdef:
                 case tobjectdef(module.wpoinfo.createdobjtypes[i]).objecttype of
                   odt_object:
                     writeln(' (object)');
                   odt_class:
                     writeln(' (class)');
                   else
                     internalerror(2008101103);
                 end;
               else
                 internalerror(2008101104);
             end;
           end;

         for i := 0 to module.wpoinfo.createdclassrefobjtypes.count-1 do
           begin
             write('  Class Of ',tdef(module.wpoinfo.createdclassrefobjtypes[i]).GetTypeName);
             case tdef(module.wpoinfo.createdclassrefobjtypes[i]).typ of
               objectdef:
                 case tobjectdef(module.wpoinfo.createdclassrefobjtypes[i]).objecttype of
                   odt_class:
                     writeln(' (classrefdef)');
                   else
                     internalerror(2008101105);
                 end
               else
                 internalerror(2008101102);
             end;
           end;
{$endif debug_devirt}

        Message1(unit_u_finished_compiling,module.modulename^);

        module_is_done(module);
        module.end_of_parsing;

        for i:=0 to module.waitingunits.count-1 do
          begin
            waitingmodule:=tmodule(module.waitingunits[i]);
            waitingmodule.remove_from_waitingforunits(module);
          end;

{$ifdef DEBUG_NODE_XML}
        XMLFinalizeNodeFile('unit');
{$endif DEBUG_NODE_XML}
      end;


    function proc_package(curr: tmodule) : boolean;
      var
        main_file : tinputfile;
        hp,hp2    : tmodule;
        pkg : tpcppackage;
        main_procinfo : tcgprocinfo;
        force_init_final : boolean;
        uu : tused_unit;
        module_name: ansistring;
        pentry: ppackageentry;
        feature : tfeature;
      begin
         Result:=True;
         Status.IsPackage:=true;
         Status.IsExe:=true;
         parse_only:=false;
         main_procinfo:=nil;
         {init_procinfo:=nil;
         finalize_procinfo:=nil;}

         if not (tf_supports_packages in target_info.flags) then
           message1(parser_e_packages_not_supported,target_info.name);

         if not RelocSectionSetExplicitly then
           RelocSection:=true;

         { Relocation works only without stabs under Windows when }
         { external linker (LD) is used.  LD generates relocs for }
         { stab sections which is not loaded in memory. It causes }
         { AV error when DLL is loaded and relocation is needed.  }
         { Internal linker does not have this problem.            }
         if RelocSection and
            (target_info.system in systems_all_windows+[system_i386_wdosx]) and
            (cs_link_extern in current_settings.globalswitches) then
           begin
              include(current_settings.globalswitches,cs_link_strip);
              { Warning stabs info does not work with reloc section !! }
              if (cs_debuginfo in current_settings.moduleswitches) and
                 (target_dbg.id=dbg_stabs) then
                begin
                  Message1(parser_w_parser_reloc_no_debug,curr.mainsource);
                  Message(parser_w_parser_win32_debug_needs_WN);
                  exclude(current_settings.moduleswitches,cs_debuginfo);
                end;
           end;
         { get correct output names }
         main_file := current_scanner.inputfile;
         while assigned(main_file.next) do
           main_file := main_file.next;

         curr.SetFileName(main_file.path+main_file.name,true);

         { consume _PACKAGE word }
         consume(_ID);

         module_name:=orgpattern;
         consume(_ID);
         while token=_POINT do
           begin
             consume(_POINT);
             module_name:=module_name+'.'+orgpattern;
             consume(_ID);
           end;

         curr.setmodulename(module_name);
         curr.ispackage:=true;
         exportlib.preparelib(module_name);
         pkg:=tpcppackage.create(module_name);

         if tf_library_needs_pic in target_info.flags then
           include(current_settings.moduleswitches,cs_create_pic);

         { setup things using the switches, do this before the semicolon, because after the semicolon has been
           read, all following directives are parsed as well }

         setupglobalswitches;

{$ifdef DEBUG_NODE_XML}
         XMLInitializeNodeFile('package', module_name);
{$endif DEBUG_NODE_XML}

         consume(_SEMICOLON);

         { global switches are read, so further changes aren't allowed }
         curr.in_global:=false;

         { set implementation flag }
         curr.in_interface:=false;
         curr.interface_compiled:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         curr.localsymtable:=tstaticsymtable.create(curr.modulename^,curr.moduleid);

         { ensure that no packages are picked up from the options }
         packagelist.clear;

         // There should always be a requires, except for the system package. So we load here
         if Assigned(curr) then
           curr.Loadlocalnamespacelist
         else
           current_namespacelist:=Nil;

         {Read the packages used by the package we compile.}
         if (token=_ID) and (idtoken=_REQUIRES) then
           begin
             { consume _REQUIRES word }
             consume(_ID);
             while true do
               begin
                 if token=_ID then
                   begin
                     module_name:=orgpattern;
                     consume(_ID);
                     while token=_POINT do
                       begin
                         consume(_POINT);
                         module_name:=module_name+'.'+orgpattern;
                         consume(_ID);
                       end;
                     add_package(module_name,false,true);
                   end
                 else
                   consume(_ID);
                 if token=_COMMA then
                   consume(_COMMA)
                 else
                   break;
               end;
             consume(_SEMICOLON);
           end;

         { now load all packages, so that we can determine whether a unit is
           already provided by one of the loaded packages }
         load_packages;

         if packagelist.Count>0 then
           begin
             { this means the SYSTEM unit *must* be part of one of the required
               packages, so load it }
             AddUnit(curr,'system',false);
             systemunit:=tglobalsymtable(symtablestack.top);
             load_intern_types;
             { system unit is loaded, now insert feature defines }
             for feature:=low(tfeature) to high(tfeature) do
               if feature in features then
                 def_system_macro('FPC_HAS_FEATURE_'+featurestr[feature]);
           end;

         {Load the units used by the program we compile.}
         if (token=_ID) and (idtoken=_CONTAINS) then
           begin
             { consume _CONTAINS word }
             consume(_ID);
             while true do
               begin
                 if token=_ID then
                   begin
                     module_name:=orgpattern;
                     consume(_ID);
                     while token=_POINT do
                       begin
                         consume(_POINT);
                         module_name:=module_name+'.'+orgpattern;
                         consume(_ID);
                       end;
                     hp:=AddUnit(curr,module_name);
                     if (hp.modulename^='SYSTEM') and not assigned(systemunit) then
                       begin
                         systemunit:=tglobalsymtable(hp.globalsymtable);
                         load_intern_types;
                       end;
                   end
                 else
                   consume(_ID);
                 if token=_COMMA then
                   consume(_COMMA)
                 else break;
               end;
             consume(_SEMICOLON);
           end;

         { All units are read, now give them a number }
         curr.updatemaps;

         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
           begin
             if (hp<>curr) and not assigned(hp.package) then
               begin
                 if mf_package_deny in hp.moduleflags then
                   message1(package_e_unit_deny_package,hp.realmodulename^);
                 { part of the package's used, aka contained units? }
                 uu:=tused_unit(curr.used_units.first);
                 while assigned(uu) do
                   begin
                     if uu.u=hp then
                       break;
                     uu:=tused_unit(uu.next);
                   end;
                 if not assigned(uu) then
                   message2(package_n_implicit_unit_import,hp.realmodulename^,curr.realmodulename^);
               end;
             { was this unit listed as a contained unit? If so => error }
             if (hp<>curr) and assigned(hp.package) then
               begin
                 uu:=tused_unit(curr.used_units.first);
                 while assigned(uu) do
                   begin
                     if uu.u=hp then
                       break;
                     uu:=tused_unit(uu.next);
                   end;
                 if assigned(uu) then
                   message2(package_e_unit_already_contained_in_package,hp.realmodulename^,hp.package.realpackagename^);
               end;
             hp:=tmodule(hp.next);
           end;

         {Insert the name of the main program into the symbol table.}
         if curr.realmodulename^<>'' then
           tabstractunitsymtable(curr.localsymtable).insertunit(cunitsym.create(curr.realmodulename^,curr));

         Message1(parser_u_parsing_implementation,curr.mainsource);

         symtablestack.push(curr.localsymtable);

         { create whole program optimisation information }
         curr.wpoinfo:=tunitwpoinfo.create;

         { should we force unit initialization? }
         force_init_final:=tstaticsymtable(curr.localsymtable).needs_init_final;
         if force_init_final or cnodeutils.force_init then
           {init_procinfo:=gen_implicit_initfinal(mf_init,curr.localsymtable)};

         { Add symbol to the exports section for win32 so smartlinking a
           DLL will include the edata section }
         if assigned(exportlib) and
            (target_info.system in [system_i386_win32,system_i386_wdosx]) and
            (mf_has_exports in curr.moduleflags) then
           current_asmdata.asmlists[al_procedures].concat(tai_const.createname(make_mangledname('EDATA',curr.localsymtable,''),0));

         { all labels must be defined before generating code }
         if Errorcount=0 then
           tstoredsymtable(curr.localsymtable).checklabels;

         symtablestack.pop(curr.localsymtable);

         { consume the last point }
         consume(_END);
         consume(_POINT);

         if (Errorcount=0) then
           begin
             { test static symtable }
             tstoredsymtable(curr.localsymtable).allsymbolsused;
             tstoredsymtable(curr.localsymtable).allprivatesused;
             tstoredsymtable(curr.localsymtable).check_forwards;

             { Note: all contained units are considered as used }
           end;

         if target_info.system in systems_all_windows+systems_nativent then
           begin
             main_procinfo:=create_main_proc('_PkgEntryPoint',potype_pkgstub,curr.localsymtable);
             main_procinfo.procdef.aliasnames.concat('_DLLMainCRTStartup');
             main_procinfo.code:=generate_pkg_stub(main_procinfo.procdef);
             main_procinfo.generate_code;
           end;

{$ifdef DEBUG_NODE_XML}
         XMLFinalizeNodeFile('package');
{$endif DEBUG_NODE_XML}

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
           begin
             Message1(unit_f_errors_in_unit,tostr(Errorcount));
             status.skip_error:=true;
             pkg.free;
             exit;
           end;

         { remove all unused units, this happends when units are removed
           from the uses clause in the source and the ppu was already being loaded }
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
          begin
            hp2:=hp;
            hp:=tmodule(hp.next);
            if assigned(hp2.package) then
              add_package_unit_ref(hp2.package);
            if hp2.is_unit and
               not assigned(hp2.globalsymtable) then
              loaded_units.remove(hp2);
          end;

         exportlib.ignoreduplicates:=true;

         { force exports }
         uu:=tused_unit(usedunits.first);
         while assigned(uu) do
           begin
             if not assigned(systemunit) and (uu.u.modulename^='SYSTEM') then
               begin
                 systemunit:=tglobalsymtable(uu.u.globalsymtable);
                 load_intern_types;
               end;
             if not assigned(uu.u.package) then
               export_unit(uu.u);

             uu:=tused_unit(uu.next);
           end;

{$ifdef arm}
         { Insert .pdata section for arm-wince.
           It is needed for exception handling. }
         if target_info.system in [system_arm_wince] then
           InsertPData;
{$endif arm}

         { generate debuginfo }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.inserttypeinfo;

         exportlib.generatelib;

         exportlib.ignoreduplicates:=false;

         { create import libraries for all packages }
         if packagelist.count>0 then
           createimportlibfromexternals;

         { generate imports }
         if curr.ImportLibraryList.Count>0 then
           importlib.generatelib;

         { Reference all DEBUGINFO sections from the main .fpc section }
         if (cs_debuginfo in current_settings.moduleswitches) then
           current_debuginfo.referencesections(current_asmdata.asmlists[al_procedures]);

         { insert own objectfile }
         insertobjectfile(curr);

         { assemble and link }
         create_objectfile(curr);

         { We might need the symbols info if not using
           the default do_extractsymbolinfo
           which is a dummy function PM }
         needsymbolinfo:=do_extractsymbolinfo<>@def_extractsymbolinfo;
         { release all local symtables that are not needed anymore }
         if (not needsymbolinfo) then
           free_localsymtables(curr.localsymtable);

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            pkg.free;
            exit;
          end;

         if (not curr.is_unit) then
           begin
             { we add all loaded units that are not part of a package to the
               package; this includes units in the "contains" section as well
               as implicitely imported ones }
             hp:=tmodule(loaded_units.first);
             while assigned(hp) do
              begin
                if (hp<>curr) then
                  begin
                    if not assigned(hp.package) then
                      begin
                        pkg.addunit(hp);
                        check_for_indirect_package_usages(hp.used_units);
                      end
                    else
                      begin
                        pentry:=ppackageentry(packagelist.find(hp.package.packagename^));
                        if not assigned(pentry) then
                          internalerror(2015112301);
                        pkg.add_required_package(hp.package);
                      end;
                  end;
                hp:=tmodule(hp.next);
              end;

             pkg.initmoduleinfo(curr);

             { create the executable when we are at level 1 }
             if (curr.is_initial) then
               begin
                 { create global resource file by collecting all resource files }
                 CollectResourceFiles;
                 { write .def file }
                 if (cs_link_deffile in current_settings.globalswitches) then
                   deffile.writefile;

                 { generate the pcp file }
                 pkg.savepcp;

                 { insert all .o files from all loaded units and
                   unload the units, we don't need them anymore.
                   Keep the curr because that is still needed }
                 hp:=tmodule(loaded_units.first);
                 while assigned(hp) do
                  begin
                    { only link in those units which should become part of this
                      package }
                    if not assigned(hp.package) then
                      linker.AddModuleFiles(hp);
                    hp2:=tmodule(hp.next);
                    if (hp<>curr) and
                       (not needsymbolinfo) then
                      begin
                        loaded_units.remove(hp);
                        hp.free;
                      end;
                    hp:=hp2;
                  end;
                 { add the library of directly used packages }
                 add_package_libs(linker);
                 { and now link the package library }
                 linker.MakeSharedLibrary
               end;

             { Give Fatal with error count for linker errors }
             if (Errorcount>0) and not status.skip_error then
              begin
                Message1(unit_f_errors_in_unit,tostr(Errorcount));
                status.skip_error:=true;
              end;

             pkg.free;
          end;
      end;

    procedure proc_create_executable(curr, sysinitmod: tmodule; islibrary : boolean);

      var
        program_uses_checkpointer : boolean;
        hp,hp2 : tmodule;

      begin
            { create global resource file by collecting all resource files }
            CollectResourceFiles;
            { write .def file }
            if (cs_link_deffile in current_settings.globalswitches) then
             deffile.writefile;
            { link SysInit (if any) first, to have behavior consistent with
              assembler startup files }
            if assigned(sysinitmod) then
              linker.AddModuleFiles(sysinitmod);
            { Does any unit use checkpointer function }
            program_uses_checkpointer:=false;
            { insert all .o files from all loaded units and
              unload the units, we don't need them anymore.
              Keep the curr because that is still needed }
            hp:=tmodule(loaded_units.first);
            while assigned(hp) do
             begin
               if (hp<>sysinitmod) and not assigned(hp.package) then
                 begin
                   linker.AddModuleFiles(hp);
                   if mf_checkpointer_called in hp.moduleflags then
                     program_uses_checkpointer:=true;
                 end;
               hp2:=tmodule(hp.next);
               if assigned(hp.package) then
                 add_package_unit_ref(hp.package);
               if (hp<>curr) and
                  (not needsymbolinfo) then
                 begin
                   loaded_units.remove(hp);
                   hp.free;
                 end;
               hp:=hp2;
             end;
            { free also unneeded units we didn't free before }
            if not needsymbolinfo then
              unloaded_units.Clear;
            { Does any unit use checkpointer function }
            if program_uses_checkpointer then
              Message1(link_w_program_uses_checkpointer,curr.modulename^);

            { add all directly used packages as libraries }
            add_package_libs(linker);
            { finally we can create an executable }
            if curr.islibrary then
              linker.MakeSharedLibrary
            else
              linker.MakeExecutable;

            { collect all necessary information for whole-program optimization }
            wpoinfomanager.extractwpoinfofromprogram;

      end;

    procedure proc_program_after_parsing(curr : tmodule; islibrary : boolean);

      var
        sysinitmod, hp,hp2 : tmodule;
        resources_used : boolean;


      begin
        sysinitmod:=nil;
        hp:=nil;
        hp2:=nil;
        resources_used:=false;

  {$ifdef DEBUG_NODE_XML}
        if IsLibrary then
          XMLFinalizeNodeFile('library')
        else
          XMLFinalizeNodeFile('program');
  {$endif DEBUG_NODE_XML}

        { reset wpo flags for all defs }
        reset_all_defs(curr);

        if (Errorcount=0) then
          begin
            { test static symtable }
            tstoredsymtable(curr.localsymtable).allsymbolsused;
            tstoredsymtable(curr.localsymtable).allprivatesused;
            tstoredsymtable(curr.localsymtable).check_forwards;

            curr.allunitsused;
          end;

        { leave when we got an error }
        if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

        { remove all unused units, this happens when units are removed
          from the uses clause in the source and the ppu was already being loaded }
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
         begin
           hp2:=hp;
           hp:=tmodule(hp.next);
           if hp2.is_unit and
              not assigned(hp2.globalsymtable) then
               begin
                 loaded_units.remove(hp2);
                 unloaded_units.concat(hp2);
               end;
         end;

        { do we need to add the variants unit? }
        maybeloadvariantsunit(curr);

        { Now that everything has been compiled we know if we need resource
          support. If not, remove the unit. }
        resources_used:=MaybeRemoveResUnit(curr);

        linker.initsysinitunitname;
        if target_info.system in systems_internal_sysinit then
        begin
          { add start/halt unit }
          sysinitmod:=AddUnit(curr,linker.sysinitunit);
        end
        else
          sysinitmod:=nil;

  {$ifdef arm}
        { Insert .pdata section for arm-wince.
          It is needed for exception handling. }
        if target_info.system in [system_arm_wince] then
          InsertPData;
  {$endif arm}

        cnodeutils.InsertThreadvars;

        { generate rtti/init tables }
        write_persistent_type_info(curr.localsymtable,false);

        { if an Objective-C module, generate rtti and module info }
        MaybeGenerateObjectiveCImageInfo(nil,curr.localsymtable);

        { generate debuginfo }
        if (cs_debuginfo in current_settings.moduleswitches) then
          current_debuginfo.inserttypeinfo;

        if islibrary or (target_info.system in systems_unit_program_exports) then
          exportlib.generatelib;

        { Reference all DEBUGINFO sections from the main .fpc section }
        if (cs_debuginfo in current_settings.moduleswitches) then
          current_debuginfo.referencesections(current_asmdata.asmlists[al_procedures]);

        { Resource strings }
        GenerateResourceStrings;

        { Windows widestring needing initialization }
        cnodeutils.InsertWideInits;

        { Resourcestring references (const foo:string=someresourcestring) }
        cnodeutils.InsertResStrInits;

        { insert Tables and StackLength }
        cnodeutils.InsertInitFinalTable(curr);
        cnodeutils.InsertThreadvarTablesTable;
        cnodeutils.InsertResourceTablesTable;
        cnodeutils.InsertWideInitsTablesTable;
        cnodeutils.InsertResStrTablesTable;
        cnodeutils.InsertMemorySizes;

        { Insert symbol to resource info }
        cnodeutils.InsertResourceInfo(resources_used);

        { create callframe info }
        create_dwarf_frame;

        { create import library for all packages }
        if packagelist.count>0 then
          createimportlibfromexternals;

        { generate imports }
        if curr.ImportLibraryList.Count>0 then
          importlib.generatelib;

        { insert own objectfile }
        insertobjectfile(curr);

        { assemble and link }
        create_objectfile(curr);

        { We might need the symbols info if not using
          the default do_extractsymbolinfo
          which is a dummy function PM }
        needsymbolinfo:=
          (do_extractsymbolinfo<>@def_extractsymbolinfo) or
          ((current_settings.genwpoptimizerswitches*WPOptimizationsNeedingAllUnitInfo)<>[]);

        { release all local symtables that are not needed anymore }
        if (not needsymbolinfo) then
          free_localsymtables(curr.localsymtable);

        { leave when we got an error }
        if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;
        { create the executable when we are at level 1 }

        if (not curr.is_unit) and (curr.is_initial) then
          proc_create_executable(curr,sysinitmod,islibrary);

        { Give Fatal with error count for linker errors }
        if (Errorcount>0) and not status.skip_error then
         begin
           Message1(unit_f_errors_in_unit,tostr(Errorcount));
           status.skip_error:=true;
         end;

        curr.state:=ms_processed;

      end;

    function proc_program_declarations(curr : tmodule; islibrary : boolean) : boolean;

      var
        initpd    : tprocdef;
        finalize_procinfo,
        init_procinfo,
        main_procinfo : tcgprocinfo;
        force_init_final : boolean;

      begin
        result:=true;
        main_procinfo:=nil;
        init_procinfo:=nil;
        finalize_procinfo:=nil;

        set_current_module(curr);

        { All units are read, now give them a number }
        curr.updatemaps;

        { consume the semicolon after maps have been updated else conditional compiling expressions
          might cause internal errors, see tw8611 }
        if curr.consume_semicolon_after_uses then
          consume(_SEMICOLON);

        connect_loaded_units(curr,nil);

        {Insert the name of the main program into the symbol table.}
        if curr.realmodulename^<>'' then
          tabstractunitsymtable(curr.localsymtable).insertunit(cunitsym.create(curr.realmodulename^,curr));

        Message1(parser_u_parsing_implementation,curr.mainsource);

        symtablestack.push(curr.localsymtable);

  {$ifdef jvm}
        { fake classdef to represent the class corresponding to the unit }
        addmoduleclass(curr);
  {$endif}

        { Insert _GLOBAL_OFFSET_TABLE_ symbol if system uses it }
        maybe_load_got(curr);

        { create whole program optimisation information }
        curr.wpoinfo:=tunitwpoinfo.create;

        { The program intialization needs an alias, so it can be called
          from the bootstrap code.}
        if islibrary then
         begin
           initpd:=nil;
           { ToDo: other systems that use indirect entry info, but check back with Windows! }
           { we need to call FPC_LIBMAIN in sysinit which in turn will call PascalMain -> create dummy stub }
           if target_info.system in systems_darwin then
             begin
               main_procinfo:=create_main_proc(make_mangledname('sysinitcallthrough',curr.localsymtable,'stub'),potype_libmainstub,curr.localsymtable);
               call_through_new_name(main_procinfo.procdef,target_info.cprefix+'FPC_LIBMAIN');
               initpd:=main_procinfo.procdef;
               main_procinfo.free;
             end;

           main_procinfo:=create_main_proc(make_mangledname('',curr.localsymtable,mainaliasname),potype_proginit,curr.localsymtable);
           { Win32 startup code needs a single name }
           if not(target_info.system in (systems_darwin+systems_aix)) then
             main_procinfo.procdef.aliasnames.concat('PASCALMAIN')
           else
             main_procinfo.procdef.aliasnames.concat(target_info.Cprefix+'PASCALMAIN');

           if not(target_info.system in systems_darwin) then
             initpd:=main_procinfo.procdef;

           cnodeutils.RegisterModuleInitFunction(initpd);
         end
        else if (target_info.system in ([system_i386_netware,system_i386_netwlibc,system_powerpc_macosclassic]+systems_darwin+systems_aix)) then
          begin
            { create a stub with the name of the desired main routine, with
              the same signature as the C "main" function, and call through to
              FPC_SYSTEMMAIN, which will initialise everything based on its
              parameters. This function cannot be in the system unit, because
              its name can be configured on the command line (for use with e.g.
              SDL, where the main function should be called SDL_main) }
            main_procinfo:=create_main_proc(mainaliasname,potype_mainstub,curr.localsymtable);
            call_through_new_name(main_procinfo.procdef,target_info.cprefix+'FPC_SYSTEMMAIN');
            main_procinfo.free;
            { now create the PASCALMAIN routine (which will be called from
              FPC_SYSTEMMAIN) }
            main_procinfo:=create_main_proc('PASCALMAIN',potype_proginit,curr.localsymtable);
          end
        else
          begin
            main_procinfo:=create_main_proc(mainaliasname,potype_proginit,curr.localsymtable);
            main_procinfo.procdef.aliasnames.concat('PASCALMAIN');
          end;
        main_procinfo.parse_body;
        { save file pos for debuginfo }
        curr.mainfilepos:=main_procinfo.entrypos;

        { finalize? }
        if token=_FINALIZATION then
          begin
             { Parse the finalize }
             finalize_procinfo:=create_main_proc(make_mangledname('',curr.localsymtable,'finalize$'),potype_unitfinalize,curr.localsymtable);
             finalize_procinfo.procdef.aliasnames.insert(make_mangledname('FINALIZE$',curr.localsymtable,''));
             finalize_procinfo.procdef.aliasnames.concat('PASCALFINALIZE');
             finalize_procinfo.parse_body;
          end;

        { Generate specializations of objectdefs methods }
        if Errorcount=0 then
          generate_specialization_procs;

        { This needs to be done before we generate the VMTs }
        if (target_cpu=tsystemcpu.cpu_wasm32) then
          add_synthetic_interface_classes_for_st(curr.localsymtable);

        { Generate VMTs }
        if Errorcount=0 then
          write_vmts(curr.localsymtable,false);

        { add implementations for synthetic method declarations added by
          the compiler }
        add_synthetic_method_implementations(curr.localsymtable);

        { generate construction functions for all attributes in the program }
        generate_attr_constrs(curr.used_rtti_attrs);

        { should we force unit initialization? }
        force_init_final:=tstaticsymtable(curr.localsymtable).needs_init_final;
        if force_init_final or cnodeutils.force_init then
          init_procinfo:=gen_implicit_initfinal(curr,mf_init,curr.localsymtable);

        { Add symbol to the exports section for win32 so smartlinking a
          DLL will include the edata section }
        if assigned(exportlib) and
           (target_info.system in [system_i386_win32,system_i386_wdosx]) and
           (mf_has_exports in curr.moduleflags) then
          current_asmdata.asmlists[al_procedures].concat(tai_const.createname(make_mangledname('EDATA',curr.localsymtable,''),0));

        if (force_init_final or cnodeutils.force_final) and
           (
             not assigned(finalize_procinfo)
             or has_no_code(finalize_procinfo.code)
           ) then
          begin
            { first release the not used finalize procinfo }
            if assigned(finalize_procinfo) then
              begin
                release_proc_symbol(finalize_procinfo.procdef);
                release_main_proc(curr,finalize_procinfo);
              end;
            finalize_procinfo:=gen_implicit_initfinal(curr,mf_finalize,curr.localsymtable);
          end;

         { the finalization routine of libraries is generic (and all libraries need to }
         { be finalized, so they can finalize any units they use                       }
         { Place in "pure assembler" list so that the llvm assembler writer
           directly emits the generated directives }
         if (islibrary) then
           cnodeutils.RegisterModuleFiniFunction(search_system_proc('fpc_lib_exit'));

        { all labels must be defined before generating code }
        if Errorcount=0 then
          tstoredsymtable(curr.localsymtable).checklabels;

        { See remark in unit init/final }
        main_procinfo.generate_code_tree;
        main_procinfo.resetprocdef;
        release_main_proc(curr,main_procinfo);
        if assigned(init_procinfo) then
          begin
            { initialization can be implicit only }
            include(curr.moduleflags,mf_init);
            init_procinfo.code:=cnodeutils.wrap_proc_body(init_procinfo.procdef,init_procinfo.code);
            init_procinfo.generate_code;
            init_procinfo.resetprocdef;
            release_main_proc(curr,init_procinfo);
          end;
        if assigned(finalize_procinfo) then
          begin
            if force_init_final or
               cnodeutils.force_init or
               not(has_no_code(finalize_procinfo.code)) then
              begin
                finalize_procinfo.code:=cnodeutils.wrap_proc_body(finalize_procinfo.procdef,finalize_procinfo.code);
                finalize_procinfo.generate_code_tree;
                include(curr.moduleflags,mf_finalize);
              end;
            finalize_procinfo.resetprocdef;
            release_main_proc(curr,finalize_procinfo);
          end;

        symtablestack.pop(curr.localsymtable);

        { consume the last point }
        consume(_POINT);


        proc_program_after_parsing(curr,islibrary);


      end;

    procedure proc_library_header(curr: tmodule);
      var
        program_name : ansistring;

      begin
        consume(_LIBRARY);
        program_name:=orgpattern;
        consume(_ID);
        while token=_POINT do
         begin
           consume(_POINT);
           program_name:=program_name+'.'+orgpattern;
           consume(_ID);
         end;
        curr.setmodulename(program_name);
        curr.islibrary:=true;
        exportlib.preparelib(program_name);

        if tf_library_needs_pic in target_info.flags then
         begin
           include(current_settings.moduleswitches,cs_create_pic);
           { also set create_pic for all unit compilation }
           include(init_settings.moduleswitches,cs_create_pic);
         end;

        { setup things using the switches, do this before the semicolon, because after the semicolon has been
         read, all following directives are parsed as well }
        setupglobalswitches;

        {$ifdef DEBUG_NODE_XML}
        XMLInitializeNodeFile('library', program_name);
        {$endif DEBUG_NODE_XML}
      end;

    type
      TProgramParam = record
        name : ansistring;
        nr : dword;
      end;
      TProgramParamArray = array of TProgramParam;

      procedure proc_program_header(curr: tmodule; out sc : TProgramParamArray);

        var
          program_name : ansistring;
          paramnum : integer;

        begin
          sc:=nil;
          consume(_PROGRAM);
          program_name:=orgpattern;
          consume(_ID);
          while token=_POINT do
            begin
              consume(_POINT);
              program_name:=program_name+'.'+orgpattern;
              consume(_ID);
            end;
          curr.setmodulename(program_name);
          if (target_info.system in systems_unit_program_exports) then
            exportlib.preparelib(program_name);
          if token=_LKLAMMER then
            begin
               consume(_LKLAMMER);
               paramnum:=1;
               repeat
                 if m_isolike_program_para in current_settings.modeswitches then
                   begin
                     if (pattern<>'INPUT') and (pattern<>'OUTPUT') then
                       begin
                         { the symtablestack is not setup here, so text must be created later on }
                         Setlength(sc,length(sc)+1);
                         with sc[high(sc)] do
                           begin
                             name:=pattern;
                             nr:=paramnum;
                           end;
                         inc(paramnum);
                       end;
                   end;
                 consume(_ID);
               until not try_to_consume(_COMMA);
               consume(_RKLAMMER);
            end;

          { setup things using the switches, do this before the semicolon, because after the semicolon has been
            read, all following directives are parsed as well }
          setupglobalswitches;


{$ifdef DEBUG_NODE_XML}
          XMLInitializeNodeFile('program', program_name);
{$endif DEBUG_NODE_XML}
        end;

    function proc_program(curr: tmodule; islibrary : boolean) : boolean;

      var
         main_file : tinputfile;
         consume_semicolon_after_uses,
         consume_semicolon_after_loaded : boolean;
         ps : tprogramparasym;
         textsym : ttypesym;
         sc : TProgramParamArray;
         i : Longint;
         feature : tfeature;
         load_ok : boolean;

      begin
         result:=true;
         Status.IsLibrary:=IsLibrary;
         Status.IsPackage:=false;
         Status.IsExe:=true;
         parse_only:=false;
         consume_semicolon_after_loaded:=false;

         { make the compiler happy and avoid an uninitialized variable warning on Setlength(sc,length(sc)+1); }
         sc:=nil;

         { DLL defaults to create reloc info }
         if islibrary or (target_info.system in [system_aarch64_win64]) then
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
            (target_info.system in systems_all_windows+[system_i386_wdosx]) and
            (cs_link_extern in current_settings.globalswitches) then
           begin
              include(current_settings.globalswitches,cs_link_strip);
              { Warning stabs info does not work with reloc section !! }
              if (cs_debuginfo in current_settings.moduleswitches) and
                 (target_dbg.id=dbg_stabs) then
                begin
                  Message1(parser_w_parser_reloc_no_debug,curr.mainsource);
                  Message(parser_w_parser_win32_debug_needs_WN);
                  exclude(current_settings.moduleswitches,cs_debuginfo);
                end;
           end;
         { get correct output names }
         main_file := current_scanner.inputfile;
         while assigned(main_file.next) do
           main_file := main_file.next;

         curr.SetFileName(main_file.path+main_file.name,true);

         if islibrary then
           begin
             proc_library_header(curr);
             consume_semicolon_after_loaded:=true;
           end
         else if token=_PROGRAM then
           { is there an program head ? }
           begin
             proc_program_header(curr,sc);
             consume_semicolon_after_loaded:=true;
           end
         else
           begin
             if (target_info.system in systems_unit_program_exports) then
               exportlib.preparelib(curr.realmodulename^);

             { setup things using the switches }
             setupglobalswitches;

{$ifdef DEBUG_NODE_XML}
             XMLInitializeNodeFile('program', curr.realmodulename^);
{$endif DEBUG_NODE_XML}
           end;

         { load all packages, so we know whether a unit is contained inside a
           package or not }
         load_packages;

         { set implementation flag }
         curr.in_interface:=false;
         curr.interface_compiled:=true;

         { insert after the unit symbol tables the static symbol table
           of the program                                              }
         curr.localsymtable:=tstaticsymtable.create(curr.modulename^,curr.moduleid);

         { load system unit }
         load_ok:=loadsystemunit(curr);

         { consume the semicolon now that the system unit is loaded }
         if consume_semicolon_after_loaded then
           consume(_SEMICOLON);

         { global switches are read, so further changes aren't allowed }
         curr.in_global:=false;
  
         { system unit is loaded, now insert feature defines }
         for feature:=low(tfeature) to high(tfeature) do
           if feature in features then
             def_system_macro('FPC_HAS_FEATURE_'+featurestr[feature]);

         { load standard units, e.g objpas,profile unit }
         load_ok:=loaddefaultunits(curr) and load_ok;

         { Load units provided on the command line }
         load_ok:=loadautounits(curr) and load_ok;

         { insert iso program parameters }
         if length(sc)>0 then
           begin
             textsym:=search_system_type('TEXT');
             if not(assigned(textsym)) then
               internalerror(2013011201);
             for i:=0 to high(sc) do
               begin
                 ps:=cprogramparasym.create(sc[i].name,sc[i].nr);
                 curr.localsymtable.insertsym(ps,true);
               end;
           end;

         { Load the units used by the program we compile. }
         if token=_USES then
           begin
             // We can do this here: if there is no uses then the namespace directive makes no sense.
             if Assigned(curr) then
               curr.Loadlocalnamespacelist
             else
               current_namespacelist:=Nil;
             parseusesclause(curr);
             load_ok:=loadunits(curr,false) and load_ok;
             consume_semicolon_after_uses:=true;
           end
         else
           consume_semicolon_after_uses:=false;

         Curr.consume_semicolon_after_uses:=consume_semicolon_after_uses;

         if not load_ok then
           curr.state:=ms_compiling_wait;


         { Can we continue compiling ? }

         result:=curr.state<>ms_compiling_wait;
         if result then
           result:=proc_program_declarations(curr,islibrary)
      end;

end.
