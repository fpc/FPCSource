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
       globtype,version,systems,tokens,
       cutils,cclasses,comphook,
       globals,verbose,fmodule,finput,fppu,
       symconst,symbase,symtype,symdef,symsym,symtable,
       aasmtai,aasmcpu,aasmbase,
       cgbase,cgobj,
       nbas,ncgutil,
       link,assemble,import,export,gendef,ppu,comprsrc,dbgbase,
       cresstr,procinfo,
       dwarf,pexports,
{$ifdef GDB}
       gdb,
{$endif GDB}
       scanner,pbase,pexpr,psystem,psub,pdecsub;

(*
    procedure fixseg(p:TAAsmoutput; sec:TAsmSectionType; secname: string);
      begin
        maybe_new_object_file(p);
        if target_info.system <> system_powerpc_macos then
          p.insert(Tai_section.Create(sec,'',0))
        else
          p.insert(Tai_section.Create(sec,secname,0));
      end;
*)

    procedure create_objectfile;
      var
        DLLScanner      : TDLLScanner;
        s               : string;
        KeepShared      : TStringList;
      begin
        { try to create import entries from system dlls }
        if target_info.DllScanSupported and
           (not current_module.linkOtherSharedLibs.Empty) then
         begin
           { Init DLLScanner }
           if assigned(CDLLScanner[target_info.system]) then
            DLLScanner:=CDLLScanner[target_info.system].Create
           else
            internalerror(200104121);
           KeepShared:=TStringList.Create;
           { Walk all shared libs }
           While not current_module.linkOtherSharedLibs.Empty do
            begin
              S:=current_module.linkOtherSharedLibs.Getusemask(link_allways);
              if not DLLScanner.scan(s) then
               KeepShared.Concat(s);
            end;
           DLLscanner.Free;
           { Recreate import section }
           if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            begin
              if assigned(asmlist[al_imports]) then
               asmlist[al_imports].clear
              else
               asmlist[al_imports]:=taasmoutput.Create;
              importlib.generatelib;
            end;
           { Readd the not processed files }
           while not KeepShared.Empty do
            begin
              s:=KeepShared.GetFirst;
              current_module.linkOtherSharedLibs.add(s,link_allways);
            end;
           KeepShared.Free;
         end;

        { Start and end of debuginfo, at least required for stabs
          to insert n_sourcefile lines }
        if (cs_debuginfo in aktmoduleswitches) or
           (cs_gdb_lineinfo in aktglobalswitches) then
          begin
            debuginfo.insertmodulestart(asmlist[al_debugstart]);
            debuginfo.insertmoduleend(asmlist[al_debugend]);
          end;

        { create the .s file and assemble it }
        GenerateAsm(false);

        { Also create a smartlinked version ? }
        if (cs_create_smart in aktmoduleswitches) and
           not(af_smartlink_sections in target_asm.flags) then
         begin
           { regenerate the importssection for win32 }
           if assigned(asmlist[al_imports]) and
              (target_info.system in [system_i386_win32,system_i386_wdosx, system_arm_wince,system_i386_wince]) then
            begin
              asmlist[al_imports].clear;
              importlib.generatesmartlib;
            end;

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

        if (cs_create_smart in aktmoduleswitches) and
           not(af_smartlink_sections in target_asm.flags) then
         begin
           current_module.linkunitstaticlibs.add(current_module.staticlibfilename^,link_smart);
           current_module.flags:=current_module.flags or uf_smart_linked;
         end;
      end;


    procedure create_dwarf;
      begin
        asmlist[al_dwarf]:=taasmoutput.create;
        { Call frame information }
        if (tf_needs_dwarf_cfi in target_info.flags) and
           (af_supports_dwarf in target_asm.flags) then
          dwarfcfi.generate_code(asmlist[al_dwarf]);
      end;


(*
    procedure insertsegment;
      var
        oldaktfilepos : tfileposinfo;
        {Note: Sections get names in macos only.}
      begin
      { Insert Ident of the compiler }
        if (not (cs_create_smart in aktmoduleswitches))
{$ifndef EXTDEBUG}
           and (not current_module.is_unit)
{$endif}
           then
         begin
           { align the first data }
           asmlist[al_globals].insert(Tai_align.Create(const_align(32)));
           asmlist[al_globals].insert(Tai_string.Create('FPC '+full_version_string+
             ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname));
         end;
        { align code segment }
        asmlist[al_procedures].concat(Tai_align.create(aktalignment.procalign));
        { Insert start and end of sections }
        fixseg(asmlist[al_procedures],sec_code,'____seg_code');
        fixseg(asmlist[al_globals],sec_data,'____seg_data');
        fixseg(asmlist[al_const],sec_rodata,'____seg_rodata');
//        fixseg(asmlist[al_bss],sec_bss,'____seg_bss');
        fixseg(asmlist[al_threadvars],sec_bss,'____seg_tbss');
        { we should use .rdata section for these two no ?
          .rdata is a read only data section (PM) }
        fixseg(asmlist[al_rtti],sec_data,'____seg_rtti');
        fixseg(asmlist[al_typedconsts],sec_data,'____seg_consts');
        fixseg(asmlist[al_rotypedconsts],sec_rodata,'____seg_consts');
        fixseg(asmlist[al_picdata],sec_data,'____seg_al_picdata');
        if assigned(asmlist[aasmtai.al_resourcestrings]) then
          fixseg(asmlist[aasmtai.al_resourcestrings],sec_data,'____seg_resstrings');
{$ifdef GDB}
        if assigned(asmlist[al_debugtypes]) then
          begin
            oldaktfilepos:=aktfilepos;
            aktfilepos.line:=0;
            asmlist[al_debugtypes].insert(Tai_symbol.Createname('gcc2_compiled',AT_DATA,0));
            asmlist[al_debugtypes].insert(Tai_symbol.Createname('fpc_compiled',AT_DATA,0));
//            fixseg(asmlist[al_debugtypes],sec_code,'____seg_debug');
            aktfilepos:=oldaktfilepos;
          end;
{$endif GDB}
      end;
*)

{$ifndef segment_threadvars}
    procedure InsertThreadvarTablesTable;
      var
        hp : tused_unit;
        ltvTables : taasmoutput;
        count : longint;
      begin
        ltvTables:=TAAsmOutput.Create;
        count:=0;
        hp:=tused_unit(usedunits.first);
        while assigned(hp) do
         begin
           If (hp.u.flags and uf_threadvars)=uf_threadvars then
            begin
              ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',hp.u.globalsymtable,''),AT_DATA,0));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program threadvars, if any }
        If (current_module.flags and uf_threadvars)=uf_threadvars then
         begin
           ltvTables.concat(Tai_const.Createname(make_mangledname('THREADVARLIST',current_module.localsymtable,''),AT_DATA,0));
           inc(count);
         end;
        { Insert TableCount at start }
        ltvTables.insert(Tai_const.Create_32bit(count));
        { insert in data segment }
        maybe_new_object_file(asmlist[al_globals]);
        new_section(asmlist[al_globals],sec_data,'FPC_THREADVARTABLES',sizeof(aint));
        asmlist[al_globals].concat(Tai_symbol.Createname_global('FPC_THREADVARTABLES',AT_DATA,0));
        asmlist[al_globals].concatlist(ltvTables);
        asmlist[al_globals].concat(Tai_symbol_end.Createname('FPC_THREADVARTABLES'));
        ltvTables.free;
      end;

    procedure AddToThreadvarList(p:tnamedindexitem;arg:pointer);
      var
        ltvTable : taasmoutput;
      begin
        ltvTable:=taasmoutput(arg);
        if (tsym(p).typ=globalvarsym) and
           (vo_is_thread_var in tglobalvarsym(p).varoptions) then
         begin
           { address of threadvar }
           ltvTable.concat(tai_const.Createname(tglobalvarsym(p).mangledname,AT_DATA,0));
           { size of threadvar }
           ltvTable.concat(tai_const.create_32bit(tglobalvarsym(p).getsize));
         end;
      end;


    procedure InsertThreadvars;
      var
        s : string;
        ltvTable : TAAsmoutput;
      begin
         ltvTable:=TAAsmoutput.create;
         if assigned(current_module.globalsymtable) then
           current_module.globalsymtable.foreach_static(@AddToThreadvarList,ltvTable);
         current_module.localsymtable.foreach_static(@AddToThreadvarList,ltvTable);
         if ltvTable.first<>nil then
          begin
            s:=make_mangledname('THREADVARLIST',current_module.localsymtable,'');
            { end of the list marker }
            ltvTable.concat(tai_const.create_sym(nil));
            { add to datasegment }
            maybe_new_object_file(asmlist[al_globals]);
            new_section(asmlist[al_globals],sec_data,s,sizeof(aint));
            asmlist[al_globals].concat(Tai_symbol.Createname_global(s,AT_DATA,0));
            asmlist[al_globals].concatlist(ltvTable);
            asmlist[al_globals].concat(Tai_symbol_end.Createname(s));
            current_module.flags:=current_module.flags or uf_threadvars;
          end;
         ltvTable.Free;
      end;
{$endif}

    Procedure InsertResourceInfo;

    var
      hp           : tused_unit;
      found        : Boolean;
      I            : Integer;
      ResourceInfo : taasmoutput;

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
        ResourceInfo:=TAAsmOutput.Create;
        if found then
          begin
          { Valid pointer to resource information }
          ResourceInfo.concat(Tai_symbol.Createname_global('FPC_RESLOCATION',AT_DATA,0));
          ResourceInfo.concat(Tai_const.Createname('FPC_RESSYMBOL',AT_DATA,0));
{$ifdef EXTERNALRESPTRS}
          current_module.linkotherofiles.add('resptrs.o',link_allways);
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
        maybe_new_object_file(asmlist[al_globals]);
        asmlist[al_globals].concatlist(ResourceInfo);
        ResourceInfo.free;
        end;
    end;

    Procedure InsertResourceTablesTable;
      var
        hp : tused_unit;
        ResourceStringTables : taasmoutput;
        count : longint;
      begin
        ResourceStringTables:=TAAsmOutput.Create;
        count:=0;
        hp:=tused_unit(usedunits.first);
        while assigned(hp) do
         begin
           If (hp.u.flags and uf_has_resources)=uf_has_resources then
            begin
              ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESOURCESTRINGLIST',hp.u.globalsymtable,''),AT_DATA,0));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program resources, if any }
        If resourcestrings.ResStrCount>0 then
         begin
           ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESOURCESTRINGLIST',current_module.localsymtable,''),AT_DATA,0));
           Inc(Count);
         end;
        { Insert TableCount at start }
        ResourceStringTables.insert(Tai_const.Create_32bit(count));
        { Add to data segment }
        maybe_new_object_file(asmlist[al_globals]);
        new_section(asmlist[al_globals],sec_data,'FPC_RESOURCESTRINGTABLES',sizeof(aint));
        asmlist[al_globals].concat(Tai_symbol.Createname_global('FPC_RESOURCESTRINGTABLES',AT_DATA,0));
        asmlist[al_globals].concatlist(ResourceStringTables);
        asmlist[al_globals].concat(Tai_symbol_end.Createname('FPC_RESOURCESTRINGTABLES'));
        ResourceStringTables.free;
      end;


    procedure InsertInitFinalTable;
      var
        hp : tused_unit;
        unitinits : taasmoutput;
        count : longint;
      begin
        unitinits:=TAAsmOutput.Create;
        count:=0;
        hp:=tused_unit(usedunits.first);
        while assigned(hp) do
         begin
           { call the unit init code and make it external }
           if (hp.u.flags and (uf_init or uf_finalize))<>0 then
            begin
              if (hp.u.flags and uf_init)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',hp.u.globalsymtable,''),AT_FUNCTION,0))
              else
               unitinits.concat(Tai_const.Create_sym(nil));
              if (hp.u.flags and uf_finalize)<>0 then
               unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',hp.u.globalsymtable,''),AT_FUNCTION,0))
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
            unitinits.concat(Tai_const.Createname(make_mangledname('INIT$',current_module.localsymtable,''),AT_FUNCTION,0))
           else
            unitinits.concat(Tai_const.Create_sym(nil));
           if (current_module.flags and uf_finalize)<>0 then
            unitinits.concat(Tai_const.Createname(make_mangledname('FINALIZE$',current_module.localsymtable,''),AT_FUNCTION,0))
           else
            unitinits.concat(Tai_const.Create_sym(nil));
           inc(count);
         end;
        { Insert TableCount,InitCount at start }
        unitinits.insert(Tai_const.Create_32bit(0));
        unitinits.insert(Tai_const.Create_32bit(count));
        { Add to data segment }
        maybe_new_object_file(asmlist[al_globals]);
        new_section(asmlist[al_globals],sec_data,'INITFINAL',sizeof(aint));
        asmlist[al_globals].concat(Tai_symbol.Createname_global('INITFINAL',AT_DATA,0));
        asmlist[al_globals].concatlist(unitinits);
        asmlist[al_globals].concat(Tai_symbol_end.Createname('INITFINAL'));
        unitinits.free;
      end;


    procedure insertmemorysizes;
      begin
        { stacksize can be specified and is now simulated }
        maybe_new_object_file(asmlist[al_globals]);
        new_section(asmlist[al_globals],sec_data,'__stklen',4);
        asmlist[al_globals].concat(Tai_symbol.Createname_global('__stklen',AT_DATA,4));
        asmlist[al_globals].concat(Tai_const.Create_32bit(stacksize));
        { Initial heapsize }
        maybe_new_object_file(asmlist[al_globals]);
        new_section(asmlist[al_globals],sec_data,'__heapsize',4);
        asmlist[al_globals].concat(Tai_symbol.Createname_global('__heapsize',AT_DATA,4));
        asmlist[al_globals].concat(Tai_const.Create_32bit(heapsize));
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
        tsymtable(hp.globalsymtable).next:=symtablestack;
        symtablestack:=hp.globalsymtable;
        if (m_mac in aktmodeswitches) and assigned(hp.globalmacrosymtable) then
          begin
            tsymtable(hp.globalmacrosymtable).next:=macrosymtablestack;
            macrosymtablestack:=hp.globalmacrosymtable;
          end;
        { insert unitsym }
        unitsym:=tunitsym.create(s,hp.globalsymtable);
        inc(unitsym.refs);
        refsymtable.insert(unitsym);
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
        AddUnit('Variants');
      end;


    procedure loaddefaultunits;
      begin
      { are we compiling the system unit? }
        if (cs_compilesystem in aktmoduleswitches) then
         begin
         { create system defines }
           createconstdefs;
         { we don't need to reset anything, it's already done in parser.pas }
           exit;
         end;
        { insert the system unit, it is allways the first }
        symtablestack:=nil;
        macrosymtablestack:=initialmacrosymtable;
        AddUnit('System');
        SystemUnit:=TGlobalSymtable(Symtablestack);
        { read default constant definitions }
        make_ref:=false;
        readconstdefs;
        make_ref:=true;
        { Set the owner of errorsym and errortype to symtable to
          prevent crashes when accessing .owner }
        generrorsym.owner:=systemunit;
        generrortype.def.owner:=systemunit;
        { Units only required for main module }
        { load heaptrace before any other units especially objpas }
        if not(current_module.is_unit) then
         begin
           { Heaptrc unit }
           if (cs_gdb_heaptrc in aktglobalswitches) then
             AddUnit('HeapTrc');
           { Lineinfo unit }
           if (cs_gdb_lineinfo in aktglobalswitches) then
             AddUnit('LineInfo');
           { Lineinfo unit }
           if (cs_gdb_valgrind in aktglobalswitches) then
             AddUnit('CMem');
{$ifdef cpufpemu}
           { Floating point emulation unit? }
           if (cs_fp_emulation in aktmoduleswitches) and not(target_info.system in system_wince) then
             AddUnit('SoftFpu');
{$endif cpufpemu}
         end;
        { Objpas unit? }
        if m_objpas in aktmodeswitches then
          AddUnit('ObjPas');
        { Macpas unit? }
        if m_mac in aktmodeswitches then
          AddUnit('MacPas');
        { Profile unit? Needed for go32v2 only }
        if (cs_profile in aktmoduleswitches) and
           (target_info.system in [system_i386_go32v2,system_i386_watcom]) then
          AddUnit('Profile');
        if (cs_load_fpcylix_unit in aktglobalswitches) then
          begin
            AddUnit('FPCylix');
            AddUnit('DynLibs');
          end;
        { save default symtablestack }
        defaultsymtablestack:=symtablestack;
        defaultmacrosymtablestack:=macrosymtablestack;
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
         s,sorg  : stringid;
         fn      : string;
         pu      : tused_unit;
         hp2     : tmodule;
         hp3     : tsymtable;
         unitsym : tunitsym;
         top_of_macrosymtable : tsymtable;

      begin
         consume(_USES);
{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         repeat
           s:=pattern;
           sorg:=orgpattern;
           consume(_ID);
           { support "<unit> in '<file>'" construct, but not for tp7 }
           if not(m_tp7 in aktmodeswitches) then
            begin
              if try_to_consume(_OP_IN) then
               fn:=FixFileName(get_stringconst)
              else
               fn:='';
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
              refsymtable.insert(unitsym);
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
         top_of_macrosymtable:= macrosymtablestack;
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
               { connect unitsym to the globalsymtable of the unit }
               pu.unitsym.unitsymtable:=pu.u.globalsymtable;
             end;
            pu:=tused_unit(pu.next);
          end;

         { set the symtable to systemunit so it gets reorderd correctly,
           then insert the units in the symtablestack }
         pu:=tused_unit(current_module.used_units.first);
         symtablestack:=defaultsymtablestack;
         macrosymtablestack:=defaultmacrosymtablestack;
         while assigned(pu) do
           begin
              if pu.in_uses then
                begin
                   { Reinsert in symtablestack }
                   hp3:=symtablestack;
                   while assigned(hp3) do
                     begin
                        { insert units only once ! }
                        if pu.u.globalsymtable=hp3 then
                          break;
                        hp3:=hp3.next;
                        { unit isn't inserted }
                        if hp3=nil then
                          begin
                             tsymtable(pu.u.globalsymtable).next:=symtablestack;
                             symtablestack:=tsymtable(pu.u.globalsymtable);
                             if (m_mac in aktmodeswitches) and assigned(pu.u.globalmacrosymtable) then
                               begin
                                 tsymtable(pu.u.globalmacrosymtable).next:=macrosymtablestack;
                                 macrosymtablestack:=tsymtable(pu.u.globalmacrosymtable);
                               end;
{$ifdef DEBUG}
                             test_symtablestack;
{$endif DEBUG}
                          end;
                     end;
                end;
              pu:=tused_unit(pu.next);
           end;

         if assigned (current_module.globalmacrosymtable) then
           top_of_macrosymtable.next.next:= macrosymtablestack
         else
           top_of_macrosymtable.next:= macrosymtablestack;
         macrosymtablestack:= top_of_macrosymtable;
         consume(_SEMICOLON);
      end;


{$IfDef GDB}
     procedure write_gdb_info;

       procedure reset_unit_type_info;
       var
         hp : tmodule;
       begin
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
           begin
             hp.is_stab_written:=false;
             hp:=tmodule(hp.next);
           end;
       end;

       procedure write_used_unit_type_info(hp:tmodule);
       var
         pu : tused_unit;
       begin
         pu:=tused_unit(hp.used_units.first);
         while assigned(pu) do
           begin
             if not pu.u.is_stab_written then
               begin
                 { prevent infinte loop for circular dependencies }
                 pu.u.is_stab_written:=true;
                 { write type info from used units, use a depth first
                   strategy to reduce the recursion in writing all
                   dependent stabs }
                 write_used_unit_type_info(pu.u);
                 if assigned(pu.u.globalsymtable) then
                   tglobalsymtable(pu.u.globalsymtable).concattypestabto(asmlist[al_debugtypes]);
               end;
             pu:=tused_unit(pu.next);
           end;
       end;

      var
        temptypestabs : taasmoutput;
        storefilepos : tfileposinfo;
        st : tsymtable;
      begin
        if not (cs_debuginfo in aktmoduleswitches) then
         exit;
        storefilepos:=aktfilepos;
        aktfilepos:=current_module.mainfilepos;
        { include symbol that will be referenced from the program to be sure to
          include this debuginfo .o file }
        if current_module.is_unit then
          begin
            current_module.flags:=current_module.flags or uf_has_debuginfo;
            st:=current_module.globalsymtable;
          end
        else
          st:=current_module.localsymtable;
        new_section(asmlist[al_debugtypes],sec_data,lower(st.name^),0);
        asmlist[al_debugtypes].concat(tai_symbol.Createname_global(make_mangledname('DEBUGINFO',st,''),AT_DATA,0));
        { first write all global/local symbols again to a temp list. This will flag
          all required tdefs. After that the temp list can be removed since the debuginfo is already
          written to the stabs when the variables/consts were written }
{$warning Hack to get all needed types}
        temptypestabs:=taasmoutput.create;
        if assigned(current_module.globalsymtable) then
          tglobalsymtable(current_module.globalsymtable).concatstabto(temptypestabs);
        if assigned(current_module.localsymtable) then
          tstaticsymtable(current_module.localsymtable).concatstabto(temptypestabs);
        temptypestabs.free;
        { reset unit type info flag }
        reset_unit_type_info;
        { write used types from the used units }
        write_used_unit_type_info(current_module);
        { last write the types from this unit }
        if assigned(current_module.globalsymtable) then
          tglobalsymtable(current_module.globalsymtable).concattypestabto(asmlist[al_debugtypes]);
        if assigned(current_module.localsymtable) then
          tstaticsymtable(current_module.localsymtable).concattypestabto(asmlist[al_debugtypes]);
        { include files }
        if (cs_gdb_dbx in aktglobalswitches) then
          begin
            asmlist[al_debugtypes].concat(tai_comment.Create(strpnew('EINCL of global '+
              tglobalsymtable(current_module.globalsymtable).name^+' has index '+
              tostr(tglobalsymtable(current_module.globalsymtable).moduleid))));
            asmlist[al_debugtypes].concat(Tai_stab.create(stab_stabs,strpnew('"'+
              tglobalsymtable(current_module.globalsymtable).name^+'",'+
              tostr(N_EINCL)+',0,0,0')));
            tglobalsymtable(current_module.globalsymtable).dbx_count_ok:={true}false;
            dbx_counter:=tglobalsymtable(current_module.globalsymtable).prev_dbx_counter;
            do_count_dbx:=false;
          end;
        aktfilepos:=storefilepos;
      end;
{$EndIf GDB}


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


    procedure free_localsymtables(st:tsymtable);
      var
        def : tstoreddef;
        pd  : tprocdef;
      begin
        def:=tstoreddef(st.defindex.first);
        while assigned(def) do
          begin
            if def.deftype=procdef then
              begin
                pd:=tprocdef(def);
                if assigned(pd.localst) and
                   (pd.localst.symtabletype<>staticsymtable) and
                   not((po_inline in pd.procoptions) or
                       ((current_module.flags and uf_local_browser)<>0)) then
                  begin
                    free_localsymtables(pd.localst);
                    pd.localst.free;
                    pd.localst:=nil;
                  end;
              end;
             def:=tstoreddef(def.indexnext);
          end;
      end;


    procedure parse_implementation_uses;
      begin
         if token=_USES then
           begin
              loadunits;
{$ifdef DEBUG}
              test_symtablestack;
{$endif DEBUG}
           end;
      end;


    procedure setupglobalswitches;
      begin
        { can't have local browser when no global browser }
        if (cs_local_browser in aktmoduleswitches) and
           not(cs_browser in aktmoduleswitches) then
          exclude(aktmoduleswitches,cs_local_browser);
        if (cs_create_pic in aktmoduleswitches) then
          def_system_macro('FPC_PIC');
      end;


    function create_main_proc(const name:string;potype:tproctypeoption;st:tsymtable):tprocdef;
      var
        stt : tsymtable;
        ps  : tprocsym;
        pd  : tprocdef;
      begin
        { there should be no current_procinfo available }
        if assigned(current_procinfo) then
         internalerror(200304275);
        {Generate a procsym for main}
        make_ref:=false;
        { try to insert in in static symtable ! }
        stt:=symtablestack;
        symtablestack:=st;
        { generate procsym }
        ps:=tprocsym.create('$'+name);
        { main are allways used }
        inc(ps.refs);
        symtablestack.insert(ps);
        pd:=tprocdef.create(main_program_level);
        include(pd.procoptions,po_global);
        pd.procsym:=ps;
        ps.addprocdef(pd);
        { restore symtable }
        make_ref:=true;
        symtablestack:=stt;
        { set procdef options }
        pd.proctypeoption:=potype;
        pd.proccalloption:=pocall_default;
        pd.forwarddef:=false;
        pd.setmangledname(target_info.cprefix+name);
        pd.aliasnames.insert(pd.mangledname);
        handle_calling_convention(pd);
        { We don't need is a local symtable. Change it into the static
          symtable }
        pd.localst.free;
        pd.localst:=st;
        { set procinfo and current_procinfo.procdef }
        current_procinfo:=cprocinfo.create(nil);
        current_module.procinfo:=current_procinfo;
        current_procinfo.procdef:=pd;
        { return procdef }
        create_main_proc:=pd;
        { main proc does always a call e.g. to init system unit }
        include(current_procinfo.flags,pi_do_call);
      end;


    procedure release_main_proc(pd:tprocdef);
      begin
        { this is a main proc, so there should be no parent }
        if not(assigned(current_procinfo)) or
           assigned(current_procinfo.parent) or
           not(current_procinfo.procdef=pd) then
         internalerror(200304276);
        { remove procinfo }
        current_module.procinfo:=nil;
        current_procinfo.free;
        current_procinfo:=nil;
        { remove localst as it was replaced by staticsymtable }
        pd.localst:=nil;
      end;


    procedure gen_implicit_initfinal(flag:word;st:tsymtable);
      var
        pd : tprocdef;
      begin
        { update module flags }
        current_module.flags:=current_module.flags or flag;
        { create procdef }
        case flag of
          uf_init :
            begin
              pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'init_implicit'),potype_unitinit,st);
              pd.aliasnames.insert(make_mangledname('INIT$',current_module.localsymtable,''));
            end;
          uf_finalize :
            begin
              pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize_implicit'),potype_unitfinalize,st);
              pd.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
            end;
          else
            internalerror(200304253);
        end;
        tcgprocinfo(current_procinfo).code:=cnothingnode.create;
        tcgprocinfo(current_procinfo).generate_code;
        release_main_proc(pd);
      end;

    procedure delete_duplicate_macros(p:TNamedIndexItem; arg:pointer);
    var
      hp: tsymentry;
    begin
      hp:= current_module.localmacrosymtable.search(p.name);
      if assigned(hp) then
        current_module.localmacrosymtable.delete(hp);
    end;

    procedure proc_unit;

      function is_assembler_generated:boolean;
      begin
        is_assembler_generated:=(Errorcount=0) and
          not(
          asmlist[al_procedures].empty and
          asmlist[al_globals].empty and
//          asmlist[al_bss].empty and
          asmlist[al_threadvars].empty and
          asmlist[al_rtti].empty and
          ((asmlist[al_imports]=nil) or asmlist[al_imports].empty) and
          ((asmlist[al_resources]=nil) or asmlist[al_resources].empty) and
          ((asmlist[aasmtai.al_resourcestrings]=nil) or asmlist[aasmtai.al_resourcestrings].empty)
        );
      end;

      var
         main_file: tinputfile;
         st     : tsymtable;
         unitst : tglobalsymtable;
{$ifdef EXTDEBUG}
         store_crc,
{$endif EXTDEBUG}
         store_interface_crc : cardinal;
         s1,s2  : ^string; {Saves stack space}
         force_init_final : boolean;
         pd : tprocdef;
         unitname8 : string[8];
         has_impl,ag: boolean;
      begin
         if m_mac in aktmodeswitches then
           begin
             ConsolidateMode;
             current_module.mode_switch_allowed:= false;
           end;

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
             s2^:=upper(SplitName(main_file.name^));
             unitname8:=copy(current_module.modulename^,1,8);
             if (cs_check_unit_name in aktglobalswitches) and
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
              include(aktmoduleswitches,cs_compilesystem);
             dispose(s2);
             dispose(s1);
          end;

         consume(_ID);
         consume(_SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { handle the global switches }
         ConsolidateMode;
         setupglobalswitches;

         message1(unit_u_loading_interface_units,current_module.modulename^);

         { update status }
         status.currentmodule:=current_module.realmodulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (current_module.modulename^='OBJPAS') then
           exclude(aktmodeswitches,m_objpas);

         { maybe turn off m_mac if we are compiling macpas }
         if (current_module.modulename^='MACPAS') then
           exclude(aktmodeswitches,m_mac);

         parse_only:=true;

         { generate now the global symboltable }
         st:=tglobalsymtable.create(current_module.modulename^,current_module.moduleid);
         refsymtable:=st;
         unitst:=tglobalsymtable(st);
         { define first as local to overcome dependency conflicts }
         current_module.localsymtable:=st;

         { the unit name must be usable as a unit specifier }
         { inside the unit itself (PM)                }
         { this also forbids to have another symbol      }
         { with the same name as the unit                  }
         refsymtable.insert(tunitsym.create(current_module.realmodulename^,unitst));

         macrosymtablestack:= initialmacrosymtable;

         { load default units, like the system unit }
         loaddefaultunits;

         current_module.localmacrosymtable.next:=macrosymtablestack;
         if assigned(current_module.globalmacrosymtable) then
           begin
             current_module.globalmacrosymtable.next:= current_module.localmacrosymtable;
             macrosymtablestack:=current_module.globalmacrosymtable;
           end
         else
           macrosymtablestack:=current_module.localmacrosymtable;

         { reset }
         make_ref:=true;

         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in aktmoduleswitches) then
           begin
              if token=_USES then
                begin
                   loadunits;
                   { has it been compiled at a higher level ?}
                   if current_module.state=ms_compiled then
                     exit;
                end;
              { ... but insert the symbol table later }
              st.next:=symtablestack;
              symtablestack:=st;
           end
         else
         { while compiling a system unit, some types are directly inserted }
           begin
              st.next:=symtablestack;
              symtablestack:=st;
              insert_intern_types(st);
           end;

         { now we know the place to insert the constants }
         constsymtable:=symtablestack;

         { move the global symtab from the temporary local to global }
         current_module.globalsymtable:=current_module.localsymtable;
         current_module.localsymtable:=nil;

         reset_all_defs;

         { number all units, so we know if a unit is used by this unit or
           needs to be added implicitly }
         current_module.updatemaps;

         { ... parse the declarations }
         Message1(parser_u_parsing_interface,current_module.realmodulename^);
         read_interface_declarations;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         { Our interface is compiled, generate CRC and switch to implementation }
         if not(cs_compilesystem in aktmoduleswitches) and
            (Errorcount=0) then
           tppumodule(current_module).getppucrc;
         current_module.in_interface:=false;
         current_module.interface_compiled:=true;

         { First reload all units depending on our interface, we need to do this
           in the implementation part to prevent errorneous circular references }
         reload_flagged_units;

         { Parse the implementation section }
         if (m_mac in aktmodeswitches) and try_to_consume(_END) then
           has_impl:= false
         else
           has_impl:= true;

         parse_only:=false;

         { generates static symbol table }
         st:=tstaticsymtable.create(current_module.modulename^,current_module.moduleid);
         current_module.localsymtable:=st;

         { Swap the positions of the local and global macro sym table}
         if assigned(current_module.globalmacrosymtable) then
           begin
             macrosymtablestack:=current_module.localmacrosymtable;
             current_module.globalmacrosymtable.next:= current_module.localmacrosymtable.next;
             current_module.localmacrosymtable.next:=current_module.globalmacrosymtable;

             current_module.globalmacrosymtable.foreach_static(@delete_duplicate_macros, nil);
           end;

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst.next;

         { we don't want implementation units symbols in unitsymtable !! PM }
         refsymtable:=st;

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

         { now we can change refsymtable }
         refsymtable:=st;

         { but reinsert the global symtable as lasts }
         unitst.next:=symtablestack;
         symtablestack:=unitst;

{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         constsymtable:=symtablestack;

         if has_impl then
           begin
             Message1(parser_u_parsing_implementation,current_module.modulename^);
             if current_module.in_interface then
               internalerror(200212285);

             { Compile the unit }
             pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'init'),potype_unitinit,st);
             pd.aliasnames.insert(make_mangledname('INIT$',current_module.localsymtable,''));
             tcgprocinfo(current_procinfo).parse_body;
             tcgprocinfo(current_procinfo).generate_code;
             tcgprocinfo(current_procinfo).resetprocdef;
             { save file pos for debuginfo }
             current_module.mainfilepos:=current_procinfo.entrypos;
             release_main_proc(pd);
           end;

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=tglobalsymtable(current_module.globalsymtable).needs_init_final or
                           tstaticsymtable(current_module.localsymtable).needs_init_final;

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         if force_init_final and ((current_module.flags and uf_init)=0) then
           gen_implicit_initfinal(uf_init,st);
         { finalize? }
         if has_impl and (token=_FINALIZATION) then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize'),potype_unitfinalize,st);
              pd.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
              tcgprocinfo(current_procinfo).parse_body;
              tcgprocinfo(current_procinfo).generate_code;
              tcgprocinfo(current_procinfo).resetprocdef;
              release_main_proc(pd);
           end
         else if force_init_final then
           gen_implicit_initfinal(uf_finalize,st);

         { the last char should always be a point }
         consume(_POINT);

         { Generate resoucestrings }
         If resourcestrings.ResStrCount>0 then
          begin
            resourcestrings.CreateResourceStringList;
            current_module.flags:=current_module.flags or uf_has_resources;
            { only write if no errors found }
            if (Errorcount=0) then
             resourcestrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
          end;

         if (Errorcount=0) then
           begin
             { tests, if all (interface) forwards are resolved }
             tstoredsymtable(symtablestack).check_forwards;
             { check if all private fields are used }
             tstoredsymtable(symtablestack).allprivatesused;
             { remove cross unit overloads }
             tstoredsymtable(symtablestack).unchain_overloaded;

             { test static symtable }
             tstoredsymtable(st).allsymbolsused;
             tstoredsymtable(st).allprivatesused;
             tstoredsymtable(st).check_forwards;
             tstoredsymtable(st).checklabels;
             tstoredsymtable(st).unchain_overloaded;

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

         { generate debuginfo }
{$ifdef GDB}
         write_gdb_info;
{$endif GDB}

         { generate wrappers for interfaces }
         gen_intf_wrappers(asmlist[al_procedures],current_module.globalsymtable);
         gen_intf_wrappers(asmlist[al_procedures],current_module.localsymtable);

         { generate a list of threadvars }
{$ifndef segment_threadvars}
         InsertThreadvars;
{$endif}

         { generate imports }
         if current_module.uses_imports then
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

         if cs_local_browser in aktmoduleswitches then
           current_module.localsymtable:=refsymtable;

         if ag then
          begin
            { create dwarf debuginfo }
            create_dwarf;
            { finish asmlist by adding segment starts }
//            insertsegment;
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

         if not(cs_compilesystem in aktmoduleswitches) then
           if store_interface_crc<>current_module.interface_crc then
             Message1(unit_u_interface_crc_changed,current_module.ppufilename^);
{$ifdef EXTDEBUG}
         if not(cs_compilesystem in aktmoduleswitches) then
           if (store_crc<>current_module.crc) and simplify_ppu then
             Message1(unit_u_implementation_crc_changed,current_module.ppufilename^);
{$endif EXTDEBUG}

         { release all overload references and local symtables that
           are not needed anymore }
         tstoredsymtable(current_module.localsymtable).unchain_overloaded;
         tstoredsymtable(current_module.globalsymtable).unchain_overloaded;
         free_localsymtables(current_module.globalsymtable);
         free_localsymtables(current_module.localsymtable);

         { remove static symtable (=refsymtable) here to save some mem, possible references
           (like procsym overloads) should already have been freed above }
         if not (cs_local_browser in aktmoduleswitches) then
           begin
              st.free;
              current_module.localsymtable:=nil;
           end;

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
         st        : tsymtable;
         hp,hp2    : tmodule;
         pd        : tprocdef;
      begin
         DLLsource:=islibrary;
         Status.IsLibrary:=IsLibrary;
         Status.IsExe:=true;
         parse_only:=false;

         { DLL defaults to create reloc info }
         if islibrary then
           begin
             if not RelocSectionSetExplicitly then
               RelocSection:=true;
           end;

         { relocation works only without stabs under win32 !! PM }
         { internal assembler uses rva for stabs info
           so it should work with relocated DLLs }
         if RelocSection and
            (target_info.system in [system_i386_win32,system_i386_wdosx]) and
            (target_info.assem<>as_i386_pecoff) then
           begin
              include(aktglobalswitches,cs_link_strip);
              { Warning stabs info does not work with reloc section !! }
              if cs_debuginfo in aktmoduleswitches then
                begin
                  Message1(parser_w_parser_reloc_no_debug,current_module.mainsource^);
                  Message(parser_w_parser_win32_debug_needs_WN);
                  exclude(aktmoduleswitches,cs_debuginfo);
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
              stringdispose(current_module.modulename);
              stringdispose(current_module.realmodulename);
              current_module.modulename:=stringdup(pattern);
              current_module.realmodulename:=stringdup(orgpattern);
              current_module.islibrary:=true;
              exportlib.preparelib(orgpattern);

              if tf_library_needs_pic in target_info.flags then
                include(aktmoduleswitches,cs_create_pic);

              consume(_ID);
              consume(_SEMICOLON);
           end
         else
           { is there an program head ? }
           if token=_PROGRAM then
            begin
              consume(_PROGRAM);
              stringdispose(current_module.modulename);
              stringdispose(current_module.realmodulename);
              current_module.modulename:=stringdup(pattern);
              current_module.realmodulename:=stringdup(orgpattern);
              if (target_info.system in [system_i386_WIN32,system_i386_wdosx]) then
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
         else if (target_info.system in [system_i386_WIN32,system_i386_wdosx]) then
           exportlib.preparelib(current_module.realmodulename^);

         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { setup things using the switches }
         ConsolidateMode;
         setupglobalswitches;

         { set implementation flag }
         current_module.in_interface:=false;
         current_module.interface_compiled:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         st:=tstaticsymtable.create(current_module.modulename^,current_module.moduleid);
         current_module.localsymtable:=st;
         refsymtable:=st;

         macrosymtablestack:= nil;

         { load standard units (system,objpas,profile unit) }
         loaddefaultunits;

         current_module.localmacrosymtable.next:=macrosymtablestack;
         macrosymtablestack:=current_module.localmacrosymtable;

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
           st.insert(tunitsym.create(current_module.realmodulename^,st));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         Message1(parser_u_parsing_implementation,current_module.mainsource^);

         { The program intialization needs an alias, so it can be called
           from the bootstrap code.}

         if islibrary then
          begin
            pd:=create_main_proc(make_mangledname('',current_module.localsymtable,mainaliasname),potype_proginit,st);
            { Win32 startup code needs a single name }
//            if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            pd.aliasnames.insert('PASCALMAIN');
          end
         else if (target_info.system = system_i386_netware) or
                 (target_info.system = system_i386_netwlibc) then
           begin
             pd:=create_main_proc('PASCALMAIN',potype_proginit,st); { main is need by the netware rtl }
           end
         else
           begin
             pd:=create_main_proc(mainaliasname,potype_proginit,st);
             pd.aliasnames.insert('PASCALMAIN');
           end;
         tcgprocinfo(current_procinfo).parse_body;
         tcgprocinfo(current_procinfo).generate_code;
         tcgprocinfo(current_procinfo).resetprocdef;
         { save file pos for debuginfo }
         current_module.mainfilepos:=current_procinfo.entrypos;
         release_main_proc(pd);

         { should we force unit initialization? }
         if tstaticsymtable(current_module.localsymtable).needs_init_final then
           begin
              { initialize section }
              gen_implicit_initfinal(uf_init,st);
              { finalize section }
              gen_implicit_initfinal(uf_finalize,st);
           end;

         { Add symbol to the exports section for win32 so smartlinking a
           DLL will include the edata section }
         if assigned(exportlib) and
            (target_info.system in [system_i386_win32,system_i386_wdosx]) and
            BinaryContainsExports then
           asmlist[al_procedures].concat(tai_const.create_sym(exportlib.edatalabel));

         If resourcestrings.ResStrCount>0 then
          begin
            resourcestrings.CreateResourceStringList;
            { only write if no errors found }
            if (Errorcount=0) then
             resourcestrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
          end;

         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'finalize'),potype_unitfinalize,st);
              pd.aliasnames.insert(make_mangledname('FINALIZE$',current_module.localsymtable,''));
              tcgprocinfo(current_procinfo).parse_body;
              tcgprocinfo(current_procinfo).generate_code;
              tcgprocinfo(current_procinfo).resetprocdef;
              release_main_proc(pd);
           end;

         { consume the last point }
         consume(_POINT);

         if (Errorcount=0) then
           begin
             { test static symtable }
             tstoredsymtable(st).allsymbolsused;
             tstoredsymtable(st).allprivatesused;
             tstoredsymtable(st).check_forwards;
             tstoredsymtable(st).checklabels;
             tstoredsymtable(st).unchain_overloaded;
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

         { generate debuginfo }
{$ifdef GDB}
         write_gdb_info;
{$endif GDB}

         { generate wrappers for interfaces }
         gen_intf_wrappers(asmlist[al_procedures],current_module.localsymtable);

{$ifndef segment_threadvars}
         { generate a list of threadvars }
         InsertThreadvars;
{$endif}

         { generate imports }
         if current_module.uses_imports then
           importlib.generatelib;

         if islibrary or
            (target_info.system in [system_i386_WIN32,system_i386_wdosx]) or
            (target_info.system=system_i386_NETWARE) then
           exportlib.generatelib;

         { insert Tables and StackLength }
{$ifndef segment_threadvars}
         insertThreadVarTablesTable;
{$endif}
         insertResourceTablesTable;
         insertinitfinaltable;
         insertmemorysizes;
         { Insert symbol to resource info }

         InsertResourceInfo;

         { create dwarf debuginfo }
         create_dwarf;

         { finish asmlist by adding segment starts }
//         insertsegment;

         { insert own objectfile }
         insertobjectfile;

         { assemble and link }
         create_objectfile;

         { release all local symtables that are not needed anymore }
         free_localsymtables(current_module.localsymtable);

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         { create the executable when we are at level 1 }
         if (compile_level=1) then
          begin
            { insert all .o files from all loaded units }
            hp:=tmodule(loaded_units.first);
            while assigned(hp) do
             begin
               linker.AddModuleFiles(hp);
               hp:=tmodule(hp.next);
             end;
            { write .def file }
            if (cs_link_deffile in aktglobalswitches) then
             deffile.writefile;
            { finally we can create a executable }
            if (not current_module.is_unit) then
             begin
               if DLLSource then
                linker.MakeSharedLibrary
               else
                linker.MakeExecutable;
               BinaryContainsExports:=false;
             end;
          end;
      end;

end.
