{
    $Id$
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
       cgbase,cpuinfo,cgobj,
       nbas,
       link,assemble,import,export,gendef,ppu,comprsrc,
       cresstr,procinfo,
       dwarf,
{$ifdef GDB}
       gdb,
{$endif GDB}
       scanner,pbase,pexpr,psystem,psub;

    procedure fixseg(p:TAAsmoutput;sec:TAsmSectionType);
      begin
        maybe_new_object_file(p);
        p.insert(Tai_section.Create(sec,'',0));
      end;


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
              if assigned(importssection)then
               importssection.clear
              else
               importssection:=taasmoutput.Create;
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

        { create the .s file and assemble it }
        GenerateAsm(false);

        { Also create a smartlinked version ? }
        if (cs_create_smart in aktmoduleswitches) then
         begin
           { regenerate the importssection for win32 }
           if assigned(importssection) and
              (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            begin
              importsSection.clear;
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

        if (cs_create_smart in aktmoduleswitches) then
         begin
           current_module.linkunitstaticlibs.add(current_module.staticlibfilename^,link_smart);
           current_module.flags:=current_module.flags or uf_smart_linked;
         end;
      end;


    procedure create_dwarf;
      begin
{$warning TODO Make it dependent on the -gd switch}
        dwarflist:=taasmoutput.create;
        { Call frame information }
        dwarfcfi.generate_code(dwarflist);
      end;


    procedure insertsegment;
      var
        oldaktfilepos : tfileposinfo;
      begin
      { Insert Ident of the compiler }
        if (not (cs_create_smart in aktmoduleswitches))
{$ifndef EXTDEBUG}
           and (not current_module.is_unit)
{$endif}
           then
         begin
           { align the first data }
           dataSegment.insert(Tai_align.Create(const_align(32)));
           dataSegment.insert(Tai_string.Create('FPC '+full_version_string+
             ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname));
         end;
        { align code segment }
        codeSegment.concat(Tai_align.Create(aktalignment.procalign));
        { Insert start and end of sections }
        fixseg(codesegment,sec_code);
        fixseg(datasegment,sec_data);
        fixseg(bsssegment,sec_bss);
        { we should use .rdata section for these two no ?
          .rdata is a read only data section (PM) }
        fixseg(rttilist,sec_data);
        fixseg(consts,sec_data);
        fixseg(picdata,sec_data);
        if assigned(resourcestringlist) then
          fixseg(resourcestringlist,sec_data);
{$ifdef GDB}
        if assigned(debuglist) then
          begin
            oldaktfilepos:=aktfilepos;
            aktfilepos.line:=0;
            debugList.insert(Tai_symbol.Createname('gcc2_compiled',AT_DATA,0));
            debugList.insert(Tai_symbol.Createname('fpc_compiled',AT_DATA,0));
            fixseg(debuglist,sec_code);
            aktfilepos:=oldaktfilepos;
          end;
{$endif GDB}
      end;


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
        { TableCount }
        ltvTables.insert(Tai_const.Create_32bit(count));
        ltvTables.insert(Tai_symbol.Createname_global('FPC_THREADVARTABLES',AT_DATA,0));
        ltvTables.insert(Tai_align.Create(const_align(sizeof(aint))));
        ltvTables.concat(Tai_symbol_end.Createname('FPC_THREADVARTABLES'));
        { insert in data segment }
        maybe_new_object_file(dataSegment);
        dataSegment.concatlist(ltvTables);
        ltvTables.free;
      end;


    procedure AddToThreadvarList(p:tnamedindexitem;arg:pointer);
      var
        ltvTable : taasmoutput;
      begin
        ltvTable:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (vo_is_thread_var in tvarsym(p).varoptions) then
         begin
           { address of threadvar }
           ltvTable.concat(tai_const.Createname(tvarsym(p).mangledname,AT_DATA,0));
           { size of threadvar }
           ltvTable.concat(tai_const.create_32bit(tvarsym(p).getsize));
         end;
      end;


    procedure InsertThreadvars;
      var
        s : string;
        ltvTable : TAAsmoutput;
      begin
         ltvTable:=TAAsmoutput.create;
         if assigned(current_module.globalsymtable) then
           current_module.globalsymtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}AddToThreadvarList,ltvTable);
         current_module.localsymtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}AddToThreadvarList,ltvTable);
         if ltvTable.first<>nil then
          begin
            s:=make_mangledname('THREADVARLIST',current_module.localsymtable,'');
            { add begin and end of the list }
            ltvTable.insert(tai_symbol.Createname_global(s,AT_DATA,0));
            ltvTable.concat(tai_const.create_sym(nil));  { end of list marker }
            ltvTable.concat(tai_symbol_end.createname(s));
            maybe_new_object_file(dataSegment);
            dataSegment.concatlist(ltvTable);
            current_module.flags:=current_module.flags or uf_threadvars;
          end;
         ltvTable.Free;
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
        If ResourceStringList<>Nil then
         begin
           ResourceStringTables.concat(Tai_const.Createname(make_mangledname('RESOURCESTRINGLIST',current_module.localsymtable,''),AT_DATA,0));
           Inc(Count);
         end;
        { TableCount }
        ResourceStringTables.insert(Tai_const.Create_32bit(count));
        ResourceStringTables.insert(Tai_symbol.Createname_global('FPC_RESOURCESTRINGTABLES',AT_DATA,0));
        ResourceStringTables.insert(Tai_align.Create(const_align(4)));
        ResourceStringTables.concat(Tai_symbol_end.Createname('FPC_RESOURCESTRINGTABLES'));
        { insert in data segment }
        maybe_new_object_file(dataSegment);
        dataSegment.concatlist(ResourceStringTables);
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
        { TableCount,InitCount }
        unitinits.insert(Tai_const.Create_32bit(0));
        unitinits.insert(Tai_const.Create_32bit(count));
        unitinits.insert(Tai_symbol.Createname_global('INITFINAL',AT_DATA,0));
        unitinits.insert(Tai_align.Create(const_align(4)));
        unitinits.concat(Tai_symbol_end.Createname('INITFINAL'));
        { insert in data segment }
        maybe_new_object_file(dataSegment);
        dataSegment.concatlist(unitinits);
        unitinits.free;
      end;


    procedure insertheap;
      begin
        maybe_new_object_file(bssSegment);
        maybe_new_object_file(dataSegment);
        { On the Macintosh Classic M68k Architecture
          The Heap variable is simply a POINTER to the
          real HEAP. The HEAP must be set up by the RTL
          and must store the pointer in this value.
          On OS/2 the heap is also intialized by the RTL. We do
          not output a pointer }
         case target_info.system of
            system_i386_OS2:
                bssSegment.concat(Tai_datablock.Create_global('HEAP',4));
            system_i386_EMX:
              ;
            system_powerpc_macos:
              ;
            system_i386_watcom:
              ;
            system_alpha_linux:
              ;
            system_m68k_Mac:
              bssSegment.concat(Tai_datablock.Create_global('HEAP',4));
            system_m68k_PalmOS:
              ;
         else
            begin
              bssSegment.concat(Tai_align.Create(var_align(heapsize)));
              bssSegment.concat(Tai_datablock.Create_global('HEAP',heapsize));
            end;
         end;
{$ifdef m68k}
         if target_info.system<>system_m68k_PalmOS then
           begin
              dataSegment.concat(Tai_align.Create(const_align(4)));
              dataSegment.concat(Tai_symbol.Createname_global('HEAPSIZE',AT_DATA,4));
              dataSegment.concat(Tai_const.Create_32bit(heapsize));
           end;
{$else m68k}
         dataSegment.concat(Tai_align.Create(const_align(4)));
         dataSegment.concat(Tai_symbol.Createname_global('HEAPSIZE',AT_DATA,4));
         dataSegment.concat(Tai_const.Create_32bit(heapsize));
{$endif m68k}
      end;


    procedure insertstacklength;
      begin
        { stacksize can be specified and is now simulated }
        dataSegment.concat(Tai_align.Create(const_align(4)));
        dataSegment.concat(Tai_symbol.Createname_global('__stklen',AT_DATA,4));
        dataSegment.concat(Tai_const.Create_32bit(stacksize));
      end;


    procedure loaddefaultunits;

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
          { insert unitsym }
          unitsym:=tunitsym.create(s,hp.globalsymtable);
          inc(unitsym.refs);
          refsymtable.insert(unitsym);
          { add to used units }
          current_module.addusedunit(hp,false,unitsym);
        end;

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
        Symtablestack:=nil;
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
{$ifdef cpufpemu}
        { Floating point emulation unit? }
        if (cs_fp_emulation in aktmoduleswitches) then
          AddUnit('SoftFpu');
{$endif cpufpemu}
        { Thread support unit? }
        if (cs_threading in aktmoduleswitches) then
          AddUnit('SysThrds');
        { Objpas unit? }
        if m_objpas in aktmodeswitches then
          AddUnit('ObjPas');
        { Profile unit? Needed for go32v2 only }
        if (cs_profile in aktmoduleswitches) and
           (target_info.system in [system_i386_go32v2,system_i386_watcom]) then
          AddUnit('Profile');
        { Units only required for main module }
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
         end;
        { save default symtablestack }
        defaultsymtablestack:=symtablestack;
      end;


    procedure loadunits;
      var
         s,sorg  : stringid;
         fn      : string;
         pu      : tused_unit;
         hp2     : tmodule;
         hp3     : tsymtable;
         unitsym : tunitsym;
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
         consume(_SEMICOLON);

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
               { connect unitsym to the globalsymtable of the unit }
               pu.unitsym.unitsymtable:=pu.u.globalsymtable;
               { increase refs of the unitsym when the unit contains
                 initialization/finalization code so it doesn't trigger
                 the unit not used hint }
               if (pu.u.flags and (uf_init or uf_finalize))<>0 then
                 inc(pu.unitsym.refs);
             end;
            pu:=tused_unit(pu.next);
          end;

         { set the symtable to systemunit so it gets reorderd correctly,
           then insert the units in the symtablestack }
         pu:=tused_unit(current_module.used_units.first);
         symtablestack:=defaultsymtablestack;
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
{$ifdef DEBUG}
                             test_symtablestack;
{$endif DEBUG}
                          end;
                     end;
                end;
              pu:=tused_unit(pu.next);
           end;
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
                   tglobalsymtable(pu.u.globalsymtable).concattypestabto(debuglist);
               end;
             pu:=tused_unit(pu.next);
           end;
       end;

      var
        vardebuglist : taasmoutput;
      begin
        if not (cs_debuginfo in aktmoduleswitches) then
         exit;
        { include symbol that will be referenced from the program to be sure to
          include this debuginfo .o file }
        if current_module.is_unit then
          begin
            current_module.flags:=current_module.flags or uf_has_debuginfo;
            debugList.concat(tai_symbol.Createname_global(make_mangledname('DEBUGINFO',current_module.globalsymtable,''),AT_DATA,0));
          end
        else
          debugList.concat(tai_symbol.Createname_global(make_mangledname('DEBUGINFO',current_module.localsymtable,''),AT_DATA,0));
        { first write all global/local symbols to a temp list. This will flag
          all required tdefs. Afterwards this list will be added }
        vardebuglist:=taasmoutput.create;
        if assigned(current_module.globalsymtable) then
          tglobalsymtable(current_module.globalsymtable).concatstabto(vardebuglist);
        if assigned(current_module.localsymtable) then
          tstaticsymtable(current_module.localsymtable).concatstabto(vardebuglist);
        { reset unit type info flag }
        reset_unit_type_info;
        { write used types from the used units }
        write_used_unit_type_info(current_module);
        { last write the types from this unit }
        if assigned(current_module.globalsymtable) then
          tglobalsymtable(current_module.globalsymtable).concattypestabto(debuglist);
        if assigned(current_module.localsymtable) then
          tstaticsymtable(current_module.localsymtable).concattypestabto(debuglist);
        { now all defs have a type in the debuglist, we now can add the vardebuglist since
          all references to defs can be solved }
        debuglist.concatlist(vardebuglist);
        vardebuglist.free;
        { include files }
        if (cs_gdb_dbx in aktglobalswitches) then
          begin
            debugList.concat(tai_comment.Create(strpnew('EINCL of global '+
              tglobalsymtable(current_module.globalsymtable).name^+' has index '+
              tostr(tglobalsymtable(current_module.globalsymtable).unitid))));
            debugList.concat(Tai_stabs.Create(strpnew('"'+
              tglobalsymtable(current_module.globalsymtable).name^+'",'+
              tostr(N_EINCL)+',0,0,0')));
            tglobalsymtable(current_module.globalsymtable).dbx_count_ok:={true}false;
            dbx_counter:=tglobalsymtable(current_module.globalsymtable).prev_dbx_counter;
            do_count_dbx:=false;
          end;
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

        { define a symbol in delphi,objfpc,tp,gpc mode }
        if (m_delphi in aktmodeswitches) then
         current_scanner.def_macro('FPC_DELPHI')
        else
         if (m_tp7 in aktmodeswitches) then
          current_scanner.def_macro('FPC_TP')
        else
         if (m_objfpc in aktmodeswitches) then
          current_scanner.def_macro('FPC_OBJFPC')
        else
         if (m_gpc in aktmodeswitches) then
          current_scanner.def_macro('FPC_GPC')
        else
         if (m_mac in aktmodeswitches) then
          current_scanner.def_macro('FPC_MACPAS');
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
        include(pd.procoptions,po_public);
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


    procedure proc_unit;

      function is_assembler_generated:boolean;
      begin
        is_assembler_generated:=(Errorcount=0) and
          not(
          codeSegment.empty and
          dataSegment.empty and
          bssSegment.empty and
          ((importssection=nil) or importsSection.empty) and
          ((resourcesection=nil) or resourceSection.empty) and
          ((resourcestringlist=nil) or resourcestringList.empty)
        );
      end;

      var
         main_file: tinputfile;
         st     : tsymtable;
         unitst : tglobalsymtable;
         store_crc,store_interface_crc : cardinal;
         s1,s2  : ^string; {Saves stack space}
         force_init_final : boolean;
         pd : tprocdef;
         unitname8 : string[8];
         has_impl: boolean;
      begin
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
         setupglobalswitches;

         message1(unit_u_loading_interface_units,current_module.modulename^);

         { update status }
         status.currentmodule:=current_module.realmodulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (current_module.modulename^='OBJPAS') then
           exclude(aktmodeswitches,m_objpas);

         parse_only:=true;

         { generate now the global symboltable }
         st:=tglobalsymtable.create(current_module.modulename^);
         refsymtable:=st;
         unitst:=tglobalsymtable(st);
         { define first as local to overcome dependency conflicts }
         current_module.localsymtable:=st;

         { the unit name must be usable as a unit specifier }
         { inside the unit itself (PM)                }
         { this also forbids to have another symbol      }
         { with the same name as the unit                  }
         refsymtable.insert(tunitsym.create(current_module.realmodulename^,unitst));

         { load default units, like the system unit }
         loaddefaultunits;

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
         current_module.numberunits;

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
           begin
             consume(_IMPLEMENTATION);
             has_impl:= true;
           end;

         if has_impl then
           Message1(unit_u_loading_implementation_units,current_module.modulename^);

         parse_only:=false;

         { generates static symbol table }
         st:=tstaticsymtable.create(current_module.modulename^);
         current_module.localsymtable:=st;

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst.next;

         { we don't want implementation units symbols in unitsymtable !! PM }
         refsymtable:=st;

         { Read the implementation units }
         if has_impl then
           parse_implementation_uses;

         if current_module.state=ms_compiled then
           exit;

         { reset ranges/stabs in exported definitions }
         reset_all_defs;

         { All units are read, now give them a number }
         current_module.numberunits;

         { now we can change refsymtable }
         refsymtable:=st;

         { but reinsert the global symtable as lasts }
         unitst.next:=symtablestack;
         symtablestack:=unitst;

{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         constsymtable:=symtablestack;

{$ifdef Splitheap}
         if testsplit then
           begin
              Split_Heap;
              allow_special:=true;
              Switch_to_temp_heap;
           end;
         { it will report all crossings }
         allow_special:=false;
{$endif Splitheap}

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
         If ResourceStrings.ResStrCount>0 then
          begin
            ResourceStrings.CreateResourceStringList;
            current_module.flags:=current_module.flags or uf_has_resources;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
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

         { generate debuginfo }
{$ifdef GDB}
         write_gdb_info;
{$endif GDB}

         { generate a list of threadvars }
         InsertThreadvars;

         { generate imports }
         if current_module.uses_imports then
           importlib.generatelib;

         { insert own objectfile, or say that it's in a library
           (no check for an .o when loading) }
         if is_assembler_generated then
           insertobjectfile
         else
           current_module.flags:=current_module.flags or uf_no_link;

         if cs_local_browser in aktmoduleswitches then
           current_module.localsymtable:=refsymtable;

         if is_assembler_generated then
          begin
            { create dwarf debuginfo }
            create_dwarf;
            { finish asmlist by adding segment starts }
            insertsegment;
            { assemble }
            create_objectfile;
          end;

         { Write out the ppufile after the object file has been created }
         store_interface_crc:=current_module.interface_crc;
         store_crc:=current_module.crc;
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

         { remove static symtable (=refsymtable) here to save some mem }
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
         main_file: tinputfile;
         st    : tsymtable;
         hp    : tmodule;
         pd : tprocdef;
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
              current_module.modulename:=stringdup(pattern);
              current_module.islibrary:=true;
              exportlib.preparelib(pattern);
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
                exportlib.preparelib(pattern);
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
           exportlib.preparelib(current_module.modulename^);

         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { setup things using the global switches }
         setupglobalswitches;

         { set implementation flag }
         current_module.in_interface:=false;
         current_module.interface_compiled:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         st:=tstaticsymtable.create(current_module.modulename^);;
         current_module.localsymtable:=st;
         refsymtable:=st;

         { load standard units (system,objpas,profile unit) }
         loaddefaultunits;

         {Load the units used by the program we compile.}
         if token=_USES then
           loadunits;

         { reset ranges/stabs in exported definitions }
         reset_all_defs;

         { All units are read, now give them a number }
         current_module.numberunits;

         {Insert the name of the main program into the symbol table.}
         if current_module.realmodulename^<>'' then
           st.insert(tunitsym.create(current_module.realmodulename^,st));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         Message1(parser_u_parsing_implementation,current_module.mainsource^);

         { The program intialization needs an alias, so it can be called
           from the bootstrap code.}
         if islibrary or
            (target_info.system in [system_powerpc_macos,system_powerpc_darwin]) then
          begin
            pd:=create_main_proc(make_mangledname('',current_module.localsymtable,'main'),potype_proginit,st);
            { Win32 startup code needs a single name }
//            if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            pd.aliasnames.insert('PASCALMAIN');
          end
         else
          begin
            pd:=create_main_proc('main',potype_proginit,st);
            pd.aliasnames.insert('PASCALMAIN');
          end;
         tcgprocinfo(current_procinfo).parse_body;
         tcgprocinfo(current_procinfo).generate_code;
         tcgprocinfo(current_procinfo).resetprocdef;
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
            assigned(current_module._exports.first) then
           codesegment.concat(tai_const.create_sym(exportlib.edatalabel));

         If ResourceStrings.ResStrCount>0 then
          begin
            ResourceStrings.CreateResourceStringList;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
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

         { generate debuginfo }
{$ifdef GDB}
         write_gdb_info;
{$endif GDB}

         { generate a list of threadvars }
         InsertThreadvars;

         { generate imports }
         if current_module.uses_imports then
           importlib.generatelib;

         if islibrary or
            (target_info.system in [system_i386_WIN32,system_i386_wdosx]) or
            (target_info.system=system_i386_NETWARE) then
           exportlib.generatelib;

         { insert Tables and Heap }
         insertThreadVarTablesTable;
         insertResourceTablesTable;
         insertinitfinaltable;
         insertheap;
         insertstacklength;

         { create dwarf debuginfo }
         create_dwarf;

         { finish asmlist by adding segment starts }
         insertsegment;

         { insert own objectfile }
         insertobjectfile;

         { assemble and link }
         create_objectfile;

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
             end;
          end;
      end;

end.
{
  $Log$
  Revision 1.157  2004-06-18 15:16:27  peter
    * fixed debuginfo symbol

  Revision 1.156  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.155  2004/05/23 20:56:42  peter
    * initialize errorsym/errortype.def.owner to prevent crashes

  Revision 1.154  2004/05/23 15:06:21  peter
    * implicit_finally flag must be set in pass1
    * add check whether the implicit frame is generated when expected

  Revision 1.153  2004/05/19 21:16:13  peter
    * add DEBUGINFO symbol to reference the .o file that includes the
      stabs info for types and global/static variables
    * debuginfo flag added to ppu to indicate whether debuginfo is
      generated or not

  Revision 1.152  2004/05/16 13:29:21  peter
    * fix checking for forwards in static symtable

  Revision 1.151  2004/05/11 18:22:16  olle
    * changed $mode mac to $mode macpas (macro defined should be FPC_MACPAS)

  Revision 1.150  2004/05/03 09:55:27  olle
    + enable omitting of IMPLEMENTATION for mode mac
    + added macro FPC_MAC for mode mac

  Revision 1.149  2004/05/02 17:26:19  peter
    * fix stabs for globals

  Revision 1.148.2.6  2004/05/03 14:59:57  peter
    * no dlltool needed for win32 linking executables

  Revision 1.148.2.5  2004/05/02 16:49:43  peter
    * generate stabs for globals/locals at the end of a unit compile

  Revision 1.148.2.4  2004/05/01 16:02:09  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.148.2.3  2004/04/12 19:34:46  peter
    * basic framework for dwarf CFI

  Revision 1.148.2.2  2004/04/12 14:45:11  peter
    * tai_const_symbol and tai_const merged

  Revision 1.148.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.148  2004/03/24 20:24:25  hajny
    * OS/2 heap management modified to be able to grow heap as needed

  Revision 1.147  2004/03/18 11:43:57  olle
    * change AT_FUNCTION to AT_DATA where appropriate

  Revision 1.146  2004/03/14 20:10:14  peter
    * fix stabs lineno for fpc_compiled

  Revision 1.145  2004/03/10 22:52:57  peter
    * more stabs fixes
    * special mode -gv for valgrind compatible stabs

  Revision 1.144  2004/03/09 20:45:04  peter
    * more stabs updates

  Revision 1.143  2004/03/08 22:07:47  peter
    * stabs updates to write stabs for def for all implictly used
      units

  Revision 1.142  2004/03/02 17:32:12  florian
    * make cycle fixed
    + pic support for darwin
    + support of importing vars from shared libs on darwin implemented

  Revision 1.141  2004/03/02 00:36:33  olle
    * big transformation of Tai_[const_]Symbol.Create[data]name*

  Revision 1.140  2004/02/26 16:16:38  peter
    * tai_const.create_ptr added

  Revision 1.139  2004/02/06 22:37:00  daniel
    * Removed not very usefull nextglobal & previousglobal fields from
      Tstoreddef, saving 78 kb of memory

  Revision 1.138  2004/02/04 22:15:15  daniel
    * Rtti generation moved to ncgutil
    * Assmtai usage of symsym removed
    * operator overloading cleanup up

  Revision 1.137  2004/01/28 16:48:24  peter
  use local string of 8 chars

  Revision 1.136  2004/01/04 21:08:09  jonas
    * Never generate a "main" symbol for PASCALMAIN for Darwin and classic
      Mac OS, they use a C-main in their system unit

  Revision 1.135  2003/12/12 19:42:21  peter
    * check unit name when expected unitname > 8 chars

  Revision 1.134  2003/12/08 22:33:43  peter
    * don't allow duplicate uses
    * fix wrong circular dependency

  Revision 1.133  2003/11/29 20:13:25  florian
    * fixed several pi_do_call problems

  Revision 1.132  2003/10/29 19:48:51  peter
    * renamed mangeldname_prefix to make_mangledname and made it more
      generic
    * make_mangledname is now also used for internal threadvar/resstring
      lists
    * Add P$ in front of program modulename to prevent duplicated symbols
      at assembler level, because the main program can have the same name
      as a unit, see webtbs/tw1251b

  Revision 1.131  2003/10/24 17:40:23  peter
    * cleanup of the entry and exit code insertion

  Revision 1.130  2003/10/22 15:22:33  peter
    * fixed unitsym-globalsymtable relation so the uses of a unit
      is counted correctly

  Revision 1.129  2003/10/21 15:14:33  peter
    * fixed memleak for initfinalcode
    * exit from generatecode when there are already errors

  Revision 1.128  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.127  2003/09/30 08:39:50  michael
  + Patch from Wiktor Sywula for watcom support

  Revision 1.126  2003/09/23 18:03:08  peter
    * add missing release of main_proc

  Revision 1.125  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.124  2003/09/09 20:59:27  daniel
    * Adding register allocation order

  Revision 1.123  2003/09/09 15:55:44  peter
    * use register with least interferences in spillregister

  Revision 1.122  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.121  2003/09/05 17:41:12  florian
    * merged Wiktor's Watcom patches in 1.1

  Revision 1.120  2003/08/23 22:29:24  peter
    * reload flagged units when interface is loaded

  Revision 1.119  2003/08/21 14:47:41  peter
    * remove convert_registers

  Revision 1.118  2003/08/20 17:48:49  peter
    * fixed stackalloc to not allocate localst.datasize twice
    * order of stackalloc code fixed for implicit init/final

  Revision 1.117  2003/08/20 09:07:00  daniel
    * New register coding now mandatory, some more convert_registers calls
      removed.

  Revision 1.116  2003/07/23 11:04:15  jonas
    * split en_exit_code into a part that may allocate a register and a part
      that doesn't, so the former can be done before the register colouring
      has been performed

  Revision 1.115  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.114  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.113  2003/06/09 12:23:30  peter
    * init/final of procedure data splitted from genentrycode
    * use asmnode getposition to insert final at the correct position
      als for the implicit try...finally

  Revision 1.112  2003/06/07 20:26:32  peter
    * re-resolving added instead of reloading from ppu
    * tderef object added to store deref info for resolving

  Revision 1.111  2003/06/03 20:21:45  mazen
  - removed unneeded ifdefs
  - removed unneeded cases for sparc and x86_64

  Revision 1.110  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.109  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.108  2003/05/25 10:27:12  peter
    * moved Comment calls to messge file

  Revision 1.107  2003/05/22 21:31:35  peter
    * defer codegeneration for nested procedures

  Revision 1.106  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.105  2003/05/11 19:31:28  florian
    * fixed implicit init/final code for units, stack frame was wrong for ppc

  Revision 1.104  2003/04/28 21:19:02  peter
    * fix stabs generation for implicit initfinal

  Revision 1.103  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.102  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.101  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.100  2003/04/12 15:13:03  peter
    * Use the original unitname when defining a unitsym

  Revision 1.99  2003/03/23 23:21:42  hajny
    + emx target added

  Revision 1.98  2003/03/17 22:20:08  peter
  *** empty log message ***

  Revision 1.97  2003/03/17 13:36:39  peter
    * fix import linking under linux

  Revision 1.96  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.95  2003/02/06 22:36:55  mazen
  * fixing bug related to errornous program main entry stack frame

  Revision 1.94  2003/01/30 21:46:20  peter
    * tai_const_symbol.createdataname added

  Revision 1.93  2003/01/11 11:19:54  hajny
    * correction from rev. 1.88 put back

  Revision 1.92  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.91  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.90  2002/12/29 18:17:23  peter
    * insert unitsym with the name as specified in the uses list

  Revision 1.89  2002/12/29 14:57:50  peter
    * unit loading changed to first register units and load them
      afterwards. This is needed to support uses xxx in yyy correctly
    * unit dependency check fixed

  Revision 1.88  2002/12/27 19:09:33  hajny
    * another (hopefully final ;-) ) fix for not linked import libraries for units with no code

  Revision 1.87  2002/12/24 23:32:56  peter
    * Use FixFilename for specified unit sourcefile in uses

  Revision 1.86  2002/12/06 16:56:58  peter
    * only compile cs_fp_emulation support when cpufpuemu is defined
    * define cpufpuemu for m68k only

  Revision 1.85  2002/11/30 21:32:24  carl
    + Add loading of softfpu in emulation mode
    + Correct routine call for softfpu
    * Extended type must also be defined even with softfpu

  Revision 1.84  2002/11/15 01:58:53  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.83  2002/11/09 15:33:26  carl
    * major alignment updates

  Revision 1.82  2002/10/16 06:32:52  michael
  + Renamed thread unit to systhrds

  Revision 1.81  2002/10/14 19:42:34  peter
    * only use init tables for threadvars

  Revision 1.80  2002/10/06 19:41:30  peter
    * Add finalization of typed consts
    * Finalization of globals in the main program

  Revision 1.79  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.78  2002/09/07 15:25:07  peter
    * old logs removed and tabs fixed

  Revision 1.77  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.76  2002/09/02 18:46:26  peter
    * insert PASCALMAIN in library for Win32 only

  Revision 1.75  2002/08/31 15:59:30  florian
    + HEAP* stuff must be generated for Linux/PPC as well
    + direct assembler reader searches now global and static symtables as well

  Revision 1.74  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.73  2002/08/18 20:06:25  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.72  2002/08/17 09:23:39  florian
    * first part of procinfo rewrite

  Revision 1.71  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.70  2002/08/10 14:46:29  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.69  2002/07/26 21:15:41  florian
    * rewrote the system handling

  Revision 1.68  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.67  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.66  2002/05/16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.65  2002/05/14 19:34:49  peter
    * removed old logs and updated copyright year

  Revision 1.64  2002/05/12 16:53:09  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.63  2002/05/06 19:54:50  carl
  + added more patches from Mazen for SPARC port

  Revision 1.62  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.61  2002/04/19 15:46:02  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.60  2002/04/14 16:53:10  carl
  + align code section and data section according to alignment rules

  Revision 1.59  2002/04/07 17:58:38  carl
  + generic stack checking

  Revision 1.58  2002/04/04 19:06:03  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

}