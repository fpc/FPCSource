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

{$define New_GDB}

interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


implementation

    uses
       globtype,version,systems,tokens,
       cutils,cclasses,comphook,
       globals,verbose,fmodule,finput,fppu,
       symconst,symbase,symtype,symdef,symsym,symtable,
       aasmbase,aasmtai,aasmcpu,
       cgbase,cpuinfo,
       ncgutil,
       link,assemble,import,export,gendef,ppu,comprsrc,
       cresstr,cpubase,
{$ifdef GDB}
       gdb,
{$endif GDB}
       scanner,pbase,pexpr,psystem,psub;

    procedure fixseg(p:TAAsmoutput;sec:TSection);
      begin
        p.insert(Tai_section.Create(sec));
        if (cs_create_smart in aktmoduleswitches) then
         p.insert(Tai_cut.Create);
        p.concat(Tai_section.Create(sec_none));
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
           if target_asm.needar then
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


    procedure insertsegment;
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
      { we should use .rdata section for these two no ? }
      { .rdata is a read only data section (PM) }
        fixseg(rttilist,sec_data);
        fixseg(consts,sec_data);
        if assigned(resourcestringlist) then
          fixseg(resourcestringlist,sec_data);
{$ifdef GDB}
        if assigned(debuglist) then
          begin
            debugList.insert(Tai_symbol.Createname('gcc2_compiled',0));
            debugList.insert(Tai_symbol.Createname('fpc_compiled',0));
            fixseg(debuglist,sec_code);
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
              ltvTables.concat(Tai_const_symbol.Createdataname(hp.u.modulename^+'_$THREADVARLIST'));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program threadvars, if any }
        If (current_module.flags and uf_threadvars)=uf_threadvars then
         begin
           ltvTables.concat(Tai_const_symbol.Createdataname(current_module.modulename^+'_$THREADVARLIST'));
           inc(count);
         end;
        { TableCount }
        ltvTables.insert(Tai_const.Create_32bit(count));
        ltvTables.insert(Tai_symbol.Createdataname_global('FPC_THREADVARTABLES',0));
        ltvTables.insert(Tai_align.Create(const_align(pointer_size)));
        ltvTables.concat(Tai_symbol_end.Createname('FPC_THREADVARTABLES'));
        { insert in data segment }
        if (cs_create_smart in aktmoduleswitches) then
          dataSegment.concat(Tai_cut.Create);
        dataSegment.concatlist(ltvTables);
        ltvTables.free;
        if count > 0 then
          have_local_threadvars := true;
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
           ltvTable.concat(tai_const_symbol.createdataname(tvarsym(p).mangledname));
           { size of threadvar }
           ltvTable.concat(tai_const.create_32bit(tvarsym(p).getsize));
         end;
      end;


    procedure InsertThreadvars;
      var
        ltvTable : TAAsmoutput;
      begin
         ltvTable:=TAAsmoutput.create;
         if assigned(current_module.globalsymtable) then
           current_module.globalsymtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}AddToThreadvarList,ltvTable);
         current_module.localsymtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}AddToThreadvarList,ltvTable);
         if ltvTable.first<>nil then
          begin
            { add begin and end of the list }
            ltvTable.insert(tai_symbol.createdataname_global(current_module.modulename^+'_$THREADVARLIST',0));
            ltvTable.concat(tai_const.create_32bit(0));  { end of list marker }
            ltvTable.concat(tai_symbol_end.createname(current_module.modulename^+'_$THREADVARLIST'));
            if (cs_create_smart in aktmoduleswitches) then
             dataSegment.concat(Tai_cut.Create);
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
              ResourceStringTables.concat(Tai_const_symbol.Createdataname(hp.u.modulename^+'_RESOURCESTRINGLIST'));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program resources, if any }
        If ResourceStringList<>Nil then
         begin
           ResourceStringTables.concat(Tai_const_symbol.Createdataname(current_module.modulename^+'_RESOURCESTRINGLIST'));
           Inc(Count);
         end;
        { TableCount }
        ResourceStringTables.insert(Tai_const.Create_32bit(count));
        ResourceStringTables.insert(Tai_symbol.Createdataname_global('FPC_RESOURCESTRINGTABLES',0));
        ResourceStringTables.insert(Tai_align.Create(const_align(4)));
        ResourceStringTables.concat(Tai_symbol_end.Createname('FPC_RESOURCESTRINGTABLES'));
        { insert in data segment }
        if (cs_create_smart in aktmoduleswitches) then
          dataSegment.concat(Tai_cut.Create);
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
               unitinits.concat(Tai_const_symbol.Createname('INIT$$'+hp.u.modulename^))
              else
               unitinits.concat(Tai_const.Create_32bit(0));
              if (hp.u.flags and uf_finalize)<>0 then
               unitinits.concat(Tai_const_symbol.Createname('FINALIZE$$'+hp.u.modulename^))
              else
               unitinits.concat(Tai_const.Create_32bit(0));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Insert initialization/finalization of the program }
        if (current_module.flags and (uf_init or uf_finalize))<>0 then
         begin
           if (current_module.flags and uf_init)<>0 then
            unitinits.concat(Tai_const_symbol.Createname('INIT$$'+current_module.modulename^))
           else
            unitinits.concat(Tai_const.Create_32bit(0));
           if (current_module.flags and uf_finalize)<>0 then
            unitinits.concat(Tai_const_symbol.Createname('FINALIZE$$'+current_module.modulename^))
           else
            unitinits.concat(Tai_const.Create_32bit(0));
           inc(count);
         end;
        { TableCount,InitCount }
        unitinits.insert(Tai_const.Create_32bit(0));
        unitinits.insert(Tai_const.Create_32bit(count));
        unitinits.insert(Tai_symbol.Createdataname_global('INITFINAL',0));
        unitinits.insert(Tai_align.Create(const_align(4)));
        unitinits.concat(Tai_symbol_end.Createname('INITFINAL'));
        { insert in data segment }
        if (cs_create_smart in aktmoduleswitches) then
          dataSegment.concat(Tai_cut.Create);
        dataSegment.concatlist(unitinits);
        unitinits.free;
      end;


    procedure insertheap;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           begin
             bssSegment.concat(Tai_cut.Create);
             dataSegment.concat(Tai_cut.Create);
           end;
        { On the Macintosh Classic M68k Architecture
          The Heap variable is simply a POINTER to the
          real HEAP. The HEAP must be set up by the RTL
          and must store the pointer in this value.
          On OS/2 the heap is also intialized by the RTL. We do
          not output a pointer }
         case target_info.system of
{$ifdef x86_64}
            system_x86_64_linux:
              ;
{$endif x86_64}
{$ifdef i386}
            system_i386_OS2,system_i386_EMX:
              ;
{$endif i386}
{$ifdef powerpc}
            system_powerpc_macos:
              ;
{$endif powerpc}
{$ifdef alpha}
            system_alpha_linux:
              ;
{$endif alpha}
{$ifdef m68k}
            system_m68k_Mac:
              bssSegment.concat(Tai_datablock.Create_global('HEAP',4));
            system_m68k_PalmOS:
              ;
{$endif m68k}
{$IFDEF SPARC}
            system_SPARC_Linux:
              ;
{$ENDIF SPARC}
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
              dataSegment.concat(Tai_symbol.Createdataname_global('HEAPSIZE',4));
              dataSegment.concat(Tai_const.Create_32bit(heapsize));
           end;
{$else m68k}
         dataSegment.concat(Tai_align.Create(const_align(4)));
         dataSegment.concat(Tai_symbol.Createdataname_global('HEAPSIZE',4));
         dataSegment.concat(Tai_const.Create_32bit(heapsize));
{$endif m68k}
      end;


    procedure insertstacklength;
      begin
        { stacksize can be specified and is now simulated }
        dataSegment.concat(Tai_align.Create(const_align(4)));
        dataSegment.concat(Tai_symbol.Createdataname_global('__stklen',4));
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
          current_module.addusedunit(hp,false);
          { add to symtable stack }
          tsymtable(hp.globalsymtable).next:=symtablestack;
          symtablestack:=hp.globalsymtable;
          { insert unitsym }
          unitsym:=tunitsym.create(s,hp.globalsymtable);
          inc(unitsym.refs);
          refsymtable.insert(unitsym);
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
           (target_info.system=system_i386_go32v2) then
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
         end;
        { save default symtablestack }
        defaultsymtablestack:=symtablestack;
      end;


    procedure loadunits;
      var
         s,sorg : stringid;
         fn     : string;
         pu     : tused_unit;
         hp2    : tmodule;
         hp3    : tsymtable;
         oldprocdef : tprocdef;
         unitsym : tunitsym;
      begin
         oldprocdef:=current_procdef;
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
              { Need to register the unit? }
              if not assigned(hp2) then
                hp2:=registerunit(current_module,sorg,fn);
              { the current module uses the unit hp2 }
              current_module.addusedunit(hp2,true);
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
            if pu.in_uses then
             begin
               { store the original name to create the unitsym }
               sorg:=pu.u.realmodulename^;
               tppumodule(pu.u).loadppu;
               { is our module compiled? then we can stop }
               if current_module.state=ms_compiled then
                exit;
               { add this unit to the dependencies }
               pu.u.adddependency(current_module);
               pu.checksum:=pu.u.crc;
               pu.interface_checksum:=pu.u.interface_crc;
               { Create unitsym, we need to use the name as specified, we
                 can not use the modulename because that can be different
                 when -Un is used. But when the names are the same then
                 force the name of the module so there will be no difference
                 in the case of the name }
               if upper(sorg)=pu.u.modulename^ then
                sorg:=pu.u.realmodulename^;
               unitsym:=tunitsym.create(sorg,pu.u.globalsymtable);
               if (pu.u.flags and (uf_init or uf_finalize))<>0 then
                 inc(unitsym.refs);
               refsymtable.insert(unitsym);
             end;
            pu:=tused_unit(pu.next);
          end;

         { set the symtable to systemunit so it gets reorderd correctly,
           then insert the units in the symtablestack }
         pu:=tused_unit(current_module.used_units.first);
         symtablestack:=defaultsymtablestack;
         while assigned(pu) do
           begin
{$IfDef GDB}
              if (cs_debuginfo in aktmoduleswitches) and
                 (cs_gdb_dbx in aktglobalswitches) and
                not pu.is_stab_written then
                begin
                   tglobalsymtable(pu.u.globalsymtable).concattypestabto(debuglist);
                   pu.is_stab_written:=true;
                   pu.unitid:=tsymtable(pu.u.globalsymtable).unitid;
                end;
{$EndIf GDB}
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
          current_procdef:=oldprocdef;
      end;


     procedure write_gdb_info;
{$IfDef GDB}
       var
         hp : tused_unit;
       begin
         if not (cs_debuginfo in aktmoduleswitches) then
          exit;
         { now insert the units in the symtablestack }
         hp:=tused_unit(current_module.used_units.first);
         while assigned(hp) do
           begin
              if (cs_debuginfo in aktmoduleswitches) and
                not hp.is_stab_written then
                begin
                   tglobalsymtable(hp.u.globalsymtable).concattypestabto(debuglist);
                   hp.is_stab_written:=true;
                   hp.unitid:=tsymtable(hp.u.globalsymtable).unitid;
                end;
              hp:=tused_unit(hp.next);
           end;
         if (not current_module.in_interface) and
            assigned(current_module.localsymtable) then
           begin
              { all types }
              tstaticsymtable(current_module.localsymtable).concattypestabto(debuglist);
              { and all local symbols}
              tstaticsymtable(current_module.localsymtable).concatstabto(debuglist);
           end
         else if assigned(current_module.globalsymtable) then
           begin
              { all types }
              tglobalsymtable(current_module.globalsymtable).concattypestabto(debuglist);
              { and all local symbols}
              tglobalsymtable(current_module.globalsymtable).concatstabto(debuglist);
           end;
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
{$Else GDB}
       begin
       end;
{$EndIf GDB}


    procedure parse_implementation_uses(symt:tsymtable);
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
          current_scanner.def_macro('FPC_GPC');
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
        pd.procsym:=ps;
        ps.addprocdef(pd);
        { restore symtable }
        make_ref:=true;
        symtablestack:=stt;
        { set procdef options }
        pd.proctypeoption:=potype;
        pd.forwarddef:=false;
        pd.setmangledname(target_info.cprefix+name);
        pd.aliasnames.insert(pd.mangledname);
        { We don't need is a local symtable. Change it into the static
          symtable }
        pd.localst.free;
        pd.localst:=st;
        { set procinfo and current_procdef }
        current_procinfo:=cprocinfo.create(nil);
        current_module.procinfo:=current_procinfo;
        current_procinfo.procdef:=pd;
        current_procdef:=pd;
        { return procdef }
        create_main_proc:=pd;
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


    procedure gen_implicit_initfinal(list:taasmoutput;flag:word;st:tsymtable);
      var
        parasize : longint;
        nostackframe : boolean;
        pd : tprocdef;
        oldexitlabel,
        oldexit2label : tasmlabel;
      begin
        { update module flags }
        current_module.flags:=current_module.flags or flag;
        { create procdef }
        case flag of
          uf_init :
            begin
              pd:=create_main_proc(current_module.modulename^+'_init_implicit',potype_unitinit,st);
              pd.aliasnames.insert('INIT$$'+current_module.modulename^);
            end;
          uf_finalize :
            begin
              pd:=create_main_proc(current_module.modulename^+'_finalize_implicit',potype_unitfinalize,st);
              pd.aliasnames.insert('FINALIZE$$'+current_module.modulename^);
            end;
          else
            internalerror(200304253);
        end;
        { save labels }
        oldexitlabel:=aktexitlabel;
        oldexit2label:=aktexit2label;
        { generate a dummy function }
        parasize:=0;
        nostackframe:=false;
        objectlibrary.getlabel(aktexitlabel);
        objectlibrary.getlabel(aktexit2label);
        include(current_procinfo.flags,pi_do_call);
        genentrycode(list,true,0,parasize,nostackframe,false);
        genexitcode(list,parasize,nostackframe,false);
        list.convert_registers;
        release_main_proc(pd);
        { restore }
        aktexitlabel:=oldexitlabel;
        aktexit2label:=oldexit2label;
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
{$ifdef GDB}
         pu     : tused_unit;
{$endif GDB}
         store_crc,store_interface_crc : cardinal;
         s2  : ^string; {Saves stack space}
         force_init_final : boolean;
         initfinalcode : taasmoutput;
         pd : tprocdef;
      begin
         initfinalcode:=taasmoutput.create;
         consume(_UNIT);
         if compile_level=1 then
          Status.IsExe:=false;

         if token=_ID then
          begin
          { create filenames and unit name }
             main_file := current_scanner.inputfile;
             while assigned(main_file.next) do
               main_file := main_file.next;

             current_module.SetFileName(main_file.path^+main_file.name^,true);

             stringdispose(current_module.modulename);
             stringdispose(current_module.realmodulename);
             current_module.modulename:=stringdup(pattern);
             current_module.realmodulename:=stringdup(orgpattern);
          { check for system unit }
             new(s2);
             s2^:=upper(SplitName(main_file.name^));
             if (cs_check_unit_name in aktglobalswitches) and
                not((current_module.modulename^=s2^) or
                    ((length(current_module.modulename^)>8) and
                     (copy(current_module.modulename^,1,8)=s2^))) then
              Message1(unit_e_illegal_unit_name,current_module.realmodulename^);
             if (current_module.modulename^='SYSTEM') then
              include(aktmoduleswitches,cs_compilesystem);
             dispose(s2);
          end;

         consume(_ID);
         consume(_SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { handle the global switches }
         setupglobalswitches;

         Comment(V_Used,'Loading interface units from '+current_module.modulename^);
//         Message1(unit_u_start_parse_interface,current_module.modulename^);

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
                     begin
                        { this unit symtable is obsolete }
                        { dispose(unitst,done);
                        disposed as localsymtable !! }
                        RestoreUnitSyms;
                        exit;
                     end;
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

         reset_global_defs;

         { number all units, so we know if a unit is used by this unit or
           needs to be added implicitly }
         current_module.numberunits;

         { ... parse the declarations }
         Comment(V_Used,'Parsing interface of '+current_module.modulename^);
//         Message1(parser_u_parsing_interface,current_module.realmodulename^);
         read_interface_declarations;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;
         {else  in inteface its somatimes necessary even if unused
          st^.allunitsused; }

{$ifdef New_GDB}
         write_gdb_info;
{$endIf Def New_GDB}

         if not(cs_compilesystem in aktmoduleswitches) then
           if (Errorcount=0) then
             tppumodule(current_module).getppucrc;

         { Parse the implementation section }
         consume(_IMPLEMENTATION);
         current_module.in_interface:=false;

         Comment(V_Used,'Loading implementation units from '+current_module.modulename^);
//         Message1(unit_u_start_parse_implementation,current_module.modulename^);

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
         parse_implementation_uses(unitst);

         if current_module.state=ms_compiled then
           begin
              RestoreUnitSyms;
              exit;
           end;

         { reset ranges/stabs in exported definitions }
         reset_global_defs;

         { All units are read, now give them a number }
         current_module.numberunits;

         { now we can change refsymtable }
         refsymtable:=st;

         { but reinsert the global symtable as lasts }
         unitst.next:=symtablestack;
         symtablestack:=unitst;

         tstoredsymtable(symtablestack).chainoperators;

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

         Comment(V_Used,'Parsing implementation of '+current_module.modulename^);
         if current_module.in_interface then
           internalerror(200212285);
//         Message1(parser_u_parsing_implementation,current_module.modulename^);

         { Compile the unit }
         pd:=create_main_proc(current_module.modulename^+'_init',potype_unitinit,st);
         pd.aliasnames.insert('INIT$$'+current_module.modulename^);
         compile_proc_body(pd,true,false);
         release_main_proc(pd);

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=tglobalsymtable(current_module.globalsymtable).needs_init_final or
                           tstaticsymtable(current_module.localsymtable).needs_init_final;

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         if force_init_final and ((current_module.flags and uf_init)=0) then
           begin
              gen_implicit_initfinal(initfinalcode,uf_init,st);
              codesegment.concatlist(initfinalcode);
           end;
         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              pd:=create_main_proc(current_module.modulename^+'_finalize',potype_unitfinalize,st);
              pd.aliasnames.insert('FINALIZE$$'+current_module.modulename^);
              compile_proc_body(pd,true,false);
              release_main_proc(pd);
           end
         else if force_init_final then
           begin
              gen_implicit_initfinal(initfinalcode,uf_finalize,st);
              codesegment.concatlist(initfinalcode);
           end;

         { the last char should always be a point }
         consume(_POINT);

         { generate a list of threadvars }
         InsertThreadvars;

         { Generate resoucestrings }
         If ResourceStrings.ResStrCount>0 then
          begin
            ResourceStrings.CreateResourceStringList;
            current_module.flags:=current_module.flags or uf_has_resources;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
          end;

         { test static symtable }
         if (Errorcount=0) then
           begin
             tstoredsymtable(st).allsymbolsused;
             tstoredsymtable(st).allunitsused;
             tstoredsymtable(st).allprivatesused;
           end;

         { size of the static data }
         datasize:=st.datasize;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
{$IfnDef New_GDB}
            if assigned(current_module.globalsymtable) then
              begin
                 { all types }
                 tglobalsymtable(current_module.globalsymtable).concattypestabto(debuglist);
                 { and all local symbols}
                 tglobalsymtable(current_module.globalsymtable).concatstabto(debuglist);
              end;
            { all local types }
            tglobalsymtable(st)^.concattypestabto(debuglist);
            { and all local symbols}
            st^.concatstabto(debuglist);
{$else New_GDB}
            write_gdb_info;
{$endIf Def New_GDB}
          end;
{$endif GDB}

         reset_global_defs;

         if (Errorcount=0) then
           begin
             { tests, if all (interface) forwards are resolved }
             tstoredsymtable(symtablestack).check_forwards;
             { check if all private fields are used }
             tstoredsymtable(symtablestack).allprivatesused;
             { remove cross unit overloads }
             tstoredsymtable(symtablestack).unchain_overloaded;
           end;

{$ifdef GDB}
         tglobalsymtable(symtablestack).is_stab_written:=false;
{$endif GDB}

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

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
{$ifdef GDB}
         pu:=tused_unit(usedunits.first);
         while assigned(pu) do
           begin
              if assigned(pu.u.globalsymtable) then
                tglobalsymtable(pu.u.globalsymtable).is_stab_written:=false;
              pu:=tused_unit(pu.next);
           end;
{$endif GDB}

         if is_assembler_generated then
          begin
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
             Comment(V_Warning,current_module.ppufilename^+' Interface CRC changed '+
               hexstr(store_crc,8)+'<>'+hexstr(current_module.interface_crc,8));
{$ifdef EXTDEBUG}
         if not(cs_compilesystem in aktmoduleswitches) then
           if (store_crc<>current_module.crc) and simplify_ppu then
             Comment(V_Note,current_module.ppufilename^+' implementation CRC changed '+
               hexstr(store_crc,8)+'<>'+hexstr(current_module.crc,8));
{$endif EXTDEBUG}

         { remove static symtable (=refsymtable) here to save some mem }
         if not (cs_local_browser in aktmoduleswitches) then
           begin
              st.free;
              current_module.localsymtable:=nil;
           end;

         RestoreUnitSyms;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

        initfinalcode.free;

        Comment(V_Used,'Finished compiling module '+current_module.modulename^);
      end;


    procedure proc_program(islibrary : boolean);
      var
         main_file: tinputfile;
         st    : tsymtable;
         hp    : tmodule;
         initfinalcode : taasmoutput;
         pd : tprocdef;
      begin
        initfinalcode:=taasmoutput.create;
         DLLsource:=islibrary;
         Status.IsLibrary:=IsLibrary;
         Status.IsExe:=true;
         parse_only:=false;
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

         tstoredsymtable(symtablestack).chainoperators;

         { reset ranges/stabs in exported definitions }
         reset_global_defs;

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
         if islibrary then
          begin
            pd:=create_main_proc(current_module.modulename^+'_main',potype_proginit,st);
            { Win32 startup code needs a single name }
//            if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
            pd.aliasnames.insert('PASCALMAIN');
            { this code is called from C so we need to save some
              registers }
            include(pd.procoptions,po_savestdregs);
          end
         else
          begin
            pd:=create_main_proc('main',potype_proginit,st);
            pd.aliasnames.insert('PASCALMAIN');
          end;
{$IFDEF SPARC}
         current_procinfo.After_Header;
{main function is declared as
  PROCEDURE main(ArgC:Integer;ArgV,EnvP:ARRAY OF PChar):Integer;CDECL;
So, all parameters are passerd into registers in sparc architecture.}
{$ENDIF SPARC}
         compile_proc_body(pd,true,false);
         release_main_proc(pd);

         { should we force unit initialization? }
         if tstaticsymtable(current_module.localsymtable).needs_init_final then
           begin
              { initialize section }
              gen_implicit_initfinal(initfinalcode,uf_init,st);
              codesegment.concatlist(initfinalcode);
              { finalize section }
              gen_implicit_initfinal(initfinalcode,uf_finalize,st);
              codesegment.concatlist(initfinalcode);
           end;

         { Add symbol to the exports section for win32 so smartlinking a
           DLL will include the edata section }
         if assigned(exportlib) and
            (target_info.system in [system_i386_win32,system_i386_wdosx]) and
            assigned(current_module._exports.first) then
           codesegment.concat(tai_const_symbol.create(exportlib.edatalabel));

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
              pd:=create_main_proc(current_module.modulename^+'_finalize',potype_unitfinalize,st);
              pd.aliasnames.insert('FINALIZE$$'+current_module.modulename^);
              compile_proc_body(pd,true,false);
              release_main_proc(pd);
           end;

         { consume the last point }
         consume(_POINT);

{$ifdef New_GDB}
         write_gdb_info;
{$endIf Def New_GDB}
         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

         { test static symtable }
         if (Errorcount=0) then
           begin
             tstoredsymtable(st).allsymbolsused;
             tstoredsymtable(st).allunitsused;
             tstoredsymtable(st).allprivatesused;
           end;

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

         datasize:=symtablestack.datasize;

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
         initfinalcode.free;
      end;

end.
{
  $Log$
  Revision 1.105  2003-05-11 19:31:28  florian
    * fixed implicit init/final code for units, stack frame was wrong for ppc

  Revision 1.104  2003/04/28 21:19:02  peter
    * fix stabs generation for implicit initfinal

  Revision 1.103  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.102  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
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
