{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

{$define New_GDB}

interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


implementation

    uses
       globtype,version,systems,tokens,
       cutils,comphook,
       globals,verbose,fmodule,finput,fppu,
       symconst,symbase,symppu,symdef,symsym,symtable,aasm,
{$ifdef newcg}
       cgbase,
{$else newcg}
       hcodegen,
{$ifdef i386}
       cgai386,
{$endif i386}
{$endif newcg}
       link,assemble,import,export,gendef,ppu,comprsrc,
       cresstr,cpubase,cpuasm,
{$ifdef GDB}
       gdb,
{$endif GDB}
       scanner,pbase,psystem,psub,parser;

    procedure create_objectfile;
      var
        DLLScanner      : TDLLScanner;
        s               : string;
      begin
        { try to create import entries from system dlls }
        if target_info.DllScanSupported and
           (not current_module.linkOtherSharedLibs.Empty) then
         begin
           { Init DLLScanner }
           if assigned(CDLLScanner[target_info.target]) then
            DLLScanner:=CDLLScanner[target_info.target].Create
           else
            internalerror(200104121);
           { Walk all shared libs }
           While not current_module.linkOtherSharedLibs.Empty do
            begin
              S:=current_module.linkOtherSharedLibs.Getusemask(link_allways);
              DLLScanner.scan(s)
            end;
           DLLscanner.Free;
           { Recreate import section }
           if (target_info.target=target_i386_win32) then
            begin
              if assigned(importssection)then
               importssection.clear
              else
               importssection:=taasmoutput.Create;
              importlib.generatelib;
            end;
         end;

        { create the .s file and assemble it }
        GenerateAsm(false);

        { Also create a smartlinked version ? }
        if (cs_create_smart in aktmoduleswitches) then
         begin
           { regenerate the importssection for win32 }
           if assigned(importssection) and
              (target_info.target=target_i386_win32) then
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

        procedure fixseg(p:TAAsmoutput;sec:tsection);
        begin
          p.insert(Tai_section.Create(sec));
          if (cs_create_smart in aktmoduleswitches) then
           p.insert(Tai_cut.Create);
          p.concat(Tai_section.Create(sec_none));
        end;

      begin
      { Insert Ident of the compiler }
        if (not (cs_create_smart in aktmoduleswitches))
{$ifndef EXTDEBUG}
           and (not current_module.is_unit)
{$endif}
           then
         begin
           dataSegment.insert(Tai_align.Create(4));
           dataSegment.insert(Tai_string.Create('FPC '+full_version_string+
             ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.shortname));
         end;
      { finish codesegment }
{$ifdef i386}
        codeSegment.concat(Tai_align.Create(16));
{$else}
        if cs_littlesize in aktglobalswitches then
          codesegment.concat(tai_align.create(2))
        else
          codesegment.concat(tai_align.create(4));
{$endif}
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
              ResourceStringTables.concat(Tai_const_symbol.Createname(hp.u.modulename^+'_RESOURCESTRINGLIST'));
              inc(count);
            end;
           hp:=tused_unit(hp.next);
         end;
        { Add program resources, if any }
        If ResourceStringList<>Nil then
         begin
           ResourceStringTables.concat(Tai_const_symbol.Createname(current_module.modulename^+'_RESOURCESTRINGLIST'));
           Inc(Count);
         end;
        { TableCount }
        ResourceStringTables.insert(Tai_const.Create_32bit(count));
        ResourceStringTables.insert(Tai_symbol.Createdataname_global('FPC_RESOURCESTRINGTABLES',0));
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
        if current_module.islibrary then
          if (current_module.flags and uf_finalize)<>0 then
            begin
              { INIT code is done by PASCALMAIN calling }
              unitinits.concat(Tai_const.Create_32bit(0));
              unitinits.concat(Tai_const_symbol.Createname('FINALIZE$$'+current_module.modulename^));
              inc(count);
            end;
        { TableCount,InitCount }
        unitinits.insert(Tai_const.Create_32bit(0));
        unitinits.insert(Tai_const.Create_32bit(count));
        unitinits.insert(Tai_symbol.Createdataname_global('INITFINAL',0));
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
         case target_info.target of
{$ifdef i386}
            target_i386_OS2:
              ;
{$endif i386}
{$ifdef alpha}
            target_alpha_linux:
              ;
{$endif alpha}
{$ifdef powerpc}
            target_powerpc_linux:
              ;
{$endif powerpc}
{$ifdef m68k}
            target_m68k_Mac:
              bssSegment.concat(Tai_datablock.Create_global('HEAP',4));
            target_m68k_PalmOS:
              ;
{$endif m68k}
         else
           bssSegment.concat(Tai_datablock.Create_global('HEAP',heapsize));
         end;
{$ifdef m68k}
         if target_info.target<>target_m68k_PalmOS then
           begin
              dataSegment.concat(Tai_symbol.Createdataname_global('HEAP_SIZE',0));
              dataSegment.concat(Tai_const.Create_32bit(heapsize));
           end;
{$else m68k}
         dataSegment.concat(Tai_symbol.Createdataname_global('HEAPSIZE',4));
         dataSegment.concat(Tai_const.Create_32bit(heapsize));
{$endif m68k}
      end;


    procedure inserttargetspecific;
      begin
        case target_info.target of
{$ifdef alpha}
          target_alpha_linux:
            ;
{$endif alpha}
{$ifdef powerpc}
          target_powerpc_linux:
            ;
{$endif powerpc}
{$ifdef i386}
          target_i386_GO32V2 :
            begin
              { stacksize can be specified }
              dataSegment.concat(Tai_symbol.Createdataname_global('__stklen',4));
              dataSegment.concat(Tai_const.Create_32bit(stacksize));
            end;
{$endif i386}
{$ifdef m68k}
          target_m68k_Atari :
            begin
              { stacksize can be specified }
              dataSegment.concat(Tai_symbol.Createdataname_global('__stklen',4));
              dataSegment.concat(Tai_const.Create_32bit(stacksize));
            end;
{$endif m68k}
        end;
      end;


    procedure loaddefaultunits;
      var
        hp : tmodule;
        unitsym : tunitsym;
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
        hp:=loadunit('System');
        systemunit:=tglobalsymtable(hp.globalsymtable);
        { it's always the first unit }
        systemunit.next:=nil;
        symtablestack:=systemunit;
        { add to the used units }
        current_module.used_units.concat(tused_unit.create(hp,true));
        unitsym:=tunitsym.create('System',systemunit);
        inc(unitsym.refs);
        refsymtable.insert(unitsym);
        { read default constant definitions }
        make_ref:=false;
        readconstdefs;
        make_ref:=true;
      { Objpas unit? }
        if m_objpas in aktmodeswitches then
         begin
           hp:=loadunit('ObjPas');
           tsymtable(hp.globalsymtable).next:=symtablestack;
           symtablestack:=hp.globalsymtable;
           { add to the used units }
           current_module.used_units.concat(tused_unit.create(hp,true));
           unitsym:=tunitsym.create('ObjPas',hp.globalsymtable);
           inc(unitsym.refs);
           refsymtable.insert(unitsym);
         end;
      { Profile unit? Needed for go32v2 only }
        if (cs_profile in aktmoduleswitches) and (target_info.target=target_i386_go32v2) then
         begin
           hp:=loadunit('Profile');
           tsymtable(hp.globalsymtable).next:=symtablestack;
           symtablestack:=hp.globalsymtable;
           { add to the used units }
           current_module.used_units.concat(tused_unit.create(hp,true));
           unitsym:=tunitsym.create('Profile',hp.globalsymtable);
           inc(unitsym.refs);
           refsymtable.insert(unitsym);
         end;
      { Units only required for main module }
        if not(current_module.is_unit) then
         begin
           { Heaptrc unit }
           if (cs_gdb_heaptrc in aktglobalswitches) then
            begin
              hp:=loadunit('HeapTrc');
              tsymtable(hp.globalsymtable).next:=symtablestack;
              symtablestack:=hp.globalsymtable;
              { add to the used units }
              current_module.used_units.concat(tused_unit.create(hp,true));
              unitsym:=tunitsym.create('HeapTrc',hp.globalsymtable);
              inc(unitsym.refs);
              refsymtable.insert(unitsym);
            end;
           { Lineinfo unit }
           if (cs_gdb_lineinfo in aktglobalswitches) then
            begin
              hp:=loadunit('LineInfo');
              tsymtable(hp.globalsymtable).next:=symtablestack;
              symtablestack:=hp.globalsymtable;
              { add to the used units }
              current_module.used_units.concat(tused_unit.create(hp,true));
              unitsym:=tunitsym.create('LineInfo',hp.globalsymtable);
              inc(unitsym.refs);
              refsymtable.insert(unitsym);
            end;
         end;
      { save default symtablestack }
        defaultsymtablestack:=symtablestack;
      end;


    procedure loadunits;
      var
         s,sorg : stringid;
         pu,
         hp : tused_unit;
         hp2 : tmodule;
         hp3 : tsymtable;
         oldprocsym:tprocsym;
         unitsym : tunitsym;
      begin
         oldprocsym:=aktprocsym;
         consume(_USES);
{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         repeat
           s:=pattern;
           sorg:=orgpattern;
           consume(_ID);
         { Give a warning if objpas is loaded }
           if s='OBJPAS' then
            Message(parser_w_no_objpas_use_mode);
         { check if the unit is already used }
           pu:=tused_unit(current_module.used_units.first);
           while assigned(pu) do
            begin
              if (pu.name^=s) then
               break;
              pu:=tused_unit(pu.next);
            end;
         { avoid uses of itself }
           if not assigned(pu) and (s<>current_module.modulename^) then
            begin
            { load the unit }
              hp2:=loadunit(sorg);
            { the current module uses the unit hp2 }
              current_module.used_units.concat(tused_unit.create(hp2,not current_module.in_implementation));
              tused_unit(current_module.used_units.last).in_uses:=true;
              if current_module.compiled then
                exit;
              unitsym:=tunitsym.create(sorg,hp2.globalsymtable);
              { never claim about unused unit if
                there is init or finalize code  PM }
              if (hp2.flags and (uf_init or uf_finalize))<>0 then
                inc(unitsym.refs);
              refsymtable.insert(unitsym);
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

         { set the symtable to systemunit so it gets reorderd correctly }
         symtablestack:=defaultsymtablestack;

         { now insert the units in the symtablestack }
         hp:=tused_unit(current_module.used_units.first);
         while assigned(hp) do
           begin
{$IfDef GDB}
              if (cs_debuginfo in aktmoduleswitches) and
                 (cs_gdb_dbx in aktglobalswitches) and
                not hp.is_stab_written then
                begin
                   tglobalsymtable(hp.u.globalsymtable).concattypestabto(debuglist);
                   hp.is_stab_written:=true;
                   hp.unitid:=tsymtable(hp.u.globalsymtable).unitid;
                end;
{$EndIf GDB}
              if hp.in_uses then
                begin
                   hp3:=symtablestack;
                   while assigned(hp3) do
                     begin
                        { insert units only once ! }
                        if hp.u.globalsymtable=hp3 then
                          break;
                        hp3:=hp3.next;
                        { unit isn't inserted }
                        if hp3=nil then
                          begin
                             tsymtable(hp.u.globalsymtable).next:=symtablestack;
                             symtablestack:=tsymtable(hp.u.globalsymtable);
{$ifdef CHAINPROCSYMS}
                             symtablestack.chainprocsyms;
{$endif CHAINPROCSYMS}
{$ifdef DEBUG}
                             test_symtablestack;
{$endif DEBUG}
                          end;
                     end;
                end;
              hp:=tused_unit(hp.next);
           end;
          aktprocsym:=oldprocsym;
      end;


     procedure write_gdb_info;
{$IfDef GDB}
       var
         hp : tused_unit;
       begin
         if not (cs_debuginfo in aktmoduleswitches) then
          exit;
         if (cs_gdb_dbx in aktglobalswitches) then
           begin
             debugList.concat(Tai_asm_comment.Create(strpnew('EINCL of global '+
               tglobalsymtable(current_module.globalsymtable).name^+' has index '+
               tostr(tglobalsymtable(current_module.globalsymtable).unitid))));
             debugList.concat(Tai_stabs.Create(strpnew('"'+
               tglobalsymtable(current_module.globalsymtable).name^+'",'+
               tostr(N_EINCL)+',0,0,0')));
             tglobalsymtable(current_module.globalsymtable).dbx_count_ok:={true}false;
             dbx_counter:=tglobalsymtable(current_module.globalsymtable).prev_dbx_counter;
             do_count_dbx:=false;
           end;

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
         if current_module.in_implementation and
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
         if (m_tp in aktmodeswitches) then
          current_scanner.def_macro('FPC_TP')
        else
         if (m_objfpc in aktmodeswitches) then
          current_scanner.def_macro('FPC_OBJFPC')
        else
         if (m_gpc in aktmodeswitches) then
          current_scanner.def_macro('FPC_GPC');
      end;


    procedure gen_main_procsym(const name:string;options:tproctypeoption;st:tsymtable);
      var
        stt : tsymtable;
      begin
        {Generate a procsym for main}
        make_ref:=false;
        aktprocsym:=tprocsym.create('$'+name);
        { main are allways used }
        inc(aktprocsym.refs);
        {Try to insert in in static symtable ! }
        stt:=symtablestack;
        symtablestack:=st;
        aktprocsym.definition:=tprocdef.create;
        symtablestack:=stt;
        aktprocsym.definition.proctypeoption:=options;
        aktprocsym.definition.setmangledname(target_info.cprefix+name);
        aktprocsym.definition.forwarddef:=false;
        make_ref:=true;
        { The localst is a local symtable. Change it into the static
          symtable }
        aktprocsym.definition.localst.free;
        aktprocsym.definition.localst:=st;
        { and insert the procsym in symtable }
        st.insert(aktprocsym);
        { set some informations about the main program }
        with procinfo^ do
         begin
           returntype:=voidtype;
           _class:=nil;
           para_offset:=8;
           framepointer:=frame_pointer;
           flags:=0;
         end;
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

      begin
         consume(_UNIT);

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

         Message1(unit_u_start_parse_interface,current_module.realmodulename^);

         { update status }
         status.currentmodule:=current_module.realmodulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (current_module.modulename^='OBJPAS') then
           exclude(aktmodeswitches,m_objpas);

         { this should be placed after uses !!}
{$ifndef UseNiceNames}
         procprefix:='_'+current_module.modulename^+'$$';
{$else UseNiceNames}
         procprefix:='_'+tostr(length(current_module.modulename^))+lowercase(current_module.modulename^)+'_';
{$endif UseNiceNames}

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
         lexlevel:=0;

         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in aktmoduleswitches) then
           begin
              if token=_USES then
                begin
                   loadunits;
                   { has it been compiled at a higher level ?}
                   if current_module.compiled then
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
         Message1(parser_u_parsing_interface,current_module.realmodulename^);
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
         current_module.in_implementation:=true;
         Message1(unit_u_start_parse_implementation,current_module.modulename^);

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

         if current_module.compiled then
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

         Message1(parser_u_parsing_implementation,current_module.realmodulename^);

         { Compile the unit }
         codegen_newprocedure;
         gen_main_procsym(current_module.modulename^+'_init',potype_unitinit,st);
         aktprocsym.definition.aliasnames.insert('INIT$$'+current_module.modulename^);
         aktprocsym.definition.aliasnames.insert(target_info.cprefix+current_module.modulename^+'_init');
         compile_proc_body(true,false);
         codegen_doneprocedure;

         { avoid self recursive destructor call !! PM }
         aktprocsym.definition.localst:=nil;

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=tglobalsymtable(current_module.globalsymtable).needs_init_final or
                           tstaticsymtable(current_module.localsymtable).needs_init_final;

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         if force_init_final and ((current_module.flags and uf_init)=0) then
           begin
              current_module.flags:=current_module.flags or uf_init;
              { now we can insert a cut }
              if (cs_create_smart in aktmoduleswitches) then
                codeSegment.concat(Tai_cut.Create);
              genimplicitunitinit(codesegment);
           end;
         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              codegen_newprocedure;
              gen_main_procsym(current_module.modulename^+'_finalize',potype_unitfinalize,st);
              aktprocsym.definition.aliasnames.insert('FINALIZE$$'+current_module.modulename^);
              aktprocsym.definition.aliasnames.insert(target_info.cprefix+current_module.modulename^+'_finalize');
              compile_proc_body(true,false);
              codegen_doneprocedure;
           end
         else if force_init_final then
           begin
              current_module.flags:=current_module.flags or uf_finalize;
              { now we can insert a cut }
              if (cs_create_smart in aktmoduleswitches) then
                codeSegment.concat(Tai_cut.Create);
              genimplicitunitfinal(codesegment);
           end;

         { the last char should always be a point }
         consume(_POINT);

         If ResourceStrings.ResStrCount>0 then
          begin
            ResourceStrings.CreateResourceStringList;
            current_module.flags:=current_module.flags or uf_has_resources;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
          end;

         { avoid self recursive destructor call !! PM }
         aktprocsym.definition.localst:=nil;
         { absence does not matter here !! }
         aktprocsym.definition.forwarddef:=false;
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

         { tests, if all (interface) forwards are resolved }
         if (Errorcount=0) then
           begin
             tstoredsymtable(symtablestack).check_forwards;
             tstoredsymtable(symtablestack).allprivatesused;
           end;

         current_module.in_implementation:=false;
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
             Comment(V_Warning,current_module.ppufilename^+' implementation CRC changed '+
               hexstr(store_crc,8)+'<>'+hexstr(current_module.interface_crc,8));
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
      end;


    procedure proc_program(islibrary : boolean);
      var
         main_file: tinputfile;
         st    : tsymtable;
         hp    : tmodule;
      begin
         DLLsource:=islibrary;
         IsExe:=true;
         parse_only:=false;
         { relocation works only without stabs under win32 !! PM }
         { internal assembler uses rva for stabs info
           so it should work with relocated DLLs }
         if RelocSection and
            (target_info.target=target_i386_win32) and
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
              if (target_info.target=target_i386_WIN32) then
                exportlib.preparelib(pattern);
              consume(_ID);
              if token=_LKLAMMER then
                begin
                   consume(_LKLAMMER);
                   idlist;
                   consume(_RKLAMMER);
                end;
              consume(_SEMICOLON);
            end
         else if (target_info.target=target_i386_WIN32) then
           exportlib.preparelib(current_module.modulename^);

         { global switches are read, so further changes aren't allowed }
         current_module.in_global:=false;

         { setup things using the global switches }
         setupglobalswitches;

         { set implementation flag }
         current_module.in_implementation:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         st:=tstaticsymtable.create(current_module.modulename^);;
         current_module.localsymtable:=st;
         refsymtable:=st;

         { load standard units (system,objpas,profile unit) }
         loaddefaultunits;

         { reset }
         lexlevel:=0;

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
           st.insert(tunitsym.create(current_module.realmodulename^,tglobalsymtable(st)));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         Message1(parser_u_parsing_implementation,current_module.mainsource^);

         { reset }
         procprefix:='';

         {The program intialization needs an alias, so it can be called
          from the bootstrap code.}
         codegen_newprocedure;
         gen_main_procsym('main',potype_proginit,st);
         aktprocsym.definition.aliasnames.insert('program_init');
         aktprocsym.definition.aliasnames.insert('PASCALMAIN');
         aktprocsym.definition.aliasnames.insert(target_info.cprefix+'main');
{$ifdef m68k}
         if target_info.target=target_m68k_PalmOS then
           aktprocsym.definition.aliasnames.insert('PilotMain');
{$endif m68k}
         compile_proc_body(true,false);

         { avoid self recursive destructor call !! PM }
         aktprocsym.definition.localst:=nil;

         { consider these symbols as global ones for browser
           but the typecasting of the globalsymtable with tglobalsymtable
           can then lead to problems (PFV)
         current_module.globalsymtable:=current_module.localsymtable;
         current_module.localsymtable:=nil;}

         If ResourceStrings.ResStrCount>0 then
          begin
            ResourceStrings.CreateResourceStringList;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings.WriteResourceFile(ForceExtension(current_module.ppufilename^,'.rst'));
          end;

         codegen_doneprocedure;

         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module.flags:=current_module.flags or uf_finalize;

              { Compile the finalize }
              codegen_newprocedure;
              gen_main_procsym(current_module.modulename^+'_finalize',potype_unitfinalize,st);
              aktprocsym.definition.aliasnames.insert('FINALIZE$$'+current_module.modulename^);
              aktprocsym.definition.aliasnames.insert(target_info.cprefix+current_module.modulename^+'_finalize');
              compile_proc_body(true,false);
              codegen_doneprocedure;
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

         { generate imports }
         if current_module.uses_imports then
          importlib.generatelib;

         if islibrary or
            (target_info.target=target_i386_WIN32) then
           exportlib.generatelib;


         { insert heap }
         insertResourceTablesTable;
         insertinitfinaltable;
         insertheap;
         inserttargetspecific;

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
      end;

end.
{
  $Log$
  Revision 1.32  2001-05-18 22:26:36  peter
    * merged alignment for non-i386

  Revision 1.31  2001/05/09 14:11:10  jonas
    * range check error fixes from Peter

  Revision 1.30  2001/05/06 14:49:17  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.29  2001/04/18 22:01:57  peter
    * registration of targets and assemblers

  Revision 1.28  2001/04/13 18:08:37  peter
    * scanner object to class

  Revision 1.27  2001/04/13 01:22:12  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.26  2001/04/02 21:20:33  peter
    * resulttype rewrite

  Revision 1.25  2001/03/13 18:45:07  peter
    * fixed some memory leaks

  Revision 1.24  2001/03/06 18:28:02  peter
    * patch from Pavel with a new and much faster DLL Scanner for
      automatic importing so $linklib works for DLLs. Thanks Pavel!

  Revision 1.23  2001/02/24 10:44:56  peter
    * generate .rst from ppufilename instead of modulename

  Revision 1.22  2001/02/21 19:37:19  peter
    * moved deref to be done after loading of implementation units. prederef
      is still done directly after loading of symbols and definitions.

  Revision 1.21  2001/01/14 22:13:52  peter
    * fixed crash with program name as a important unit name

  Revision 1.20  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.19  2000/11/29 00:30:36  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.18  2000/11/01 23:04:37  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.17  2000/10/31 22:02:50  peter
    * symtable splitted, no real code changes

  Revision 1.16  2000/10/21 14:36:26  peter
    * merged pierres fixes

  Revision 1.15  2000/10/15 09:08:58  peter
    * use System for the systemunit instead of target dependent

  Revision 1.14  2000/10/15 07:47:51  peter
    * unit names and procedure names are stored mixed case

  Revision 1.13  2000/10/04 14:51:08  pierre
   * IsExe restored

  Revision 1.12  2000/09/30 16:07:40  peter
    * filepos when unit not found (merged)

  Revision 1.11  2000/09/24 21:33:47  peter
    * message updates merges

  Revision 1.10  2000/09/24 15:06:22  peter
    * use defines.inc

  Revision 1.9  2000/08/31 07:53:02  michael
  + Applied patch from Peter

  Revision 1.8  2000/08/29 19:00:01  peter
    * _init and _finalize procsyms also need a $ prefix

  Revision 1.7  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.6  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.5  2000/08/25 08:48:22  jonas
    * fixed bug with include files at the very beginning of .pp/.pas files
      (wrong name used for generating exe/checking unit name) (merged from
      fixes branch)

  Revision 1.4  2000/08/21 11:27:44  pierre
   * fix the stabs problems

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:45  michael
  + removed logs
}
