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
            { close old_current_ppu on system that are
              short on file handles like DOS system PM }
{$ifdef GO32V1}
{$define SHORT_ON_FILE_HANDLES}
{$endif GO32V1}
{$ifdef GO32V2}
{$define SHORT_ON_FILE_HANDLES}
{$endif GO32V2}

{$define New_GDB}

  interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


  implementation

    uses
       globtype,version,systems,tokens,
       cobjects,comphook,compiler,
       globals,verbose,files,
       symconst,symtable,aasm,types,
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
       scanner,pbase,psystem,pdecl,psub,parser;

    procedure create_objectfile;
      begin
        { create the .s file and assemble it }
        GenerateAsm(false);

        { Also create a smartlinked version ? }
        if (cs_create_smart in aktmoduleswitches) then
         begin
           { regenerate the importssection for win32 }
           if assigned(importssection) and
              (target_info.target=target_i386_win32) then
            begin
              importssection^.clear;
              importlib^.generatesmartlib;
            end;

           GenerateAsm(true);
           if target_asm.needar then
             Linker^.MakeStaticLibrary;
         end;

        { resource files }
        CompileResourceFiles;
      end;


    procedure insertobjectfile;
    { Insert the used object file for this unit in the used list for this unit }
      begin
        current_module^.linkunitofiles.insert(current_module^.objfilename^,link_static);
        current_module^.flags:=current_module^.flags or uf_static_linked;

        if (cs_create_smart in aktmoduleswitches) then
         begin
           current_module^.linkunitstaticlibs.insert(current_module^.staticlibfilename^,link_smart);
           current_module^.flags:=current_module^.flags or uf_smart_linked;
         end;
      end;


    procedure insertsegment;

        procedure fixseg(p:paasmoutput;sec:tsection);
        begin
          p^.insert(new(pai_section,init(sec)));
          if (cs_create_smart in aktmoduleswitches) then
           p^.insert(new(pai_cut,init));
          p^.concat(new(pai_section,init(sec_none)));
        end;

      begin
      { Insert Ident of the compiler }
        if (not (cs_create_smart in aktmoduleswitches))
{$ifndef EXTDEBUG}
           and (not current_module^.is_unit)
{$endif}
           then
         begin
           datasegment^.insert(new(pai_align,init(4)));
           datasegment^.insert(new(pai_string,init('FPC '+full_version_string+
             ' ['+date_string+'] for '+target_cpu_string+' - '+target_info.short_name)));
         end;
      { finish codesegment }
        codesegment^.concat(new(pai_align,init(16)));
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
            debuglist^.insert(new(pai_symbol,initname('gcc2_compiled',0)));
            fixseg(debuglist,sec_code);
          end;
{$endif GDB}
      end;


    Procedure InsertResourceTablesTable;
      var
        hp : pused_unit;
        ResourceStringTables : taasmoutput;
        count : longint;
      begin
        ResourceStringTables.init;
        count:=0;
        hp:=pused_unit(usedunits.first);
        while assigned(hp) do
          begin
          If (hp^.u^.flags and uf_has_resources)=uf_has_resources then
            begin
            ResourceStringTables.concat(new(pai_const_symbol,initname(hp^.u^.modulename^+'_RESOURCESTRINGLIST')));
            inc(count);
            end;
          hp:=Pused_unit(hp^.next);
          end;
        { Add program resources, if any }
        If ResourceStringList<>Nil then
          begin
          ResourceStringTables.concat(new(pai_const_symbol,initname(Current_Module^.modulename^+'_RESOURCESTRINGLIST')));
          Inc(Count);
          end;
        { TableCount }
{ doesn't work because of bug in the compiler !! (JM)
        With ResourceStringTables do}
          begin
          ResourceStringTables.insert(new(pai_const,init_32bit(count)));
          ResourceStringTables.insert(new(pai_symbol,initdataname_global('FPC_RESOURCESTRINGTABLES',0)));
          ResourceStringTables.concat(new(pai_symbol_end,initname('FPC_RESOURCESTRINGTABLES')));
          end;
        { insert in data segment }
        if (cs_create_smart in aktmoduleswitches) then
          datasegment^.concat(new(pai_cut,init));
        datasegment^.concatlist(@ResourceStringTables);
        ResourceStringTables.done;
      end;


    procedure InsertInitFinalTable;
      var
        hp : pused_unit;
        unitinits : taasmoutput;
        count : longint;
      begin
        unitinits.init;
        count:=0;
        hp:=pused_unit(usedunits.first);
        while assigned(hp) do
         begin
           { call the unit init code and make it external }
           if (hp^.u^.flags and (uf_init or uf_finalize))<>0 then
            begin
              if (hp^.u^.flags and uf_init)<>0 then
               begin
                 unitinits.concat(new(pai_const_symbol,initname('INIT$$'+hp^.u^.modulename^)));
               end
              else
               unitinits.concat(new(pai_const,init_32bit(0)));
              if (hp^.u^.flags and uf_finalize)<>0 then
               begin
                 unitinits.concat(new(pai_const_symbol,initname('FINALIZE$$'+hp^.u^.modulename^)));
               end
              else
               unitinits.concat(new(pai_const,init_32bit(0)));
              inc(count);
            end;
           hp:=Pused_unit(hp^.next);
         end;
        if current_module^.islibrary then
          if (current_module^.flags and uf_finalize)<>0 then
            begin
              { INIT code is done by PASCALMAIN calling }
              unitinits.concat(new(pai_const,init_32bit(0)));
              unitinits.concat(new(pai_const_symbol,initname('FINALIZE$$'+current_module^.modulename^)));
              inc(count);
            end;
        { TableCount,InitCount }
        unitinits.insert(new(pai_const,init_32bit(0)));
        unitinits.insert(new(pai_const,init_32bit(count)));
        unitinits.insert(new(pai_symbol,initdataname_global('INITFINAL',0)));
        unitinits.concat(new(pai_symbol_end,initname('INITFINAL')));
        { insert in data segment }
        if (cs_create_smart in aktmoduleswitches) then
          datasegment^.concat(new(pai_cut,init));
        datasegment^.concatlist(@unitinits);
        unitinits.done;
      end;


    procedure insertheap;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           begin
             bsssegment^.concat(new(pai_cut,init));
             datasegment^.concat(new(pai_cut,init));
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
              bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)));
            target_m68k_PalmOS:
              ;
{$endif m68k}
         else
           bsssegment^.concat(new(pai_datablock,init_global('HEAP',heapsize)));
         end;
{$ifdef m68k}
         if target_info.target<>target_m68k_PalmOS then
           begin
              datasegment^.concat(new(pai_symbol,initdataname_global('HEAP_SIZE',0)));
              datasegment^.concat(new(pai_const,init_32bit(heapsize)));
           end;
{$else m68k}
         datasegment^.concat(new(pai_symbol,initdataname_global('HEAPSIZE',4)));
         datasegment^.concat(new(pai_const,init_32bit(heapsize)));
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
              datasegment^.concat(new(pai_symbol,initdataname_global('__stklen',4)));
              datasegment^.concat(new(pai_const,init_32bit(stacksize)));
            end;
{$endif i386}
{$ifdef m68k}
          target_m68k_Atari :
            begin
              { stacksize can be specified }
              datasegment^.concat(new(pai_symbol,initdataname_global('__stklen',4)));
              datasegment^.concat(new(pai_const,init_32bit(stacksize)));
            end;
{$endif m68k}
        end;
      end;


    function loadunit(const s : string;compile_system:boolean) : pmodule;forward;


    procedure load_usedunits(compile_system:boolean);
      var
        pu         : pused_unit;
        loaded_unit  : pmodule;
        load_refs    : boolean;
        nextmapentry : longint;
      begin
        load_refs:=true;
      { init the map }
        new(current_module^.map);
        fillchar(current_module^.map^,sizeof(tunitmap),#0);
{$ifdef NEWMAP}
        current_module^.map^[0]:=current_module;
{$endif NEWMAP}
        nextmapentry:=1;
      { load the used units from interface }
        current_module^.in_implementation:=false;
        pu:=pused_unit(current_module^.used_units.first);
        while assigned(pu) do
         begin
           if (not pu^.loaded) and (pu^.in_interface) then
            begin
              loaded_unit:=loadunit(pu^.name^,false);
              if current_module^.compiled then
               exit;
            { register unit in used units }
              pu^.u:=loaded_unit;
              pu^.loaded:=true;
            { doubles are not important for that list PM }
              pu^.u^.dependent_units.concat(new(pdependent_unit,init(current_module)));
            { need to recompile the current unit ? }
              if loaded_unit^.crc<>pu^.checksum then
               begin
                 Message2(unit_u_recompile_crc_change,current_module^.modulename^,pu^.name^);
                 current_module^.recompile_reason:=rr_crcchanged;
                 current_module^.do_compile:=true;
                 dispose(current_module^.map);
                 current_module^.map:=nil;
                 exit;
               end;
            { setup the map entry for deref }
{$ifndef NEWMAP}
              current_module^.map^[nextmapentry]:=loaded_unit^.globalsymtable;
{$else NEWMAP}
              current_module^.map^[nextmapentry]:=loaded_unit;
{$endif NEWMAP}
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=pused_unit(pu^.next);
         end;
      { ok, now load the unit }
        current_module^.globalsymtable:=new(punitsymtable,loadasunit);
      { now only read the implementation part }
        current_module^.in_implementation:=true;
      { load the used units from implementation }
        pu:=pused_unit(current_module^.used_units.first);
        while assigned(pu) do
         begin
           if (not pu^.loaded) and (not pu^.in_interface) then
            begin
              loaded_unit:=loadunit(pu^.name^,false);
              if current_module^.compiled then
               exit;
            { register unit in used units }
              pu^.u:=loaded_unit;
              pu^.loaded:=true;
            { need to recompile the current unit ? }
              if (loaded_unit^.interface_crc<>pu^.interface_checksum) {and
                 not(current_module^.in_second_compile) } then
                begin
                  Message2(unit_u_recompile_crc_change,current_module^.modulename^,pu^.name^+' {impl}');
                  current_module^.recompile_reason:=rr_crcchanged;
                  current_module^.do_compile:=true;
                  dispose(current_module^.map);
                  current_module^.map:=nil;
                  exit;
                end;
            { setup the map entry for deref }
{$ifndef NEWMAP}
              current_module^.map^[nextmapentry]:=loaded_unit^.globalsymtable;
{$else NEWMAP}
              current_module^.map^[nextmapentry]:=loaded_unit;
{$endif NEWMAP}
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=pused_unit(pu^.next);
         end;
        { load browser info if stored }
        if ((current_module^.flags and uf_has_browser)<>0) and load_refs then
          punitsymtable(current_module^.globalsymtable)^.load_symtable_refs;
        { remove the map, it's not needed anymore }
        dispose(current_module^.map);
        current_module^.map:=nil;
      end;


    function loadunit(const s : string;compile_system:boolean) : pmodule;
      const
        ImplIntf : array[boolean] of string[15]=('interface','implementation');
      var
        st : punitsymtable;
        second_time : boolean;
        old_current_ppu : pppufile;
        old_current_module,hp,hp2 : pmodule;
        name : string;{ necessary because current_module^.mainsource^ is reset in compile !! }
        scanner : pscannerfile;

        procedure loadppufile;
        begin
        { load interface section }
          if not current_module^.do_compile then
           load_interface;
        { only load units when we don't recompile }
          if not current_module^.do_compile then
           load_usedunits(compile_system);
        { recompile if set }
          if current_module^.do_compile then
           begin
           { we don't need the ppufile anymore }
             if assigned(current_module^.ppufile) then
              begin
                dispose(current_module^.ppufile,done);
                current_module^.ppufile:=nil;
                current_ppu:=nil;
              end;
           { recompile the unit or give a fatal error if sources not available }
             if not(current_module^.sources_avail) and
                not(current_module^.sources_checked) then
               if (not current_module^.search_unit(current_module^.modulename^,true))
                  and (length(current_module^.modulename^)>8) then
                 current_module^.search_unit(copy(current_module^.modulename^,1,8),true);
             if not(current_module^.sources_avail) then
               begin
                  if current_module^.recompile_reason=rr_noppu then
                    Message1(unit_f_cant_find_ppu,current_module^.modulename^)
                  else
                    Message1(unit_f_cant_compile_unit,current_module^.modulename^);
               end
             else
              begin
                if current_module^.in_compile then
                  begin
                    current_module^.in_second_compile:=true;
                    Message1(parser_d_compiling_second_time,current_module^.modulename^);
                  end;
                current_scanner^.tempcloseinputfile;
                name:=current_module^.mainsource^;
                if assigned(scanner) then
                  scanner^.invalid:=true;
                compile(name,compile_system);
                current_module^.in_second_compile:=false;
                if (not current_scanner^.invalid) then
                  current_scanner^.tempopeninputfile;
              end;
           end
          else
           begin
           { only reassemble ? }
             if (current_module^.do_assemble) then
              OnlyAsm;
           end;
         if assigned(current_module^.ppufile) then
           begin
              dispose(current_module^.ppufile,done);
              current_module^.ppufile:=nil;
              current_ppu:=nil;
           end;
        end;

      var
         dummy : pmodule;

      begin
         old_current_module:=current_module;
         old_current_ppu:=current_ppu;
         { Info }
         Message3(unit_u_load_unit,current_module^.modulename^,ImplIntf[current_module^.in_implementation],s);
         { unit not found }
         st:=nil;
         dummy:=nil;
         { search all loaded units }
         hp:=pmodule(loaded_units.first);
         while assigned(hp) do
           begin
              if hp^.modulename^=s then
                begin
                   { forced to reload ? }
                   if hp^.do_reload then
                    begin
                      hp^.do_reload:=false;
                      break;
                    end;
                   { the unit is already registered   }
                   { and this means that the unit     }
                   { is already compiled              }
                   { else there is a cyclic unit use  }
                   if assigned(hp^.globalsymtable) then
                     st:=punitsymtable(hp^.globalsymtable)
                   else
                    begin
                    { both units in interface ? }
                      if (not current_module^.in_implementation) and (not hp^.in_implementation) then
                       begin
                       { check for a cycle }
                         hp2:=current_module^.loaded_from;
                         while assigned(hp2) and (hp2<>hp) do
                          begin
                            if hp2^.in_implementation then
                             hp2:=nil
                            else
                             hp2:=hp2^.loaded_from;
                          end;
                         if assigned(hp2) then
                          Message2(unit_f_circular_unit_reference,current_module^.modulename^,hp^.modulename^);
                       end;
                    end;
                   break;
                end
              else if copy(hp^.modulename^,1,8)=s then
                dummy:=hp;
              { the next unit }
              hp:=pmodule(hp^.next);
           end;
         if assigned(dummy) and not assigned(hp) then
           Message2(unit_w_unit_name_error,s,dummy^.modulename^);
       { the unit is not in the symtable stack }
         if (not assigned(st)) then
          begin
            if assigned(hp) then
             begin
               { remove the old unit }
               loaded_units.remove(hp);
               scanner:=hp^.scanner;
               hp^.reset;
               hp^.scanner:=scanner;
               { try to reopen ppu }
               hp^.search_unit(s,false);
               { try to load the unit a second time first }
               current_module:=hp;
               current_module^.in_second_load:=true;
               Message1(unit_u_second_load_unit,current_module^.modulename^);
               second_time:=true;
             end
            else
          { generates a new unit info record }
             begin
                current_module:=new(pmodule,init(s,true));
                scanner:=nil;
                second_time:=false;
             end;
            current_ppu:=current_module^.ppufile;
            { close old_current_ppu on system that are
              short on file handles like DOS PM }
{$ifdef SHORT_ON_FILE_HANDLES}
            if assigned(old_current_ppu) then
              old_current_ppu^.tempclose;
{$endif SHORT_ON_FILE_HANDLES}
          { now we can register the unit }
            current_module^.loaded_from:=old_current_module;
            loaded_units.insert(current_module);
          { now realy load the ppu }
            loadppufile;
          { set compiled flag }
            current_module^.compiled:=true;
          { load return pointer }
            hp:=current_module;
          { for a second_time recompile reload all dependent units,
            for a first time compile register the unit _once_ }
            if second_time then
             begin
               { now reload all dependent units }
               hp2:=pmodule(loaded_units.first);
               while assigned(hp2) do
                begin
                  if hp2^.do_reload then
                   dummy:=loadunit(hp2^.modulename^,false);
                  hp2:=pmodule(hp2^.next);
                end;
             end
            else
             usedunits.concat(new(pused_unit,init(current_module,true)));
          end;
         { set the old module }
{$ifdef SHORT_ON_FILE_HANDLES}
         if assigned(old_current_ppu) then
           old_current_ppu^.tempopen;
{$endif SHORT_ON_FILE_HANDLES}
         current_ppu:=old_current_ppu;
         current_module:=old_current_module;
         loadunit:=hp;
      end;


    procedure loaddefaultunits;
      var
        hp : pmodule;
        unitsym : punitsym;
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
        hp:=loadunit(upper(target_info.system_unit),true);
        systemunit:=hp^.globalsymtable;
        { it's always the first unit }
        systemunit^.next:=nil;
        symtablestack:=systemunit;
        { add to the used units }
        current_module^.used_units.concat(new(pused_unit,init(hp,true)));
        unitsym:=new(punitsym,init('SYSTEM',systemunit));
        inc(unitsym^.refs);
        refsymtable^.insert(unitsym);
        { read default constant definitions }
        make_ref:=false;
        readconstdefs;
        { if POWER is defined in the RTL then use it for starstar overloading }
{$ifdef DONOTCHAINOPERATORS}
        getsym('POWER',false);
{$endif DONOTCHAINOPERATORS}
        make_ref:=true;
{$ifdef DONOTCHAINOPERATORS}
        { Code now in chainoperators PM }
        if assigned(srsym) and (srsym^.typ=procsym) and (overloaded_operators[_STARSTAR]=nil) then
          overloaded_operators[_STARSTAR]:=pprocsym(srsym);
{$endif DONOTCHAINOPERATORS}
      { Objpas unit? }
        if m_objpas in aktmodeswitches then
         begin
           hp:=loadunit('OBJPAS',false);
           psymtable(hp^.globalsymtable)^.next:=symtablestack;
           symtablestack:=hp^.globalsymtable;
           { add to the used units }
           current_module^.used_units.concat(new(pused_unit,init(hp,true)));
           unitsym:=new(punitsym,init('OBJPAS',hp^.globalsymtable));
           inc(unitsym^.refs);
           refsymtable^.insert(unitsym);
         end;
      { Profile unit? Needed for go32v2 only }
        if (cs_profile in aktmoduleswitches) and (target_info.target=target_i386_go32v2) then
         begin
           hp:=loadunit('PROFILE',false);
           psymtable(hp^.globalsymtable)^.next:=symtablestack;
           symtablestack:=hp^.globalsymtable;
           { add to the used units }
           current_module^.used_units.concat(new(pused_unit,init(hp,true)));
           unitsym:=new(punitsym,init('PROFILE',hp^.globalsymtable));
           inc(unitsym^.refs);
           refsymtable^.insert(unitsym);
         end;
      { Units only required for main module }
        if not(current_module^.is_unit) then
         begin
           { Heaptrc unit }
           if (cs_gdb_heaptrc in aktglobalswitches) then
            begin
              hp:=loadunit('HEAPTRC',false);
              psymtable(hp^.globalsymtable)^.next:=symtablestack;
              symtablestack:=hp^.globalsymtable;
              { add to the used units }
              current_module^.used_units.concat(new(pused_unit,init(hp,true)));
              unitsym:=new(punitsym,init('HEAPTRC',hp^.globalsymtable));
              inc(unitsym^.refs);
              refsymtable^.insert(unitsym);
            end;
           { Lineinfo unit }
           if (cs_gdb_lineinfo in aktglobalswitches) then
            begin
              hp:=loadunit('LINEINFO',false);
              psymtable(hp^.globalsymtable)^.next:=symtablestack;
              symtablestack:=hp^.globalsymtable;
              { add to the used units }
              current_module^.used_units.concat(new(pused_unit,init(hp,true)));
              unitsym:=new(punitsym,init('LINEINFO',hp^.globalsymtable));
              inc(unitsym^.refs);
              refsymtable^.insert(unitsym);
            end;
         end;
      { save default symtablestack }
        defaultsymtablestack:=symtablestack;
      end;


    procedure loadunits;
      var
         s : stringid;
         pu,
         hp : pused_unit;
         hp2 : pmodule;
         hp3 : psymtable;
         oldprocsym:Pprocsym;
         unitsym : punitsym;
      begin
         oldprocsym:=aktprocsym;
         consume(_USES);
{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         repeat
           s:=pattern;
           consume(_ID);
         { Give a warning if objpas is loaded }
           if s='OBJPAS' then
            Message(parser_w_no_objpas_use_mode);
         { check if the unit is already used }
           pu:=pused_unit(current_module^.used_units.first);
           while assigned(pu) do
            begin
              if (pu^.name^=s) then
               break;
              pu:=pused_unit(pu^.next);
            end;
         { avoid uses of itself }
           if not assigned(pu) and (s<>current_module^.modulename^) then
            begin
            { load the unit }
              hp2:=loadunit(s,false);
            { the current module uses the unit hp2 }
              current_module^.used_units.concat(new(pused_unit,init(hp2,not current_module^.in_implementation)));
              pused_unit(current_module^.used_units.last)^.in_uses:=true;
              if current_module^.compiled then
                exit;
              unitsym:=new(punitsym,init(s,hp2^.globalsymtable));
              { never claim about unused unit if
                there is init or finalize code  PM }
              if (hp2^.flags and (uf_init or uf_finalize))<>0 then
                inc(unitsym^.refs);
              refsymtable^.insert(unitsym);
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
         hp:=pused_unit(current_module^.used_units.first);
         while assigned(hp) do
           begin
{$IfDef GDB}
              if (cs_debuginfo in aktmoduleswitches) and
                 (cs_gdb_dbx in aktglobalswitches) and
                not hp^.is_stab_written then
                begin
                   punitsymtable(hp^.u^.globalsymtable)^.concattypestabto(debuglist);
                   hp^.is_stab_written:=true;
                   hp^.unitid:=psymtable(hp^.u^.globalsymtable)^.unitid;
                end;
{$EndIf GDB}
              if hp^.in_uses then
                begin
                   hp3:=symtablestack;
                   while assigned(hp3) do
                     begin
                        { insert units only once ! }
                        if hp^.u^.globalsymtable=hp3 then
                          break;
                        hp3:=hp3^.next;
                        { unit isn't inserted }
                        if hp3=nil then
                          begin
                             psymtable(hp^.u^.globalsymtable)^.next:=symtablestack;
                             symtablestack:=psymtable(hp^.u^.globalsymtable);
{$ifdef CHAINPROCSYMS}
                             symtablestack^.chainprocsyms;
{$endif CHAINPROCSYMS}
{$ifdef DEBUG}
                             test_symtablestack;
{$endif DEBUG}
                          end;
                     end;
                end;
              hp:=pused_unit(hp^.next);
           end;
          aktprocsym:=oldprocsym;
      end;


     procedure write_gdb_info;
{$IfDef GDB}
       var
         hp : pused_unit;
       begin
         if not (cs_debuginfo in aktmoduleswitches) then
          exit;
         if (cs_gdb_dbx in aktglobalswitches) then
           begin
             debuglist^.concat(new(pai_asm_comment,init(strpnew('EINCL of global '+
               punitsymtable(current_module^.globalsymtable)^.name^+' has index '+
               tostr(punitsymtable(current_module^.globalsymtable)^.unitid)))));
             debuglist^.concat(new(pai_stabs,init(strpnew('"'+
               punitsymtable(current_module^.globalsymtable)^.name^+'",'+
               tostr(N_EINCL)+',0,0,0'))));
             punitsymtable(current_module^.globalsymtable)^.dbx_count_ok:={true}false;
             dbx_counter:=punitsymtable(current_module^.globalsymtable)^.prev_dbx_counter;
             do_count_dbx:=false;
           end;

         { now insert the units in the symtablestack }
         hp:=pused_unit(current_module^.used_units.first);
         while assigned(hp) do
           begin
              if (cs_debuginfo in aktmoduleswitches) and
                not hp^.is_stab_written then
                begin
                   punitsymtable(hp^.u^.globalsymtable)^.concattypestabto(debuglist);
                   hp^.is_stab_written:=true;
                   hp^.unitid:=psymtable(hp^.u^.globalsymtable)^.unitid;
                end;
              hp:=pused_unit(hp^.next);
           end;
         if current_module^.in_implementation then
           begin
              if assigned(current_module^.localsymtable) then
                begin
                   { all types }
                   punitsymtable(current_module^.localsymtable)^.concattypestabto(debuglist);
                   { and all local symbols}
                   punitsymtable(current_module^.localsymtable)^.concatstabto(debuglist);
                end;
           end
         else
           begin
              if assigned(current_module^.globalsymtable) then
                begin
                   { all types }
                   punitsymtable(current_module^.globalsymtable)^.concattypestabto(debuglist);
                   { and all local symbols}
                   punitsymtable(current_module^.globalsymtable)^.concatstabto(debuglist);
                end;
           end;
        end;
{$Else GDB}
       begin
       end;
{$EndIf GDB}


    procedure parse_implementation_uses(symt:Psymtable);
      begin
         if token=_USES then
           begin
              symt^.symtabletype:=unitsymtable;
              loadunits;
              symt^.symtabletype:=globalsymtable;
{$ifdef DEBUG}
              test_symtablestack;
{$endif DEBUG}
           end;
      end;


    procedure setupglobalswitches;

        procedure def_symbol(const s:string);
        var
          mac : pmacrosym;
        begin
          mac:=new(pmacrosym,init(s));
          mac^.defined:=true;
          Message1(parser_m_macro_defined,mac^.name);
          macros^.insert(mac);
        end;

      begin
        { can't have local browser when no global browser }
        if (cs_local_browser in aktmoduleswitches) and
           not(cs_browser in aktmoduleswitches) then
          aktmoduleswitches:=aktmoduleswitches-[cs_local_browser];

        { define a symbol in delphi,objfpc,tp,gpc mode }
        if (m_delphi in aktmodeswitches) then
         def_symbol('FPC_DELPHI')
        else
         if (m_tp in aktmodeswitches) then
          def_symbol('FPC_TP')
        else
         if (m_objfpc in aktmodeswitches) then
          def_symbol('FPC_OBJFPC')
        else
         if (m_gpc in aktmodeswitches) then
          def_symbol('FPC_GPC');

        { turn ansistrings on by default ? }
        if (m_default_ansistring in aktmodeswitches) then
          aktlocalswitches:=aktlocalswitches+[cs_ansistrings];
      end;


    procedure gen_main_procsym(const name:string;options:tproctypeoption;st:psymtable);
      var
        stt : psymtable;
      begin
        {Generate a procsym for main}
        make_ref:=false;
        aktprocsym:=new(Pprocsym,init(name));
        { main are allways used }
        inc(aktprocsym^.refs);
        {Try to insert in in static symtable ! }
        stt:=symtablestack;
        symtablestack:=st;
        aktprocsym^.definition:=new(Pprocdef,init);
        symtablestack:=stt;
        aktprocsym^.definition^.proctypeoption:=options;
        aktprocsym^.definition^.setmangledname(target_os.cprefix+name);
        aktprocsym^.definition^.forwarddef:=false;
        make_ref:=true;
        { The localst is a local symtable. Change it into the static
          symtable }
        dispose(aktprocsym^.definition^.localst,done);
        aktprocsym^.definition^.localst:=st;
        { and insert the procsym in symtable }
        st^.insert(aktprocsym);
        { set some informations about the main program }
        with procinfo^ do
         begin
           returntype.setdef(voiddef);
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
          codesegment^.empty and
          datasegment^.empty and
          bsssegment^.empty and
          ((importssection=nil) or importssection^.empty) and
          ((resourcesection=nil) or resourcesection^.empty) and
          ((resourcestringlist=nil) or resourcestringlist^.empty)
        );
      end;

      var
{$ifdef fixLeaksOnError}
         names  : Pstringcontainer;
{$else fixLeaksOnError}
         names  : Tstringcontainer;
{$endif fixLeaksOnError}
         st     : psymtable;
         unitst : punitsymtable;
{$ifdef GDB}
         pu     : pused_unit;
{$endif GDB}
{$ifndef Dont_use_double_checksum}
        store_crc,store_interface_crc : longint;
{$endif}
         s1,s2  : ^string; {Saves stack space}
         force_init_final : boolean;

      begin
         consume(_UNIT);
         if Compile_Level=1 then
           IsExe:=false;

         if token=_ID then
          begin
          { create filenames and unit name }
             current_module^.SetFileName(current_scanner^.inputfile^.path^+current_scanner^.inputfile^.name^,true);
             stringdispose(current_module^.modulename);
             current_module^.modulename:=stringdup(upper(pattern));
          { check for system unit }
             new(s1);
             new(s2);
             s1^:=upper(target_info.system_unit);
             s2^:=upper(SplitName(current_scanner^.inputfile^.name^));
             if (cs_compilesystem in aktmoduleswitches) then
              begin
                if ((length(current_module^.modulename^)>8) or
                   (current_module^.modulename^<>s1^) or
                   (current_module^.modulename^<>s2^)) then
                  Message1(unit_e_illegal_unit_name,current_module^.modulename^);
              end
             else
              begin
                if (cs_check_unit_name in aktglobalswitches) and
                   not((current_module^.modulename^=s2^) or
                       ((length(current_module^.modulename^)>8) and
                        (copy(current_module^.modulename^,1,8)=s2^))) then
                 Message1(unit_e_illegal_unit_name,current_module^.modulename^);
                if (current_module^.modulename^=s1^) then
                 Message(unit_w_switch_us_missed);
              end;
             dispose(s2);
             dispose(s1);
          end;

         consume(_ID);
         consume(_SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module^.in_global:=false;

         { handle the global switches }
         setupglobalswitches;

         Message1(unit_u_start_parse_interface,current_module^.modulename^);

         { update status }
         status.currentmodule:=current_module^.modulename^;

         { maybe turn off m_objpas if we are compiling objpas }
         if (current_module^.modulename^='OBJPAS') then
           aktmodeswitches:=aktmodeswitches-[m_objpas];

         { this should be placed after uses !!}
{$ifndef UseNiceNames}
         procprefix:='_'+current_module^.modulename^+'$$';
{$else UseNiceNames}
         procprefix:='_'+tostr(length(current_module^.modulename^))+lowercase(current_module^.modulename^)+'_';
{$endif UseNiceNames}

         parse_only:=true;

         { generate now the global symboltable }
         st:=new(punitsymtable,init(globalsymtable,current_module^.modulename^));
         refsymtable:=st;
         unitst:=punitsymtable(st);
         { define first as local to overcome dependency conflicts }
         current_module^.localsymtable:=st;

         { the unit name must be usable as a unit specifier }
         { inside the unit itself (PM)                }
         { this also forbids to have another symbol      }
         { with the same name as the unit                  }
         refsymtable^.insert(new(punitsym,init(current_module^.modulename^,unitst)));

         { a unit compiled at command line must be inside the loaded_unit list }
         if (compile_level=1) then
           loaded_units.insert(current_module);

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
                   unitst^.symtabletype:=unitsymtable;
                   loadunits;
                   { has it been compiled at a higher level ?}
                   if current_module^.compiled then
                     begin
                        { this unit symtable is obsolete }
                        { dispose(unitst,done);
                        disposed as localsymtable !! }
                        RestoreUnitSyms;
                        exit;
                     end;
                   unitst^.symtabletype:=globalsymtable;
                end;
              { ... but insert the symbol table later }
              st^.next:=symtablestack;
              symtablestack:=st;
           end
         else
         { while compiling a system unit, some types are directly inserted }
           begin
              st^.next:=symtablestack;
              symtablestack:=st;
              insert_intern_types(st);
           end;

         { now we know the place to insert the constants }
         constsymtable:=symtablestack;

         { move the global symtab from the temporary local to global }
         current_module^.globalsymtable:=current_module^.localsymtable;
         current_module^.localsymtable:=nil;

         reset_global_defs;

         { number all units, so we know if a unit is used by this unit or
           needs to be added implicitly }
         numberunits;

         { ... parse the declarations }
         Message1(parser_u_parsing_interface,current_module^.modulename^);
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

  {$ifndef Dont_use_double_checksum}
         if not(cs_compilesystem in aktmoduleswitches) then
           if (Errorcount=0) then
             writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack),true);
  {$endif Test_Double_checksum}

         { Parse the implementation section }
         consume(_IMPLEMENTATION);
         current_module^.in_implementation:=true;
         Message1(unit_u_start_parse_implementation,current_module^.modulename^);

         parse_only:=false;

         { generates static symbol table }
         st:=new(punitsymtable,init(staticsymtable,current_module^.modulename^));
         current_module^.localsymtable:=st;

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst^.next;

         { we don't want implementation units symbols in unitsymtable !! PM }
         refsymtable:=st;

         { Read the implementation units }
         parse_implementation_uses(unitst);

         if current_module^.compiled then
           begin
              RestoreUnitSyms;
              exit;
           end;

         { reset ranges/stabs in exported definitions }
         reset_global_defs;

         { All units are read, now give them a number }
         numberunits;

         { now we can change refsymtable }
         refsymtable:=st;

         { but reinsert the global symtable as lasts }
         unitst^.next:=symtablestack;
         symtablestack:=unitst;

{$ifndef DONOTCHAINOPERATORS}
          symtablestack^.chainoperators;
{$endif DONOTCHAINOPERATORS}

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

         Message1(parser_u_parsing_implementation,current_module^.modulename^);

         { Compile the unit }
         codegen_newprocedure;
         gen_main_procsym(current_module^.modulename^+'_init',potype_unitinit,st);
{$ifdef fixLeaksOnError}
         new(names,init);
         strContStack.push(names);
         names^.insert('INIT$$'+current_module^.modulename^);
         names^.insert(target_os.cprefix+current_module^.modulename^+'_init');
         compile_proc_body(names^,true,false);
         if names <> PstringContainer(strContStack.pop) then
           writeln('Problem with strContStack in pmodules (1)');
         dispose(names,done);
{$else fixLeaksOnError}
         names.init;
         names.insert('INIT$$'+current_module^.modulename^);
         names.insert(target_os.cprefix+current_module^.modulename^+'_init');
         compile_proc_body(names,true,false);
         names.done;
{$endif fixLeaksOnError}
         codegen_doneprocedure;

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;

         { if the unit contains ansi/widestrings, initialization and
           finalization code must be forced }
         force_init_final:=needs_init_final(current_module^.globalsymtable)
           or needs_init_final(current_module^.localsymtable);

         { should we force unit initialization? }
         { this is a hack, but how can it be done better ? }
         if force_init_final and ((current_module^.flags and uf_init)=0) then
           begin
              current_module^.flags:=current_module^.flags or uf_init;
              { now we can insert a cut }
              if (cs_create_smart in aktmoduleswitches) then
                codesegment^.concat(new(pai_cut,init));
              genimplicitunitinit(codesegment);
           end;
         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module^.flags:=current_module^.flags or uf_finalize;

              { Compile the finalize }
              codegen_newprocedure;
              gen_main_procsym(current_module^.modulename^+'_finalize',potype_unitfinalize,st);
{$ifdef fixLeaksOnError}
              new(names,init);
              strContStack.push(names);
              names^.insert('FINALIZE$$'+current_module^.modulename^);
              names^.insert(target_os.cprefix+current_module^.modulename^+'_finalize');
              compile_proc_body(names^,true,false);
              if names <> PstringContainer(strContStack.pop) then
                writeln('Problem with strContStack in pmodules (2)');
              dispose(names,done);
{$else fixLeaksOnError}
              names.init;
              names.insert('FINALIZE$$'+current_module^.modulename^);
              names.insert(target_os.cprefix+current_module^.modulename^+'_finalize');
              compile_proc_body(names,true,false);
              names.done;
{$endif fixLeaksOnError}
              codegen_doneprocedure;
           end
         else if force_init_final then
           begin
              current_module^.flags:=current_module^.flags or uf_finalize;
              { now we can insert a cut }
              if (cs_create_smart in aktmoduleswitches) then
                codesegment^.concat(new(pai_cut,init));
              genimplicitunitfinal(codesegment);
           end;

         { the last char should always be a point }
         consume(_POINT);

         If ResourceStrings^.ResStrCount>0 then
          begin
            ResourceStrings^.CreateResourceStringList;
            current_module^.flags:=current_module^.flags or uf_has_resources;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings^.WriteResourceFile(Current_module^.ModuleName^);
          end;

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;
         { absence does not matter here !! }
         aktprocsym^.definition^.forwarddef:=false;
         { test static symtable }
         if (Errorcount=0) then
           begin
             st^.allsymbolsused;
             st^.allunitsused;
             st^.allprivatesused;
           end;

         { size of the static data }
         datasize:=st^.datasize;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
{$IfnDef New_GDB}
            if assigned(current_module^.globalsymtable) then
              begin
                 { all types }
                 punitsymtable(current_module^.globalsymtable)^.concattypestabto(debuglist);
                 { and all local symbols}
                 punitsymtable(current_module^.globalsymtable)^.concatstabto(debuglist);
              end;
            { all local types }
            punitsymtable(st)^.concattypestabto(debuglist);
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
             symtablestack^.check_forwards;
             symtablestack^.allprivatesused;
           end;

         { now we have a correct unit, change the symtable type }
         current_module^.in_implementation:=false;
         symtablestack^.symtabletype:=unitsymtable;
{$ifdef GDB}
         punitsymtable(symtablestack)^.is_stab_written:=false;
{$endif GDB}

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            closecurrentppu;
            exit;
          end;

         { generate imports }
         if current_module^.uses_imports then
           importlib^.generatelib;

         { insert own objectfile, or say that it's in a library
           (no check for an .o when loading) }
         if is_assembler_generated then
           insertobjectfile
         else
           current_module^.flags:=current_module^.flags or uf_no_link;

         if cs_local_browser in aktmoduleswitches then
           current_module^.localsymtable:=refsymtable;
         { Write out the ppufile }
  {$ifndef Dont_use_double_checksum}
        store_interface_crc:=current_module^.interface_crc;
        store_crc:=current_module^.crc;
  {$endif Test_Double_checksum}
         if (Errorcount=0) then
           writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack),false);

  {$ifndef Dont_use_double_checksum}
         if not(cs_compilesystem in aktmoduleswitches) then
           if store_interface_crc<>current_module^.interface_crc then
             Do_comment(V_Warning,current_module^.ppufilename^+' Interface CRC changed '+
               tostr(store_crc)+'<>'+tostr(current_module^.interface_crc));
  {$ifdef EXTDEBUG}
         if not(cs_compilesystem in aktmoduleswitches) then
           if (store_crc<>current_module^.crc) and simplify_ppu then
             Do_comment(V_Warning,current_module^.ppufilename^+' implementation CRC changed '+
               tostr(store_crc)+'<>'+tostr(current_module^.interface_crc));
  {$endif EXTDEBUG}
  {$endif ndef Dont_use_Double_checksum}
          { must be done only after local symtable ref stores !! }
          closecurrentppu;
{$ifdef GDB}
         pu:=pused_unit(usedunits.first);
         while assigned(pu) do
           begin
              if assigned(pu^.u^.globalsymtable) then
                punitsymtable(pu^.u^.globalsymtable)^.is_stab_written:=false;
              pu:=pused_unit(pu^.next);
           end;
{$endif GDB}

         { remove static symtable (=refsymtable) here to save some mem }
         if not (cs_local_browser in aktmoduleswitches) then
           begin
              dispose(st,done);
              current_module^.localsymtable:=nil;
           end;


         RestoreUnitSyms;

         if is_assembler_generated then
          begin
          { finish asmlist by adding segment starts }
            insertsegment;
          { assemble }
            create_objectfile;
          end;

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
         st    : psymtable;
         hp    : pmodule;
{$ifdef fixLeaksOnError}
         names : Pstringcontainer;
{$else fixLeaksOnError}
         names : Tstringcontainer;
{$endif fixLeaksOnError}
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
              aktglobalswitches:=aktglobalswitches+[cs_link_strip];
              { Warning stabs info does not work with reloc section !! }
              if cs_debuginfo in aktmoduleswitches then
                begin
                  Message1(parser_w_parser_reloc_no_debug,current_module^.mainsource^);
                  Message(parser_w_parser_win32_debug_needs_WN);
                  aktmoduleswitches:=aktmoduleswitches-[cs_debuginfo];
                end;
           end;

         { get correct output names }
         current_module^.SetFileName(current_scanner^.inputfile^.path^+current_scanner^.inputfile^.name^,true);

         if islibrary then
           begin
              consume(_LIBRARY);
              stringdispose(current_module^.modulename);
              current_module^.modulename:=stringdup(pattern);
              current_module^.islibrary:=true;
              exportlib^.preparelib(pattern);
              consume(_ID);
              consume(_SEMICOLON);
           end
         else
           { is there an program head ? }
           if token=_PROGRAM then
            begin
              consume(_PROGRAM);
              stringdispose(current_module^.modulename);
              current_module^.modulename:=stringdup(pattern);
              if (target_info.target=target_i386_WIN32) then
                exportlib^.preparelib(pattern);
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
           exportlib^.preparelib(current_module^.modulename^);

         { global switches are read, so further changes aren't allowed }
         current_module^.in_global:=false;

         { setup things using the global switches }
         setupglobalswitches;

         { set implementation flag }
         current_module^.in_implementation:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                             }
         st:=new(punitsymtable,init(staticsymtable,current_module^.modulename^));
         current_module^.localsymtable:=st;
         symtablestack:=st;
         refsymtable:=st;

         { necessary for browser }
         loaded_units.insert(current_module);

         { load standard units (system,objpas,profile unit) }
         loaddefaultunits;

         { reset }
         lexlevel:=0;

         {Load the units used by the program we compile.}
         if token=_USES then
           loadunits;

{$ifndef DONOTCHAINOPERATORS}
          symtablestack^.chainoperators;
{$endif DONOTCHAINOPERATORS}

         { reset ranges/stabs in exported definitions }
         reset_global_defs;

         { All units are read, now give them a number }
         numberunits;

         {Insert the name of the main program into the symbol table.}
         if current_module^.modulename^<>'' then
           {st^.insert(new(pprogramsym,init(current_module^.modulename^)));}
           st^.insert(new(punitsym,init(current_module^.modulename^,punitsymtable(st))));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         Message1(parser_u_parsing_implementation,current_module^.mainsource^);

         { reset }
         procprefix:='';

         {The program intialization needs an alias, so it can be called
          from the bootstrap code.}
         codegen_newprocedure;
         gen_main_procsym('main',potype_proginit,st);
{$ifdef fixLeaksOnError}
         new(names,init);
         strContStack.push(names);
         names^.insert('program_init');
         names^.insert('PASCALMAIN');
         names^.insert(target_os.cprefix+'main');
 {$ifdef m68k}
         if target_info.target=target_m68k_PalmOS then
           names^.insert('PilotMain');
 {$endif m68k}
         compile_proc_body(names^,true,false);
         if names <> PstringContainer(strContStack.pop) then
           writeln('Problem with strContStack in pmodules (1)');
         dispose(names,done);
{$else fixLeaksOnError}
         names.init;
         names.insert('program_init');
         names.insert('PASCALMAIN');
         names.insert(target_os.cprefix+'main');
 {$ifdef m68k}
         if target_info.target=target_m68k_PalmOS then
           names.insert('PilotMain');
 {$endif m68k}
         compile_proc_body(names,true,false);
         names.done;
{$endif fixLeaksOnError}

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;

         { consider these symbols as global ones }
         { for browser }
         current_module^.globalsymtable:=current_module^.localsymtable;
         current_module^.localsymtable:=nil;

         If ResourceStrings^.ResStrCount>0 then
          begin
            ResourceStrings^.CreateResourceStringList;
            { only write if no errors found }
            if (Errorcount=0) then
             ResourceStrings^.WriteResourceFile(Current_module^.ModuleName^);
          end;

         codegen_doneprocedure;

         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module^.flags:=current_module^.flags or uf_finalize;

              { Compile the finalize }
              codegen_newprocedure;
              gen_main_procsym(current_module^.modulename^+'_finalize',potype_unitfinalize,st);
{$ifdef fixLeaksOnError}
              new(names,init);
              strContStack.push(names);
              names^.insert('FINALIZE$$'+current_module^.modulename^);
              names^.insert(target_os.cprefix+current_module^.modulename^+'_finalize');
              compile_proc_body(names^,true,false);
              if names <> PstringContainer(strContStack.pop) then
                writeln('Problem with strContStack in pmodules (1)');
              dispose(names,done);
{$else fixLeaksOnError}
              names.init;
              names.insert('FINALIZE$$'+current_module^.modulename^);
              names.insert(target_os.cprefix+current_module^.modulename^+'_finalize');
              compile_proc_body(names,true,false);
              names.done;
{$endif fixLeaksOnError}
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
             st^.allsymbolsused;
             st^.allunitsused;
             st^.allprivatesused;
           end;

         { generate imports }
         if current_module^.uses_imports then
          importlib^.generatelib;

         if islibrary or
            (target_info.target=target_i386_WIN32) then
           exportlib^.generatelib;


         { insert heap }
         insertResourceTablesTable;
         insertinitfinaltable;
         insertheap;
         inserttargetspecific;

         datasize:=symtablestack^.datasize;

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
            hp:=pmodule(loaded_units.first);
            while assigned(hp) do
             begin
               Linker^.AddModuleFiles(hp);
               hp:=pmodule(hp^.next);
             end;
            { write .def file }
            if (cs_link_deffile in aktglobalswitches) then
             deffile.writefile;
            { finally we can create a executable }
            if (not current_module^.is_unit) then
             begin
               if DLLSource then
                Linker^.MakeSharedLibrary
               else
                Linker^.MakeExecutable;
             end;
          end;
      end;

end.
{
  $Log$
  Revision 1.3  2000-07-13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:45  michael
  + removed logs

}
