{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

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

{$define STORENUMBER}

{ define TEST_IMPL does not work well }
{ replaced by $define  Double_checksum}
{ other way to get correct type info, in test (PM) }

{$define New_GDB}

  interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


  implementation

    uses
       globtype,version,systems,tokens,
       cobjects,comphook,globals,verbose,files,
       symtable,aasm,hcodegen,
       link,assemble,import,export,gendef,ppu,comprsrc
{$ifdef i386}
{$ifdef Ag386Bin}
       ,i386base,i386asm
{$else}
       ,i386
{$endif}
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ,scanner,pbase,psystem,pdecl,psub,parser;


    procedure create_objectfile;
      begin
        { create the .s file and assemble it }
        GenerateAsm;

        { resource files }
        CompileResourceFiles;

        { When creating a library call the linker. And insert the output
          of the linker files }
        if (cs_create_sharedlib in aktmoduleswitches) then
          Linker.MakeSharedLibrary
        else
          if (cs_create_staticlib in aktmoduleswitches)
{$ifndef AG386BIN}
             or (cs_smartlink in aktmoduleswitches)
{$endif}
             then
            Linker.MakeStaticLibrary(SmartLinkFilesCnt);
      end;


    procedure insertobjectfile;
    { Insert the used object file for this unit in the used list for this unit }
      begin
        if (cs_create_sharedlib in aktmoduleswitches) then
          begin
            current_module^.linksharedlibs.insert(current_module^.sharedlibfilename^);
            current_module^.flags:=current_module^.flags or uf_shared_linked;
          end
        else
         begin
           if (cs_create_staticlib in aktmoduleswitches) or
              (cs_smartlink in aktmoduleswitches) then
             begin
               current_module^.linkstaticlibs.insert(current_module^.staticlibfilename^);
               current_module^.flags:=current_module^.flags or uf_static_linked;
             end
           else
             begin
               current_module^.linkunitfiles.insert(current_module^.objfilename^);
               current_module^.flags:=current_module^.flags or uf_obj_linked;
             end;
         end;
      end;


    procedure insertsegment;

        procedure fixseg(p:paasmoutput;sec:tsection);
        begin
          p^.insert(new(pai_section,init(sec)));
          if (cs_smartlink in aktmoduleswitches) then
           p^.insert(new(pai_cut,init));
          p^.concat(new(pai_section,init(sec_none)));
        end;

      begin
      {Insert Ident of the compiler}
        if (not (cs_smartlink in aktmoduleswitches))
{$ifndef EXTDEBUG}
           and (not current_module^.is_unit)
{$endif}
           then
         begin
           datasegment^.insert(new(pai_align,init(4)));
           datasegment^.insert(new(pai_string,init('FPC '+version_string+
             ' for '+target_cpu_string+' - '+target_info.short_name)));
         end;
      { Insert start and end of sections }
        fixseg(codesegment,sec_code);
        fixseg(datasegment,sec_data);
        fixseg(bsssegment,sec_bss);
      { we should use .rdata section for these two no ? }
      { .rdata is a read only data section (PM) }
        fixseg(rttilist,sec_data);
        fixseg(consts,sec_data);
{$ifdef GDB}
        if assigned(debuglist) then
          fixseg(debuglist,sec_code);
{$endif GDB}
      end;


    procedure insertheap;
      begin
         if (cs_smartlink in aktmoduleswitches) then
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
{$ifdef m68k}
            target_m68k_Mac:
              bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)));
            target_m68k_PalmOS:
              ;
{$endif m68k}
         else
           bsssegment^.concat(new(pai_datablock,init_global('HEAP',heapsize)));
         end;
{$ifdef i386}
         datasegment^.concat(new(pai_symbol,init_global('HEAPSIZE')));
         datasegment^.concat(new(pai_const,init_32bit(heapsize)));
{$endif i386}
{$ifdef m68k}
         if target_info.target<>target_m68k_PalmOS then
           begin
              datasegment^.concat(new(pai_symbol,init_global('HEAP_SIZE')));
              datasegment^.concat(new(pai_const,init_32bit(heapsize)));
           end;
{$endif m68k}
      end;


    procedure inserttargetspecific;
      begin
        case target_info.target of
{$ifdef i386}
          target_i386_GO32V2 :
            begin
              { stacksize can be specified }
              datasegment^.concat(new(pai_symbol,init_global('__stklen')));
              datasegment^.concat(new(pai_const,init_32bit(stacksize)));
            end;
          target_i386_WIN32 :
            begin
              { Generate an external entry to be sure that _mainCRTStarup will be
                linked, can't use concat_external because those aren't written for
                asw (PFV) }
             target_link.bindcmd[1]:=target_link.bindcmd[1]+' -d '+deffile.fname;
             if DLLsource then
              target_link.binders:=2;
             if RelocSection then
              begin
               target_link.linkcmd:=target_link.linkcmd+' --base-file base.$$$';
               target_link.bindcmd[1]:=target_link.bindcmd[1]+' --base-file base.$$$';
               target_link.binders:=2;
              end;
             if apptype=at_gui then
              begin
               target_link.linkcmd:='--subsystem windows '+target_link.linkcmd;
               target_link.bindcmd[2]:='--subsystem windows '+target_link.bindcmd[2];
              end;
             if DLLsource then
              begin
               target_link.linkcmd:='--dll '+target_link.linkcmd;
               target_link.bindcmd[2]:='--dll '+target_link.bindcmd[2];
              end;
            end;
{$endif i386}
{$ifdef m68k}
          target_m68k_Atari :
            begin
              { stacksize can be specified }
              datasegment^.concat(new(pai_symbol,init_global('__stklen')));
              datasegment^.concat(new(pai_const,init_32bit(stacksize)));
            end;
{$endif m68k}
        end;
      end;


    function loadunit(const s : string;compile_system:boolean) : pmodule;forward;


    procedure load_usedunits(compile_system:boolean);
      var
        pu           : pused_unit;
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
                 current_module^.do_compile:=true;
                 { if the checksum was known but has changed then
                   we should also recompile the loaded unit ! }
                 if (pu^.checksum<>0) and (loaded_unit^.sources_avail) then
                   begin
                      Message2(unit_u_recompile_crc_change,loaded_unit^.modulename^,current_module^.modulename^);
                      loaded_unit^.do_compile:=true;
                   end;
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
      { if this is the system unit insert the intern symbols }
        if compile_system then
         begin
           make_ref:=false;
           insertinternsyms(psymtable(current_module^.globalsymtable));
           make_ref:=true;
         end;
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
{$ifdef Double_checksum}
            { need to recompile the current unit ? }
              if (loaded_unit^.interface_crc<>pu^.interface_checksum) then
              { checksum change whereas it was already known
                loade_unit was changed so we need to recompile this unit }
                begin
                  {if (loaded_unit^.sources_avail) then
                   begin
                      loaded_unit^.do_compile:=true;
                   end;          }
                  Message2(unit_u_recompile_crc_change,loaded_unit^.modulename^,current_module^.modulename^);
                  loaded_unit^.do_compile:=true;
                  if(pu^.interface_checksum<>0) then
                    load_refs:=false;
                 end;
{$endif def Double_checksum}
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
        name : string;{ necessary because
        current_module^.mainsource^ is reset in compile !! }
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
              end;
           { recompile the unit or give a fatal error if sources not available }
             if not(current_module^.sources_avail) then
               if (not current_module^.search_unit(current_module^.modulename^,true))
                  and (length(current_module^.modulename^)>8) then
                 current_module^.search_unit(copy(current_module^.modulename^,1,8),true);
             if not(current_module^.sources_avail) then
              Message1(unit_f_cant_compile_unit,current_module^.modulename^)
             else
              begin
                if current_module^.in_second_compile then
                  Message1(parser_d_compiling_second_time,current_module^.modulename^);
                current_scanner^.tempcloseinputfile;
                name:=current_module^.mainsource^;
                if assigned(scanner) then
                  scanner^.invalid:=true;
                compile(name,compile_system);
                if (not current_scanner^.invalid) then
                  current_scanner^.tempopeninputfile;
              end;
           end
          else
           begin
           { only reassemble ? }
             if (current_module^.do_assemble) then
              OnlyAsm;
           { add the files for the linker }
             Linker.AddModuleFiles(current_module);
           end;
         if assigned(current_module^.ppufile) then
           begin
              dispose(current_module^.ppufile,done);
              current_module^.ppufile:=nil;
           end;
        end;

      begin
         old_current_module:=current_module;
         old_current_ppu:=current_ppu;
         { Info }
         Message3(unit_u_load_unit,current_module^.modulename^,ImplIntf[current_module^.in_implementation],s);
         { unit not found }
         st:=nil;
         { search all loaded units }
         hp:=pmodule(loaded_units.first);
         while assigned(hp) do
           begin
              if hp^.modulename^=s then
                begin
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
{$ifdef Double_checksum}
            else if  hp^.do_reload_ppu then
                begin
                  { remove the old unit }
                  loaded_units.remove(hp);
                  scanner:=hp^.scanner;
                  name:=hp^.modulename^;
                  hp^.reset;
                  hp^.do_reload_ppu:=false;
                  hp^.scanner:=scanner;
                  { try to reopen ppu }
                  hp^.search_unit(name,false);
                  { try to load the unit a second time first }
                  current_module:=hp;
                  current_module^.in_second_compile:=true;
                { now realy load the ppu }
                  loadppufile;
                { set compiled flag }
                  current_module^.compiled:=true;
                end;
{$else Double_Checksum}
               ;
{$endif Double_checksum}
              { the next unit }
              hp:=pmodule(hp^.next);
           end;
       { the unit is not in the symtable stack }
         if (not assigned(st))
{$ifdef Double_checksum}
            or (assigned(hp) and hp^.do_reload_ppu)
{$endif Double_checksum}
            then
          begin
            if assigned(hp) then
             begin
               { remove the old unit }
               loaded_units.remove(hp);
               scanner:=hp^.scanner;
               hp^.reset;
{$ifdef Double_checksum}
               hp^.do_reload_ppu:=false;
{$endif Double_checksum}
               hp^.scanner:=scanner;
               { try to reopen ppu }
               hp^.search_unit(s,false);
               { try to load the unit a second time first }
               current_module:=hp;
               current_module^.in_second_compile:=true;
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
          { now we can register the unit }
            current_module^.loaded_from:=old_current_module;
            loaded_units.insert(current_module);
          { now realy load the ppu }
            loadppufile;
          { set compiled flag }
            current_module^.compiled:=true;
          { register the unit _once_ }
          { this is buggy PM }
            if not second_time then
              usedunits.concat(new(pused_unit,init(current_module,true)));
          { load return pointer }
            hp:=current_module;
          end;
         { set the old module }
         current_ppu:=old_current_ppu;
         current_module:=old_current_module;
         loadunit:=hp;
      end;


    procedure loaddefaultunits;
      var
        hp : pmodule;
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
        refsymtable^.insert(new(punitsym,init('SYSTEM',systemunit)));
        { read default constant definitions }
        make_ref:=false;
        readconstdefs;
        make_ref:=true;
        { if POWER is defined in the RTL then use it for starstar overloading }
        getsym('POWER',false);
        if assigned(srsym) and (srsym^.typ=procsym) and (overloaded_operators[STARSTAR]=nil) then
          overloaded_operators[STARSTAR]:=pprocsym(srsym);
      { Objpas unit? }
        if m_objpas in aktmodeswitches then
         begin
           hp:=loadunit('OBJPAS',false);
           objpasunit:=hp^.globalsymtable;
           { insert in stack }
           objpasunit^.next:=symtablestack;
           symtablestack:=objpasunit;
           { add to the used units }
           current_module^.used_units.concat(new(pused_unit,init(hp,true)));
           refsymtable^.insert(new(punitsym,init('OBJPAS',objpasunit)));
         end
        else
         objpasunit:=nil;
      { Profile unit? Needed for go32v2 only }
        if (cs_profile in aktmoduleswitches) and (target_info.target=target_i386_go32v2) then
         begin
           hp:=loadunit('PROFILE',false);
           psymtable(hp^.globalsymtable)^.next:=symtablestack;
           symtablestack:=hp^.globalsymtable;
           { add to the used units }
           current_module^.used_units.concat(new(pused_unit,init(hp,true)));
           refsymtable^.insert(new(punitsym,init('PROFILE',hp^.globalsymtable)));
         end;
      { Heaptrc unit? (not needed for units), this is here to be sure that it is really
        loaded as first unit }
        if (cs_gdb_heaptrc in aktglobalswitches) and not(current_module^.is_unit)then
         begin
           hp:=loadunit('HEAPTRC',false);
           psymtable(hp^.globalsymtable)^.next:=symtablestack;
           symtablestack:=hp^.globalsymtable;
           { add to the used units }
           current_module^.used_units.concat(new(pused_unit,init(hp,true)));
           refsymtable^.insert(new(punitsym,init('HEAPTRC',hp^.globalsymtable)));
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
      begin
         oldprocsym:=aktprocsym;
         consume(_USES);
{$ifdef DEBUG}
         test_symtablestack;
{$endif DEBUG}
         repeat
           s:=pattern;
           consume(ID);
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
              refsymtable^.insert(new(punitsym,init(s,hp2^.globalsymtable)));
            end
           else
            Message1(sym_e_duplicate_id,s);
           if token=COMMA then
            begin
              pattern:='';
              consume(COMMA);
            end
           else
            break;
         until false;
         consume(SEMICOLON);

         { set the symtable to systemunit so it gets reorderd correctly }
         symtablestack:=defaultsymtablestack;

         { now insert the units in the symtablestack }
         hp:=pused_unit(current_module^.used_units.first);
         while assigned(hp) do
           begin
{$IfDef GDB}
{$IfnDef New_GDB}
              if (cs_debuginfo in aktmoduleswitches) and
                not hp^.is_stab_written then
                begin
                   punitsymtable(hp^.u^.globalsymtable)^.concattypestabto(debuglist);
                   hp^.is_stab_written:=true;
                   hp^.unitid:=psymtable(hp^.u^.globalsymtable)^.unitid;
                end;
{$endIf nDef New_GDB}
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


    procedure gen_main_procsym(const name:string;options:longint;st:psymtable);
{$ifdef Double_checksum}
    var
      stt : psymtable;
{$endif Double_checksum}
      begin
        {Generate a procsym for main}
        make_ref:=false;
        aktprocsym:=new(Pprocsym,init(name));
{$ifdef Double_checksum}
        {Try to insert in in static symtable ! }
        stt:=symtablestack;
        symtablestack:=st;
{$endif Double_checksum}
        aktprocsym^.definition:=new(Pprocdef,init);
{$ifdef Double_checksum}
        symtablestack:=stt;
{$endif Double_checksum}
        aktprocsym^.definition^.options:=aktprocsym^.definition^.options or options;
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
        with procinfo do
         begin
           retdef:=voiddef;
           _class:=nil;
           call_offset:=8;
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
          ((resourcesection=nil) or resourcesection^.empty)
        );
      end;

      var
         names  : Tstringcontainer;
         st     : psymtable;
         unitst : punitsymtable;
{$ifdef GDB}
         pu     : pused_unit;
{$endif GDB}
{$ifdef Double_checksum}
        store_crc : longint;
{$endif def Double_checksum}
         s1,s2  : ^string; {Saves stack space}
      begin
         consume(_UNIT);
         if token=ID then
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

         consume(ID);
         consume(SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module^.in_global:=false;

         { can't have local browser when no global browser }
         if (cs_local_browser in aktmoduleswitches) and
            not(cs_browser in aktmoduleswitches) then
           aktmoduleswitches:=aktmoduleswitches-[cs_local_browser];

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
         { inside the unit itself (PM)                      }
         { this also forbids to have another symbol         }
         { with the same name as the unit                   }
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
         { ... parse the declarations }
         read_interface_declarations;

         { leave when we got an error }
         if (Errorcount>0) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(Errorcount));
            status.skip_error:=true;
            exit;
          end;

{$ifdef New_GDB}
         write_gdb_info;
{$endIf Def New_GDB}

{$ifdef Double_CheckSum}
{$ifdef Test_Double_checksum}
         if (Errorcount=0) then
           writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack),true);
{$endif Test_Double_checksum}
{$endif Double_CheckSum}

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

{$ifndef STORENUMBER}
         { number the definitions, so a deref from other units works }
         refsymtable^.number_defs;
         refsymtable^.number_symbols;
{$endif}

         { we don't want implementation units symbols in unitsymtable !! PM }
         refsymtable:=st;

         { Read the implementation units }
         parse_implementation_uses(unitst);

         if current_module^.compiled then
           begin
              exit;
           end;

         { reset ranges/stabs in exported definitions }
         { If I find who removed this line !!!!!!!
           I AM TIRED OF THIS !!!!!!!!!!!
           DONT TOUCH  WITHOUT ASKING ME Pierre Muller }

         reset_global_defs;
         { All units are read, now give them a number }
         numberunits;

         { now we can change refsymtable }
         refsymtable:=st;

         { but reinsert the global symtable as lasts }
         unitst^.next:=symtablestack;
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

         { Generate a procsym }
         gen_main_procsym(current_module^.modulename^+'_init',pounitinit,st);

         { Compile the unit }
         codegen_newprocedure;
         names.init;
         names.insert('INIT$$'+current_module^.modulename^);
         names.insert(target_os.cprefix+current_module^.modulename^+'_init');
         compile_proc_body(names,true,false);
         names.done;
         codegen_doneprocedure;

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;

         { finalize? }
         if token=_FINALIZATION then
           begin
              { set module options }
              current_module^.flags:=current_module^.flags or uf_finalize;

              { Generate a procsym }
              gen_main_procsym(current_module^.modulename^+'_finalize',pounitfinalize,st);

              { Compile the finalize }
              codegen_newprocedure;
              names.init;
              names.insert('FINALIZE$$'+current_module^.modulename^);
              names.insert(target_os.cprefix+current_module^.modulename^+'_finalize');
              compile_proc_body(names,true,false);
              names.done;
              codegen_doneprocedure;

              { avoid self recursive destructor call !! PM }
              aktprocsym^.definition^.localst:=nil;
           end;

         { the last char should always be a point }
         consume(POINT);

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;
         { absence does not matter here !! }
         aktprocsym^.definition^.forwarddef:=false;
         { test static symtable }
         if (Errorcount=0) then
           st^.allsymbolsused;

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
           symtablestack^.check_forwards;

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
            exit;
          end;

         { generate imports }
         if current_module^.uses_imports then
           importlib^.generatelib;

         { insert own objectfile, or say that it's in a library
           (no check for an .o when loading) }
         if is_assembler_generated then
           insertobjectfile;

         if cs_local_browser in aktmoduleswitches then
           current_module^.localsymtable:=refsymtable;
         { Write out the ppufile }
{$ifdef Double_checksum}
        store_crc:=current_module^.interface_crc;
{$endif def Double_checksum}
         if (Errorcount=0) then
           writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack),false);

{$ifdef Double_checksum}
{$ifdef Test_Double_checksum}
        if store_crc<>current_module^.interface_crc then
          Def_comment(V_Warning,current_module^.ppufilename^+' CRC changed '+
           tostr(store_crc)+'<>'+tostr(current_module^.interface_crc));
{$endif def TestDouble_checksum}
{$endif def Double_checksum}
          { must be done only after local symtable ref stores !! }
          closecurrentppu;
{$ifdef GDB}
         pu:=pused_unit(usedunits.first);
         while assigned(pu) do
           begin
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

         if is_assembler_generated then
          begin
          { finish asmlist by adding segment starts }
            insertsegment;
          { assemble }
            create_objectfile;
          end;

         { add the files for the linker from current_module }
         Linker.AddModuleFiles(current_module);
      end;


    procedure proc_program(islibrary : boolean);
      var
         st    : psymtable;
         names : Tstringcontainer;
      begin
{* Changes made by Ozerski 23.10.1998}
         DLLsource:=islibrary;
{* End changes}
         parse_only:=false;
         if islibrary then
           begin
              consume(_LIBRARY);
              stringdispose(current_module^.modulename);
              current_module^.modulename:=stringdup(pattern);
              current_module^.islibrary:=true;
              exportlib^.preparelib(pattern);
              consume(ID);
              consume(SEMICOLON);
           end
         else
           { is there an program head ? }
           if token=_PROGRAM then
            begin
              consume(_PROGRAM);
              stringdispose(current_module^.modulename);
              current_module^.modulename:=stringdup(pattern);
              consume(ID);
              if token=LKLAMMER then
                begin
                   consume(LKLAMMER);
                   idlist;
                   consume(RKLAMMER);
                end;
              consume(SEMICOLON);
            end;

         { global switches are read, so further changes aren't allowed }
         current_module^.in_global:=false;

         { can't have local browser when no global browser }
         if (cs_local_browser in aktmoduleswitches) and
            not(cs_browser in aktmoduleswitches) then
           aktmoduleswitches:=aktmoduleswitches-[cs_local_browser];
         { set implementation flag }
         current_module^.in_implementation:=true;

         { insert after the unit symbol tables the static symbol table }
         { of the program                                              }
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


         reset_global_defs;

         {Insert the name of the main program into the symbol table.}
         if current_module^.modulename^<>'' then
           {st^.insert(new(pprogramsym,init(current_module^.modulename^)));}
           st^.insert(new(punitsym,init(current_module^.modulename^,punitsymtable(st))));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         { Generate a procsym for main }
         gen_main_procsym('main',poproginit,st);

         { reset }
         procprefix:='';
         in_except_block:=false;

         {The program intialization needs an alias, so it can be called
          from the bootstrap code.}
         codegen_newprocedure;
         names.init;
         names.insert('program_init');
         names.insert('PASCALMAIN');
         names.insert(target_os.cprefix+'main');
{$ifdef m68k}
         if target_info.target=target_m68k_PalmOS then
           names.insert('PilotMain');
{$endif}
         compile_proc_body(names,true,false);
         names.done;

         { avoid self recursive destructor call !! PM }
         aktprocsym^.definition^.localst:=nil;

         { consider these symbols as global ones }
         { for browser }
         current_module^.globalsymtable:=current_module^.localsymtable;
         current_module^.localsymtable:=nil;

         codegen_doneprocedure;

         { consume the last point }
         consume(POINT);

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

         { generate imports }
         if current_module^.uses_imports then
          importlib^.generatelib;

         if islibrary then
           exportlib^.generatelib;

         { insert heap }
         insertheap;

         inserttargetspecific;

         datasize:=symtablestack^.datasize;

         { finish asmlist by adding segment starts }
         insertsegment;

         { insert own objectfile }
         insertobjectfile;

         { assemble and link }
         create_objectfile;

         { add the files for the linker from current_module }
         Linker.AddModuleFiles(current_module);

         { create the executable when we are at level 1 }
         if (compile_level=1) then
          begin
            if (cs_link_deffile in aktglobalswitches) then
             deffile.writefile;
            if (not current_module^.is_unit) then
             Linker.MakeExecutable;
          end;
      end;

end.
{
  $Log$
  Revision 1.108  1999-04-14 09:14:52  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.107  1999/04/08 10:53:54  michael
  * Fixed forgotten ;

  Revision 1.106  1999/04/07 15:39:30  pierre
    + double_checksum code added

  Revision 1.105  1999/03/26 00:05:38  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.104  1999/03/24 23:17:17  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.103  1999/03/18 20:30:46  peter
    + .a writer

  Revision 1.102  1999/03/16 21:07:25  peter
    * check for dup uses

  Revision 1.101  1999/02/25 21:02:43  peter
    * ag386bin updates
    + coff writer

  Revision 1.100  1999/02/23 18:29:20  pierre
    * win32 compilation error fix
    + some work for local browser (not cl=omplete yet)

  Revision 1.99  1999/02/22 13:06:58  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.98  1999/02/22 02:15:29  peter
    * updates for ag386bin

  Revision 1.97  1999/02/16 00:45:31  peter
    * fixed crashes by forgotten strpnew() for init_symbol

  Revision 1.96  1999/02/05 08:54:27  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.95  1999/02/03 09:44:36  pierre
    * symbol nubering begins with 1 in number_symbols
    * program tmodule has globalsymtable for its staticsymtable
      (to get it displayed in IDE globals list)
    + list of symbol (browcol) greatly improved for IDE

  Revision 1.94  1999/01/12 14:25:31  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.93  1999/01/06 12:39:46  peter
    * renamed resource -> comprsrc (conflicted with FV)

  Revision 1.92  1998/12/28 23:26:23  peter
    + resource file handling ($R directive) for Win32

  Revision 1.91  1998/12/15 17:14:17  peter
    * fix for tp7

  Revision 1.90  1998/12/15 10:23:26  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.89  1998/12/11 00:03:34  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.88  1998/12/08 10:18:11  peter
    + -gh for heaptrc unit

  Revision 1.87  1998/12/01 23:40:53  pierre
   * new try for correct debug info generation

  Revision 1.86  1998/11/30 09:43:22  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.85  1998/11/28 16:20:54  peter
    + support for dll variables

  Revision 1.84  1998/11/18 09:18:03  pierre
    + automatic loading of profile unit with -pg option
      in go32v2 mode (also defines FPC_PROFILE)
    * some memory leaks removed
    * unreleased temp problem with sets solved

  Revision 1.83  1998/11/16 11:29:00  pierre
    * stackcheck removed for i386_win32
    * exportlist does not crash at least !!
      (was need for tests dir !)z

  Revision 1.82  1998/11/12 12:55:16  pierre
   * fix for bug0176 and bug0177

  Revision 1.81  1998/11/12 11:34:58  peter
    * fix for empty .o files and linking of libs

  Revision 1.80  1998/11/06 09:48:14  pierre
   * double initialization code calling bug fixed

  Revision 1.79  1998/11/03 11:33:11  peter
    + search_unit arg to only search for sources

  Revision 1.78  1998/10/30 12:23:41  peter
    * fix for lognunitname and longunit.pas

  Revision 1.77  1998/10/29 11:35:52  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.76  1998/10/28 18:26:15  pierre
   * removed some erros after other errors (introduced by useexcept)
   * stabs works again correctly (for how long !)

  Revision 1.75  1998/10/27 13:45:35  pierre
    * classes get a vmt allways
    * better error info (tried to remove
      several error strings introduced by the tpexcept handling)

  Revision 1.74  1998/10/26 09:34:50  peter
    * unit check name works now for all units, not only systemunit

  Revision 1.73  1998/10/22 23:53:27  peter
    * leave when an error has been in the interface (else other units could
      be compiled with the implementation uses!)
    * don't always compile when not in implementation with a second_load

  Revision 1.72  1998/10/22 11:36:34  peter
    * fixed imports generation at the wrong place

  Revision 1.71  1998/10/21 20:13:10  peter
    * check for importsection for empty asm file

  Revision 1.70  1998/10/20 09:30:05  peter
    * set also in_library flag when no .o is generated

  Revision 1.68  1998/10/19 08:54:59  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.67  1998/10/13 13:10:25  peter
    * new style for m68k/i386 infos and enums

  Revision 1.66  1998/10/09 16:36:05  pierre
    * some memory leaks specific to usebrowser define fixed
    * removed tmodule.implsymtable (was like tmodule.localsymtable)

  Revision 1.65  1998/10/09 14:38:55  pierre
   * add a second load for PPU file

  Revision 1.64  1998/10/09 08:56:28  pierre
    * several memory leaks fixed

  Revision 1.63  1998/10/08 23:29:01  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.62  1998/10/08 17:17:25  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.61  1998/10/08 13:48:47  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.60  1998/10/06 17:16:54  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.59  1998/10/05 21:33:26  peter
    * fixed 161,165,166,167,168

  Revision 1.58  1998/09/30 16:43:37  peter
    * fixed unit interdependency with circular uses

  Revision 1.57  1998/09/30 12:11:52  peter
    * fixed circular uses which looped forever

  Revision 1.56  1998/09/28 11:22:15  pierre
   * did not compile for browser
   * merge from fixes


  Revision 1.48.2.1  1998/09/28 10:55:16  pierre
  fix for current_module dispose bug

  Revision 1.55  1998/09/28 11:04:03  peter
    * fixed loaddefaultunits which was at the wrong place for programs, so
      the default defs were not loaded when main was initialized

  Revision 1.54  1998/09/24 23:49:12  peter
    + aktmodeswitches

  Revision 1.53  1998/09/23 12:20:50  pierre
    * main program tmodule had no symtable (crashed browser)
    * unit symbols problem fixed !!

  Revision 1.52  1998/09/22 17:13:49  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.51  1998/09/22 15:40:55  peter
    * some extra ifdef GDB

  Revision 1.50  1998/09/21 08:45:17  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.49  1998/09/18 08:01:36  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.48  1998/09/09 15:33:07  peter
    * fixed in_global to allow directives also after interface token

  Revision 1.47  1998/09/09 11:50:55  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.46  1998/09/03 11:24:01  peter
    * moved more inputfile things from tscannerfile to tinputfile
    * changed ifdef Sourceline to cs_asm_source

  Revision 1.45  1998/08/31 12:26:28  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.44  1998/08/26 15:35:33  peter
    * fixed scannerfiles for macros
    + $I %<environment>%

  Revision 1.43  1998/08/26 10:08:47  peter
    * fixed problem with libprefix at the wrong place
    * fixed lib generation with smartlinking and no -CS used

  Revision 1.42  1998/08/19 18:04:54  peter
    * fixed current_module^.in_implementation flag

  Revision 1.41  1998/08/17 10:10:08  peter
    - removed OLDPPU

  Revision 1.40  1998/08/17 09:17:50  peter
    * static/shared linking updates

  Revision 1.39  1998/08/14 21:56:37  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.38  1998/08/10 14:50:13  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.37  1998/08/10 10:18:31  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.36  1998/07/14 14:46:54  peter
    * released NEWINPUT

  Revision 1.35  1998/07/08 12:39:38  peter
    * heap_size for m68k

  Revision 1.34  1998/07/07 11:20:03  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.33  1998/06/25 11:15:34  pierre
    * ppu files where not closed in newppu !!
      second compilation was impossible due to too many opened files
      (not visible in 'make cycle' as we remove all the ppu files)

  Revision 1.32  1998/06/25 08:48:16  florian
    * first version of rtti support

  Revision 1.31  1998/06/24 14:48:35  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.30  1998/06/17 14:10:16  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.29  1998/06/16 08:56:25  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.28  1998/06/13 00:10:10  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.27  1998/06/11 13:58:08  peter
    * small fix to let newppu compile

  Revision 1.26  1998/06/09 16:01:47  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.25  1998/06/08 22:59:49  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.24  1998/06/08 13:13:44  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.23  1998/06/05 17:47:29  peter
    * some better uses clauses

  Revision 1.22  1998/06/05 14:37:34  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.21  1998/06/04 23:51:53  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.20  1998/06/04 09:55:42  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.19  1998/06/03 23:40:38  peter
    + unlimited file support, release tempclose

  Revision 1.18  1998/06/03 22:49:00  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.17  1998/05/28 14:40:25  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.16  1998/05/27 19:45:06  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.15  1998/05/23 01:21:22  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.14  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.13  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.12  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.11  1998/05/06 18:36:53  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

  Revision 1.10  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.9  1998/05/01 16:38:45  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.8  1998/04/30 15:59:41  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.7  1998/04/29 10:33:59  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.6  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.5  1998/04/14 23:27:03  florian
    + exclude/include with constant second parameter added

  Revision 1.4  1998/04/10 14:41:43  peter
    * removed some Hints
    * small speed optimization for AsmLn

  Revision 1.3  1998/04/03 09:51:00  daniel
  * Fixed heap allocation for OS/2.
}
