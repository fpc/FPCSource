
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

{.$define TEST_IMPL does not work well }

  interface

    procedure proc_unit;
    procedure proc_program(islibrary : boolean);


  implementation

    uses
       cobjects,comphook,systems,globals,
       symtable,aasm,files,
       hcodegen,verbose,
       link,assemble,import,gendef,ppu
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ,scanner,pbase,psystem,pdecl,psub,parser;


    procedure create_objectfile;
      begin
        { create the .s file and assemble it }
        GenerateAsm;

        { When creating a library call the linker. And insert the output
          of the linker files }
        if (cs_create_sharedlib in aktmoduleswitches) then
          Linker.MakeSharedLibrary
        else
          if (cs_create_staticlib in aktmoduleswitches) or
             (cs_smartlink in aktmoduleswitches) then
            Linker.MakeStaticLibrary(SmartLinkFilesCnt);

        { add the files for the linker from current_module }
        Linker.AddModuleFiles(current_module);
      end;


    procedure insertobjectfile;
    { Insert the used object file for this unit in the used list for this unit }
      begin
        if (cs_create_sharedlib in aktmoduleswitches) then
          current_module^.linksharedlibs.insert(current_module^.sharedlibfilename^)
        else
         begin
           if (cs_create_staticlib in aktmoduleswitches) or
              (cs_smartlink in aktmoduleswitches) then
             current_module^.linkstaticlibs.insert(current_module^.staticlibfilename^)
           else
             current_module^.linkofiles.insert(current_module^.objfilename^);
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
           datasegment^.insert(new(pai_string,init('FPC '+version_string+' for '+target_string+' - '+target_info.short_name)));
         end;
      { Insert start and end of sections }
        fixseg(codesegment,sec_code);
        fixseg(datasegment,sec_data);
        fixseg(bsssegment,sec_bss);
        fixseg(consts,sec_data);
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
       target_i386_OS2 : ;
{$endif i386}
{$ifdef m68k}
       target_m68k_Mac : bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)));
{$endif m68k}
         else
           bsssegment^.concat(new(pai_datablock,init_global('HEAP',heapsize)));
         end;
{$ifdef i386}
         datasegment^.concat(new(pai_symbol,init_global('HEAPSIZE')));
{$endif i386}
{$ifdef m68k}
         datasegment^.concat(new(pai_symbol,init_global('HEAP_SIZE')));
{$endif m68k}
         datasegment^.concat(new(pai_const,init_32bit(heapsize)));
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
              datasegment^.concat(new(pai_const,init_symbol('_mainCRTStartup')));
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
        nextmapentry : longint;
      begin
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
            { need to recompile the current unit ? }
              if loaded_unit^.crc<>pu^.checksum then
               begin
                 Message2(unit_u_recompile_crc_change,current_module^.modulename^,pu^.name^);
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
{$ifdef TEST_IMPL}
            { need to recompile the current unit ? }
              if loaded_unit^.crc<>pu^.checksum then
               begin
                 Message2(unit_u_recompile_crc_change,current_module^.modulename^,pu^.name^);
                 current_module^.do_compile:=true;
                 dispose(current_module^.map);
                 current_module^.map:=nil;
                 exit;
               end;
{$endif TEST_IMPL}
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
{$ifdef UseBrowser}
        if cs_browser in aktmoduleswitches then
          punitsymtable(current_module^.globalsymtable)^.load_symtable_refs;
        if ((current_module^.flags and uf_has_browser)<>0) and
           (cs_local_browser in aktmoduleswitches) then
         begin
           current_module^.localsymtable:=new(psymtable,load);
           psymtable(current_module^.localsymtable)^.name:=
              stringdup('implementation of '+psymtable(current_module^.globalsymtable)^.name^);
           psymtable(current_module^.localsymtable)^.load_browser;
         end;
{$endif UseBrowser}
        { remove the map, it's not needed anymore }
        dispose(current_module^.map);
        current_module^.map:=nil;
      end;


    function loadunit(const s : string;compile_system:boolean) : pmodule;
      const
        ImplIntf : array[boolean] of string[15]=('interface','implementation');
      var
        st : punitsymtable;
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
               if (not current_module^.search_unit(current_module^.modulename^))
                  and (length(current_module^.modulename^)>8) then
                 current_module^.search_unit(copy(current_module^.modulename^,1,8));
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
(*
                if assigned(old_current_module^.scanner) then
                 begin
                   current_scanner^.tempcloseinputfile;
                   current_scanner:=nil;
                   { the current_scanner is always the same
                     as current_module^.scanner (PFV) }
                     NO !!! unless you changed the code
                     because it is only change in compile
                     whereas current_module is changed here !!
                 end;
                compile(current_module^.mainsource^,compile_system);
                if (not old_current_module^.compiled) and
                   assigned(old_current_module^.scanner) then
                 begin
                   current_scanner:=old_current_module^.scanner;
                   current_scanner^.tempopeninputfile;
                 end; *)
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
                end;
              { the next unit }
              hp:=pmodule(hp^.next);
           end;
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
               hp^.search_unit(s);
               { try to load the unit a second time first }
               if not current_module^.in_implementation then
                 hp^.do_compile:=true;
               current_module:=hp;
               current_module^.in_second_compile:=true;
               Message1(unit_u_second_load_unit,current_module^.modulename^);
             end
            else
          { generates a new unit info record }
             begin
                current_module:=new(pmodule,init(s,true));
                scanner:=nil;
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
      { save default symtablestack }
        defaultsymtablestack:=symtablestack;
      end;


    procedure loadunits;
      var
         s : stringid;
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
           hp2:=loadunit(s,false);
         { the current module uses the unit hp2 }
           current_module^.used_units.concat(new(pused_unit,init(hp2,not current_module^.in_implementation)));
           pused_unit(current_module^.used_units.last)^.in_uses:=true;
           if current_module^.compiled then
             exit;
           refsymtable^.insert(new(punitsym,init(s,hp2^.globalsymtable)));
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
              if (cs_debuginfo in aktmoduleswitches) and
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
      begin
        {Generate a procsym for main}
        make_ref:=false;
        aktprocsym:=new(Pprocsym,init(name));
        aktprocsym^.definition:=new(Pprocdef,init);
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
      var
         { unitname : stringid; }
         names  : Tstringcontainer;
         st     : psymtable;
         unitst : punitsymtable;
{$ifdef GDB}
         pu     : pused_unit;
{$endif GDB}
         i      : longint;
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
             s2^:=upper(current_scanner^.inputfile^.name^);
             { strip extension, there could only be one dot }
             i:=pos('.',s2^);
             if i>0 then
              s2^:=Copy(s2^,1,i-1);
             if (cs_compilesystem in aktmoduleswitches)  then
              begin
                if (cs_check_unit_name in aktglobalswitches) and
                   ((length(current_module^.modulename^)>8) or
                    (current_module^.modulename^<>s1^) or
                    (current_module^.modulename^<>s2^)) then
                  Message1(unit_e_illegal_unit_name,s1^);
              end
             else
              if (current_module^.modulename^=s1^) then
               Message(unit_w_switch_us_missed);
             dispose(s2);
             dispose(s1);
          end;

         consume(ID);
         consume(SEMICOLON);
         consume(_INTERFACE);
         { global switches are read, so further changes aren't allowed }
         current_module^.in_global:=false;
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
         procprefix:='_'+tostr(length(current_module^.unitname^))+lowercase(current_module^.unitname^)+'_';
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

         { ... parse the declarations }
         read_interface_declarations;

{$ifdef GDB}
         { add all used definitions}
         if (cs_debuginfo in aktmoduleswitches) then
           begin
              { all types }
              punitsymtable(refsymtable)^.concattypestabto(debuglist);
              { and all local symbols}
              refsymtable^.concatstabto(debuglist);
           end;
{$endif GDB}

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

         { number the definitions, so a deref from other units works }
         refsymtable^.number_defs;
{$ifdef UseBrowser}
         refsymtable^.number_symbols;
         { we don't want implementation units symbols in unitsymtable !! PM }
         refsymtable:=st;
{$endif UseBrowser}
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
         st^.allsymbolsused;

         { size of the static data }
         datasize:=st^.datasize;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
            { all types }
            punitsymtable(st)^.concattypestabto(debuglist);
            { and all local symbols}
            st^.concatstabto(debuglist);
          end;
{$endif GDB}

         { tests, if all (interface) forwards are resolved }
         symtablestack^.check_forwards;

         { now we have a correct unit, change the symtable type }
         current_module^.in_implementation:=false;
         symtablestack^.symtabletype:=unitsymtable;
{$ifdef GDB}
         punitsymtable(symtablestack)^.is_stab_written:=false;
{$endif GDB}

         { leave when we got an error }
         if status.errorcount>0 then
          begin
            Message1(unit_f_errors_in_unit,tostr(status.errorcount));
            exit;
          end;

         { insert own objectfile }
         insertobjectfile;

         { Write out the ppufile }
         writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack));

         { write local browser }
{$ifdef UseBrowser}
         if cs_local_browser in aktmoduleswitches then
          begin
            current_module^.localsymtable:=refsymtable;
            refsymtable^.write;
            refsymtable^.write_browser;
          end;
{$endif UseBrowser}

{$ifdef GDB}
         pu:=pused_unit(usedunits.first);
         while assigned(pu) do
           begin
              punitsymtable(pu^.u^.globalsymtable)^.is_stab_written:=false;
              pu:=pused_unit(pu^.next);
           end;
{$endif GDB}

         { remove static symtable (=refsymtable) here to save some mem }
{$ifndef UseBrowser}
         dispose(st,done);
         current_module^.localsymtable:=nil;
{$endif UseBrowser}

         { generate imports }
         if current_module^.uses_imports then
          importlib^.generatelib;

         { finish asmlist by adding segment starts }
         insertsegment;

         { assemble }
         create_objectfile;
      end;


    procedure proc_program(islibrary : boolean);
      var
         st    : psymtable;
         names : Tstringcontainer;
      begin
         parse_only:=false;
         if islibrary then
           begin
              consume(_LIBRARY);
              stringdispose(current_module^.modulename);
              current_module^.modulename:=stringdup(pattern);
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

         { load standard units (system,objpas unit) }
         loaddefaultunits;

         { reset }
         lexlevel:=0;

         {Load the units used by the program we compile.}
         if token=_USES then
           loadunits;

         {Insert the name of the main program into the symbol table.}
         if current_module^.modulename^<>'' then
           st^.insert(new(pprogramsym,init(current_module^.modulename^)));

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

         codegen_doneprocedure;

         { consume the last point }
         consume(POINT);

         { leave when we got an error }
         if status.errorcount>0 then
          begin
            Message1(unit_f_errors_in_unit,tostr(status.errorcount));
            exit;
          end;

         { insert heap }
         insertheap;

         { generate imports }
         if current_module^.uses_imports then
          importlib^.generatelib;

         inserttargetspecific;

         datasize:=symtablestack^.datasize;

         { finish asmlist by adding segment starts }
         insertsegment;

         { insert own objectfile }
         insertobjectfile;

         { assemble and link }
         create_objectfile;

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
  Revision 1.68  1998-10-19 08:54:59  pierre
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
