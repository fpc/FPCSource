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

{define TEST_IMPL does not work well }

  interface

    uses
      files;

    procedure loadsystemunit;
    procedure proc_unit;
    procedure proc_program(islibrary : boolean);

  implementation

    uses
       cobjects,verbose,comphook,systems,globals,
       symtable,aasm,hcodegen,
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
        if (cs_create_staticlib in aktmoduleswitches) then
         Linker.MakeStaticLibrary(SmartLinkFilesCnt)
        else
         if (cs_create_sharedlib in aktmoduleswitches) then
          Linker.MakeSharedLibrary;
        { add the files for the linker from current_module }
        Linker.AddModuleFiles(current_module);
      end;


    procedure insertobjectfile;
    { Insert the used object file for this unit in the used list for this unit }
      begin
        if (cs_create_staticlib in aktmoduleswitches) then
         current_module^.linkstaticlibs.insert(current_module^.staticlibfilename^)
        else
         if (cs_create_sharedlib in aktmoduleswitches) then
          current_module^.linksharedlibs.insert(current_module^.sharedlibfilename^)
        else
          current_module^.linkofiles.insert(current_module^.objfilename^);
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
          target_OS2 : ;
{$endif i386}
{$ifdef m68k}
       target_Mac68K : bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)));
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
{$ifdef i386}
        case target_info.target of
       target_GO32V2 : begin
                       { stacksize can be specified }
                         datasegment^.concat(new(pai_symbol,init_global('__stklen')));
                         datasegment^.concat(new(pai_const,init_32bit(stacksize)));
                       end;
        target_WIN32 : begin
                       { Generate an external entry to be sure that _mainCRTStarup will be
                         linked, can't use concat_external because those aren't written for
                         asw (PFV) }
                         datasegment^.concat(new(pai_const,init_symbol('_mainCRTStartup')));
                       end;
        end;
{$endif i386}
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
        nextmapentry:=1;
      { load the used units from interface }
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
                 current_module^.do_compile:=true;
                 exit;
               end;
            { setup the map entry for deref }
              current_module^.map^[nextmapentry]:=loaded_unit^.symtable;
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=pused_unit(pu^.next);
         end;
      { ok, now load the unit }
        current_module^.symtable:=new(punitsymtable,loadasunit);
      { if this is the system unit insert the intern symbols }
        if compile_system then
         begin
           make_ref:=false;
           insertinternsyms(psymtable(current_module^.symtable));
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
                 current_module^.do_compile:=true;
                 exit;
               end;
            { setup the map entry for deref }
              current_module^.map^[nextmapentry]:=loaded_unit^.symtable;
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
{$endif TEST_IMPL}
            end;
           pu:=pused_unit(pu^.next);
         end;
      { remove the map, it's not needed anymore }
        dispose(current_module^.map);
        current_module^.map:=nil;
      end;


    function loadunit(const s : string;compile_system:boolean) : pmodule;
      var
        st : punitsymtable;
        old_current_ppu : pppufile;
        old_current_module,hp : pmodule;

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
           { we needn't the ppufile }
             if assigned(current_module^.ppufile) then
              begin
                dispose(current_module^.ppufile,done);
                current_module^.ppufile:=nil;
              end;
           { recompile the unit or give a fatal error if sources not available }
             if not(current_module^.sources_avail) then
              Message1(unit_f_cant_compile_unit,current_module^.modulename^)
             else
              begin
                current_scanner^.close;
                compile(current_module^.mainsource^,compile_system);
                if (not old_current_module^.compiled) then
                 current_scanner^.reopen;
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
         { be sure not to mix lines from different files }
         { update_line; }
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
                   if assigned(hp^.symtable) then
                     st:=punitsymtable(hp^.symtable)
                   else
                    begin
                    { recompile the unit ? }
                      if (not current_module^.in_implementation) and (hp^.in_implementation) then
                       Message(unit_f_circular_unit_reference);
                    end;
                   break;
                end;
              { the next unit }
              hp:=pmodule(hp^.next);
           end;
       { the unit is not in the symtable stack }
         if (not assigned(st)) then
          begin
          { if the unit is loaded remove it first }
            if assigned(hp) then
             begin
               { remove the old unit }
               loaded_units.remove(hp);
               dispose(hp,done);
             end;
          { generates a new unit info record }
            current_module:=new(pmodule,init(s,true));
            current_ppu:=current_module^.ppufile;
          { now we can register the unit }
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


    procedure loadsystemunit;
      var
        hp : pmodule;
      begin
      { if the current file isn't a system unit the the system unit
        will be loaded }
        if not(cs_compilesystem in aktmoduleswitches) then
          begin
            hp:=loadunit(upper(target_info.system_unit),true);
            systemunit:=hp^.symtable;
          { add to the used units }
            current_module^.used_units.concat(new(pused_unit,init(hp,true)));
          { read default constant definitions }
            make_ref:=false;
            readconstdefs;
          { we could try to overload caret by default }
            symtablestack:=systemunit;
          { if POWER is defined in the RTL then use it for starstar overloading }
            getsym('POWER',false);
            if assigned(srsym) and (srsym^.typ=procsym) and
               (overloaded_operators[STARSTAR]=nil) then
              overloaded_operators[STARSTAR]:=pprocsym(srsym);
            make_ref:=true;
          end
        else
          begin
             createconstdefs;
             systemunit:=nil;
          end;
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
           refsymtable^.insert(new(punitsym,init(s,hp2^.symtable)));
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
         symtablestack:=systemunit;

         { now insert the units in the symtablestack }
         hp:=pused_unit(current_module^.used_units.first);
         while assigned(hp) do
           begin
{$IfDef GDB}
              if (cs_debuginfo in aktmoduleswitches) and
                not hp^.is_stab_written then
                begin
                   punitsymtable(hp^.u^.symtable)^.concattypestabto(debuglist);
                   hp^.is_stab_written:=true;
                   hp^.unitid:=psymtable(hp^.u^.symtable)^.unitid;
                end;
{$EndIf GDB}
              if hp^.in_uses then
                begin
                   hp3:=symtablestack;
                   while assigned(hp3) do
                     begin
                        { insert units only once ! }
                        if hp^.u^.symtable=hp3 then
                          break;
                        hp3:=hp3^.next;
                        { unit isn't inserted }
                        if hp3=nil then
                          begin
                             psymtable(hp^.u^.symtable)^.next:=symtablestack;
                             symtablestack:=psymtable(hp^.u^.symtable);
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

      var
         old_module_in_implementation : boolean;
      begin
         if token=_USES then
           begin
              old_module_in_implementation:=module_in_implementation;
              module_in_implementation:=true;
              current_module^.in_implementation:=true;
              symt^.symtabletype:=unitsymtable;
              loadunits;
              symt^.symtabletype:=globalsymtable;
{$ifdef DEBUG}
              test_symtablestack;
{$endif DEBUG}
              module_in_implementation:=old_module_in_implementation;
           end;
      end;

    procedure proc_unit;

      var
         { unitname : stringid; }
         names  : Tstringcontainer;
         p      : psymtable;
         unitst : punitsymtable;
         pu     : pused_unit;
         i      : longint;
         s1,s2  : ^string; {Saves stack space}
      begin
         consume(_UNIT);
         if token=ID then
          begin
          { create filenames and unit name }
             current_module^.SetFileName(current_scanner^.inputfile^.path^+current_scanner^.inputfile^.name^);
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

         { update status }
         status.currentmodule:=current_module^.modulename^;

         { this should be placed after uses !!}
{$ifndef UseNiceNames}
         procprefix:='_'+current_module^.modulename^+'$$';
{$else UseNiceNames}
         procprefix:='_'+tostr(length(current_module^.unitname^))+lowercase(current_module^.unitname^)+'_';
{$endif UseNiceNames}

         parse_only:=true;

         { generate now the global symboltable }
         p:=new(punitsymtable,init(globalsymtable,current_module^.modulename^));
         refsymtable:=p;
         unitst:=punitsymtable(p);

         { the unit name must be usable as a unit specifier }
         { inside the unit itself (PM)                      }
         { this also forbids to have another symbol         }
         { with the same name as the unit                   }
         refsymtable^.insert(new(punitsym,init(current_module^.modulename^,unitst)));
         { set the symbol table for the current unit }
         { this must be set later for interdependency }
         { current_module^.symtable:=psymtable(p); }

         { a unit compiled at command line must be inside the loaded_unit list }
         if (compile_level=1) then
           loaded_units.insert(current_module);

         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in aktmoduleswitches) then
           begin
              { insert the system unit }
              { it is allways the first }
              systemunit^.next:=nil;
              symtablestack:=systemunit;
              refsymtable^.insert(new(punitsym,init('SYSTEM',systemunit)));

              if token=_USES then
                begin
                   unitst^.symtabletype:=unitsymtable;
                   loadunits;
                   { has it been compiled at a higher level ?}
                   if current_module^.compiled then
                     exit;
                   unitst^.symtabletype:=globalsymtable;
                end;

              { ... but insert the symbol table later }
              p^.next:=symtablestack;
              symtablestack:=p;
           end
         else
         { while compiling a system unit, some types are directly inserted }
           begin
              p^.next:=symtablestack;
              symtablestack:=p;
              insert_intern_types(p);
           end;

         { displaced for inter-dependency considerations }
         current_module^.symtable:=psymtable(p);

         constsymtable:=symtablestack;
         { ... parse the declarations }
         read_interface_declarations;
         consume(_IMPLEMENTATION);

         parse_only:=false;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktmoduleswitches) then
           begin
              { all types }
              punitsymtable(refsymtable)^.concattypestabto(debuglist);
              { and all local symbols}
              refsymtable^.concatstabto(debuglist);
           end;
{$endif GDB}

         { generates static symbol table }
         p:=new(punitsymtable,init(staticsymtable,current_module^.modulename^));

         {Generate a procsym.}
         make_ref:=false;
         aktprocsym:=new(Pprocsym,init(current_module^.modulename^+'_init'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or pounitinit;
         aktprocsym^.definition^.setmangledname(current_module^.modulename^+'_init');
         make_ref:=true;

         {The generated procsym has a local symtable. Discard it and turn
          it into the static one.}
         dispose(aktprocsym^.definition^.localst,done);
         aktprocsym^.definition^.localst:=p;

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst^.next;

         { number the definitions, so a deref from other units works }
         refsymtable^.number_defs;

         { Read the implementation units }
         parse_implementation_uses(unitst);

         numberunits;


         { now we can change refsymtable }
         refsymtable:=p;

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

         { set some informations }
         procinfo.retdef:=voiddef;
         procinfo._class:=nil;
         procinfo.call_offset:=8;
         { for temporary values }
         procinfo.framepointer:=frame_pointer;
         { clear flags }
         procinfo.flags:=0;

         { Create a new procedure }
         codegen_newprocedure;

         { Compile the unit }
         names.init;
         names.insert(current_module^.modulename^+'_init');
         names.insert('INIT$$'+current_module^.modulename^);
         compile_proc_body(names,true,false);
         names.done;

         { Shutdown the codegen for this procedure }
         codegen_doneprocedure;
{$ifdef dummy}
         if token=_FINALIZATION then
           begin
              current_module^.flags:=current_module^.flags or uf_finalize;
              { clear flags }
              procinfo.flags:=0;

              {Reset the codegenerator.}
              codegen_newprocedure;

              names.init;
              names.insert(current_module^.modulename^+'_finalize');
              names.insert('FINALIZE$$'+current_module^.modulename^);
              compile_proc_body(names,true,false);
              names.done;

              codegen_doneprocedure;
           end;
{$endif dummy}
         consume(POINT);

         { size of the static data }
         datasize:=symtablestack^.datasize;

         { unsed static symbols ? }
         symtablestack^.allsymbolsused;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
            { all types }
            punitsymtable(symtablestack)^.concattypestabto(debuglist);
            { and all local symbols}
            symtablestack^.concatstabto(debuglist);
          end;
{$endif GDB}

         current_module^.in_implementation:=false;
         { deletes all symtables generated in the implementation part }
         while symtablestack^.symtabletype<>globalsymtable do
           dellexlevel;

         { tests, if all forwards are resolved }
         symtablestack^.check_forwards;
         symtablestack^.symtabletype:=unitsymtable;
         punitsymtable(symtablestack)^.is_stab_written:=false;

         { insert own objectfile }
         insertobjectfile;

         {Write out the unit if the compile was succesfull.}
         if status.errorcount=0 then
          writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack));

         pu:=pused_unit(usedunits.first);
         while assigned(pu) do
           begin
              punitsymtable(pu^.u^.symtable)^.is_stab_written:=false;
              pu:=pused_unit(pu^.next);
           end;
         inc(datasize,symtablestack^.datasize);

         { leave when we got an error }
         if status.errorcount>0 then
          exit;

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

         { insert after the unit symbol tables the static symbol table }
         { of the program                                              }
         st:=new(punitsymtable,init(staticsymtable,current_module^.modulename^));

         {Generate a procsym.}
         make_ref:=false;
         aktprocsym:=new(Pprocsym,init('main'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poproginit;
         aktprocsym^.definition^.setmangledname(target_os.Cprefix+'main');
         make_ref:=true;
         {The localst is a local symtable. Change it into the static
          symtable.}
         dispose(aktprocsym^.definition^.localst,done);
         aktprocsym^.definition^.localst:=st;

         refsymtable:=st;

         { necessary for browser }
         loaded_units.insert(current_module);

         {Insert the symbols of the system unit into the stack of symbol
          tables.}
         symtablestack:=systemunit;
         systemunit^.next:=nil;
         refsymtable^.insert(new(punitsym,init('SYSTEM',systemunit)));

         {Load the units used by the program we compile.}
         if token=_USES then
           loadunits;

         {Insert the name of the main program into the symbol table.}
         if current_module^.modulename^<>'' then
           st^.insert(new(pprogramsym,init(current_module^.modulename^)));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         { set some informations about the main program }
         with procinfo do
          begin
            retdef:=voiddef;
            _class:=nil;
            call_offset:=8;
            framepointer:=frame_pointer;
            flags:=0;
          end;

         procprefix:='';
         in_except_block:=false;

         codegen_newprocedure;

         {The program intialization needs an alias, so it can be called
          from the bootstrap code.}
         names.init;
         names.insert('program_init');
         names.insert('PASCALMAIN');
         names.insert(target_os.cprefix+'main');
         compile_proc_body(names,true,false);
         names.done;

         codegen_doneprocedure;

         consume(POINT);

         { leave when we got an error }
         if status.errorcount>0 then
          exit;

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
  Revision 1.41  1998-08-17 10:10:08  peter
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
