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

  interface

    uses
       dos,strings,
       cobjects,globals,scanner,symtable,aasm,tree,pass_1,
       types,hcodegen,files,verbose,systems,link,assemble
{$ifdef GDB}
       ,gdb
{$endif GDB}
{$ifdef NEWPPU}
       ,ppu
{$endif}
       { parser specific stuff }
       ,pbase,pdecl,pstatmnt,psub,psystem
       { processor specific stuff }
{$ifdef i386}
       ,i386
       ,cgai386
       ,tgeni386
       ,cgi386
       ,aopt386
{$endif}
{$ifdef m68k}
       ,m68k
       ,cga68k
       ,tgen68k
       ,cg68k
{$endif}
       ;

    procedure addlinkerfiles(hp:pmodule);
    function  loadunit(const s : string;compile_system, in_uses : boolean) : pmodule;
    procedure proc_unit;
    procedure proc_program(islibrary : boolean);

  implementation

    uses
       parser;

    procedure addlinkerfiles(hp:pmodule);
      begin
        with hp^ do
         begin
           while not linkofiles.empty do
            Linker.AddObject(linkofiles.Get);
           while not linksharedlibs.empty do
            Linker.AddSharedLibrary(linksharedlibs.Get);
           while not linkstaticlibs.empty do
            Linker.AddStaticLibrary(linkstaticlibs.Get);
         end;
      end;

    procedure insertsegment;
      begin
      {Insert Ident of the compiler}
        if (not (cs_smartlink in aktswitches))
{$ifndef EXTDEBUG}
           and (not current_module^.is_unit)
{$endif}
           then
         begin
           datasegment^.insert(new(pai_align,init(4)));
           datasegment^.insert(new(pai_string,init('FPC '+version_string+' for '+target_string+' - '+target_info.short_name)));
         end;
      { Insert start and end of sections }
        codesegment^.insert(new(pai_section,init(sec_code)));
        codesegment^.concat(new(pai_section,init(sec_none)));
        datasegment^.insert(new(pai_section,init(sec_data)));
        datasegment^.concat(new(pai_section,init(sec_none)));
        bsssegment^.insert(new(pai_section,init(sec_bss)));
        bsssegment^.concat(new(pai_section,init(sec_none)));
        consts^.insert(new(pai_asm_comment,init('Constants')));
        consts^.insert(new(pai_section,init(sec_data)));
        consts^.concat(new(pai_section,init(sec_none)));
      end;

    procedure insertheap;
      begin
         if (cs_smartlink in aktswitches) then
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
          target_OS2 : ;
       target_Mac68K : bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)));
         else
           bsssegment^.concat(new(pai_datablock,init_global('HEAP',heapsize)));
         end;
         datasegment^.concat(new(pai_symbol,init_global('HEAPSIZE')));
         datasegment^.concat(new(pai_const,init_32bit(heapsize)));
      end;


    procedure inserttargetspecific;
      var
        i : longint;
      begin
        case target_info.target of
       target_GO32V2 : begin
                       { stacksize can be specified }
                         datasegment^.concat(new(pai_symbol,init_global('__stklen')));
                         datasegment^.concat(new(pai_const,init_32bit(stacksize)));
                       end;
        target_WIN32 : begin
                       { generate the last entry for the imports directory }
                         if not(assigned(importssection)) then
                           importssection:=new(paasmoutput,init);
                       { $3 ensure that it is the last entry, all other entries }
                       { are written to $2                                      }
                         importssection^.concat(new(pai_section,init_idata(3)));
                         for i:=1 to 5 do
                           importssection^.concat(new(pai_const,init_32bit(0)));
                       end;
        end;
      end;



    procedure load_ppu(oldhp,hp : pmodule;compile_system : boolean);
      var
         loaded_unit  : pmodule;
         b            : byte;
         checksum,
{$ifndef NEWPPU}
         count,
{$endif NEWPPU}
         nextmapentry : longint;
         hs           : string;
      begin
         { init the map }
         new(hp^.map);
         nextmapentry:=1;

{$ifdef NEWPPU}
         { load the used units from interface }
         b:=hp^.ppufile^.readentry;
         if b=ibloadunit_int then
          begin
            while not hp^.ppufile^.endofentry do
             begin
               hs:=hp^.ppufile^.getstring;
               checksum:=hp^.ppufile^.getlongint;
               loaded_unit:=loadunit(hs,false,false);
               if hp^.compiled then
                exit;
             { if the crc of a used unit is the same as written to the
               PPU file, we needn't to recompile the current unit }
               if (loaded_unit^.crc<>checksum) then
                begin
                { we have to compile the current unit remove stuff which isn't
                  needed }
                { forget the map }
                  dispose(hp^.map);
                  hp^.map:=nil;
                { remove the ppufile }
                  dispose(hp^.ppufile,done);
                  hp^.ppufile:=nil;
                { recompile or give an fatal error }
                  if not(hp^.sources_avail) then
                   Message1(unit_f_cant_compile_unit,hp^.modulename^)
                  else
                   begin
                      if assigned(oldhp^.current_inputfile) then
                        oldhp^.current_inputfile^.tempclose;
                      compile(hp^.mainsource^,compile_system);
                      if (not oldhp^.compiled) and assigned(oldhp^.current_inputfile) then
                        oldhp^.current_inputfile^.tempreopen;
                   end;
                  exit;
                end;
             { setup the map entry for deref }
               hp^.map^[nextmapentry]:=loaded_unit^.symtable;
               inc(nextmapentry);
               if nextmapentry>maxunits then
                Message(unit_f_too_much_units);
             end;
          { ok, now load the unit }
            hp^.symtable:=new(punitsymtable,load(hp));
          { if this is the system unit insert the intern symbols }
            make_ref:=false;
            if compile_system then
              insertinternsyms(psymtable(hp^.symtable));
            make_ref:=true;
          end;
       { now only read the implementation part }
         hp^.in_implementation:=true;
       { load the used units from implementation }
         b:=hp^.ppufile^.readentry;
         if b=ibloadunit_imp then
          begin
            while not hp^.ppufile^.endofentry do
             begin
               hs:=hp^.ppufile^.getstring;
               checksum:=hp^.ppufile^.getlongint;
               loaded_unit:=loadunit(hs,false,false);
               if hp^.compiled then
                exit;
             end;
          end;
{$ifdef NEWPPU}
       { The next entry should be an ibendimplementation }
         b:=hp^.ppufile^.readentry;
         if b <> ibendimplementation then
          Message1(unit_f_ppu_invalid_entry,tostr(b));
       { The next entry should be an ibend }
         b:=hp^.ppufile^.readentry;
         if b <> ibend then
          Message1(unit_f_ppu_invalid_entry,tostr(b));
{$endif}
         hp^.ppufile^.close;
{!         dispose(hp^.ppufile,done);}
{$else}
         { load the used units from interface }
         hp^.ppufile^.read_data(b,1,count);
         while (b=ibloadunit) do
           begin
              { read unit name }
              hp^.ppufile^.read_data(hs[0],1,count);
              hp^.ppufile^.read_data(hs[1],byte(hs[0]),count);
              hp^.ppufile^.read_data(checksum,4,count);
              loaded_unit:=loadunit(hs,false,false);
              if hp^.compiled then
                exit;
              { if the crc of a used unit is the same as }
              { written to the PPU file, we needn't to   }
              { recompile the current unit               }
              if (loaded_unit^.crc<>checksum) then
                begin
                   { we have to compile the current unit }
                   { remove stuff which isn't needed     }
                   { forget the map }
                   dispose(hp^.map);
                   hp^.map:=nil;
                   hp^.ppufile^.close;
                   dispose(hp^.ppufile,done);
                   hp^.ppufile:=nil;
                   if not(hp^.sources_avail) then
                    Message1(unit_f_cant_compile_unit,hp^.modulename^)
                   else
                    begin
                       if assigned(oldhp^.current_inputfile) then
                         oldhp^.current_inputfile^.tempclose;
                       compile(hp^.mainsource^,compile_system);
                       if (not oldhp^.compiled) and assigned(oldhp^.current_inputfile) then
                         oldhp^.current_inputfile^.tempreopen;
                    end;
                   exit;
                end;
              { setup the map entry for deref }
              hp^.map^[nextmapentry]:=loaded_unit^.symtable;
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
              { read until ibend }
              hp^.ppufile^.read_data(b,1,count);
           end;
         { ok, now load the unit }
         hp^.symtable:=new(punitsymtable,load(hp));
         { if this is the system unit insert the intern }
         { symbols                                      }
         make_ref:=false;
         if compile_system then
           insertinternsyms(psymtable(hp^.symtable));
         make_ref:=true;
         { now only read the implementation part }
         hp^.in_implementation:=true;
         { load the used units from implementation }
         hp^.ppufile^.read_data(b,1,count);
         while (b<>ibend) and (b=ibloadunit) do
           begin
              { read unit name }
              hp^.ppufile^.read_data(hs[0],1,count);
              hp^.ppufile^.read_data(hs[1],byte(hs[0]),count);
              hp^.ppufile^.read_data(checksum,4,count);
              loaded_unit:=loadunit(hs,false,false);
              if hp^.compiled then exit;
              { if the crc of a used unit is the same as }
              { written to the PPU file, we needn't to   }
              { recompile the current unit               }
              { but for the implementation part          }
              { the written crc is false, because        }
              { not defined when writing the ppufile !!  }
{$ifdef TEST_IMPL}
              if (loaded_unit^.crc<>0) and (loaded_unit^.crc<>checksum) then
                begin
                   { we have to compile the current unit }
                   { remove stuff which isn't needed     }
                   { forget the map }
                   dispose(hp^.map);
                   hp^.map:=nil;
                   hp^.ppufile^.close;
                   dispose(hp^.ppufile,done);
                   hp^.ppufile:=nil;
                   if not(hp^.sources_avail) then
                    Message1(unit_f_cant_compile_unit,hp^.modulename^)
                   else
                     begin
                        oldhp^.current_inputfile^.tempclose;
                        compile(hp^.mainsource^,compile_system);
                        oldhp^.current_inputfile^.tempclose;
                     end;
                   exit;
                end;
{$endif TEST_IMPL}
              { read until ibend }
              hp^.ppufile^.read_data(b,1,count);
           end;
         hp^.ppufile^.close;
{$endif}
         dispose(hp^.map);
         hp^.map:=nil;
      end;


    function loadunit(const s : string;compile_system, in_uses : boolean) : pmodule;
      var
         st : punitsymtable;
         old_current_module,hp,nextmodule : pmodule;
         pu : pused_unit;
         hs : pstring;
      begin
         old_current_module:=current_module;
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
       { no error and the unit isn't loaded }
         if not(assigned(hp)) and (st=nil) then
           begin
              { generates a new unit info record }
              hp:=new(pmodule,init(s,true));
              { now we can register the unit }
              loaded_units.insert(hp);
              current_module:=hp;
              { force build ? }
              if (hp^.do_compile) or (hp^.sources_avail and do_build) then
                begin
                   { we needn't the ppufile }
                   if assigned(hp^.ppufile) then
                     begin
                        dispose(hp^.ppufile,done);
                        hp^.ppufile:=nil;
                     end;
                   if not(hp^.sources_avail) then
                    Message1(unit_f_cant_compile_unit,hp^.modulename^)
                   else
                    begin
                       if assigned(old_current_module^.current_inputfile) then
                         old_current_module^.current_inputfile^.tempclose;
                       compile(hp^.mainsource^,compile_system);
                      if (not old_current_module^.compiled) and assigned(old_current_module^.current_inputfile) then
                         old_current_module^.current_inputfile^.tempreopen;
                    end;
                end
              else
                begin
                { only reassemble ? }
                  if (hp^.do_assemble) then
                    OnlyAsm(hp^.asmfilename^);
                 { we should know there the PPU file else it's an error and
                   we can't load the unit }
{$ifdef NEWPPU}
{                  if hp^.ppufile^.name^<>'' then}
{$else}
                  if hp^.ppufile^.name^<>'' then
{$endif}
                    load_ppu(old_current_module,hp,compile_system);
                 { add the files for the linker }
                  addlinkerfiles(hp);
                end;
              { register the unit _once_ }
              usedunits.concat(new(pused_unit,init(hp,0)));
              { the unit is written, so we can set the symtable type }
              { to unitsymtable, else we get some dupid errors       }
              { this is not the right place because of the           }
              { ready label                                          }
              { psymtable(hp^.symtable)^.symtabletype:=unitsymtable; }
              { placed at this end of proc_unit                      }
              psymtable(hp^.symtable)^.unitid:=0;
              { reset the unitnumbers for the other units }
              pu:=pused_unit(old_current_module^.used_units.first);
              while assigned(pu) do
                begin
                   psymtable(pu^.u^.symtable)^.unitid:=pu^.unitid;
                   pu:=pused_unit(pu^.next);
                end;
           end
         else
           if assigned(hp) and (st=nil) then
             begin
                { we have to compile the unit again, but it is already inserted !!}
                { we may have problem with the lost symtable !! }
                current_module:=hp;
                { we must preserve the unit chain }
                nextmodule:=pmodule(hp^.next);
                { we have to cleanup a little }
                hp^.special_done;
                new(hs);
                hs^:=hp^.mainsource^;
                hp^.init(hs^,true);
                dispose(hs);
                { we must preserve the unit chain }
                hp^.next:=nextmodule;
                if assigned(hp^.ppufile) then
                 load_ppu(old_current_module,hp,compile_system)
                else
                 begin
{$ifdef UseBrowser}
                    { here we need to remove the names ! }
                    hp^.sourcefiles.done;
                    hp^.sourcefiles.init;
{$endif not UseBrowser}
                   if assigned(old_current_module^.current_inputfile) then
                     old_current_module^.current_inputfile^.tempclose;
                   Message1(parser_d_compiling_second_time,hp^.mainsource^);
                   compile(hp^.mainsource^,compile_system);
                   if (not old_current_module^.compiled) and assigned(old_current_module^.current_inputfile) then
                     old_current_module^.current_inputfile^.tempreopen;
                 end;
                current_module^.compiled:=true;
             end;
         { set the old module }
         current_module:=old_current_module;
         { the current module uses the unit hp }
         current_module^.used_units.concat(new(pused_unit,init(hp,0)));
         pused_unit(current_module^.used_units.last)^.in_uses:=in_uses;
         if in_uses and not current_module^.in_implementation then
           pused_unit(current_module^.used_units.last)^.in_interface:=true;
         loadunit:=hp;
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
           hp2:=loadunit(s,false,true);
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

         { now insert the units in the symtablestack }
          hp:=pused_unit(current_module^.used_units.first);
         { set the symtable to systemunit so it gets reorderd correctly }
         symtablestack:=systemunit;
         while assigned(hp) do
           begin
{$IfDef GDB}
              if (cs_debuginfo in aktswitches) and
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
         names:Tstringcontainer;
         p : psymtable;
         unitst : punitsymtable;
         pu     : pused_unit;
         s1,s2  : ^string; {Saves stack space}
      begin
         consume(_UNIT);

         if token=ID then
          begin
          { create filenames and unit name }
             current_module^.SetFileName(current_module^.current_inputfile^.path^,current_module^.current_inputfile^.name^);
             stringdispose(current_module^.modulename);
             current_module^.modulename:=stringdup(upper(pattern));

          { check for system unit }
             new(s1);
             new(s2);
             s1^:=upper(target_info.system_unit);
             s2^:=upper(current_module^.current_inputfile^.name^);
             if (cs_compilesystem in aktswitches)  then
              begin
                if (cs_check_unit_name in aktswitches) and
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

          { Add Object File }
             if (cs_smartlink in aktswitches) then
              current_module^.linkstaticlibs.insert(current_module^.libfilename^)
             else
              current_module^.linkofiles.insert(current_module^.objfilename^);
          end;

         consume(ID);
         consume(SEMICOLON);
         consume(_INTERFACE);

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
           begin
              loaded_units.insert(current_module);
              if cs_unit_to_lib in initswitches then
                begin
                current_module^.flags:=current_module^.flags or uf_in_library;
                if cs_shared_lib in initswitches then
                  current_module^.flags:=current_module^.flags or uf_shared_library;
                end;
           end;


         { insert qualifier for the system unit (allows system.writeln) }
         if not(cs_compilesystem in aktswitches) then
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
         refsymtable^.number_defs;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktswitches) then
           begin
              { all types }
              punitsymtable(refsymtable)^.concattypestabto(debuglist);
              { and all local symbols}
              refsymtable^.concatstabto(debuglist);
           end;
{$endif GDB}
         { for interdependent units
         the crc is included in the ppufile
         but it is not known when writing the first ppufile
         so I tried to add a fake writing of the ppu
         just to get the CRC
         but the result is different for the real CRC
         it calculates after, I don't know why

         Answer:
         -------
         When reading the interface part, the compiler assumes
         that all registers are modified by a procedure
         usedinproc:=$ff !
         If the definition is read, the compiler determines
         the used registers and write the correct value
         to usedinproc

         only_calculate_crc:=true;
         writeunitas(current_module^.current_inputfile^.path^+current_module^.current_inputfile^.name^+
                     +'.PPS',punitsymtable(symtablestack));
         only_calculate_crc:=false;
         }
         { generates static symbol table }
         p:=new(punitsymtable,init(staticsymtable,current_module^.modulename^));
         { must be done only after _USES !! (PM)
         refsymtable:=p;}

         {Generate a procsym.}
         aktprocsym:=new(Pprocsym,init(current_module^.modulename^+'_init'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or pounitinit;
         aktprocsym^.definition^.setmangledname(current_module^.modulename^+'_init');

         {The generated procsym has a local symtable. Discard it and turn
          it into the static one.}
         dispose(aktprocsym^.definition^.localst,done);
         aktprocsym^.definition^.localst:=p;


         { testing !!!!!!!!! }
         { we set the interface part as a unitsymtable  }
         { for the case we need to compile another unit }

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst^.next;

         parse_implementation_uses(unitst);
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

         {Reset the codegenerator.}
         codegen_newprocedure;

         names.init;
         names.insert(current_module^.modulename^+'_init');
         names.insert('INIT$$'+current_module^.modulename^);
         compile_proc_body(names,true,false);
         names.done;

         codegen_doneprocedure;

         consume(POINT);


         { size of the static data }
         datasize:=symtablestack^.datasize;

         { unsed static symbols ? }
         symtablestack^.allsymbolsused;

{$ifdef GDB}
         { add all used definitions even for implementation}
         if (cs_debuginfo in aktswitches) then
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
         { finish asmlist by adding segment starts }
         insertsegment;
      end;



    procedure proc_program(islibrary : boolean);

      var
         st    : psymtable;
         names : Tstringcontainer;
      begin
         { Trying to compile the system unit... }
         { if no unit defined... then issue a   }
         { fatal error (avoids pointer problems)}
         { when referencing the non-existant    }
         { system unit.                         }

         { System Unit should be compiled using proc_unit !! (PFV) }
{         if (cs_compilesystem in aktswitches) then
         Begin
           if token<>_UNIT then
            Message1(scan_f_syn_expected,'UNIT');
           consume(_UNIT);
         end;}

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
         aktprocsym:=new(Pprocsym,init('main'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poproginit;
         aktprocsym^.definition^.setmangledname(target_os.Cprefix+'main');
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

         if (cs_smartlink in aktswitches) then
          current_module^.linkstaticlibs.insert(current_module^.libfilename^)
         else
          current_module^.linkofiles.insert(current_module^.objfilename^);

         insertheap;
         inserttargetspecific;

         datasize:=symtablestack^.datasize;

         { finish asmlist by adding segment starts }
         insertsegment;
      end;

end.
{
  $Log$
  Revision 1.20  1998-06-04 09:55:42  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Come test_funcret improvements (not yet working)S: ----------------------------------------------------------------------

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
