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
       { parser specific stuff }
       ,pbase,pdecl,pstatmnt,psub
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

    function loadunit(const s : string;compile_system, in_uses : boolean) : pmodule;
    procedure proc_unit;
    procedure proc_program(islibrary : boolean);

  implementation

    uses
       parser;

    {$I innr.inc}

    procedure insertinternsyms(p : psymtable);

      begin
         p^.insert(new(psyssym,init('CONCAT',in_concat_x)));
         p^.insert(new(psyssym,init('WRITE',in_write_x)));
         p^.insert(new(psyssym,init('WRITELN',in_writeln_x)));
         p^.insert(new(psyssym,init('ASSIGNED',in_assigned_x)));
         p^.insert(new(psyssym,init('READ',in_read_x)));
         p^.insert(new(psyssym,init('READLN',in_readln_x)));
         p^.insert(new(psyssym,init('OFS',in_ofs_x)));
         p^.insert(new(psyssym,init('SIZEOF',in_sizeof_x)));
         p^.insert(new(psyssym,init('TYPEOF',in_typeof_x)));
         p^.insert(new(psyssym,init('LOW',in_low_x)));
         p^.insert(new(psyssym,init('HIGH',in_high_x)));
         p^.insert(new(psyssym,init('SEG',in_seg_x)));
         p^.insert(new(psyssym,init('ORD',in_ord_x)));
         p^.insert(new(psyssym,init('PRED',in_pred_x)));
         p^.insert(new(psyssym,init('SUCC',in_succ_x)));

         { for testing purpose }
         p^.insert(new(psyssym,init('DECI',in_dec_x)));
         p^.insert(new(psyssym,init('INCI',in_inc_x)));
         p^.insert(new(psyssym,init('STR',in_str_x_string)));
      end;

    procedure load_ppu(hp : pmodule;compile_system : boolean);

      var
         loaded_unit  : pmodule;
         b            : byte;
         checksum,
         count,
         nextmapentry : longint;
         hs           : string;
      begin
         { init the map }
         new(hp^.map);
         nextmapentry:=1;

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
              if (loaded_unit^.crc<>checksum) or
                 (do_build and loaded_unit^.sources_avail) then
                begin
                   { we have to compile the current unit }
                   { remove stuff which isn't needed     }
                   { forget the map }
                   dispose(hp^.map);
                   hp^.map:=nil;
                   hp^.ppufile^.close;
                   dispose(hp^.ppufile,done);
                   hp^.ppufile:=nil;
                   compile(hp^.mainsource^,compile_system);
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
         hp^.symtable:=new(punitsymtable,load(hp^.unitname^));

         { if this is the system unit insert the intern }
         { symbols                                      }
         if compile_system then
           insertinternsyms(psymtable(hp^.symtable));

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
              if {(loaded_unit^.crc<>checksum) or}
                (do_build and loaded_unit^.sources_avail) then
                begin
                   { we have to compile the current unit }
                   { remove stuff which isn't needed     }
                   { forget the map }
                   dispose(hp^.map);
                   hp^.map:=nil;
                   hp^.ppufile^.close;
                   dispose(hp^.ppufile,done);
                   hp^.ppufile:=nil;
                   compile(hp^.mainsource^,compile_system);
                   exit;
                end;
              { read until ibend }
              hp^.ppufile^.read_data(b,1,count);
           end;
         hp^.ppufile^.close;
         dispose(hp^.map);
         hp^.map:=nil;
      end;


    function loadunit(const s : string;compile_system, in_uses : boolean) : pmodule;

      var
         st : punitsymtable;
         old_current_module,hp,nextmodule : pmodule;
         pu : pused_unit;
         a  : pasmfile;
         hs : pstring;
         i  : longint;
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
              if hp^.unitname^=s then
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
                    Message1(unit_f_cant_compile_unit,hp^.unitname^)
                   else
                    compile(hp^.mainsource^,compile_system);
                end
              else
                begin
                { only reassemble ? }
                  if (hp^.do_assemble) then
                   begin
                     a:=new(PAsmFile,Init(hp^.asmfilename^));
                     a^.DoAssemble;
                     dispose(a,Done);
                   end;
                 { we should know there the PPU file else it's an error and
                   we can't load the unit }
                  if hp^.ppufile^.name^<>'' then
                    begin
                      if (hp^.flags and uf_in_library)=0 then
                       Linker.AddObjectFile(hp^.objfilename^);
                      load_ppu(hp,compile_system);
                    end;
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
                 load_ppu(hp,compile_system)
                else
                 begin
                   Message1(parser_d_compiling_second_time,hp^.mainsource^);
                   compile(hp^.mainsource^,compile_system);
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

      procedure parse_uses(symt:Psymtable);

      begin
         if token=_USES then
           begin
              current_module^.in_implementation:=true;
              symt^.symtabletype:=unitsymtable;
              loadunits;
              symt^.symtabletype:=globalsymtable;
{$ifdef DEBUG}
              test_symtablestack;
{$endif DEBUG}
           end;
      end;

    procedure proc_unit;

      var
         unitname : stringid;
{$ifdef GDB}
         { several defs to simulate more or less C++ objects for GDB }
         vmtdef : precdef;
         pvmtdef : ppointerdef;
         vmtarraydef : parraydef;
         vmtsymtable : psymtable;
{$endif GDB}
         names:Tstringcontainer;
         p : psymtable;
         unitst : punitsymtable;
         pu : pused_unit;
         { the output ppufile is written to this path }
         s1,s2,s3:^string; {Saves stack space, but only eats heap
                            space when there is a lot of heap free.}

      begin
         consume(_UNIT);

         stringdispose(current_module^.objfilename);
         stringdispose(current_module^.ppufilename);
       { create filenames and check unit name }
         new(s1);
         new(s2);
         new(s3);
         s1^:=FixFileName(current_module^.current_inputfile^.path^+current_module^.current_inputfile^.name^);
         current_module^.objfilename:=stringdup(s1^+target_info.objext);
         current_module^.ppufilename:=stringdup(s1^+target_info.unitext);

         s1^:=upper(pattern);
         s2^:=upper(target_info.system_unit);
         s3^:=upper(current_module^.current_inputfile^.name^);
         if (cs_compilesystem in aktswitches)  then
          begin
            if (cs_check_unit_name in aktswitches) and
               ((length(pattern)>8) or (s1^<>s2^) or (s1^<>s3^)) then
                Message1(unit_e_illegal_unit_name,s1^);
          end
         else
          if (s1^=s2^) then
           Message(unit_w_switch_us_missed);
         dispose(s3);
         dispose(s2);
         dispose(s1);

       { add object }
         Linker.AddObjectFile(current_module^.objfilename^);

         unitname:=pattern;

         consume(ID);
         consume(SEMICOLON);
         consume(_INTERFACE);

         { this should be placed after uses !!}
{$ifndef UseNiceNames}
         procprefix:='_'+unitname+'$$';
{$else UseNiceNames}
         procprefix:='_'+tostr(length(unitname))+lowercase(unitname)+'_';
{$endif UseNiceNames}

         parse_only:=true;

         { generate now the global symboltable }
         p:=new(punitsymtable,init(globalsymtable,unitname));
         refsymtable:=p;
         unitst:=punitsymtable(p);

         { set the symbol table for the current unit }
         { this must be set later for interdependency }
         { current_module^.symtable:=psymtable(p); }

         { a unit compiled at command line must be inside the loaded_unit list }
         if (compile_level=1) then
           begin
              current_module^.unitname:=stringdup(unitname);
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
              p^.insert(new(ptypesym,init('longint',s32bitdef)));
              p^.insert(new(ptypesym,init('ulong',u32bitdef)));
              p^.insert(new(ptypesym,init('void',voiddef)));
              p^.insert(new(ptypesym,init('char',cchardef)));
{$ifdef i386}
              p^.insert(new(ptypesym,init('s64real',c64floatdef)));
{$endif i386}
              p^.insert(new(ptypesym,init('s80real',s80floatdef)));
              p^.insert(new(ptypesym,init('cs32fixed',s32fixeddef)));
              p^.insert(new(ptypesym,init('byte',u8bitdef)));
              p^.insert(new(ptypesym,init('string',cstringdef)));
              p^.insert(new(ptypesym,init('word',u16bitdef)));
              p^.insert(new(ptypesym,init('boolean',booldef)));
              p^.insert(new(ptypesym,init('void_pointer',voidpointerdef)));
              p^.insert(new(ptypesym,init('file',cfiledef)));
{$ifdef i386}
              p^.insert(new(ptypesym,init('REAL',new(pfloatdef,init(s64real)))));
              p^.insert(new(ptypesym,init('COMP',new(pfloatdef,init(s64bit)))));
              p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s80real)))));
{$endif}
{$ifdef m68k}
              { internal definitions }
              p^.insert(new(ptypesym,init('s32real',c64floatdef)));
              { mappings... }
              p^.insert(new(ptypesym,init('REAL',new(pfloatdef,init(s32real)))));
              if (cs_fp_emulation) in aktswitches then
                   p^.insert(new(ptypesym,init('DOUBLE',new(pfloatdef,init(s32real)))))
              else
                   p^.insert(new(ptypesym,init('DOUBLE',new(pfloatdef,init(s64real)))));
{              p^.insert(new(ptypesym,init('COMP',new(pfloatdef,init(s32real)))));}
              if (cs_fp_emulation) in aktswitches then
                   p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s32real)))))
              else
                   p^.insert(new(ptypesym,init('EXTENDED',new(pfloatdef,init(s80real)))));
{$endif}
              p^.insert(new(ptypesym,init('SINGLE',new(pfloatdef,init(s32real)))));
              p^.insert(new(ptypesym,init('POINTER',new(ppointerdef,init(voiddef)))));
              p^.insert(new(ptypesym,init('STRING',cstringdef)));
              p^.insert(new(ptypesym,init('BOOLEAN',new(porddef,init(bool8bit,0,1)))));
              p^.insert(new(ptypesym,init('CHAR',new(porddef,init(uchar,0,255)))));
              p^.insert(new(ptypesym,init('TEXT',new(pfiledef,init(ft_text,nil)))));
              p^.insert(new(ptypesym,init('CARDINAL',new(porddef,init(u32bit,0,$ffffffff)))));
              p^.insert(new(ptypesym,init('FIXED',new(pfloatdef,init(f32bit)))));
              p^.insert(new(ptypesym,init('FIXED16',new(pfloatdef,init(f16bit)))));
              p^.insert(new(ptypesym,init('TYPEDFILE',new(pfiledef,init(ft_typed,voiddef)))));
              { !!!!!
              p^.insert(new(ptypesym,init('COMP',new(porddef,init(s64bit,0,0)))));
              p^.insert(new(ptypesym,init('SINGLE',new(porddef,init(s32real,0,0)))));
              p^.insert(new(ptypesym,init('EXTENDED',new(porddef,init(s80real,0,0)))));
              p^.insert(new(ptypesym,init('FILE',new(pfiledef,init(ft_untyped,nil)))));
              }
              { Add a type for virtual method tables in lowercase }
              { so it isn't reachable!                            }
{$ifdef GDB}
              vmtsymtable:=new(psymtable,init(recordsymtable));
              vmtdef:=new(precdef,init(vmtsymtable));
              pvmtdef:=new(ppointerdef,init(vmtdef));
              vmtsymtable^.insert(new(pvarsym,init('parent',pvmtdef)));
              vmtsymtable^.insert(new(pvarsym,init('length',globaldef('longint'))));
              vmtsymtable^.insert(new(pvarsym,init('mlength',globaldef('longint'))));
              vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
              vmtarraydef^.definition := voidpointerdef;
              vmtsymtable^.insert(new(pvarsym,init('__pfn',vmtarraydef)));
              p^.insert(new(ptypesym,init('__vtbl_ptr_type',vmtdef)));
              p^.insert(new(ptypesym,init('pvmt',pvmtdef)));
              vmtarraydef:=new(parraydef,init(0,1,s32bitdef));
              vmtarraydef^.definition := pvmtdef;
              p^.insert(new(ptypesym,init('vtblarray',vmtarraydef)));
              insertinternsyms(p);
{$endif GDB}
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
         p:=new(punitsymtable,init(staticsymtable,unitname));
         refsymtable:=p;

         {Generate a procsym.}
         aktprocsym:=new(Pprocsym,init(unitname+'_init'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or pounitinit;
         aktprocsym^.definition^.setmangledname(unitname+'_init');

         {The generated procsym has a local symtable. Discard it and turn
          it into the static one.}
         dispose(aktprocsym^.definition^.localst,done);
         aktprocsym^.definition^.localst:=p;

         names.init;
         names.insert(unitname+'_init');

         { testing !!!!!!!!! }
         { we set the interface part as a unitsymtable  }
         { for the case we need to compile another unit }

         { remove the globalsymtable from the symtable stack }
         { to reinsert it after loading the implementation units }
         symtablestack:=unitst^.next;

         parse_uses(unitst);

         { duplicated here to be sure }
{$ifndef UseNiceNames}
         procprefix:='_'+unitname+'$$';
{$else UseNiceNames}
         procprefix:='_'+tostr(length(unitname))+lowercase(unitname)+'_';
{$endif UseNiceNames}

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
{$endif Splitheap}

{$ifdef Splitheap}
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

         names.insert('INIT$$'+unitname);

         compile_proc_body(names,true,false);

         codegen_doneprocedure;

         consume(POINT);

         names.done;

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
         if errorcount=0 then
          writeunitas(current_module^.ppufilename^,punitsymtable(symtablestack));

         pu:=pused_unit(usedunits.first);
         while assigned(pu) do
           begin
              punitsymtable(pu^.u^.symtable)^.is_stab_written:=false;
              pu:=pused_unit(pu^.next);
           end;
         inc(datasize,symtablestack^.datasize);
      end;

    procedure proc_program(islibrary : boolean);

      var
         i : longint;
         st : psymtable;
         programname : stringid;
         names:Tstringcontainer;
      begin
         { Trying to compile the system unit... }
         { if no unit defined... then issue a   }
         { fatal error (avoids pointer problems)}
         { when referencing the non-existant    }
         { system unit.                         }
         if (cs_compilesystem in aktswitches) then
         Begin
           if token<>_UNIT then
            Message1(scan_f_syn_expected,'UNIT');
           consume(_UNIT);
         end;

         parse_only:=false;
         programname:='';

         if islibrary then
           begin
              consume(_LIBRARY);
              programname:=pattern;
              consume(ID);
              consume(SEMICOLON);
           end
         else
           { is there an program head ? }
           if token=_PROGRAM then
           begin
              consume(_PROGRAM);
              programname:=pattern;
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
         st:=new(punitsymtable,init(staticsymtable,programname));

         {Generate a procsym.}
         aktprocsym:=new(Pprocsym,init('program_init'));
         aktprocsym^.definition:=new(Pprocdef,init);
         aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poproginit;
         aktprocsym^.definition^.setmangledname('program_init');
         {The localst is a local symtable. Change it into the static
          symtable.}
         dispose(aktprocsym^.definition^.localst,done);
         aktprocsym^.definition^.localst:=st;

         names.init;
         names.insert('program_init');

         refsymtable:=st;

         {Insert the symbols of the system unit into the stack of symbol
          tables.}
         symtablestack:=systemunit;
         systemunit^.next:=nil;
         refsymtable^.insert(new(punitsym,init('SYSTEM',systemunit)));

         {Load the units used by the program we compile.}
         if token=_USES then loadunits;

         {Insert the name of the main program into the symbol table.}
         if programname<>'' then
           st^.insert(new(pprogramsym,init(programname)));

         { ...is also constsymtable, this is the symtable where }
         { the elements of enumeration types are inserted       }
         constsymtable:=st;

         codegen_newprocedure;

         { set some informations about the main program }
         procinfo.retdef:=voiddef;
         procinfo._class:=nil;
         procinfo.call_offset:=8;

         {Set the framepointer of the program initialization to the
          default framepointer (EBP on i386).}
         procinfo.framepointer:=frame_pointer;

         { clear flags }
         procinfo.flags:=0;

         procprefix:='';
         in_except_block:=false;


         {The program intialization needs an alias, so it can be called
          from the bootstrap code.}
         case target_info.target of
            target_GO32V1,
            target_GO32V2,
            target_OS2,
            target_WIN32:
              names.insert('_main');
            target_LINUX:
              names.insert('main');
         end;
         names.insert('PASCALMAIN');

         compile_proc_body(names,true,false);

         codegen_doneprocedure;

         Linker.AddObjectFile(current_module^.unitname^);
         current_module^.linkofiles.insert(current_module^.unitname^);

        { On the Macintosh Classic M68k Architecture   }
        { The Heap variable is simply a POINTER to the }
        { real HEAP. The HEAP must be set up by the RTL }
        { and must store the pointer in this value.    }
         if (target_info.target = target_MAC68k) then
          bsssegment^.concat(new(pai_datablock,init_global('HEAP',4)))
         else
          bsssegment^.concat(new(pai_datablock,init_global('HEAP',heapsize)));
         if target_info.target=target_GO32V2 then
           begin
              { stacksize can be specified }
              datasegment^.concat(new(pai_symbol,init_global('__stklen')));
              datasegment^.concat(new(pai_const,init_32bit(stacksize)));
           end;
         if (target_info.target=target_WIN32) then
           begin
              { generate the last entry for the imports directory }
              if not(assigned(importssection)) then
                importssection:=new(paasmoutput,init);
              { $3 ensure that it is the last entry, all other entries }
              { are written to $2                                      }
              importssection^.concat(new(pai_section,init('.idata$3')));
              for i:=1 to 5 do
                importssection^.concat(new(pai_const,init_32bit(0)));
           end;

         {I prefer starting with a heapsize of 256K in OS/2. The heap can
          grow later until the size specified on the command line. Allocating
          four megs at once can hurt performance when more programs are in
          memory.}
         datasegment^.concat(new(pai_symbol,init_global('HEAPSIZE')));
         if target_info.target=target_OS2 then
          heapsize:=256*1024;
         datasegment^.concat(new(pai_const,init_32bit(heapsize)));
         datasize:=symtablestack^.datasize;

         names.done;

         consume(POINT);

         symtablestack^.check_forwards;
         symtablestack^.allsymbolsused;
      end;

end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.43  1998/03/20 23:31:34  florian
    * bug0113 fixed
    * problem with interdepened units fixed ("options.pas problem")
    * two small extensions for future AMD 3D support

  Revision 1.42  1998/03/11 22:22:52  florian
    * Fixed circular unit uses, when the units are not in the current dir (from Peter)
    * -i shows correct info, not <lf> anymore (from Peter)
    * linking with shared libs works again (from Peter)

  Revision 1.41  1998/03/10 17:19:29  peter
    * fixed bug0108
    * better linebreak scanning (concentrated in nextchar(), it supports
      #10, #13, #10#13, #13#10

  Revision 1.40  1998/03/10 16:27:42  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.39  1998/03/10 01:17:24  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.38  1998/03/05 22:43:50  florian
    * some win32 support stuff added

  Revision 1.37  1998/03/04 01:35:08  peter
    * messages for unit-handling and assembler/linker
    * the compiler compiles without -dGDB, but doesn't work yet
    + -vh for Hint

  Revision 1.36  1998/03/03 23:18:45  florian
    * ret $8 problem with unit init/main program fixed

  Revision 1.35  1998/03/02 13:38:48  peter
    + importlib object
    * doesn't crash on a systemunit anymore
    * updated makefile and depend

  Revision 1.34  1998/03/02 01:49:04  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.33  1998/03/01 22:46:20  florian
    + some win95 linking stuff
    * a couple of bugs fixed:
      bug0055,bug0058,bug0059,bug0064,bug0072,bug0093,bug0095,bug0098

  Revision 1.32  1998/02/28 09:30:58  florian
    + writing of win32 import section added

  Revision 1.31  1998/02/28 03:57:08  carl
    + replaced target_info.short_name by target_info.target (a bit faster)

  Revision 1.30  1998/02/27 09:25:58  daniel
  * Changed symtable handling so no junk symtable is put on the symtablestack.

  Revision 1.28  1998/02/24 14:20:54  peter
    + tstringcontainer.empty
    * ld -T option restored for linux
    * libraries are placed before the objectfiles in a .PPU file
    * removed 'uses link' from files.pas

  Revision 1.27  1998/02/24 00:19:19  peter
    * makefile works again (btw. linux does like any char after a \ )
    * removed circular unit with assemble and files
    * fixed a sigsegv in pexpr
    * pmodule init unit/program is the almost the same, merged them

  Revision 1.26  1998/02/22 23:55:18  peter
    * small fix

  Revision 1.25  1998/02/22 23:03:28  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.24  1998/02/22 18:51:06  carl
    * where the heck did the HEAP for m68k go??????? REINSTATED

  Revision 1.23  1998/02/21 05:50:14  carl
    * bugfix of crash with Us switch

  Revision 1.22  1998/02/19 00:11:08  peter
    * fixed -g to work again
    * fixed some typos with the scriptobject

  Revision 1.21  1998/02/17 21:20:57  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.20  1998/02/16 12:51:38  michael
  + Implemented linker object

  Revision 1.19  1998/02/14 01:45:29  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.18  1998/02/13 22:26:37  peter
    * fixed a few SigSegv's
    * INIT$$ was not written for linux!
    * assembling and linking works again for linux and dos
    + assembler object, only attasmi3 supported yet
    * restore pp.pas with AddPath etc.

  Revision 1.17  1998/02/13 10:35:27  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.16  1998/02/12 17:19:22  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.15  1998/02/12 11:50:28  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.14  1998/01/30 17:31:26  pierre
    * bug of cyclic symtablestack fixed

  Revision 1.13  1998/01/28 13:48:49  michael
  + Initial implementation for making libs from within FPC. Not tested, as compiler does not run

  Revision 1.12  1998/01/19 15:46:25  peter
  * fixed INIT$$lowercase generation

  Revision 1.11  1998/01/19 09:32:28  michael
  * Shared Lib and GDB/RHIDE Bufixes from Peter Vreman.

  Revision 1.10  1998/01/17 01:57:39  michael
  + Start of shared library support. First working version.

  Revision 1.9  1998/01/16 18:03:17  florian
    * small bug fixes, some stuff of delphi styled constructores added

  Revision 1.8  1998/01/13 23:11:15  florian
    + class methods

  Revision 1.7  1998/01/13 17:13:09  michael
  * File time handling and file searching is now done in an OS-independent way,
    using the new file treating functions in globals.pas.

  Revision 1.6  1998/01/13 16:16:03  pierre
    *  bug in interdependent units handling
       - primary unit was not in loaded_units list
       - current_module^.symtable was assigned too early
       - donescanner must not call error if the compilation
       of the unit was done at a higher level.

  Revision 1.5  1998/01/12 13:03:32  florian
    + parsing of class methods implemented

  Revision 1.4  1998/01/11 10:54:24  florian
    + generic library support

  Revision 1.3  1998/01/11 04:17:36  carl
  + floating point support for m68k

  Revision 1.2  1998/01/09 09:10:01  michael
  + Initial implementation, second try

}
