{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    This unit handles the symbol tables

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
{$ifdef TP}
  {$N+,E+,F+,D-}
{$endif}
unit symtable;

  interface

    uses
{$ifdef TP}
{$ifndef Delphi}
       objects,
{$endif Delphi}
{$endif}
       strings,cobjects,
       globtype,globals,tokens,systems,verbose,
       symconst,
       aasm
       ,cpubase
       ,cpuinfo
{$ifdef GDB}
       ,gdb
{$endif}
       ;

{************************************************
           Some internal constants
************************************************}

   const
       hasharraysize    = 256;
  {$ifdef TP}
       indexgrowsize    = 16;
  {$else}
       indexgrowsize    = 64;
  {$endif}


{************************************************
            Needed forward pointers
************************************************}

    type
       { needed for owner (table) of symbol }
       psymtable     = ^tsymtable;
       punitsymtable = ^tunitsymtable;

       { needed for names by the definitions }
       ptypesym = ^ttypesym;
       penumsym = ^tenumsym;
       pprocsym = ^tprocsym;

       pref = ^tref;
       tref = object
         nextref     : pref;
         posinfo     : tfileposinfo;
         moduleindex : word;
         is_written  : boolean;
         constructor init(ref:pref;pos:pfileposinfo);
         destructor  done; virtual;
       end;

      { Deref entry options }
      tdereftype = (derefnil,derefaktrecordindex,derefaktstaticindex,
                    derefunit,derefrecord,derefindex,
                    dereflocal,derefpara);

      pderef = ^tderef;
      tderef = object
        dereftype : tdereftype;
        index     : word;
        next      : pderef;
        constructor init(typ:tdereftype;i:word);
        destructor  done;
      end;

      psymtableentry = ^tsymtableentry;
      tsymtableentry = object(tnamedindexobject)
        owner      : psymtable;
      end;

{************************************************
                    TDef
************************************************}

{$i symdefh.inc}

{************************************************
                   TSym
************************************************}

{$i symsymh.inc}

{************************************************
                 TSymtable
************************************************}

       tsymtabletype = (invalidsymtable,withsymtable,staticsymtable,
                        globalsymtable,unitsymtable,
                        objectsymtable,recordsymtable,
                        macrosymtable,localsymtable,
                        parasymtable,inlineparasymtable,
                        inlinelocalsymtable,stt_exceptsymtable,
                        { only used for PPU reading of static part
                          of a unit }
                        staticppusymtable);

       tcallback = procedure(p : psym);

       tsearchhasharray = array[0..hasharraysize-1] of psym;
       psearchhasharray = ^tsearchhasharray;

       tsymtable = object
          symtabletype : tsymtabletype;
          { each symtable gets a number }
          unitid    : word{integer give range check errors PM};
          name      : pstring;
          datasize  : longint;
          dataalignment : longint;
          symindex,
          defindex  : pindexarray;
          symsearch : pdictionary;
          next      : psymtable;
          defowner  : pdef; { for records and objects }
          { alignment used in this symtable }
          alignment : longint;
          { only used for parameter symtable to determine the offset relative }
          { to the frame pointer and for local inline }
          address_fixup : longint;
          { this saves all definition to allow a proper clean up }
          { separate lexlevel from symtable type }
          symtablelevel : byte;
          constructor init(t : tsymtabletype);
          destructor  done;virtual;
          { access }
          function getdefnr(l : longint) : pdef;
          function getsymnr(l : longint) : psym;
          { load/write }
          constructor load;
          procedure write;
          constructor loadas(typ : tsymtabletype);
          procedure writeas;
          procedure loaddefs;
          procedure loadsyms;
          procedure writedefs;
          procedure writesyms;
          procedure deref;
          procedure clear;
          function  rename(const olds,news : stringid):psym;
          procedure foreach(proc2call : tnamedindexcallback);
          function  insert(sym : psym):psym;
          function  search(const s : stringid) : psym;
          function  speedsearch(const s : stringid;speedvalue : longint) : psym;
          procedure registerdef(p : pdef);
          procedure allsymbolsused;
          procedure allunitsused;
          procedure check_forwards;
          procedure checklabels;
          { change alignment for args  only parasymtable }
          procedure set_alignment(_alignment : byte);
          { find arg having offset  only parasymtable }
          function  find_at_offset(l : longint) : pvarsym;
{$ifdef CHAINPROCSYMS}
          procedure chainprocsyms;
{$endif CHAINPROCSYMS}
          procedure load_browser;
          procedure write_browser;
{$ifdef BrowserLog}
          procedure writebrowserlog;
{$endif BrowserLog}
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          function getnewtypecount : word; virtual;
       end;

       tunitsymtable = object(tsymtable)
          unittypecount  : word;
          unitsym       : punitsym;
{$ifdef GDB}
          dbx_count : longint;
          prev_dbx_counter : plongint;
          dbx_count_ok : boolean;
          is_stab_written : boolean;
{$endif GDB}
          constructor init(t : tsymtabletype;const n : string);
          constructor loadasunit;
          destructor done;virtual;
          procedure writeasunit;
{$ifdef GDB}
          procedure concattypestabto(asmlist : paasmoutput);
{$endif GDB}
          procedure load_symtable_refs;
          function getnewtypecount : word; virtual;
       end;

       pwithsymtable = ^twithsymtable;
       twithsymtable = object(tsymtable)
          { used for withsymtable for allowing constructors }
          direct_with : boolean;
          { in fact it is a ptree }
          withnode : pointer;
          { ptree to load of direct with var }
          { already usable before firstwith
            needed for firstpass of function parameters PM }
          withrefnode : pointer;
          constructor init;
          destructor  done;virtual;
        end;

{****************************************************************************
                              Var / Consts
****************************************************************************}

    const
       systemunit           : punitsymtable = nil; { pointer to the system unit }
       current_object_option : tsymoptions = [sp_public];

    var
       { for STAB debugging }
       globaltypecount  : word;
       pglobaltypecount : pword;

       registerdef : boolean;      { true, when defs should be registered }

       defaultsymtablestack,       { symtablestack after default units
                                     have been loaded }
       symtablestack : psymtable;  { linked list of symtables }

       srsym : psym;           { result of the last search }
       srsymtable : psymtable;
       lastsrsym : psym;           { last sym found in statement }
       lastsrsymtable : psymtable;
       lastsymknown : boolean;

       constsymtable : psymtable;  { symtable were the constants can be
                                     inserted }

       voidpointerdef : ppointerdef; { pointer for Void-Pointerdef      }
       charpointerdef : ppointerdef; { pointer for Char-Pointerdef      }
       voidfarpointerdef : ppointerdef;

       cformaldef : pformaldef;    { unique formal definition     }
       voiddef   : porddef;     { Pointer to Void (procedure)       }
       cchardef  : porddef;     { Pointer to Char                  }
       booldef   : porddef;     { pointer to boolean type          }
       u8bitdef  : porddef;     { Pointer to 8-Bit unsigned      }
       u16bitdef : porddef;     { Pointer to 16-Bit unsigned    }
       u32bitdef : porddef;     { Pointer to 32-Bit unsigned    }
       s32bitdef : porddef;     { Pointer to 32-Bit signed        }

       cu64bitdef : porddef;       { pointer to 64 bit unsigned def }
       cs64bitdef : porddef;    { pointer to 64 bit signed def, }
                                   { calculated by the int unit on i386 }

       s32floatdef : pfloatdef;    { pointer for realconstn         }
       s64floatdef : pfloatdef;    { pointer for realconstn         }
       s80floatdef : pfloatdef;    { pointer to type of temp. floats   }
       s32fixeddef : pfloatdef;    { pointer to type of temp. fixed    }

       cshortstringdef : pstringdef;  { pointer to type of short string const   }
       clongstringdef  : pstringdef;  { pointer to type of long string const   }
       cansistringdef  : pstringdef;  { pointer to type of ansi string const  }
       cwidestringdef  : pstringdef;  { pointer to type of wide string const  }
       openshortstringdef : pstringdef;  { pointer to type of an open shortstring,
                                            needed for readln() }
       openchararraydef : parraydef;     { pointer to type of an open array of char,
                                            needed for readln() }

       cfiledef : pfiledef;       { get the same definition for all file }
                                  { uses for stabs }

       firstglobaldef,   { linked list of all globals defs }
       lastglobaldef : pdef;   { used to reset stabs/ranges }

       class_tobject : pobjectdef; { pointer to the anchestor of all   }
                                   { clases                         }
       pvmtdef     : ppointerdef;  { type of classrefs }

       aktprocsym : pprocsym;      { pointer to the symbol for the
                                     currently be parsed procedure }

       aktcallprocsym : pprocsym;  { pointer to the symbol for the
                                     currently be called procedure,
                                     only set/unset in firstcall }

       aktvarsym : pvarsym;     { pointer to the symbol for the
                                     currently read var, only used
                                     for variable directives }

       procprefix : string;     { eindeutige Namen bei geschachtel- }
                                   { ten Unterprogrammen erzeugen      }

       lexlevel : longint;       { level of code                     }
                                   { 1 for main procedure             }
                                   { 2 for normal function or proc     }
                                   { higher for locals           }
    const
       main_program_level = 1;
       unit_init_level = 1;
       normal_function_level = 2;
       in_loading : boolean = false;

{$ifdef i386}
       bestrealdef : ^pfloatdef = @s80floatdef;
{$endif}
{$ifdef m68k}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}
{$ifdef alpha}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}
{$ifdef powerpc}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}

    var

       macros : psymtable;       { pointer for die Symboltabelle mit  }
                                   { Makros                         }

       read_member : boolean;      { true, wenn Members aus einer PPU-  }
                                   { Datei gelesen werden, d.h. ein     }
                                   { varsym seine Adresse einlesen soll }

       generrorsym : psym;       { Jokersymbol, wenn das richtige    }
                                   { Symbol nicht gefunden wird }

       generrordef : pdef;       { Jokersymbol for eine fehlerhafte  }
                                   { Typdefinition                   }

       aktobjectdef : pobjectdef;  { used for private functions check !! }

    const
       { last operator which can be overloaded }
       first_overloaded = _PLUS;
       last_overloaded  = _ASSIGNMENT;
    var
       overloaded_operators : array[first_overloaded..last_overloaded] of pprocsym;
       { unequal is not equal}
    const
       overloaded_names : array [first_overloaded..last_overloaded] of string[16] =
         ('plus','minus','star','slash','equal',
          'greater','lower','greater_or_equal',
          'lower_or_equal','as','is','in','sym_diff',
          'starstar','assign');

{*** Unit aliases ***}

    type
       punit_alias = ^tunit_alias;
       tunit_alias = object(tnamedindexobject)
          newname : pstring;
          constructor init(const n:string);
          destructor  done;virtual;
       end;

    var
       unitaliases : pdictionary;

    procedure addunitalias(const n:string);
    function getunitalias(const n:string):string;


{****************************************************************************
                             Functions
****************************************************************************}

{*** Misc ***}
    function  globaldef(const s : string) : pdef;
    function  findunitsymtable(st:psymtable):psymtable;
    procedure duplicatesym(sym:psym);

{*** Search ***}
    function  search_a_symtable(const symbol:string;symtabletype:tsymtabletype):Psym;
    procedure getsym(const s : stringid;notfounderror : boolean);
    procedure getsymonlyin(p : psymtable;const s : stringid);

{*** PPU Write/Loading ***}
    procedure writeunitas(const s : string;unittable : punitsymtable;only_crc : boolean);
    procedure closecurrentppu;
    procedure numberunits;
    procedure load_interface;

{*** GDB ***}
{$ifdef GDB}
    function  typeglobalnumber(const s : string) : string;
{$endif}

{*** Definition ***}
   procedure reset_global_defs;

{*** Object Helpers ***}
    function search_class_member(pd : pobjectdef;const n : string) : psym;
    function search_default_property(pd : pobjectdef) : ppropertysym;

{*** Macro ***}
    procedure def_macro(const s : string);
    procedure set_macro(const s : string;value : string);

{*** symtable stack ***}
    procedure dellexlevel;
{$ifdef DEBUG}
    procedure test_symtablestack;
    procedure list_symtablestack;
{$endif DEBUG}

{*** Init / Done ***}
    procedure InitSymtable;
    procedure DoneSymtable;


implementation

  uses
     version,
     types,ppu,
     gendef,files
     ,tree
     ,cresstr
{$ifdef newcg}
     ,cgbase
{$else}
     ,hcodegen
{$endif}
{$ifdef BrowserLog}
     ,browlog
{$endif BrowserLog}
     ;

  var
     aktrecordsymtable : psymtable; { current record read from ppu symtable }
     aktstaticsymtable : psymtable; { current static for local ppu symtable }
{$ifdef GDB}
     asmoutput : paasmoutput;
{$endif GDB}
{$ifdef TP}
{$ifndef Delphi}
   {$ifndef dpmi}
       symbolstream : temsstream;  { stream which is used to store some info }
   {$else}
       symbolstream : tmemorystream;
   {$endif}
{$endif Delphi}
{$endif}

   {to dispose the global symtable of a unit }
  const
     dispose_global : boolean = false;
     memsizeinc = 2048; { for long stabstrings }
     tagtypes : Set of tdeftype =
       [recorddef,enumdef,
       {$IfNDef GDBKnowsStrings}
       stringdef,
       {$EndIf not GDBKnowsStrings}
       {$IfNDef GDBKnowsFiles}
       filedef,
       {$EndIf not GDBKnowsFiles}
       objectdef];

{*****************************************************************************
                             Helper Routines
*****************************************************************************}

{$ifdef unused}
    function demangledparas(s : string) : string;
      var
         r : string;
         l : longint;
      begin
         demangledparas:='';
         r:=',';
         { delete leading $$'s }
         l:=pos('$$',s);
         while l<>0 do
           begin
              delete(s,1,l+1);
              l:=pos('$$',s);
           end;
         { delete leading _$'s }
         l:=pos('_$',s);
         while l<>0 do
           begin
              delete(s,1,l+1);
              l:=pos('_$',s);
           end;
         l:=pos('$',s);
         if l=0 then
           exit;
         delete(s,1,l);
         while s<>'' do
          begin
            l:=pos('$',s);
            if l=0 then
             l:=length(s)+1;
            r:=r+copy(s,1,l-1)+',';
            delete(s,1,l);
          end;
         delete(r,1,1);
         delete(r,length(r),1);
         demangledparas:=r;
      end;
{$endif}


    procedure numberunits;
      var
        counter : longint;
        hp      : pused_unit;
        hp1     : pmodule;
      begin
        { Reset all numbers to -1 }
        hp1:=pmodule(loaded_units.first);
        while assigned(hp1) do
         begin
           if assigned(hp1^.globalsymtable) then
             psymtable(hp1^.globalsymtable)^.unitid:=$ffff;
           hp1:=pmodule(hp1^.next);
         end;
        { Our own symtable gets unitid 0, for a program there is
          no globalsymtable }
        if assigned(current_module^.globalsymtable) then
          psymtable(current_module^.globalsymtable)^.unitid:=0;
        { number units }
        counter:=1;
        hp:=pused_unit(current_module^.used_units.first);
        while assigned(hp) do
         begin
           psymtable(hp^.u^.globalsymtable)^.unitid:=counter;
           inc(counter);
           hp:=pused_unit(hp^.next);
         end;
      end;


    function findunitsymtable(st:psymtable):psymtable;
      begin
        findunitsymtable:=nil;
        repeat
          if not assigned(st) then
           internalerror(5566561);
          case st^.symtabletype of
            localsymtable,
            parasymtable,
            staticsymtable :
              break;
            globalsymtable,
            unitsymtable :
              begin
                findunitsymtable:=st;
                break;
              end;
            objectsymtable,
            recordsymtable :
              st:=st^.defowner^.owner;
            else
              internalerror(5566562);
          end;
        until false;
      end;


   procedure setstring(var p : pchar;const s : string);
     begin
{$ifndef Delphi}
{$ifdef TP}

       if use_big then
        begin
          p:=pchar(symbolstream.getsize);
          symbolstream.seek(longint(p));
          symbolstream.writestr(@s);
        end
       else
{$endif TP}
{$endif Delphi}
        p:=strpnew(s);
     end;


     procedure duplicatesym(sym:psym);
       var
         st : psymtable;
       begin
         Message1(sym_e_duplicate_id,sym^.name);
         st:=findunitsymtable(sym^.owner);
         if assigned(st) then
          begin
            with sym^.fileinfo do
             begin
               if st^.unitid=0 then
                Message2(sym_h_duplicate_id_where,current_module^.sourcefiles^.get_file_name(fileindex),tostr(line))
               else
                Message2(sym_h_duplicate_id_where,'unit '+st^.name^,tostr(line));
             end;
          end;
       end;


{****************************************************************************
                               TRef
****************************************************************************}

    constructor tref.init(ref :pref;pos : pfileposinfo);
      begin
        nextref:=nil;
        if pos<>nil then
          posinfo:=pos^;
        if assigned(current_module) then
          moduleindex:=current_module^.unit_index;
        if assigned(ref) then
          ref^.nextref:=@self;
        is_written:=false;
      end;


    destructor tref.done;
      var
         inputfile : pinputfile;
      begin
         inputfile:=get_source_file(moduleindex,posinfo.fileindex);
         if inputfile<>nil then
           dec(inputfile^.ref_count);
         if assigned(nextref) then
          dispose(nextref,done);
         nextref:=nil;
      end;


{****************************************************************************
                               TDeref
****************************************************************************}

    constructor tderef.init(typ:tdereftype;i:word);
      begin
        dereftype:=typ;
        index:=i;
        next:=nil;
      end;


    destructor tderef.done;
      begin
      end;


{*****************************************************************************
                           PPU Reading Writing
*****************************************************************************}

{$I symppu.inc}


{*****************************************************************************
                            Definition Helpers
*****************************************************************************}

    function globaldef(const s : string) : pdef;

      var st : string;
          symt : psymtable;
      begin
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           getsym(st,false);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym^.typ = unitsym then
               begin
               symt := punitsym(srsym)^.unitsymtable;
               srsym := symt^.search(st);
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then getsym(st,false);
         if srsym = nil then
           getsymonlyin(systemunit,st);
         if srsym^.typ<>typesym then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         globaldef := ptypesym(srsym)^.definition;
      end;

{*****************************************************************************
                        Symbol / Definition Resolving
*****************************************************************************}

    procedure resolvederef(var p:pderef;var st:psymtable;var idx:word);
      var
        hp : pderef;
        pd : pdef;
      begin
        st:=nil;
        idx:=0;
        while assigned(p) do
         begin
           case p^.dereftype of
             derefaktrecordindex :
               begin
                 st:=aktrecordsymtable;
                 idx:=p^.index;
               end;
             derefaktstaticindex :
               begin
                 st:=aktstaticsymtable;
                 idx:=p^.index;
               end;
             derefunit :
               begin
{$ifdef NEWMAP}
                 st:=psymtable(current_module^.map^[p^.index]^.globalsymtable);
{$else NEWMAP}
                 st:=psymtable(current_module^.map^[p^.index]);
{$endif NEWMAP}
               end;
             derefrecord :
               begin
                 pd:=st^.getdefnr(p^.index);
                 case pd^.deftype of
                   recorddef :
                     st:=precorddef(pd)^.symtable;
                   objectdef :
                     st:=pobjectdef(pd)^.symtable;
                 else
                   internalerror(556658);
                 end;
               end;
             dereflocal :
               begin
                  pd:=st^.getdefnr(p^.index);
                  case pd^.deftype of
                    procdef :
                      st:=pprocdef(pd)^.localst;
                    else
                   internalerror(556658);
                 end;
               end;
             derefpara :
               begin
                  pd:=st^.getdefnr(p^.index);
                  case pd^.deftype of
                    procdef :
                      st:=pprocdef(pd)^.parast;
                    else
                   internalerror(556658);
                 end;
               end;
             derefindex :
               begin
                 idx:=p^.index;
               end;
             else
               internalerror(556658);
           end;
           hp:=p;
           p:=p^.next;
           dispose(hp,done);
         end;
      end;


    procedure resolvedef(var def:pdef);
      var
        st   : psymtable;
        idx  : word;
      begin
        resolvederef(pderef(def),st,idx);
        if assigned(st) then
         def:=st^.getdefnr(idx)
        else
         def:=nil;
      end;

    procedure resolvesym(var sym:psym);
      var
        st   : psymtable;
        idx  : word;
      begin
        resolvederef(pderef(sym),st,idx);
        if assigned(st) then
         sym:=st^.getsymnr(idx)
        else
         sym:=nil;
      end;


{*****************************************************************************
                        Symbol Call Back Functions
*****************************************************************************}

    procedure derefsym(p : pnamedindexobject);
      begin
         psym(p)^.deref;
      end;

    procedure derefsymsdelayed(p : pnamedindexobject);
      begin
         if psym(p)^.typ in [absolutesym,propertysym] then
           psym(p)^.deref;
      end;

    procedure check_forward(sym : pnamedindexobject);
      begin
         if psym(sym)^.typ=procsym then
           pprocsym(sym)^.check_forward
         { check also object method table            }
         { we needn't to test the def list          }
         { because each object has to have a type sym }
         else
          if (psym(sym)^.typ=typesym) and
             assigned(ptypesym(sym)^.definition) and
             (ptypesym(sym)^.definition^.deftype=objectdef) then
           pobjectdef(ptypesym(sym)^.definition)^.check_forwards;
      end;

    procedure labeldefined(p : pnamedindexobject);
      begin
        if (psym(p)^.typ=labelsym) and
           not(plabelsym(p)^.defined) then
          Message1(sym_w_label_not_defined,p^.name);
      end;

    procedure unitsymbolused(p : pnamedindexobject);
      begin
         if (psym(p)^.typ=unitsym) and
            (punitsym(p)^.refs=0) then
           comment(V_info,'Unit '+p^.name+' is not used');
      end;

    procedure varsymbolused(p : pnamedindexobject);
      begin
         if (psym(p)^.typ=varsym) and
            ((psym(p)^.owner^.symtabletype in [parasymtable,localsymtable,staticsymtable])) then
           { unused symbol should be reported only if no }
           { error is reported                     }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (pvarsym(p)^.refs=0) and
              (Errorcount=0) and
              (copy(p^.name,1,3)<>'val') and
              (copy(p^.name,1,4)<>'high') then
             begin
                if (psym(p)^.owner^.symtabletype=parasymtable) or pvarsym(p)^.islocalcopy then
                  MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_not_used,p^.name)
                else
                  MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_not_used,p^.name);
             end;
      end;

{$ifdef GDB}
    procedure concatstab(p : pnamedindexobject);
      begin
        if psym(p)^.typ <> procsym then
          psym(p)^.concatstabto(asmoutput);
      end;

    procedure concattypestab(p : pnamedindexobject);
      begin
        if psym(p)^.typ = typesym then
         begin
           psym(p)^.isstabwritten:=false;
           psym(p)^.concatstabto(asmoutput);
         end;
      end;

    procedure forcestabto(asmlist : paasmoutput; pd : pdef);
      begin
        if not pd^.is_def_stab_written then
         begin
           if assigned(pd^.sym) then
            pd^.sym^.isusedinstab := true;
           pd^.concatstabto(asmlist);
         end;
      end;
{$endif}

{$ifdef CHAINPROCSYMS}
    procedure chainprocsym(p : psym);
      var
         storesymtablestack : psymtable;
      begin
         if p^.typ=procsym then
           begin
              storesymtablestack:=symtablestack;
              symtablestack:=p^.owner^.next;
              while assigned(symtablestack) do
                begin
                  { search for same procsym in other units }
                  getsym(p^.name,false);
                  if assigned(srsym) and (srsym^.typ=procsym) then
                    begin
                       pprocsym(p)^.nextprocsym:=pprocsym(srsym);
                       symtablestack:=storesymtablestack;
                       exit;
                    end
                  else if srsym=nil then
                    symtablestack:=nil
                  else
                    symtablestack:=srsymtable^.next;
                end;
              symtablestack:=storesymtablestack;
           end;
      end;
{$endif}

    procedure write_refs(sym : pnamedindexobject);
      begin
         psym(sym)^.write_references;
      end;

{$ifdef BrowserLog}
    procedure add_to_browserlog(sym : pnamedindexobject);
      begin
         psym(sym)^.add_to_browserlog;
      end;
{$endif UseBrowser}


{*****************************************************************************
                          Search Symtables for Syms
*****************************************************************************}

    procedure getsym(const s : stringid;notfounderror : boolean);
      var
        speedvalue : longint;
      begin
         speedvalue:=getspeedvalue(s);
         lastsrsym:=nil;
         srsymtable:=symtablestack;
         while assigned(srsymtable) do
           begin
              srsym:=srsymtable^.speedsearch(s,speedvalue);
              if assigned(srsym) then
                exit
              else
                srsymtable:=srsymtable^.next;
           end;
         if notfounderror then
           begin
              Message1(sym_e_id_not_found,s);
              srsym:=generrorsym;
           end
         else
           srsym:=nil;
      end;


    procedure getsymonlyin(p : psymtable;const s : stringid);
      begin
         { the caller have to take care if srsym=nil (FK) }
         srsym:=nil;
         if assigned(p) then
           begin
              srsymtable:=p;
              srsym:=srsymtable^.search(s);
              if assigned(srsym) then
                exit
              else
               begin
                  if (punitsymtable(srsymtable)=punitsymtable(current_module^.globalsymtable)) then
                    begin
                       getsymonlyin(psymtable(current_module^.localsymtable),s);
                       if assigned(srsym) then
                         srsymtable:=psymtable(current_module^.localsymtable)
                       else
                         Message1(sym_e_id_not_found,s);
                    end
                  else
                    Message1(sym_e_id_not_found,s);
               end;
           end;
      end;


    function search_a_symtable(const symbol:string;symtabletype:tsymtabletype):Psym;
    {Search for a symbol in a specified symbol table. Returns nil if
     the symtable is not found, and also if the symbol cannot be found
     in the desired symtable }
    var hsymtab:Psymtable;
        res:Psym;
    begin
        res:=nil;
        hsymtab:=symtablestack;
        while (hsymtab<>nil) and (hsymtab^.symtabletype<>symtabletype) do
            hsymtab:=hsymtab^.next;
        if hsymtab<>nil then
            {We found the desired symtable. Now check if the symbol we
             search for is defined in it }
            res:=hsymtab^.search(symbol);
        search_a_symtable:=res;
    end;


{****************************************************************************
                                TSYMTABLE
****************************************************************************}

    constructor tsymtable.init(t : tsymtabletype);
      begin
         symtabletype:=t;
         symtablelevel:=0;
         defowner:=nil;
         unitid:=0;
         next:=nil;
         name:=nil;
         address_fixup:=0;
         datasize:=0;
         dataalignment:=1;
         new(symindex,init(indexgrowsize));
         new(defindex,init(indexgrowsize));
         if symtabletype<>withsymtable then
           begin
              new(symsearch,init);
              symsearch^.noclear:=true;
           end
         else
           symsearch:=nil;
         alignment:=def_alignment;
      end;


    destructor tsymtable.done;
      begin
        stringdispose(name);
        dispose(symindex,done);
        dispose(defindex,done);
        { symsearch can already be disposed or set to nil for withsymtable }
        if assigned(symsearch) then
         begin
           dispose(symsearch,done);
           symsearch:=nil;
         end;
      end;


    constructor twithsymtable.init;
      begin
         inherited init(withsymtable);
         direct_with:=false;
         withnode:=nil;
         withrefnode:=nil;
      end;


    destructor twithsymtable.done;
      begin
        symsearch:=nil;
        inherited done;
      end;


{***********************************************
                Helpers
***********************************************}

   function tsymtable.getnewtypecount : word;
      begin
         getnewtypecount:=pglobaltypecount^;
         inc(pglobaltypecount^);
      end;

    procedure tsymtable.registerdef(p : pdef);
      begin
         defindex^.insert(p);
         { set def owner and indexnb }
         p^.owner:=@self;
      end;


    procedure tsymtable.foreach(proc2call : tnamedindexcallback);
      begin
        symindex^.foreach(proc2call);
      end;


{***********************************************
       LOAD / WRITE SYMTABLE FROM PPU
***********************************************}

    procedure tsymtable.loaddefs;
      var
        hp : pdef;
        b  : byte;
      begin
      { load start of definition section, which holds the amount of defs }
         if current_ppu^.readentry<>ibstartdefs then
          Message(unit_f_ppu_read_error);
         current_ppu^.getlongint;
      { read definitions }
         repeat
           b:=current_ppu^.readentry;
           case b of
              ibpointerdef : hp:=new(ppointerdef,load);
                ibarraydef : hp:=new(parraydef,load);
                  iborddef : hp:=new(porddef,load);
                ibfloatdef : hp:=new(pfloatdef,load);
                 ibprocdef : hp:=new(pprocdef,load);
          ibshortstringdef : hp:=new(pstringdef,shortload);
           iblongstringdef : hp:=new(pstringdef,longload);
           ibansistringdef : hp:=new(pstringdef,ansiload);
           ibwidestringdef : hp:=new(pstringdef,wideload);
               ibrecorddef : hp:=new(precorddef,load);
               ibobjectdef : hp:=new(pobjectdef,load);
                 ibenumdef : hp:=new(penumdef,load);
                  ibsetdef : hp:=new(psetdef,load);
              ibprocvardef : hp:=new(pprocvardef,load);
                 ibfiledef : hp:=new(pfiledef,load);
             ibclassrefdef : hp:=new(pclassrefdef,load);
               ibformaldef : hp:=new(pformaldef,load);
                 ibenddefs : break;
                     ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           hp^.owner:=@self;
           defindex^.insert(hp);
         until false;
      end;


    procedure tsymtable.loadsyms;
      var
        b   : byte;
        sym : psym;
      begin
      { load start of definition section, which holds the amount of defs }
         if current_ppu^.readentry<>ibstartsyms then
          Message(unit_f_ppu_read_error);
         { skip amount of symbols, not used currently }
         current_ppu^.getlongint;
         { load datasize,dataalignment of this symboltable }
         datasize:=current_ppu^.getlongint;
         dataalignment:=current_ppu^.getlongint;
      { now read the symbols }
         repeat
           b:=current_ppu^.readentry;
           case b of
                ibtypesym : sym:=new(ptypesym,load);
                ibprocsym : sym:=new(pprocsym,load);
               ibconstsym : sym:=new(pconstsym,load);
                 ibvarsym : sym:=new(pvarsym,load);
             ibfuncretsym : sym:=new(pfuncretsym,load);
            ibabsolutesym : sym:=new(pabsolutesym,load);
                ibenumsym : sym:=new(penumsym,load);
          ibtypedconstsym : sym:=new(ptypedconstsym,load);
            ibpropertysym : sym:=new(ppropertysym,load);
                ibunitsym : sym:=new(punitsym,load);
               iblabelsym : sym:=new(plabelsym,load);
                 ibsyssym : sym:=new(psyssym,load);
                ibendsyms : break;
                    ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
           sym^.owner:=@self;
           symindex^.insert(sym);
           symsearch^.insert(sym);
         until false;
      end;


    procedure tsymtable.writedefs;
      var
         pd : pdef;
      begin
      { each definition get a number, write then the amount of defs to the
         ibstartdef entry }
         current_ppu^.putlongint(defindex^.count);
         current_ppu^.writeentry(ibstartdefs);
      { now write the definition }
         pd:=pdef(defindex^.first);
         while assigned(pd) do
           begin
              pd^.write;
              pd:=pdef(pd^.next);
           end;
      { write end of definitions }
         current_ppu^.writeentry(ibenddefs);
      end;


    procedure tsymtable.writesyms;
      var
        pd : psym;
      begin
       { each definition get a number, write then the amount of syms and the
         datasize to the ibsymdef entry }
         current_ppu^.putlongint(symindex^.count);
         current_ppu^.putlongint(datasize);
         current_ppu^.putlongint(dataalignment);
         current_ppu^.writeentry(ibstartsyms);
       { foreach is used to write all symbols }
         pd:=psym(symindex^.first);
         while assigned(pd) do
           begin
              pd^.write;
              pd:=psym(pd^.next);
           end;
       { end of symbols }
         current_ppu^.writeentry(ibendsyms);
      end;


    procedure tsymtable.deref;
      var
        hp : pdef;
        hs : psym;
      begin
        hp:=pdef(defindex^.first);
        while assigned(hp) do
         begin
           hp^.deref;
           hp^.symderef;
           hp:=pdef(hp^.next);
         end;

        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.deref;
           hs:=psym(hs^.next);
         end;
      end;


    constructor tsymtable.load;
      var
         st_loading : boolean;
      begin
        st_loading:=in_loading;
        in_loading:=true;
{$ifndef NEWMAP}
        current_module^.map^[0]:=@self;
{$else NEWMAP}
        current_module^.globalsymtable:=@self;
{$endif NEWMAP}

        symtabletype:=unitsymtable;
        symtablelevel:=0;

        { unused for units }
        address_fixup:=0;

        datasize:=0;
        defowner:=nil;
        name:=nil;
        unitid:=0;
        defowner:=nil;
        new(symindex,init(indexgrowsize));
        new(defindex,init(indexgrowsize));
        new(symsearch,init);
        symsearch^.usehash;
        symsearch^.noclear:=true;
        alignment:=def_alignment;

      { load definitions }
        loaddefs;

      { load symbols }
        loadsyms;

      { Now we can deref the symbols and definitions }
        if not(symtabletype in [objectsymtable,recordsymtable]) then
          deref;

{$ifdef NEWMAP}
        { necessary for dependencies }
        current_module^.globalsymtable:=nil;
{$endif NEWMAP}
        in_loading:=st_loading;
      end;


    procedure tsymtable.write;
      begin
      { write definitions }
         writedefs;
      { write symbols }
         writesyms;
      end;


    constructor tsymtable.loadas(typ : tsymtabletype);
      var
         storesymtable : psymtable;
         st_loading : boolean;
      begin
         st_loading:=in_loading;
         in_loading:=true;
         symtabletype:=typ;
         new(symindex,init(indexgrowsize));
         new(defindex,init(indexgrowsize));
         new(symsearch,init);
         symsearch^.noclear:=true;
         defowner:=nil;
         storesymtable:=aktrecordsymtable;
         if typ in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=@self;
         { used for local browser }
         if typ=staticppusymtable then
           begin
              aktstaticsymtable:=@self;
              symsearch^.usehash;
           end;
         name:=nil;
         alignment:=def_alignment;
         { isn't used there }
         datasize:=0;
         address_fixup:= 0;
         { also unused }
         unitid:=0;

      { load definitions }
      { we need the correct symtable for registering }
         if not (typ in [recordsymtable,objectsymtable]) then
           begin
             next:=symtablestack;
             symtablestack:=@self;
           end;

      { load definitions }
         loaddefs;

      { load symbols }
         loadsyms;

      { now we can deref the syms and defs }
         if not (typ in [recordsymtable,objectsymtable]) then
           deref;

         aktrecordsymtable:=storesymtable;
         if not (typ in [recordsymtable,objectsymtable]) then
           begin
             symtablestack:=next;
           end;
        in_loading:=st_loading;
      end;


    procedure tsymtable.writeas;
      var
         oldtyp : byte;
         storesymtable : psymtable;
      begin
         oldtyp:=current_ppu^.entrytyp;
         storesymtable:=aktrecordsymtable;
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=@self;
         if (symtabletype in [recordsymtable,objectsymtable]) then
         current_ppu^.entrytyp:=subentryid;
         { write definitions }
         writedefs;
         { write symbols }
         writesyms;
         current_ppu^.entrytyp:=oldtyp;
         aktrecordsymtable:=storesymtable;
      end;


{***********************************************
          Get Symbol / Def by Number
***********************************************}

    function tsymtable.getsymnr(l : longint) : psym;
      var
        hp : psym;
      begin
        hp:=psym(symindex^.search(l));
        if hp=nil then
         internalerror(10999);
        getsymnr:=hp;
      end;

    function tsymtable.getdefnr(l : longint) : pdef;
      var
        hp : pdef;
      begin
        hp:=pdef(defindex^.search(l));
        if hp=nil then
         internalerror(10998);
        getdefnr:=hp;
      end;


{***********************************************
                Table Access
***********************************************}

    procedure tsymtable.clear;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
         recorddef  or objectdef symtable }
         if symtabletype=withsymtable then
           exit;
         symindex^.clear;
         defindex^.clear;
      end;


    function tsymtable.insert(sym:psym):psym;
      var
         hp : psymtable;
         hsym : psym;
      begin
         { set owner and sym indexnb }
         sym^.owner:=@self;
{$ifdef CHAINPROCSYMS}
         { set the nextprocsym field }
         if sym^.typ=procsym then
           chainprocsym(sym);
{$endif CHAINPROCSYMS}
         { writes the symbol in data segment if required }
         { also sets the datasize of owner             }
         if not in_loading then
           sym^.insert_in_data;
         if (symtabletype in [staticsymtable,globalsymtable]) then
           begin
              hp:=symtablestack;
              while assigned(hp) do
                begin
                   if hp^.symtabletype in [staticsymtable,globalsymtable] then
                    begin
                       hsym:=hp^.search(sym^.name);
                       if assigned(hsym) then
                         DuplicateSym(hsym);
                    end;
                  hp:=hp^.next;
                end;
           end;

         { check for duplicate id in local and parsymtable symtable }
         if (symtabletype=localsymtable) then
           { to be on the sure side: }
           begin
              if assigned(next) and
                (next^.symtabletype=parasymtable) then
                begin
                   hsym:=next^.search(sym^.name);
                   if assigned(hsym) then
                     DuplicateSym(hsym);
                end
              else if (current_module^.flags and uf_local_browser)=0 then
                internalerror(43789);
           end;

         { check for duplicate id in local symtable of methods }
         if (symtabletype=localsymtable) and
           assigned(next) and
           assigned(next^.next) and
          { funcretsym is allowed !! }
           (sym^.typ <> funcretsym) and
           (next^.next^.symtabletype=objectsymtable) then
           begin
              hsym:=search_class_member(pobjectdef(next^.next^.defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                (not(sp_private in hsym^.symoptions) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;
         { check for duplicate id in para symtable of methods }
         if (symtabletype=parasymtable) and
           assigned(procinfo^._class) and
         { but not in nested procedures !}
            (not(assigned(procinfo^.parent)) or
             (assigned(procinfo^.parent) and
              not(assigned(procinfo^.parent^._class)))
            ) and
          { funcretsym is allowed !! }
           (sym^.typ <> funcretsym) then
           begin
              hsym:=search_class_member(procinfo^._class,sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                (not(sp_private in hsym^.symoptions) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;
         { check for duplicate field id in inherited classes }
         if (sym^.typ=varsym) and
            (symtabletype=objectsymtable) and
            assigned(defowner) then
           begin
              hsym:=search_class_member(pobjectdef(defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                (not(sp_private in hsym^.symoptions) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;
         { register definition of typesym }
         if (sym^.typ = typesym) and
            assigned(ptypesym(sym)^.definition) then
          begin
            if not(assigned(ptypesym(sym)^.definition^.owner)) and
               (ptypesym(sym)^.definition^.deftype<>errordef) then
              registerdef(ptypesym(sym)^.definition);
{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) and
               (symtabletype in [globalsymtable,staticsymtable]) then
              begin
                ptypesym(sym)^.isusedinstab := true;
                sym^.concatstabto(debuglist);
              end;
{$endif GDB}
          end;
         { insert in index and search hash }
         symindex^.insert(sym);
         symsearch^.insert(sym);
         insert:=sym;
      end;


    function tsymtable.search(const s : stringid) : psym;
      begin
        {search:=psym(symsearch^.search(s));
         this bypasses the ref generation (PM) }
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function tsymtable.speedsearch(const s : stringid;speedvalue : longint) : psym;
      var
        hp : psym;
      begin
        hp:=psym(symsearch^.speedsearch(s,speedvalue));
        if assigned(hp) then
         begin
           { reject non static members in static procedures,
             be carefull aktprocsym^.definition is not allways
             loaded already (PFV) }
           if (symtabletype=objectsymtable) and
              not(sp_static in hp^.symoptions) and
              allow_only_static
              {assigned(aktprocsym) and
              assigned(aktprocsym^.definition) and
              ((aktprocsym^.definition^.options and postaticmethod)<>0)} then
                  Message(sym_e_only_static_in_static);
           if (symtabletype=unitsymtable) and
              assigned(punitsymtable(@self)^.unitsym) then
             inc(punitsymtable(@self)^.unitsym^.refs);
           { unitsym are only loaded for browsing PM    }
           { this was buggy anyway because we could use }
           { unitsyms from other units in _USES !!      }
           if (symtabletype=unitsymtable) and (hp^.typ=unitsym) and
              assigned(current_module) and (current_module^.globalsymtable<>@self) then
             hp:=nil;
           if assigned(hp) and
              (cs_browser in aktmoduleswitches) and make_ref then
             begin
                hp^.lastref:=new(pref,init(hp^.lastref,@tokenpos));
                { for symbols that are in tables without
                browser info or syssyms (PM) }
                if hp^.refcount=0 then
                  hp^.defref:=hp^.lastref;
                inc(hp^.refcount);
             end;
         end;
        speedsearch:=hp;
      end;


    function tsymtable.rename(const olds,news : stringid):psym;
      begin
        rename:=psym(symsearch^.rename(olds,news));
      end;


{***********************************************
                Browser
***********************************************}

    procedure tsymtable.load_browser;
      var
        b     : byte;
        sym   : psym;
        prdef : pdef;
        oldrecsyms : psymtable;
      begin
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         if symtabletype=staticppusymtable then
           aktstaticsymtable:=@self;
         b:=current_ppu^.readentry;
         if b <> ibbeginsymtablebrowser then
           Message1(unit_f_ppu_invalid_entry,tostr(b));
         repeat
           b:=current_ppu^.readentry;
           case b of
           ibsymref : begin
                        sym:=readsymref;
                        resolvesym(sym);
                        if assigned(sym) then
                          sym^.load_references;
                      end;
           ibdefref : begin
                        prdef:=readdefref;
                        resolvedef(prdef);
                        if assigned(prdef) then
                         begin
                           if prdef^.deftype<>procdef then
                            Message(unit_f_ppu_read_error);
                           pprocdef(prdef)^.load_references;
                         end;
                      end;
            ibendsymtablebrowser : break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=oldrecsyms;
      end;


    procedure tsymtable.write_browser;
      var
         oldrecsyms : psymtable;
      begin
         { symbol numbering for references
           should have been done in write PM
         number_symbols;
         number_defs;   }

         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         current_ppu^.writeentry(ibbeginsymtablebrowser);
         foreach({$ifndef TP}@{$endif}write_refs);
         current_ppu^.writeentry(ibendsymtablebrowser);
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=oldrecsyms;
      end;


{$ifdef BrowserLog}
    procedure tsymtable.writebrowserlog;
      begin
        if cs_browser in aktmoduleswitches then
         begin
           if assigned(name) then
             Browserlog.AddLog('---Symtable '+name^)
           else
             begin
                if (symtabletype=recordsymtable) and
                  assigned(defowner^.sym) then
                  Browserlog.AddLog('---Symtable '+defowner^.sym^.name)
                else
                  Browserlog.AddLog('---Symtable with no name');
             end;
           Browserlog.Ident;
           foreach({$ifndef TP}@{$endif}add_to_browserlog);
           browserlog.Unident;
         end;
      end;
{$endif BrowserLog}


{***********************************************
           Process all entries
***********************************************}

    { checks, if all procsyms and methods are defined }
    procedure tsymtable.check_forwards;
      begin
         foreach({$ifndef TP}@{$endif}check_forward);
      end;

    procedure tsymtable.checklabels;
      begin
         foreach({$ifndef TP}@{$endif}labeldefined);
      end;

    procedure tsymtable.set_alignment(_alignment : byte);
      var
         sym : pvarsym;
         l : longint;
      begin
        { this can not be done if there is an
          hasharray ! }
        alignment:=_alignment;
        if (symtabletype<>parasymtable) then
          internalerror(1111);
        sym:=pvarsym(symindex^.first);
        datasize:=0;
        { there can be only varsyms }
        while assigned(sym) do
          begin
             l:=sym^.getpushsize;
             sym^.address:=datasize;
             datasize:=align(datasize+l,alignment);
             sym:=pvarsym(sym^.next);
          end;
      end;

    function tsymtable.find_at_offset(l : longint) : pvarsym;
      var
         sym : pvarsym;
      begin
        find_at_offset:=nil;
        { this can not be done if there is an
          hasharray ! }
        if (symtabletype<>parasymtable) then
          internalerror(1111);
        sym:=pvarsym(symindex^.first);
        while assigned(sym) do
          begin
             if sym^.address+address_fixup=l then
               begin
                 find_at_offset:=sym;
                 exit;
               end;
             sym:=pvarsym(sym^.next);
          end;
      end;

    procedure tsymtable.allunitsused;
      begin
         foreach({$ifndef TP}@{$endif}unitsymbolused);
      end;

    procedure tsymtable.allsymbolsused;
      begin
         foreach({$ifndef TP}@{$endif}varsymbolused);
      end;

{$ifdef CHAINPROCSYMS}
    procedure tsymtable.chainprocsyms;
      begin
         foreach({$ifndef TP}@{$endif}chainprocsym);
      end;
{$endif CHAINPROCSYMS}

{$ifdef GDB}
      procedure tsymtable.concatstabto(asmlist : paasmoutput);
      begin
        asmoutput:=asmlist;
        foreach({$ifndef TP}@{$endif}concatstab);
      end;
{$endif}


{****************************************************************************
                              TUNITSYMTABLE
****************************************************************************}

    constructor tunitsymtable.init(t : tsymtabletype; const n : string);
      begin
         inherited init(t);
         name:=stringdup(upper(n));
         unitid:=0;
         unitsym:=nil;
         symsearch^.usehash;
       { reset GDB things }
{$ifdef GDB}
         if t = globalsymtable then
           begin
              prev_dbx_counter := dbx_counter;
              dbx_counter := @dbx_count;
           end;
         is_stab_written:=false;
         if cs_gdb_dbx in aktglobalswitches then
           begin
             dbx_count := 0;
             if (symtabletype=globalsymtable) then
               pglobaltypecount := @unittypecount;
             debuglist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'+tostr(N_BINCL)+',0,0,0'))));
             unitid:=current_module^.unitcount;
             inc(current_module^.unitcount);
             debuglist^.concat(new(pai_asm_comment,init(strpnew('Global '+name^+' has index '+tostr(unitid)))));
           end;
{$endif GDB}
      end;


    constructor tunitsymtable.loadasunit;
      var
        storeGlobalTypeCount : pword;
        b : byte;
      begin
         unitsym:=nil;
         unitid:=0;
         if (current_module^.flags and uf_has_dbx)<>0 then
           begin
              storeGlobalTypeCount:=PGlobalTypeCount;
              PglobalTypeCount:=@UnitTypeCount;
           end;

       { load symtables }
         inherited load;
       { set the name after because it is set to nil in tsymtable.load !! }
         name:=stringdup(current_module^.modulename^);

       { dbx count }
{$ifdef GDB}
         if (current_module^.flags and uf_has_dbx)<>0 then
           begin
              b := current_ppu^.readentry;
              if b <> ibdbxcount then
               Message(unit_f_ppu_dbx_count_problem)
              else
               dbx_count := readlong;
              dbx_count_ok := true;
              PGlobalTypeCount:=storeGlobalTypeCount;
           end
         else
           dbx_count := 0;
         is_stab_written:=false;
{$endif GDB}

         b:=current_ppu^.readentry;
         if b<>ibendimplementation then
           Message1(unit_f_ppu_invalid_entry,tostr(b));
      end;


     destructor tunitsymtable.done;
       var
          pus : punitsym;
       begin
          pus:=unitsym;
          while assigned(pus) do
            begin
               unitsym:=pus^.prevsym;
               pus^.prevsym:=nil;
               pus^.unitsymtable:=nil;
               pus:=unitsym;
            end;
          inherited done;
       end;

       procedure tunitsymtable.load_symtable_refs;
         var
            b : byte;
            unitindex : word;
         begin
         if ((current_module^.flags and uf_local_browser)<>0) then
           begin
              current_module^.localsymtable:=new(punitsymtable,loadas(staticppusymtable));
              psymtable(current_module^.localsymtable)^.name:=
                stringdup('implementation of '+psymtable(current_module^.globalsymtable)^.name^);
           end;
         { load browser }
         if (current_module^.flags and uf_has_browser)<>0 then
           begin
              {if not (cs_browser in aktmoduleswitches) then
                current_ppu^.skipuntilentry(ibendbrowser)
              else }
                begin
                   load_browser;
                   unitindex:=1;
                   while assigned(current_module^.map^[unitindex]) do
                     begin
                        {each unit wrote one browser entry }
                        load_browser;
                        inc(unitindex);
                     end;
                   b:=current_ppu^.readentry;
                   if b<>ibendbrowser then
                     Message1(unit_f_ppu_invalid_entry,tostr(b));
                end;
           end;
         if ((current_module^.flags and uf_local_browser)<>0) then
           psymtable(current_module^.localsymtable)^.load_browser;
         end;


    procedure tunitsymtable.writeasunit;
      var
         pu        : pused_unit;
      begin
      { first the unitname }
        current_ppu^.putstring(name^);
        current_ppu^.writeentry(ibmodulename);

        writesourcefiles;
        writeusedmacros;

        writeusedunit;

      { write the objectfiles and libraries that come for this unit,
        preserve the containers becuase they are still needed to load
        the link.res. All doesn't depend on the crc! It doesn't matter
        if a unit is in a .o or .a file }
        current_ppu^.do_crc:=false;
        writelinkcontainer(current_module^.linkunitofiles,iblinkunitofiles,true);
        writelinkcontainer(current_module^.linkunitstaticlibs,iblinkunitstaticlibs,true);
        writelinkcontainer(current_module^.linkunitsharedlibs,iblinkunitsharedlibs,true);
        writelinkcontainer(current_module^.linkotherofiles,iblinkotherofiles,false);
        writelinkcontainer(current_module^.linkotherstaticlibs,iblinkotherstaticlibs,true);
        writelinkcontainer(current_module^.linkothersharedlibs,iblinkothersharedlibs,true);
        current_ppu^.do_crc:=true;

        current_ppu^.writeentry(ibendinterface);

      { write the symtable entries }
        inherited write;

      { write dbx count }
{$ifdef GDB}
        if cs_gdb_dbx in aktglobalswitches then
         begin
{$IfDef EXTDEBUG}
           writeln('Writing dbx_count ',dbx_count,' in unit ',name^,'.ppu');
{$ENDIF EXTDEBUG}
           current_ppu^.putlongint(dbx_count);
           current_ppu^.writeentry(ibdbxcount);
         end;
{$endif GDB}

        current_ppu^.writeentry(ibendimplementation);
        { all after doesn't affect crc }
        current_ppu^.do_crc:=false;

         { write static symtable
           needed for local debugging of unit functions }
        if ((current_module^.flags and uf_local_browser)<>0) and
           assigned(current_module^.localsymtable) then
          psymtable(current_module^.localsymtable)^.write;
      { write all browser section }
        if (current_module^.flags and uf_has_browser)<>0 then
         begin
           write_browser;
           pu:=pused_unit(current_module^.used_units.first);
           while assigned(pu) do
            begin
              psymtable(pu^.u^.globalsymtable)^.write_browser;
              pu:=pused_unit(pu^.next);
            end;
           current_ppu^.writeentry(ibendbrowser);
         end;
        if ((current_module^.flags and uf_local_browser)<>0) and
           assigned(current_module^.localsymtable) then
          psymtable(current_module^.localsymtable)^.write_browser;

      { the last entry ibend is written automaticly }
      end;


   function tunitsymtable.getnewtypecount : word;

      begin
{$ifdef GDB}
         if not (cs_gdb_dbx in aktglobalswitches) then
           getnewtypecount:=tsymtable.getnewtypecount
         else
{$endif GDB}
           if symtabletype = staticsymtable then
           getnewtypecount:=tsymtable.getnewtypecount
         else
           begin
              getnewtypecount:=unittypecount;
              inc(unittypecount);
           end;
      end;


{$ifdef GDB}

      procedure tunitsymtable.concattypestabto(asmlist : paasmoutput);
        var prev_dbx_count : plongint;
        begin
           if is_stab_written then exit;
           if not assigned(name) then name := stringdup('Main_program');
           if symtabletype = unitsymtable then
             begin
                unitid:=current_module^.unitcount;
                inc(current_module^.unitcount);
             end;
           asmlist^.concat(new(pai_asm_comment,init(strpnew('Begin unit '+name^
                  +' has index '+tostr(unitid)))));
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if dbx_count_ok then
                  begin
                     asmlist^.insert(new(pai_asm_comment,init(strpnew('"repeated" unit '+name^
                              +' has index '+tostr(unitid)))));
                     do_count_dbx:=true;
                     asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                       +tostr(N_EXCL)+',0,0,'+tostr(dbx_count)))));
                     exit;
                  end;
                prev_dbx_count := dbx_counter;
                dbx_counter := nil;
                if symtabletype = unitsymtable then
                  asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                    +tostr(N_BINCL)+',0,0,0'))));
                dbx_counter := @dbx_count;
             end;
           asmoutput:=asmlist;
           foreach({$ifndef TP}@{$endif}concattypestab);
           if cs_gdb_dbx in aktglobalswitches then
             begin
                dbx_counter := prev_dbx_count;
                do_count_dbx:=true;
                asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                  +tostr(N_EINCL)+',0,0,0'))));
                dbx_count_ok := true;
             end;
           asmlist^.concat(new(pai_asm_comment,init(strpnew('End unit '+name^
                  +' has index '+tostr(unitid)))));
           is_stab_written:=true;
        end;
{$endif}

{****************************************************************************
                              Definitions
****************************************************************************}

{$I symdef.inc}

{****************************************************************************
                                Symbols
****************************************************************************}

{$I symsym.inc}

{****************************************************************************
                               GDB Helpers
****************************************************************************}

{$ifdef GDB}
    function typeglobalnumber(const s : string) : string;

      var st : string;
          symt : psymtable;
          old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         typeglobalnumber := '0';
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           getsym(st,false);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym^.typ = unitsym then
               begin
               symt := punitsym(srsym)^.unitsymtable;
               srsym := symt^.search(st);
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then getsym(st,true);
         if srsym^.typ<>typesym then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         typeglobalnumber := ptypesym(srsym)^.definition^.numberstring;
         make_ref:=old_make_ref;
      end;
{$endif GDB}


{****************************************************************************
                           Definition Helpers
****************************************************************************}

   procedure reset_global_defs;
     var
       def     : pdef;
{$ifdef debug}
       prevdef : pdef;
{$endif debug}
     begin
{$ifdef debug}
        prevdef:=nil;
{$endif debug}
{$ifdef GDB}
        pglobaltypecount:=@globaltypecount;
{$endif GDB}
        def:=firstglobaldef;
        while assigned(def) do
          begin
{$ifdef GDB}
            if assigned(def^.sym) then
              def^.sym^.isusedinstab:=false;
            def^.is_def_stab_written:=false;
{$endif GDB}
            {if not current_module^.in_implementation then}
              begin
                { reset rangenr's }
                case def^.deftype of
                  orddef   : porddef(def)^.rangenr:=0;
                  enumdef  : penumdef(def)^.rangenr:=0;
                  arraydef : parraydef(def)^.rangenr:=0;
                end;
                if def^.deftype<>objectdef then
                  def^.has_rtti:=false;
                def^.has_inittable:=false;
              end;
{$ifdef debug}
            prevdef:=def;
{$endif debug}
            def:=def^.nextglobal;
          end;
     end;


{****************************************************************************
                              Object Helpers
****************************************************************************}

    function search_class_member(pd : pobjectdef;const n : string) : psym;
    { searches n in symtable of pd and all anchestors }
      var
         sym : psym;
      begin
         sym:=nil;
         while assigned(pd) do
           begin
              sym:=pd^.symtable^.search(n);
              if assigned(sym) then
                break;
              pd:=pd^.childof;
           end;
         { this is needed for static methods in do_member_read pexpr unit PM
           caused bug0214 }
         if assigned(sym) then
           begin
             srsymtable:=pd^.symtable;
           end;
         search_class_member:=sym;
      end;

   var
      _defaultprop : ppropertysym;

   procedure testfordefaultproperty(p : pnamedindexobject);
     begin
        if (psym(p)^.typ=propertysym) and
           (ppo_defaultproperty in ppropertysym(p)^.propoptions) then
          _defaultprop:=ppropertysym(p);
     end;


   function search_default_property(pd : pobjectdef) : ppropertysym;
   { returns the default property of a class, searches also anchestors }
     begin
        _defaultprop:=nil;
        while assigned(pd) do
          begin
             pd^.symtable^.foreach({$ifndef TP}@{$endif}testfordefaultproperty);
             if assigned(_defaultprop) then
               break;
             pd:=pd^.childof;
          end;
        search_default_property:=_defaultprop;
     end;


{****************************************************************************
                               Macro's
****************************************************************************}

      procedure def_macro(const s : string);
        var
          mac : pmacrosym;
        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               Message1(parser_m_macro_defined,mac^.name);
               macros^.insert(mac);
             end;
           mac^.defined:=true;
           mac^.defined_at_startup:=true;
        end;


      procedure set_macro(const s : string;value : string);
        var
          mac : pmacrosym;
        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               macros^.insert(mac);
             end
           else
             begin
                if assigned(mac^.buftext) then
                  freemem(mac^.buftext,mac^.buflen);
             end;
           Message2(parser_m_macro_set_to,mac^.name,value);
           mac^.buflen:=length(value);
           getmem(mac^.buftext,mac^.buflen);
           move(value[1],mac^.buftext^,mac^.buflen);
           mac^.defined:=true;
           mac^.defined_at_startup:=true;
        end;


{****************************************************************************
                              TUNIT_ALIAS
 ****************************************************************************}

    constructor tunit_alias.init(const n:string);
      var
        i : longint;
      begin
        i:=pos('=',n);
        if i=0 then
         fail;
        inherited initname(Copy(n,1,i-1));
        newname:=stringdup(Copy(n,i+1,255));
      end;


    destructor tunit_alias.done;
      begin
        stringdispose(newname);
        inherited done;
      end;


    procedure addunitalias(const n:string);
      begin
        unitaliases^.insert(new(punit_alias,init(Upper(n))));
      end;


    function getunitalias(const n:string):string;
      var
        p : punit_alias;
      begin
        p:=punit_alias(unitaliases^.search(Upper(n)));
        if assigned(p) then
         getunitalias:=punit_alias(p)^.newname^
        else
         getunitalias:=n;
      end;


{****************************************************************************
                            Symtable Stack
****************************************************************************}

    procedure dellexlevel;
      var
         p : psymtable;
      begin
         p:=symtablestack;
         symtablestack:=p^.next;
         { symbol tables of unit interfaces are never disposed }
         { this is handle by the unit unitm                 }
         if not(p^.symtabletype in [unitsymtable,globalsymtable,stt_exceptsymtable]) or dispose_global then
          dispose(p,done);
      end;

{$ifdef DEBUG}
    procedure test_symtablestack;
      var
         p : psymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              p:=p^.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;

    procedure list_symtablestack;
      var
         p : psymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              writeln(i,' ',p^.name^);
              p:=p^.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;
{$endif DEBUG}


{****************************************************************************
                           Init/Done Symtable
****************************************************************************}

{$ifndef Delphi}
{$ifdef tp}
   procedure do_streamerror;
     begin
       if symbolstream.status=-2 then
        WriteLn('Error: Not enough EMS memory')
       else
        WriteLn('Error: EMS Error ',symbolstream.status);
       halt(1);
     end;
{$endif TP}
{$endif Delphi}

   procedure InitSymtable;
     begin
{$ifndef Delphi}
{$ifdef TP}
     { Allocate stream }
        if use_big then
         begin
           streamerror:=@do_streamerror;
         { symbolstream.init('TMPFILE',stcreate,16000); }
         {$ifndef dpmi}
           symbolstream.init(10000,4000000); {using ems streams}
         {$else}
           symbolstream.init(1000000,16000); {using memory streams}
         {$endif}
           if symbolstream.errorinfo=stiniterror then
            do_streamerror;
         { write something, because pos 0 means nil pointer }
           symbolstream.writestr(@inputfile);
         end;
{$endif tp}
{$endif Delphi}
      { Reset symbolstack }
        registerdef:=false;
        read_member:=false;
        symtablestack:=nil;
        systemunit:=nil;
{$ifdef GDB}
        firstglobaldef:=nil;
        lastglobaldef:=nil;
{$endif GDB}
        globaltypecount:=1;
        pglobaltypecount:=@globaltypecount;
     { create error syms and def }
        generrorsym:=new(perrorsym,init);
        generrordef:=new(perrordef,init);
     { unit aliases }
        unitaliases:=new(pdictionary,init);
     end;


   procedure DoneSymtable;
      begin
        dispose(generrorsym,done);
        dispose(generrordef,done);
        dispose(unitaliases,done);
{$ifndef Delphi}
{$ifdef TP}
      { close the stream }
        if use_big then
         symbolstream.done;
{$endif}
{$endif Delphi}
     end;

end.
{
  $Log$
  Revision 1.55  1999-11-04 10:54:02  peter
    + -Ua<oldname>=<newname> unit alias support

  Revision 1.54  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.53  1999/10/06 17:39:15  peter
    * fixed stabs writting for forward types

  Revision 1.52  1999/10/03 19:44:42  peter
    * removed objpasunit reference, tvarrec is now searched in systemunit
      where it already was located

  Revision 1.51  1999/10/01 08:02:49  peter
    * forward type declaration rewritten

  Revision 1.50  1999/09/28 20:48:25  florian
    * fixed bug 610
    + added $D- for TP in symtable.pas else it can't be compiled anymore
      (too much symbols :()

  Revision 1.49  1999/09/27 23:44:59  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.48  1999/09/12 21:35:38  florian
    * fixed a crash under Linux. Why doesn't have the damned Windows DPMI nil pointer
      protection???

  Revision 1.47  1999/09/12 08:48:09  florian
    * bugs 593 and 607 fixed
    * some other potential bugs with array constructors fixed
    * for classes compiled in $M+ and it's childs, the default access method
      is now published
    * fixed copyright message (it is now 1993-99)

  Revision 1.46  1999/09/10 18:48:10  florian
    * some bug fixes (e.g. must_be_valid and procinfo^.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.45  1999/09/08 08:05:44  peter
    * fixed bug 248

  Revision 1.44  1999/08/31 15:46:21  pierre
    * do_crc must be false for all browser stuff
    + tmacrosym defined_at_startup set in def_macro and set_macro

  Revision 1.43  1999/08/27 10:39:24  pierre
   * uf_local_browser made problem when computing interface CRC

  Revision 1.42  1999/08/13 21:33:13  peter
    * support for array constructors extended and more error checking

  Revision 1.41  1999/08/13 14:24:22  pierre
    + stabs for classes and classref working,
      a class still needs an ^ to get that content of it,
      but the class fields inside a class don't result into an
      infinite loop anymore!

  Revision 1.40  1999/08/10 16:25:42  pierre
   * unitid changed to word

  Revision 1.39  1999/08/10 12:33:36  pierre
   * pprocsym defined earlier for use in tprocdef

  Revision 1.38  1999/08/05 16:53:18  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.37  1999/08/04 13:03:09  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.36  1999/08/04 00:23:31  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.35  1999/08/03 22:03:22  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.34  1999/08/03 17:51:45  florian
    * reduced memory usage by factor 2-3 (it
      improved also the speed) by reducing the
      growsize of the symbol tables

  Revision 1.33  1999/08/03 00:03:24  florian
    * added bestrealdef for alpha and powerpc

  Revision 1.32  1999/08/01 23:09:27  michael
  * procbase -> cpubase

  Revision 1.31  1999/08/01 23:04:50  michael
  + Changes for Alpha

  Revision 1.30  1999/07/24 00:13:26  peter
    * also number units for program

  Revision 1.29  1999/07/23 16:05:33  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.28  1999/07/23 12:02:20  peter
    * fixed crash in previous commit

  Revision 1.27  1999/07/23 11:37:50  peter
    * error for illegal type reference, instead of 10998

  Revision 1.26  1999/07/22 09:37:58  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.25  1999/07/18 14:47:34  florian
    * bug 487 fixed, (inc(<property>) isn't allowed)
    * more fixes to compile with Delphi

  Revision 1.24  1999/07/03 00:30:01  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.23  1999/06/28 17:02:44  pierre
   merged from v0-99-12 branch

  Revision 1.21.2.2  1999/06/28 16:59:55  pierre
   * fix to get method reference info

  Revision 1.21.2.1  1999/06/22 16:26:46  pierre
   * local browser stuff corrected

  Revision 1.21  1999/06/08 22:23:50  pierre
   * staticppusymtable was loaded a tsymtable instead of tunitsymtable

  Revision 1.20  1999/06/02 22:44:23  pierre
   * previous wrong log corrected

  Revision 1.19  1999/06/02 22:25:53  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.18  1999/06/01 14:45:58  peter
    * @procvar is now always needed for FPC

  Revision 1.17  1999/05/27 19:45:08  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.16  1999/05/23 18:42:16  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.15  1999/05/17 23:51:41  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.14  1999/05/14 17:52:29  peter
    * new deref code

  Revision 1.13  1999/05/13 21:59:48  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.12  1999/05/10 22:34:59  pierre
   * one more unitsym problem fix

  Revision 1.11  1999/05/10 15:02:51  pierre
  unitsym finally problem fixed

  Revision 1.10  1999/05/09 12:46:26  peter
    + hint where a duplicate sym is already defined

  Revision 1.9  1999/05/08 19:52:40  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.8  1999/05/06 21:38:38  peter
    * don't register errordef

  Revision 1.7  1999/05/06 09:05:31  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.6  1999/05/05 09:19:16  florian
    * more fixes to get it with delphi running

  Revision 1.5  1999/05/01 13:24:43  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.4  1999/04/29 17:25:37  peter
    * small fix for deref

  Revision 1.3  1999/04/26 18:30:03  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.151  1999/04/26 13:31:54  peter
    * release storenumber,double_checksum

  Revision 1.150  1999/04/25 17:36:13  peter
    * typo fix for storenumber

  Revision 1.149  1999/04/21 22:05:28  pierre
    + tsymtable.find_at_offset function
      used by ra386att to give arg name from ebp offset with -vz option

  Revision 1.148  1999/04/21 16:31:44  pierre
  ra386att.pas : commit problem !

  Revision 1.147  1999/04/21 09:43:57  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.146  1999/04/19 09:33:14  pierre
    + added tsymtable.set_alignment(longint) function
      to change the offsets of all function args
      if declared as cdecl or stdcall
      (this must be done after because the cdecl is parsed after
      insertion of the function parameterss into parast symboltable)

  Revision 1.145  1999/04/17 13:16:24  peter
    * fixes for storenumber

  Revision 1.144  1999/04/15 10:01:45  peter
    * small update for storenumber

  Revision 1.143  1999/04/14 09:15:04  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.142  1999/04/08 14:54:10  pierre
   * suppression of val para unused warnings

  Revision 1.141  1999/04/07 15:31:09  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.140  1999/03/31 13:55:24  peter
    * assembler inlining working for ag386bin

  Revision 1.139  1999/03/24 23:17:30  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.138  1999/03/21 22:49:11  florian
    * private ids of objects can be reused in child classes
      if they are in another unit

  Revision 1.137  1999/03/17 22:23:20  florian
    * a FPC compiled compiler checks now also in debug mode in assigned
      if a pointer points to the heap
    * when a symtable is loaded, there is no need to check for duplicate
      symbols. This leads to crashes because defowner isn't assigned
      in this case

  Revision 1.136  1999/03/01 13:45:07  pierre
   + added staticppusymtable symtable type for local browsing

  Revision 1.135  1999/02/23 18:29:28  pierre
    * win32 compilation error fix
    + some work for local browser (not cl=omplete yet)

  Revision 1.134  1999/02/22 15:09:42  florian
    * behaviaor of PROTECTED and PRIVATE fixed, works now like TP/Delphi

  Revision 1.133  1999/02/22 13:07:12  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.132  1999/02/22 02:15:40  peter
    * updates for ag386bin

  Revision 1.131  1999/02/16 00:44:34  peter
    * tp7 fix, assigned() can only be used on vars, not on functions

  Revision 1.130  1999/02/15 13:13:16  pierre
   * fix for bug0216

  Revision 1.129  1999/02/11 09:46:29  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.128  1999/02/09 23:03:05  florian
    * check for duplicate field names in inherited classes/objects
    * bug with self from the mailing list solved (the problem
      was that classes were sometimes pushed wrong)

  Revision 1.127  1999/02/08 11:29:06  pierre
   * fix for bug0214
     several problems where combined
     search_class_member did not set srsymtable
     => in do_member_read the call node got a wrong symtable
     in cg386cal the vmt was pushed twice without chacking if it exists
     now %esi is set to zero and pushed if not vmt
     (not very efficient but should work !)

  Revision 1.126  1999/02/05 08:54:31  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.125  1999/02/03 09:44:33  pierre
    * symbol nubering begins with 1 in number_symbols
    * program tmodule has globalsymtable for its staticsymtable
      (to get it displayed in IDE globals list)
    + list of symbol (browcol) greatly improved for IDE

  Revision 1.124  1999/01/27 12:58:33  pierre
   * unused var warning suppressed for high of open arrays

  Revision 1.123  1999/01/21 16:41:03  pierre
   * fix for constructor inside with statements

  Revision 1.122  1999/01/20 10:16:44  peter
    * don't update crc when writing objs,libs and sources

  Revision 1.121  1999/01/14 21:50:00  peter
    * fixed forwardpointer problem with multiple forwards for the same
      typesym. It now uses a linkedlist instead of a single pointer

  Revision 1.120  1999/01/13 14:29:22  daniel
  * nonextfield repaired

  Revision 1.119  1999/01/12 14:25:38  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.118  1999/01/05 08:20:10  florian
    * mainly problem with invalid case ranges fixed (reported by Jonas)

  Revision 1.117  1998/12/30 22:15:57  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.116  1998/12/30 13:41:16  peter
    * released valuepara

  Revision 1.115  1998/12/11 00:03:48  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.114  1998/12/10 09:47:29  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.113  1998/12/08 10:18:17  peter
    + -gh for heaptrc unit

  Revision 1.112  1998/12/04 10:18:10  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.111  1998/11/30 16:34:46  pierre
    * corrected problems with rangecheck
    + added needed code for no rangecheck  in CRC32 functions in ppu unit
    * enumdef lso need its rangenr reset to zero
      when calling reset_global_defs

  Revision 1.110  1998/11/28 16:20:58  peter
    + support for dll variables

  Revision 1.109  1998/11/27 14:50:49  peter
    + open strings, $P switch support

  Revision 1.108  1998/11/24 23:00:32  peter
    * small crash prevention

  Revision 1.107  1998/11/20 15:36:01  florian
    * problems with rtti fixed, hope it works

  Revision 1.106  1998/11/18 15:44:20  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.105  1998/11/17 10:39:18  peter
    * has_rtti,has_inittable reset

  Revision 1.104  1998/11/16 10:13:52  peter
    * label defines are checked at the end of the proc

  Revision 1.103  1998/11/13 15:40:32  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.102  1998/11/12 16:43:34  florian
    * functions with ansi strings as result didn't work, solved

  Revision 1.101  1998/11/12 12:55:18  pierre
   * fix for bug0176 and bug0177

  Revision 1.100  1998/11/10 10:09:15  peter
    * va_list -> array of const

  Revision 1.99  1998/11/09 11:44:38  peter
    + va_list for printf support

  Revision 1.98  1998/11/05 23:33:35  peter
    * symtable.done sets vars to nil

  Revision 1.97  1998/11/05 12:03:00  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.96  1998/10/28 18:26:19  pierre
   * removed some erros after other errors (introduced by useexcept)
   * stabs works again correctly (for how long !)

  Revision 1.95  1998/10/21 08:40:01  florian
    + ansistring operator +
    + $h and string[n] for n>255 added
    * small problem with TP fixed

  Revision 1.94  1998/10/20 08:07:03  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.93  1998/10/19 08:55:08  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.92  1998/10/16 13:12:56  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.91  1998/10/16 08:48:38  peter
    * fixed some misplaced $endif GDB

  Revision 1.90  1998/10/15 15:13:32  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.89  1998/10/14 13:38:25  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.88  1998/10/09 16:36:07  pierre
    * some memory leaks specific to usebrowser define fixed
    * removed tmodule.implsymtable (was like tmodule.localsymtable)

  Revision 1.87  1998/10/09 11:47:57  pierre
    * still more memory leaks fixes !!

  Revision 1.86  1998/10/08 17:17:35  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.85  1998/10/08 13:48:51  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.84  1998/10/06 17:16:58  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.83  1998/09/26 17:45:45  peter
    + idtoken and only one token table

  Revision 1.82  1998/09/25 09:52:57  peter
    + store also datasize and # of symbols in ppu
    * # of defs is now also stored in structs

  Revision 1.81  1998/09/24 23:49:21  peter
    + aktmodeswitches

  Revision 1.80  1998/09/23 12:20:51  pierre
    * main program tmodule had no symtable (crashed browser)
    * unit symbols problem fixed !!

  Revision 1.79  1998/09/23 12:03:57  peter
    * overloading fix for array of const

  Revision 1.78  1998/09/22 17:13:54  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.77  1998/09/22 15:37:24  peter
    + array of const start

  Revision 1.76  1998/09/21 10:00:08  peter
    * store number of defs in ppu file

  Revision 1.75  1998/09/21 08:58:31  peter
    + speedsearch, which also needs speedvalue as parameter

  Revision 1.74  1998/09/21 08:45:25  pierre
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

  Revision 1.73  1998/09/20 09:38:47  florian
    * hasharray for defs fixed
    * ansistring code generation corrected (init/final, assignement)

  Revision 1.72  1998/09/19 22:56:18  florian
    + hash table for getdefnr added

  Revision 1.71  1998/09/18 08:01:40  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.70  1998/09/09 11:50:57  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.69  1998/09/07 23:10:25  florian
    * a lot of stuff fixed regarding rtti and publishing of properties,
      basics should now work

  Revision 1.68  1998/09/07 19:33:26  florian
    + some stuff for property rtti added:
       - NameIndex of the TPropInfo record is now written correctly
       - the DEFAULT/NODEFAULT keyword is supported now
       - the default value and the storedsym/def are now written to
         the PPU fiel

  Revision 1.67  1998/09/07 18:46:14  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.66  1998/09/07 17:37:05  florian
    * first fixes for published properties

  Revision 1.65  1998/09/06 22:42:03  florian
    + rtti genreation for properties added

  Revision 1.64  1998/09/05 22:11:04  florian
    + switch -vb
    * while/repeat loops accept now also word/longbool conditions
    * makebooltojump did an invalid ungetregister32, fixed

  Revision 1.63  1998/09/04 17:34:23  pierre
    * bug with datalabel corrected
    + assembler errors better commented
    * one nested record crash removed

  Revision 1.62  1998/09/04 08:42:10  peter
    * updated some error messages

  Revision 1.61  1998/09/03 16:03:21  florian
    + rtti generation
    * init table generation changed

  Revision 1.60  1998/09/01 17:39:52  peter
    + internal constant functions

  Revision 1.59  1998/09/01 12:53:27  peter
    + aktpackenum

  Revision 1.58  1998/09/01 07:54:26  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.57  1998/08/31 12:26:33  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.56  1998/08/21 14:08:55  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.55  1998/08/21 08:43:32  pierre
    * pocdecl and poclearstack are now different
      external must but written as last specification

  Revision 1.54  1998/08/20 09:26:48  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.53  1998/08/19 18:04:56  peter
    * fixed current_module^.in_implementation flag

  Revision 1.51  1998/08/18 14:17:12  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.50  1998/08/17 10:10:13  peter
    - removed OLDPPU

  Revision 1.49  1998/08/12 19:39:31  peter
    * fixed some crashes

  Revision 1.48  1998/08/10 14:50:32  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.47  1998/08/10 10:00:19  peter
    * Moved symbolstream to symtable.pas

  Revision 1.46  1998/08/08 10:19:19  florian
    * small fixes to write the extended type correct

  Revision 1.45  1998/08/02 16:42:00  florian
    * on o : tobject do should also work now, the exceptsymtable shouldn't be
      disposed by dellexlevel

  Revision 1.44  1998/07/30 11:18:21  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.43  1998/07/28 21:52:56  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.42  1998/07/20 10:23:03  florian
    * better ansi string assignement

  Revision 1.41  1998/07/18 22:54:31  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.40  1998/07/14 14:47:09  peter
    * released NEWINPUT

  Revision 1.39  1998/07/10 00:00:06  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.38  1998/07/07 11:20:17  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.37  1998/06/24 14:48:42  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.36  1998/06/17 14:10:19  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.35  1998/06/16 08:56:35  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.34  1998/06/15 15:38:12  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.33  1998/06/15 14:10:53  daniel
  * File was ruined, fixed.

  Revision 1.31  1998/06/13 00:10:20  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.30  1998/06/09 16:01:53  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.29  1998/06/07 15:30:26  florian
    + first working rtti
    + data init/final. for local variables

  Revision 1.28  1998/06/06 09:27:39  peter
    * new depend file generated

  Revision 1.27  1998/06/05 14:37:38  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.26  1998/06/04 23:52:03  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.25  1998/06/04 09:55:48  pierre
    * demangled name of procsym reworked to become independant of the
      mangling scheme

  Revision 1.24  1998/06/03 22:49:04  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.23  1998/05/28 14:40:30  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.22  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifndef OLDPPU

  Revision 1.21  1998/05/23 01:21:31  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.20  1998/05/21 19:33:37  peter
    + better procedure directive handling and only one table

  Revision 1.19  1998/05/20 09:42:37  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.18  1998/05/11 13:07:57  peter
    + $ifndef OLDPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.17  1998/05/06 08:38:48  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.16  1998/05/05 15:24:20  michael
  * Fix to save units with classes.

  Revision 1.15  1998/05/04 17:54:29  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.14  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.13  1998/05/01 09:01:25  florian
    + correct semantics of private and protected
    * small fix in variable scope:
       a id can be used in a parameter list of a method, even it is used in
       an anchestor class as field id

  Revision 1.12  1998/05/01 07:43:57  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.11  1998/04/30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.10  1998/04/29 10:34:05  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.9  1998/04/27 23:10:29  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.8  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.7  1998/04/13 22:20:36  florian
    + stricter checking for duplicate id, solves also bug0097

  Revision 1.6  1998/04/13 17:20:43  florian
    * tdef.done much faster implemented

  Revision 1.5  1998/04/10 21:36:56  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.4  1998/04/08 16:58:08  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/07 13:19:52  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.2  1998/04/06 13:09:04  daniel
  * Emergency solution for bug in reset_gdb_info.
}
