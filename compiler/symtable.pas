{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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
  {$N+,E+,F+,L-}
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
       globtype,globals,tokens,systems,
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
       psym = ^tsym;
       pdef = ^tdef;
       ptypesym = ^ttypesym;
       penumsym = ^tenumsym;
       pprocsym = ^tprocsym;
       tcallback = procedure(p : psym);

       pref = ^tref;
       tref = object
         nextref     : pref;
         posinfo     : tfileposinfo;
         moduleindex : word;
         is_written  : boolean;
         constructor init(ref:pref;pos:pfileposinfo);
         procedure   freechain;
         destructor  done; virtual;
       end;

      { Deref entry options }
      tdereftype = (derefnil,derefaktrecordindex,derefaktstaticindex,
                    derefunit,derefrecord,derefindex,
                    dereflocal,derefpara,derefaktlocal);

      pderef = ^tderef;
      tderef = object
        dereftype : tdereftype;
        index     : word;
        next      : pderef;
        constructor init(typ:tdereftype;i:word);
        destructor  done;
      end;

      ttype = object
        def : pdef;
        sym : psym;
        procedure reset;
        procedure setdef(p:pdef);
        procedure setsym(p:psym);
        procedure load;
        procedure write;
        procedure resolve;
      end;

      psymlistitem = ^tsymlistitem;
      tsymlistitem = record
        sym  : psym;
        next : psymlistitem;
      end;

      psymlist = ^tsymlist;
      tsymlist = object
        def      : pdef;
        firstsym,
        lastsym  : psymlistitem;
        constructor init;
        constructor load;
        destructor  done;
        function  empty:boolean;
        procedure setdef(p:pdef);
        procedure addsym(p:psym);
        procedure clear;
        function  getcopy:psymlist;
        procedure resolve;
        procedure write;
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
{          alignment : longint; }
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
          procedure insert(sym : psym);
          procedure insert_in(psymt : psymtable;offset : longint);
          function  search(const s : stringid) : psym;
          function  speedsearch(const s : stringid;speedvalue : longint) : psym;
          procedure registerdef(p : pdef);
          procedure allsymbolsused;
          procedure allprivatesused;
          procedure allunitsused;
          procedure check_forwards;
          procedure checklabels;
          { change alignment for args  only parasymtable }
          procedure set_alignment(_alignment : longint);
          { find arg having offset  only parasymtable }
          function  find_at_offset(l : longint) : pvarsym;
{$ifdef CHAINPROCSYMS}
          procedure chainprocsyms;
{$endif CHAINPROCSYMS}
{$ifndef DONOTCHAINOPERATORS}
          procedure chainoperators;
{$endif DONOTCHAINOPERATORS}
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
       cwidechardef : porddef;  { Pointer to WideChar }
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
    type
       toverloaded_operators = array[first_overloaded..last_overloaded] of pprocsym;
    var
       overloaded_operators : toverloaded_operators;
       { unequal is not equal}
    const
       overloaded_names : array [first_overloaded..last_overloaded] of string[16] =
         ('plus','minus','star','slash','equal',
          'greater','lower','greater_or_equal',
          'lower_or_equal',
          'sym_diff','starstar',
          'as','is','in','or',
          'and','div','mod','not','shl','shr','xor',
          'assign');

{$ifdef UNITALIASES}
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
{$endif UNITALIASES}


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
    procedure RestoreUnitSyms;
{$ifdef DEBUG}
    procedure test_symtablestack;
    procedure list_symtablestack;
{$endif DEBUG}

{*** Init / Done ***}
    procedure InitSymtable;
    procedure DoneSymtable;


implementation

  uses
     version,verbose,
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
     ,cpuasm
     ;

  var
     aktrecordsymtable : psymtable; { current record read from ppu symtable }
     aktstaticsymtable : psymtable; { current static for local ppu symtable }
     aktlocalsymtable  : psymtable; { current proc local for local ppu symtable }
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
         with sym^.fileinfo do
           begin
             if assigned(st) and (st^.unitid<>0) then
               Message2(sym_h_duplicate_id_where,'unit '+st^.name^,tostr(line))
             else
               Message2(sym_h_duplicate_id_where,current_module^.sourcefiles^.get_file_name(fileindex),tostr(line));
           end;
       end;


{*****************************************************************************
                               PPU Reading Writing
*****************************************************************************}

{$I symppu.inc}


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
             derefaktlocal :
               begin
                 st:=aktlocalsymtable;
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

    procedure tref.freechain;
      var
        p,q : pref;
      begin
        p:=nextref;
        nextref:=nil;
        while assigned(p) do
          begin
            q:=p^.nextref;
            dispose(p,done);
            p:=q;
          end;
      end;

    destructor tref.done;
      var
         inputfile : pinputfile;
      begin
         inputfile:=get_source_file(moduleindex,posinfo.fileindex);
         if inputfile<>nil then
           dec(inputfile^.ref_count);
         nextref:=nil;
      end;


{****************************************************************************
                                   TType
****************************************************************************}

    procedure ttype.reset;
      begin
        def:=nil;
        sym:=nil;
      end;


    procedure ttype.setdef(p:pdef);
      begin
        def:=p;
        sym:=nil;
      end;


    procedure ttype.setsym(p:psym);
      begin
        sym:=p;
        case p^.typ of
          typesym :
            def:=ptypesym(p)^.restype.def;
          propertysym :
            def:=ppropertysym(p)^.proptype.def;
          else
            internalerror(1234005);
        end;
      end;


    procedure ttype.load;
      begin
        def:=pdef(readderef);
        sym:=psym(readderef);
      end;


    procedure ttype.write;
      begin
        if assigned(sym) then
         begin
           writederef(nil);
           writederef(sym);
         end
        else
         begin
           writederef(def);
           writederef(nil);
         end;
      end;


    procedure ttype.resolve;
      begin
        if assigned(sym) then
         begin
           resolvesym(sym);
           setsym(sym);
         end
        else
         resolvedef(def);
      end;


{****************************************************************************
                                    TSymList
****************************************************************************}

    constructor tsymlist.init;
      begin
        def:=nil; { needed for procedures }
        firstsym:=nil;
        lastsym:=nil;
      end;


    constructor tsymlist.load;
      var
        sym : psym;
      begin
        def:=readdefref;
        firstsym:=nil;
        lastsym:=nil;
        repeat
          sym:=readsymref;
          if sym=nil then
           break;
          addsym(sym);
        until false;
      end;


    destructor tsymlist.done;
      begin
        clear;
      end;


    function tsymlist.empty:boolean;
      begin
        empty:=(firstsym=nil);
      end;


    procedure tsymlist.clear;
      var
        hp : psymlistitem;
      begin
        while assigned(firstsym) do
         begin
           hp:=firstsym;
           firstsym:=firstsym^.next;
           dispose(hp);
         end;
        firstsym:=nil;
        lastsym:=nil;
        def:=nil;
      end;


    procedure tsymlist.setdef(p:pdef);
      begin
        def:=p;
      end;


    procedure tsymlist.addsym(p:psym);
      var
        hp : psymlistitem;
      begin
        if not assigned(p) then
         exit;
        new(hp);
        hp^.sym:=p;
        hp^.next:=nil;
        if assigned(lastsym) then
         lastsym^.next:=hp
        else
         firstsym:=hp;
        lastsym:=hp;
      end;


    function tsymlist.getcopy:psymlist;
      var
        hp : psymlist;
        hp2 : psymlistitem;
      begin
        new(hp,init);
        hp^.def:=def;
        hp2:=firstsym;
        while assigned(hp2) do
         begin
           hp^.addsym(hp2^.sym);
           hp2:=hp2^.next;
         end;
        getcopy:=hp;
      end;


    procedure tsymlist.write;
      var
        hp : psymlistitem;
      begin
        writederef(def);
        hp:=firstsym;
        while assigned(hp) do
         begin
           writederef(hp^.sym);
           hp:=hp^.next;
         end;
        writederef(nil);
      end;


    procedure tsymlist.resolve;
      var
        hp : psymlistitem;
      begin
        resolvedef(def);
        hp:=firstsym;
        while assigned(hp) do
         begin
           resolvesym(hp^.sym);
           hp:=hp^.next;
         end;
      end;


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
         globaldef := ptypesym(srsym)^.restype.def;
      end;

{*****************************************************************************
                        Symbol Call Back Functions
*****************************************************************************}

    procedure derefsym(p : pnamedindexobject);
      begin
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
             assigned(ptypesym(sym)^.restype.def) and
             (ptypesym(sym)^.restype.def^.deftype=objectdef) then
           pobjectdef(ptypesym(sym)^.restype.def)^.check_forwards;
      end;

    procedure labeldefined(p : pnamedindexobject);
      begin
        if (psym(p)^.typ=labelsym) and
           not(plabelsym(p)^.defined) then
         begin
           if plabelsym(p)^.used then
            Message1(sym_e_label_used_and_not_defined,p^.name)
           else
            Message1(sym_w_label_not_defined,p^.name);
         end;
      end;

    procedure unitsymbolused(p : pnamedindexobject);
      begin
         if (psym(p)^.typ=unitsym) and
            (punitsym(p)^.refs=0) and
            { do not claim for unit name itself !! }
            (punitsym(p)^.unitsymtable^.symtabletype=unitsymtable) then
           MessagePos2(psym(p)^.fileinfo,sym_n_unit_not_used,
             p^.name,current_module^.modulename^);
      end;

    procedure varsymbolused(p : pnamedindexobject);
      begin
         if (psym(p)^.typ=varsym) and
            ((psym(p)^.owner^.symtabletype in
             [parasymtable,localsymtable,objectsymtable,staticsymtable])) then
          begin
           { unused symbol should be reported only if no }
           { error is reported                     }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (Errorcount<>0) or
              (copy(p^.name,1,3)='val') or
              (copy(p^.name,1,4)='high') then
             exit;
           if (pvarsym(p)^.refs=0) then
             begin
                if (psym(p)^.owner^.symtabletype=parasymtable) or (vo_is_local_copy in pvarsym(p)^.varoptions) then
                  begin
                    MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_not_used,p^.name);
                  end
                else if (psym(p)^.owner^.symtabletype=objectsymtable) then
                  MessagePos2(psym(p)^.fileinfo,sym_n_private_identifier_not_used,psym(p)^.owner^.name^,p^.name)
                else
                  MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_not_used,p^.name);
             end
           else if pvarsym(p)^.varstate=vs_assigned then
             begin
                if (psym(p)^.owner^.symtabletype=parasymtable) then
                  begin
                    if (pvarsym(p)^.varspez<>vs_var)  then
                      MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_only_set,p^.name)
                  end
                else if (vo_is_local_copy in pvarsym(p)^.varoptions) then
                  begin
                    if (pvarsym(p)^.varspez<>vs_var) then
                      MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_only_set,p^.name);
                  end
                else if (psym(p)^.owner^.symtabletype=objectsymtable) then
                  MessagePos2(psym(p)^.fileinfo,sym_n_private_identifier_only_set,psym(p)^.owner^.name^,p^.name)
                else if (psym(p)^.owner^.symtabletype<>parasymtable) then
                  if not (vo_is_exported in pvarsym(p)^.varoptions) then
                    MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_only_set,p^.name);
             end;
         end
      else if ((psym(p)^.owner^.symtabletype in
              [objectsymtable,parasymtable,localsymtable,staticsymtable])) then
          begin
           if (Errorcount<>0) then
             exit;
           { do not claim for inherited private fields !! }
           if (psym(p)^.refs=0) and (psym(p)^.owner^.symtabletype=objectsymtable) then
             MessagePos2(psym(p)^.fileinfo,sym_n_private_method_not_used,psym(p)^.owner^.name^,p^.name)
           { units references are problematic }
           else if (psym(p)^.refs=0) and not(psym(p)^.typ in [funcretsym,enumsym,unitsym]) then
             if (psym(p)^.typ<>procsym) or not (pprocsym(p)^.is_global) or
             { all program functions are declared global
               but unused should still be signaled PM }
                ((psym(p)^.owner^.symtabletype=staticsymtable) and
                not current_module^.is_unit) then
             MessagePos2(psym(p)^.fileinfo,sym_h_local_symbol_not_used,SymTypeName[psym(p)^.typ],p^.name);
          end;
      end;

    procedure TestPrivate(p : pnamedindexobject);
      begin
        if sp_private in psym(p)^.symoptions then
          varsymbolused(p);
      end;

    procedure objectprivatesymbolused(p : pnamedindexobject);
      begin
         {
           Don't test simple object aliases PM
         }
         if (psym(p)^.typ=typesym) and
            (ptypesym(p)^.restype.def^.deftype=objectdef) and
            (ptypesym(p)^.restype.def^.typesym=ptypesym(p)) then
           pobjectdef(ptypesym(p)^.restype.def)^.symtable^.foreach(
             {$ifndef TP}@{$endif}TestPrivate);
      end;

{$ifdef GDB}
    procedure concatstab(p : pnamedindexobject);
      begin
        if psym(p)^.typ <> procsym then
          psym(p)^.concatstabto(asmoutput);
      end;

    procedure resetstab(p : pnamedindexobject);
      begin
        if psym(p)^.typ <> procsym then
          psym(p)^.isstabwritten:=false;
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
           if assigned(pd^.typesym) then
            pd^.typesym^.isusedinstab := true;
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

{$ifndef DONOTCHAINOPERATORS}
    procedure tsymtable.chainoperators;
      var
        p : pprocsym;
        t : ttoken;
        def : pprocdef;
        storesymtablestack : psymtable;
      begin
         storesymtablestack:=symtablestack;
         symtablestack:=@self;
         make_ref:=false;
         for t:=first_overloaded to last_overloaded do
           begin
              p:=nil;
              def:=nil;
              overloaded_operators[t]:=nil;
              { each operator has a unique lowercased internal name PM }
              while assigned(symtablestack) do
                begin
                  getsym(overloaded_names[t],false);
                  if (t=_STARSTAR) and (srsym=nil) then
                    begin
                      symtablestack:=systemunit;
                      getsym('POWER',false);
                    end;
                  if assigned(srsym) then
                    begin
                       if (srsym^.typ<>procsym) then
                         internalerror(12344321);
                       if assigned(p) then
                         begin
{$ifdef CHAINPROCSYMS}
                           p^.nextprocsym:=pprocsym(srsym);
{$endif CHAINPROCSYMS}
                           def^.nextoverloaded:=pprocsym(srsym)^.definition;
                         end
                       else
                         overloaded_operators[t]:=pprocsym(srsym);
                       p:=pprocsym(srsym);
                       def:=p^.definition;
                       while assigned(def^.nextoverloaded) and
                         (def^.nextoverloaded^.owner=p^.owner) do
                         def:=def^.nextoverloaded;
                       def^.nextoverloaded:=nil;
                       symtablestack:=srsymtable^.next;
                    end
                  else
                    begin
                      symtablestack:=nil;
{$ifdef CHAINPROCSYMS}
                      if assigned(p) then
                        p^.nextprocsym:=nil;
{$endif CHAINPROCSYMS}
                    end;
                  { search for same procsym in other units }
                end;
              symtablestack:=@self;
           end;
         make_ref:=true;
         symtablestack:=storesymtablestack;
      end;
{$endif DONOTCHAINOPERATORS}

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
         if t=parasymtable then
          dataalignment:=4
         else
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


    procedure order_overloads(p : Pnamedindexobject);
      begin
         if psym(p)^.typ=procsym then
           pprocsym(p)^.order_overloaded;
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
        { first deref the ttypesyms }
        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.prederef;
           hs:=psym(hs^.next);
         end;
        { deref the definitions }
        hp:=pdef(defindex^.first);
        while assigned(hp) do
         begin
           hp^.deref;
           hp:=pdef(hp^.next);
         end;
        { deref the symbols }
        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.deref;
           hs:=psym(hs^.next);
         end;
      end;

    { this procedure is reserved for inserting case variant into
      a record symtable }
    { the offset is the location of the start of the variant
      and datasize and dataalignment corresponds to
      the complete size (see code in pdecl unit) PM }
    procedure tsymtable.insert_in(psymt : psymtable;offset : longint);
      var
        ps,nps : pvarsym;
        pd,npd : pdef;
        storesize,storealign : longint;
      begin
        storesize:=psymt^.datasize;
        storealign:=psymt^.dataalignment;
        psymt^.datasize:=offset;
        ps:=pvarsym(symindex^.first);
        while assigned(ps) do
          begin
            { this is used to insert case variant into the main
              record }
            if ps^.address=0 then
              psymt^.datasize:=offset;
            nps:=pvarsym(ps^.next);
            symindex^.deleteindex(ps);
            ps^.next:=nil;
            ps^.left:=nil;
            ps^.right:=nil;
            psymt^.insert(ps);
            ps:=nps;
          end;
        pd:=pdef(defindex^.first);
        while assigned(pd) do
          begin
            npd:=pdef(pd^.next);
            defindex^.deleteindex(pd);
            pd^.next:=nil;
            pd^.left:=nil;
            pd^.right:=nil;
            psymt^.registerdef(pd);
            pd:=npd;
          end;
        psymt^.datasize:=storesize;
        psymt^.dataalignment:=storealign;
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
       { reset }
         defowner:=nil;
         name:=nil;
         if typ=parasymtable then
          dataalignment:=4
         else
          dataalignment:=1;
         datasize:=0;
         address_fixup:= 0;
         unitid:=0;
       { setup symtabletype specific things }
         case typ of
           unitsymtable :
             begin
               symtablelevel:=0;
{$ifndef NEWMAP}
               current_module^.map^[0]:=@self;
{$else NEWMAP}
               current_module^.globalsymtable:=@self;
{$endif NEWMAP}
             end;
           recordsymtable,
           objectsymtable :
             begin
               storesymtable:=aktrecordsymtable;
               aktrecordsymtable:=@self;
             end;
           parasymtable,
           localsymtable :
             begin
               storesymtable:=aktlocalsymtable;
               aktlocalsymtable:=@self;
             end;
         { used for local browser }
           staticppusymtable :
             begin
               aktstaticsymtable:=@self;
               symsearch^.usehash;
             end;
         end;

      { we need the correct symtable for registering }
         if not (typ in [localsymtable,parasymtable,recordsymtable,objectsymtable]) then
           begin
             next:=symtablestack;
             symtablestack:=@self;
           end;

      { load definitions }
         loaddefs;

      { load symbols }
         loadsyms;

         if not (typ in [localsymtable,parasymtable,recordsymtable,objectsymtable]) then
          begin
            { now we can deref the syms and defs }
            deref;
            { restore symtablestack }
            symtablestack:=next;
          end;

         case typ of
           unitsymtable :
             begin
{$ifdef NEWMAP}
               { necessary for dependencies }
               current_module^.globalsymtable:=nil;
{$endif NEWMAP}
             end;
           recordsymtable,
           objectsymtable :
             aktrecordsymtable:=storesymtable;
           localsymtable,
           parasymtable :
             aktlocalsymtable:=storesymtable;
         end;

        in_loading:=st_loading;
      end;


    procedure tsymtable.writeas;
      var
         oldtyp : byte;
         storesymtable : psymtable;
      begin
         storesymtable:=aktrecordsymtable;
         case symtabletype of
           recordsymtable,
           objectsymtable :
             begin
               storesymtable:=aktrecordsymtable;
               aktrecordsymtable:=@self;
               oldtyp:=current_ppu^.entrytyp;
               current_ppu^.entrytyp:=subentryid;
             end;
           parasymtable,
           localsymtable :
             begin
               storesymtable:=aktlocalsymtable;
               aktlocalsymtable:=@self;
             end;
         end;
       { order procsym overloads }
         foreach({$ifndef TP}@{$endif}Order_overloads);
         { write definitions }
         writedefs;
         { write symbols }
         writesyms;
         case symtabletype of
           recordsymtable,
           objectsymtable :
             begin
               current_ppu^.entrytyp:=oldtyp;
               aktrecordsymtable:=storesymtable;
             end;
           localsymtable,
           parasymtable :
             aktlocalsymtable:=storesymtable;
         end;
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


    procedure tsymtable.insert(sym:psym);
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
         { check the current symtable }
         hsym:=search(sym^.name);
         if assigned(hsym) then
          begin
            { in TP and Delphi you can have a local with the
              same name as the function, the function is then hidden for
              the user. (Under delphi it can still be accessed using result),
              but don't allow hiding of RESULT }
            if (m_tp in aktmodeswitches) and
               (hsym^.typ=funcretsym) and
               not((m_result in aktmodeswitches) and
                   (hsym^.name='RESULT')) then
             hsym^.owner^.rename(hsym^.name,'hidden'+hsym^.name)
            else
             begin
               DuplicateSym(hsym);
               exit;
             end;
          end;
         { check for duplicate id in local and parasymtable symtable }
         if (symtabletype=localsymtable) then
           { to be on the save side: }
           begin
              if assigned(next) and
                (next^.symtabletype=parasymtable) then
                begin
                  hsym:=next^.search(sym^.name);
                  if assigned(hsym) then
                   begin
                     { a parameter and the function can have the same
                       name in TP and Delphi, but RESULT not }
                     if (m_tp in aktmodeswitches) and
                        (sym^.typ=funcretsym) and
                        not((m_result in aktmodeswitches) and
                            (sym^.name='RESULT')) then
                      sym^.setname('hidden'+sym^.name)
                     else
                      begin
                        DuplicateSym(hsym);
                        exit;
                      end;
                   end;
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
              if assigned(hsym) and
                { private ids can be reused }
                (not(sp_private in hsym^.symoptions) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                begin
                   { delphi allows to reuse the names in a class, but not
                     in object (tp7 compatible) }
                   if not((m_delphi in aktmodeswitches) and
                          (pobjectdef(next^.next^.defowner)^.is_class)) then
                    begin
                      DuplicateSym(hsym);
                      exit;
                    end;
                end;
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
              if assigned(hsym) and
                { private ids can be reused }
                (not(sp_private in hsym^.symoptions) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                begin
                   { delphi allows to reuse the names in a class, but not
                     in object (tp7 compatible) }
                   if not((m_delphi in aktmodeswitches) and
                          (procinfo^._class^.is_class)) then
                    begin
                      DuplicateSym(hsym);
                      exit;
                    end;
                end;
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
               begin
                 DuplicateSym(hsym);
                 exit;
               end;
           end;
         { register definition of typesym }
         if (sym^.typ = typesym) and
            assigned(ptypesym(sym)^.restype.def) then
          begin
            if not(assigned(ptypesym(sym)^.restype.def^.owner)) and
               (ptypesym(sym)^.restype.def^.deftype<>errordef) then
              registerdef(ptypesym(sym)^.restype.def);
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
        newref : pref;
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
           {if (symtabletype=unitsymtable) and (hp^.typ=unitsym) and
              assigned(current_module) and (current_module^.globalsymtable<>@self) then
             hp:=nil;}
           if assigned(hp) and
              (cs_browser in aktmoduleswitches) and make_ref then
             begin
                new(newref,init(hp^.lastref,@tokenpos));
                { for symbols that are in tables without
                browser info or syssyms (PM) }
                if hp^.refcount=0 then
                  begin
                    hp^.defref:=newref;
                    hp^.lastref:=newref;
                  end
                else
                if resolving_forward and assigned(hp^.defref) then
                { put it as second reference }
                  begin
                   newref^.nextref:=hp^.defref^.nextref;
                   hp^.defref^.nextref:=newref;
                   hp^.lastref^.nextref:=nil;
                  end
                else
                  hp^.lastref:=newref;
                inc(hp^.refcount);
             end;
           if assigned(hp) and make_ref then
             begin
               inc(hp^.refs);
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
         if symtabletype in [recordsymtable,objectsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         if symtabletype in [parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktlocalsymtable;
              aktlocalsymtable:=@self;
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
        if symtabletype in [recordsymtable,objectsymtable] then
          aktrecordsymtable:=oldrecsyms;
        if symtabletype in [parasymtable,localsymtable] then
          aktlocalsymtable:=oldrecsyms;
      end;


    procedure tsymtable.write_browser;
      var
         oldrecsyms : psymtable;
      begin
         { symbol numbering for references
           should have been done in write PM
         number_symbols;
         number_defs;   }

         if symtabletype in [recordsymtable,objectsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         if symtabletype in [parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktlocalsymtable;
              aktlocalsymtable:=@self;
           end;
         current_ppu^.writeentry(ibbeginsymtablebrowser);
         foreach({$ifndef TP}@{$endif}write_refs);
         current_ppu^.writeentry(ibendsymtablebrowser);
        if symtabletype in [recordsymtable,objectsymtable] then
          aktrecordsymtable:=oldrecsyms;
        if symtabletype in [parasymtable,localsymtable] then
          aktlocalsymtable:=oldrecsyms;
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
                  assigned(defowner^.typesym) then
                  Browserlog.AddLog('---Symtable '+defowner^.typesym^.name)
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

    procedure tsymtable.set_alignment(_alignment : longint);
      var
         sym : pvarsym;
         l : longint;
      begin
        dataalignment:=_alignment;
        if (symtabletype<>parasymtable) then
          internalerror(1111);
        sym:=pvarsym(symindex^.first);
        datasize:=0;
        { there can be only varsyms }
        while assigned(sym) do
          begin
             l:=sym^.getpushsize;
             sym^.address:=datasize;
             datasize:=align(datasize+l,dataalignment);
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

    procedure tsymtable.allprivatesused;
      begin
         foreach({$ifndef TP}@{$endif}objectprivatesymbolused);
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
        if symtabletype in [inlineparasymtable,inlinelocalsymtable] then
          foreach({$ifndef TP}@{$endif}resetstab);

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
         if (t = globalsymtable) then
           begin
              prev_dbx_counter := dbx_counter;
              dbx_counter := nil;
           end;
         is_stab_written:=false;
         dbx_count := -1;
         if cs_gdb_dbx in aktglobalswitches then
           begin
             dbx_count := 0;
             unittypecount:=1;
             if (symtabletype=globalsymtable) then
               pglobaltypecount := @unittypecount;
             unitid:=current_module^.unitcount;
             debuglist^.concat(new(pai_asm_comment,init(strpnew('Global '+name^+' has index '+tostr(unitid)))));
             debuglist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'+tostr(N_BINCL)+',0,0,0'))));
             inc(current_module^.unitcount);
             dbx_count_ok:=false;
             dbx_counter:=@dbx_count;
             do_count_dbx:=true;
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
{$ifdef GDB}
         if cs_gdb_dbx in aktglobalswitches then
           begin
              UnitTypeCount:=1;
              storeGlobalTypeCount:=PGlobalTypeCount;
              PglobalTypeCount:=@UnitTypeCount;
           end;
{$endif GDB}

       { load symtables }
         inherited loadas(unitsymtable);

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
              dbx_count_ok := {true}false;
           end
         else
           begin
             dbx_count := -1;
             dbx_count_ok:=false;
           end;
         if cs_gdb_dbx in aktglobalswitches then
           PGlobalTypeCount:=storeGlobalTypeCount;
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
        inherited writeas;

      { all after doesn't affect crc }
        current_ppu^.do_crc:=false;

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

         { write static symtable
           needed for local debugging of unit functions }
        if ((current_module^.flags and uf_local_browser)<>0) and
           assigned(current_module^.localsymtable) then
          psymtable(current_module^.localsymtable)^.writeas;
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
           if (symtabletype = unitsymtable) and
              (current_module^.globalsymtable<>@Self) then
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
                     asmlist^.concat(new(pai_asm_comment,init(strpnew('"repeated" unit '+name^
                              +' has index '+tostr(unitid)+' dbx count = '+tostr(dbx_count)))));
                     asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                       +tostr(N_EXCL)+',0,0,'+tostr(dbx_count)))));
                     exit;
                  end
                else if (current_module^.globalsymtable<>@Self) then
                  begin
                    prev_dbx_count := dbx_counter;
                    dbx_counter := nil;
                    do_count_dbx:=false;
                    if symtabletype = unitsymtable then
                      asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                        +tostr(N_BINCL)+',0,0,0'))));
                    dbx_counter := @dbx_count;
                    dbx_count:=0;
                    do_count_dbx:=assigned(dbx_counter);
                  end;
             end;
           asmoutput:=asmlist;
           foreach({$ifndef TP}@{$endif}concattypestab);
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if (current_module^.globalsymtable<>@Self) then
                  begin
                    dbx_counter := prev_dbx_count;
                    do_count_dbx:=false;
                    asmlist^.concat(new(pai_asm_comment,init(strpnew('End unit '+name^
                      +' has index '+tostr(unitid)))));
                    asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                      +tostr(N_EINCL)+',0,0,0'))));
                    do_count_dbx:=assigned(dbx_counter);
                    dbx_count_ok := {true}false;
                  end;
             end;
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
         typeglobalnumber := ptypesym(srsym)^.restype.def^.numberstring;
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
            if assigned(def^.typesym) then
              def^.typesym^.isusedinstab:=false;
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


{$ifdef UNITALIASES}
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
{$endif UNITALIASES}


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

    procedure RestoreUnitSyms;
      var
         p : psymtable;
      begin
         p:=symtablestack;
         while assigned(p) do
           begin
             if (p^.symtabletype=unitsymtable) and
               assigned(punitsymtable(p)^.unitsym) and
               ((punitsymtable(p)^.unitsym^.owner=psymtable(current_module^.globalsymtable)) or
                (punitsymtable(p)^.unitsym^.owner=psymtable(current_module^.localsymtable))) then
                 punitsymtable(p)^.unitsym^.restoreunitsym;
             p:=p^.next;
           end;
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
     var
       token : ttoken;
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
{$ifdef UNITALIASES}
     { unit aliases }
        unitaliases:=new(pdictionary,init);
{$endif}
       for token:=first_overloaded to last_overloaded do
         overloaded_operators[token]:=nil;
     end;


   procedure DoneSymtable;
      begin
        dispose(generrorsym,done);
        dispose(generrordef,done);
{$ifdef UNITALIASES}
        dispose(unitaliases,done);
{$endif}
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
  Revision 1.101  2000-06-23 21:34:10  pierre
   * align all variants to same start address

  Revision 1.100  2000/06/18 18:11:32  peter
    * C record packing fixed to also check first entry of the record
      if bigger than the recordalignment itself
    * variant record alignment uses alignment per variant and saves the
      highest alignment value

  Revision 1.99  2000/06/14 19:00:58  peter
    * rename the result of a function to hide it instead of using setname

  Revision 1.98  2000/06/14 16:51:18  peter
    * removed unused label i left in when testing

  Revision 1.97  2000/06/09 21:34:40  peter
    * checking for dup id with para of methods fixed for delphi mode

  Revision 1.96  2000/06/05 20:41:17  pierre
    + support for NOT overloading
    + unsupported overloaded operators generate errors

  Revision 1.95  2000/06/02 21:17:26  pierre
   fix bug in tbs/tbs0317

  Revision 1.94  2000/06/02 18:48:48  florian
    + fieldtable support for classes

  Revision 1.93  2000/06/01 19:07:52  peter
    * delphi/tp mode fixes for dup id checking (tbs319,tbf320)

  Revision 1.92  2000/05/23 14:15:44  pierre
   * fix for bug 959

  Revision 1.91  2000/05/12 05:59:57  pierre
   * * get it to compile with Delphi by Kovacs Attila Zoltan

  Revision 1.90  2000/05/11 09:40:12  pierre
    * some DBX changes but it still does not work !

  Revision 1.89  2000/05/03 14:34:05  pierre
   * fix the unitsym chain

  Revision 1.88  2000/04/27 11:35:04  pierre
   * power to ** operator fixed

  Revision 1.87  2000/04/27 10:06:04  pierre
    * fix for snapshot failue
    * order_overloaded reintrocduced and adapted to operators

  Revision 1.86  2000/04/26 08:54:19  pierre
    * More changes for operator bug
      Order_overloaded method removed because it conflicted with
      new implementation where the defs are ordered
      according to the unit loading order !

  Revision 1.85  2000/04/25 23:55:30  pierre
    + Hint about unused unit
    * Testop bug fixed !!
      Now the operators are only applied if the unit is explicitly loaded

  Revision 1.84  2000/04/24 12:45:44  peter
    * made overloaded_operators local per unit, but it still doesn't work
      correct

  Revision 1.83  2000/03/27 21:15:34  pierre
   * fix bug 294 in a BP compatible way ie. hidding the function result

  Revision 1.82  2000/03/22 09:25:57  florian
    * bug 294 fixed: parameters can have now the same name as the function/
      procedure, this is compatible with TP/Delphi

  Revision 1.81  2000/03/20 09:34:33  florian
    * in delphi mode: method parameters can now have the same name as parameters

  Revision 1.80  2000/03/01 13:56:31  pierre
   * fix for bug 840

  Revision 1.79  2000/03/01 00:03:10  pierre
    * fixes for locals in inlined procedures
      fix for bug797
    + stabs generation for inlined paras and locals

  Revision 1.78  2000/02/20 20:49:45  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

  Revision 1.77  2000/02/11 13:53:49  pierre
   * avoid stack overflow in tref.done (bug 846)

  Revision 1.76  2000/02/09 13:23:05  peter
    * log truncated

  Revision 1.75  2000/01/12 10:38:18  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.74  2000/01/09 00:37:56  pierre
   * avoid testing object types that are simple aliases for unused privates

  Revision 1.73  2000/01/07 01:14:41  peter
    * updated copyright to 2000

  Revision 1.72  2000/01/03 19:26:04  peter
    * fixed resolving of ttypesym which are reference from object/record
      fields.

  Revision 1.71  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.70  1999/12/02 11:28:27  peter
    * moved verbose to implementation uses

  Revision 1.69  1999/12/01 22:32:35  pierre
   * give info of original duplicated symbol more often

  Revision 1.68  1999/11/30 10:40:56  peter
    + ttype, tsymlist

  Revision 1.67  1999/11/24 11:41:05  pierre
   * defaultsymtablestack is now restored after parser.compile

  Revision 1.66  1999/11/22 00:23:09  pierre
   * also complain about unused functions in program

  Revision 1.65  1999/11/19 14:49:15  pierre
   * avoid certain wrong notes/hints

  Revision 1.64  1999/11/18 15:34:48  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.63  1999/11/17 17:05:06  pierre
   * Notes/hints changes

  Revision 1.62  1999/11/15 22:00:48  peter
    * labels used but not defined give error instead of warning, the warning
      is now only with declared but not defined and not used.

  Revision 1.61  1999/11/15 17:52:59  pierre
    + one field added for ttoken record for operator
      linking the id to the corresponding operator token that
      can now now all be overloaded
    * overloaded operators are resetted to nil in InitSymtable
      (bug when trying to compile a uint that overloads operators twice)

  Revision 1.60  1999/11/09 23:35:50  pierre
   + better reference pos for forward defs

  Revision 1.59  1999/11/06 16:21:57  jonas
    + search optimial register to use in alignment code (compile with
      -dalignreg, -dalignregdebug to see chosen register in
      assembler code). Still needs support in ag386bin.

  Revision 1.58  1999/11/06 14:34:28  peter
    * truncated log to 20 revs

  Revision 1.57  1999/11/05 17:18:03  pierre
    * local browsing works at first level
      ie for function defined in interface or implementation
      not yet for functions inside other functions

  Revision 1.56  1999/11/04 23:13:25  peter
    * moved unit alias support into ifdef

}