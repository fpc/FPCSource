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
unit symtable;

{$i defines.inc}

interface

    uses
       { common }
       cutils,cobjects,
       { global }
       globtype,tokens,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,
       { assembler }
       aasm
       ;


{****************************************************************************
                             Symtable types
****************************************************************************}

    type
       pstoredsymtable = ^tstoredsymtable;
       tstoredsymtable = object(tsymtable)
          constructor init(t : tsymtabletype);
          { load/write }
          constructor loadas(typ : tsymtabletype);
          procedure writeas;
          procedure loaddefs;
          procedure loadsyms;
          procedure writedefs;
          procedure writesyms;
          procedure deref;
          procedure insert(sym : psymentry);virtual;
          procedure insert_in(psymt : psymtable;offset : longint);
          function  speedsearch(const s : stringid;speedvalue : longint) : psymentry;virtual;
          procedure allsymbolsused;
          procedure allprivatesused;
          procedure allunitsused;
          procedure check_forwards;
          procedure checklabels;
          { change alignment for args  only parasymtable }
          procedure set_alignment(_alignment : longint);
{$ifdef CHAINPROCSYMS}
          procedure chainprocsyms;
{$endif CHAINPROCSYMS}
{$ifndef DONOTCHAINOPERATORS}
          procedure chainoperators;
{$endif DONOTCHAINOPERATORS}
          procedure load_browser;
          procedure write_browser;
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
          function getnewtypecount : word; virtual;
{$endif GDB}
       end;

       punitsymtable = ^tunitsymtable;
       tunitsymtable = object(tstoredsymtable)
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
          function getnewtypecount : word; virtual;
{$endif GDB}
          procedure load_symtable_refs;
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
          procedure clear;virtual;
        end;


    var
       srsym          : psym;           { result of the last search }
       srsymtable     : psymtable;
       lastsrsym      : psym;           { last sym found in statement }
       lastsrsymtable : psymtable;
       lastsymknown   : boolean;
       constsymtable  : psymtable;      { symtable were the constants can be inserted }
       systemunit     : punitsymtable;  { pointer to the system unit }
       read_member : boolean;      { reading members of an symtable }

       lexlevel : longint;       { level of code                     }
                                   { 1 for main procedure             }
                                   { 2 for normal function or proc     }
                                   { higher for locals           }

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
    procedure numberunits;
    procedure load_interface;

{*** Object Helpers ***}
    function search_class_member(pd : pobjectdef;const n : string) : psym;
    function search_default_property(pd : pobjectdef) : ppropertysym;

{*** symtable stack ***}
    procedure dellexlevel;
    procedure RestoreUnitSyms;
{$ifdef DEBUG}
    procedure test_symtablestack;
    procedure list_symtablestack;
{$endif DEBUG}

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

{*** Init / Done ***}
    procedure InitSymtable;
    procedure DoneSymtable;


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



implementation

    uses
      { global }
      version,verbose,globals,
      { target }
      systems,
      { ppu }
      symppu,ppu,
      { module }
      finput,fmodule,
{$ifdef GDB}
      gdb,
{$endif GDB}
      { scanner }
      scanner,
      { codegen }
      hcodegen
      ;


    var
      in_loading : boolean;  { remove !!! }


{*****************************************************************************
                        Symbol Call Back Functions
*****************************************************************************}

    procedure write_refs(sym : pnamedindexobject);
      begin
         pstoredsym(sym)^.write_references;
      end;

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
            Message1(sym_e_label_used_and_not_defined,plabelsym(p)^.realname)
           else
            Message1(sym_w_label_not_defined,plabelsym(p)^.realname);
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
                    MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_not_used,psym(p)^.realname);
                  end
                else if (psym(p)^.owner^.symtabletype=objectsymtable) then
                  MessagePos2(psym(p)^.fileinfo,sym_n_private_identifier_not_used,psym(p)^.owner^.name^,psym(p)^.realname)
                else
                  MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_not_used,psym(p)^.realname);
             end
           else if pvarsym(p)^.varstate=vs_assigned then
             begin
                if (psym(p)^.owner^.symtabletype=parasymtable) then
                  begin
                    if not(pvarsym(p)^.varspez in [vs_var,vs_out])  then
                      MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_only_set,psym(p)^.realname)
                  end
                else if (vo_is_local_copy in pvarsym(p)^.varoptions) then
                  begin
                    if not(pvarsym(p)^.varspez in [vs_var,vs_out]) then
                      MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_only_set,psym(p)^.realname);
                  end
                else if (psym(p)^.owner^.symtabletype=objectsymtable) then
                  MessagePos2(psym(p)^.fileinfo,sym_n_private_identifier_only_set,psym(p)^.owner^.name^,psym(p)^.realname)
                else if (psym(p)^.owner^.symtabletype<>parasymtable) then
                  if not (vo_is_exported in pvarsym(p)^.varoptions) then
                    MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_only_set,psym(p)^.realname);
             end;
         end
      else if ((psym(p)^.owner^.symtabletype in
              [objectsymtable,parasymtable,localsymtable,staticsymtable])) then
          begin
           if (Errorcount<>0) then
             exit;
           { do not claim for inherited private fields !! }
           if (pstoredsym(p)^.refs=0) and (psym(p)^.owner^.symtabletype=objectsymtable) then
             MessagePos2(psym(p)^.fileinfo,sym_n_private_method_not_used,psym(p)^.owner^.name^,psym(p)^.realname)
           { units references are problematic }
           else if (pstoredsym(p)^.refs=0) and not(psym(p)^.typ in [funcretsym,enumsym,unitsym]) then
             if (psym(p)^.typ<>procsym) or not (pprocsym(p)^.is_global) or
             { all program functions are declared global
               but unused should still be signaled PM }
                ((psym(p)^.owner^.symtabletype=staticsymtable) and
                not current_module^.is_unit) then
             MessagePos2(psym(p)^.fileinfo,sym_h_local_symbol_not_used,SymTypeName[psym(p)^.typ],psym(p)^.realname);
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
            (ptypesym(p)^.restype.def^.typesym=psym(p)) then
           pobjectdef(ptypesym(p)^.restype.def)^.symtable^.foreach(
             {$ifdef FPCPROCVAR}@{$endif}TestPrivate);
      end;

{$ifdef GDB}
    var
      asmoutput : paasmoutput;

    procedure concatstab(p : pnamedindexobject);
      begin
        if psym(p)^.typ <> procsym then
          pstoredsym(p)^.concatstabto(asmoutput);
      end;

    procedure resetstab(p : pnamedindexobject);
      begin
        if psym(p)^.typ <> procsym then
          pstoredsym(p)^.isstabwritten:=false;
      end;

    procedure concattypestab(p : pnamedindexobject);
      begin
        if psym(p)^.typ = typesym then
         begin
           pstoredsym(p)^.isstabwritten:=false;
           pstoredsym(p)^.concatstabto(asmoutput);
         end;
      end;
{$endif GDB}

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


{****************************************************************************
                              STORED SYMTABLE
****************************************************************************}

    constructor tstoredsymtable.init(t : tsymtabletype);
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


{$ifndef DONOTCHAINOPERATORS}
    procedure tstoredsymtable.chainoperators;
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


    procedure tstoredsymtable.loaddefs;
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


    procedure tstoredsymtable.loadsyms;
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


    procedure tstoredsymtable.writedefs;
      var
         pd : pstoreddef;
      begin
      { each definition get a number, write then the amount of defs to the
         ibstartdef entry }
         current_ppu^.putlongint(defindex^.count);
         current_ppu^.writeentry(ibstartdefs);
      { now write the definition }
         pd:=pstoreddef(defindex^.first);
         while assigned(pd) do
           begin
              pd^.write;
              pd:=pstoreddef(pd^.indexnext);
           end;
      { write end of definitions }
         current_ppu^.writeentry(ibenddefs);
      end;


    procedure tstoredsymtable.writesyms;
      var
        pd : pstoredsym;
      begin
       { each definition get a number, write then the amount of syms and the
         datasize to the ibsymdef entry }
         current_ppu^.putlongint(symindex^.count);
         current_ppu^.putlongint(datasize);
         current_ppu^.putlongint(dataalignment);
         current_ppu^.writeentry(ibstartsyms);
       { foreach is used to write all symbols }
         pd:=pstoredsym(symindex^.first);
         while assigned(pd) do
           begin
              pd^.write;
              pd:=pstoredsym(pd^.indexnext);
           end;
       { end of symbols }
         current_ppu^.writeentry(ibendsyms);
      end;


{***********************************************
                Browser
***********************************************}

    procedure tstoredsymtable.load_browser;
      var
        b     : byte;
        sym   : pstoredsym;
        prdef : pstoreddef;
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
                        sym:=pstoredsym(readderef);
                        resolvesym(sym);
                        if assigned(sym) then
                          sym^.load_references;
                      end;
           ibdefref : begin
                        prdef:=pstoreddef(readderef);
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


    procedure tstoredsymtable.write_browser;
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
         foreach({$ifdef FPCPROCVAR}@{$endif}write_refs);
         current_ppu^.writeentry(ibendsymtablebrowser);
        if symtabletype in [recordsymtable,objectsymtable] then
          aktrecordsymtable:=oldrecsyms;
        if symtabletype in [parasymtable,localsymtable] then
          aktlocalsymtable:=oldrecsyms;
      end;


{$ifdef GDB}
   function tstoredsymtable.getnewtypecount : word;
      begin
         getnewtypecount:=pglobaltypecount^;
         inc(pglobaltypecount^);
      end;
{$endif GDB}


    procedure order_overloads(p : Pnamedindexobject);
      begin
         if psym(p)^.typ=procsym then
           pprocsym(p)^.order_overloaded;
      end;


    procedure tstoredsymtable.deref;
      var
        hp : pdef;
        hs : psym;
      begin
        { first deref the ttypesyms }
        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.prederef;
           hs:=psym(hs^.indexnext);
         end;
        { deref the definitions }
        hp:=pdef(defindex^.first);
        while assigned(hp) do
         begin
           hp^.deref;
           hp:=pdef(hp^.indexnext);
         end;
        { deref the symbols }
        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.deref;
           hs:=psym(hs^.indexnext);
         end;
      end;

    { this procedure is reserved for inserting case variant into
      a record symtable }
    { the offset is the location of the start of the variant
      and datasize and dataalignment corresponds to
      the complete size (see code in pdecl unit) PM }
    procedure tstoredsymtable.insert_in(psymt : psymtable;offset : longint);
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
            psymt^.datasize:=ps^.address+offset;
            nps:=pvarsym(ps^.indexnext);
            symindex^.deleteindex(ps);
            ps^.indexnext:=nil;
            ps^.left:=nil;
            ps^.right:=nil;
            psymt^.insert(ps);
            ps:=nps;
          end;
        pd:=pdef(defindex^.first);
        while assigned(pd) do
          begin
            npd:=pdef(pd^.indexnext);
            defindex^.deleteindex(pd);
            pd^.indexnext:=nil;
            pd^.left:=nil;
            pd^.right:=nil;
            psymt^.registerdef(pd);
            pd:=npd;
          end;
        psymt^.datasize:=storesize;
        psymt^.dataalignment:=storealign;
      end;

    constructor tstoredsymtable.loadas(typ : tsymtabletype);
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


    procedure tstoredsymtable.writeas;
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
         foreach({$ifdef FPCPROCVAR}@{$endif}Order_overloads);
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


    procedure tstoredsymtable.insert(sym:psymentry);
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
           pstoredsym(sym)^.insert_in_data;
         if (symtabletype in [staticsymtable,globalsymtable]) then
           begin
              hp:=symtablestack;
              while assigned(hp) do
                begin
                   if hp^.symtabletype in [staticsymtable,globalsymtable] then
                    begin
                       hsym:=psym(hp^.search(sym^.name));
                       if assigned(hsym) then
                         DuplicateSym(hsym);
                    end;
                  hp:=hp^.next;
                end;
           end;
         { check the current symtable }
         hsym:=psym(search(sym^.name));
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
                  hsym:=psym(next^.search(sym^.name));
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
                          is_class(pdef(next^.next^.defowner))) then
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
                          is_class(procinfo^._class)) then
                    begin
                      DuplicateSym(hsym);
                      exit;
                    end;
                end;
           end;
         { check for duplicate field id in inherited classes }
         if (sym^.typ=varsym) and
            (symtabletype=objectsymtable) and
            assigned(defowner) and
            (
             not(m_delphi in aktmodeswitches) or
             is_object(pdef(defowner))
            ) then
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
                {sym^.concatstabto(debuglist);}
              end;
{$endif GDB}
          end;
         { insert in index and search hash }
         symindex^.insert(sym);
         symsearch^.insert(sym);
      end;


    function tstoredsymtable.speedsearch(const s : stringid;speedvalue : longint) : psymentry;
      var
        hp : pstoredsym;
        newref : pref;
      begin
        hp:=pstoredsym(inherited speedsearch(s,speedvalue));
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

{$ifdef GDB}
           { if it is a type, we need the stabs of this type
             this might be the cause of the class debug problems
             as TCHILDCLASS.Create did not generate appropriate
             stabs debug info if TCHILDCLASS wasn't used anywhere else PM }
           if (hp^.typ=typesym) and make_ref then
             begin
               if assigned(ptypesym(hp)^.restype.def) then
                 pstoreddef(ptypesym(hp)^.restype.def)^.numberstring
               else
                 ptypesym(hp)^.isusedinstab:=true;
             end;
{$endif GDB}
           { unitsym are only loaded for browsing PM    }
           { this was buggy anyway because we could use }
           { unitsyms from other units in _USES !!      }
           {if (symtabletype=unitsymtable) and (hp^.typ=unitsym) and
              assigned(current_module) and (current_module^.globalsymtable<>@self) then
             hp:=nil;}
           if assigned(hp) and
              (cs_browser in aktmoduleswitches) and make_ref then
             begin
                new(newref,init(hp^.lastref,@akttokenpos));
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

{***********************************************
           Process all entries
***********************************************}

    { checks, if all procsyms and methods are defined }
    procedure tstoredsymtable.check_forwards;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}check_forward);
      end;

    procedure tstoredsymtable.checklabels;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}labeldefined);
      end;

    procedure tstoredsymtable.set_alignment(_alignment : longint);
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
             sym:=pvarsym(sym^.indexnext);
          end;
      end;

    procedure tstoredsymtable.allunitsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}unitsymbolused);
      end;

    procedure tstoredsymtable.allsymbolsused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}varsymbolused);
      end;

    procedure tstoredsymtable.allprivatesused;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}objectprivatesymbolused);
      end;

{$ifdef CHAINPROCSYMS}
    procedure tstoredsymtable.chainprocsyms;
      begin
         foreach({$ifdef FPCPROCVAR}@{$endif}chainprocsym);
      end;
{$endif CHAINPROCSYMS}

{$ifdef GDB}
    procedure tstoredsymtable.concatstabto(asmlist : paasmoutput);
      begin
        asmoutput:=asmlist;
        if symtabletype in [inlineparasymtable,inlinelocalsymtable] then
          foreach({$ifdef FPCPROCVAR}@{$endif}resetstab);

        foreach({$ifdef FPCPROCVAR}@{$endif}concatstab);
      end;
{$endif}


{****************************************************************************
                              TWITHSYMTABLE
****************************************************************************}

    constructor twithsymtable.init;
      begin
         inherited init(withsymtable);
         direct_with:=false;
         withnode:=nil;
         withrefnode:=nil;
         { we don't need the symsearch }
         dispose(symsearch,done);
         symsearch:=nil;
      end;


    destructor twithsymtable.done;
      begin
        symsearch:=nil;
        inherited done;
      end;

    procedure twithsymtable.clear;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
           recorddef  or objectdef symtable }
      end;


{****************************************************************************
                            PPU Writing Helpers
****************************************************************************}

    procedure writesourcefiles;
      var
        hp    : pinputfile;
        i,j : longint;
      begin
      { second write the used source files }
        current_ppu^.do_crc:=false;
        hp:=current_module^.sourcefiles^.files;
      { write source files directly in good order }
        j:=0;
        while assigned(hp) do
          begin
            inc(j);
            hp:=hp^.ref_next;
          end;
        while j>0 do
          begin
            hp:=current_module^.sourcefiles^.files;
            for i:=1 to j-1 do
              hp:=hp^.ref_next;
            current_ppu^.putstring(hp^.name^);
            dec(j);
         end;
        current_ppu^.writeentry(ibsourcefiles);
        current_ppu^.do_crc:=true;
      end;

    procedure writeusedmacro(p:pnamedindexobject);
      begin
        if pmacro(p)^.is_used or pmacro(p)^.defined_at_startup then
          begin
            current_ppu^.putstring(p^.name);
            current_ppu^.putbyte(byte(pmacro(p)^.defined_at_startup));
            current_ppu^.putbyte(byte(pmacro(p)^.is_used));
          end;
      end;

    procedure writeusedmacros;
      begin
        current_ppu^.do_crc:=false;
        current_scanner^.macros^.foreach({$ifdef FPCPROCVAR}@{$endif}writeusedmacro);
        current_ppu^.writeentry(ibusedmacros);
        current_ppu^.do_crc:=true;
      end;


    procedure writeusedunit;
      var
        hp      : pused_unit;
      begin
        numberunits;
        hp:=pused_unit(current_module^.used_units.first);
        while assigned(hp) do
         begin
           { implementation units should not change
             the CRC PM }
           current_ppu^.do_crc:=hp^.in_interface;
           current_ppu^.putstring(hp^.name^);
           { the checksum should not affect the crc of this unit ! (PFV) }
           current_ppu^.do_crc:=false;
           current_ppu^.putlongint(hp^.checksum);
           current_ppu^.putlongint(hp^.interface_checksum);
           current_ppu^.putbyte(byte(hp^.in_interface));
           current_ppu^.do_crc:=true;
           hp:=pused_unit(hp^.next);
         end;
        current_ppu^.do_interface_crc:=true;
        current_ppu^.writeentry(ibloadunit);
      end;


    procedure writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
      var
        hcontainer : tlinkcontainer;
        s : string;
        mask : longint;
      begin
        hcontainer.init;
        while not p.empty do
         begin
           s:=p.get(mask);
           if strippath then
            current_ppu^.putstring(SplitFileName(s))
           else
            current_ppu^.putstring(s);
           current_ppu^.putlongint(mask);
           hcontainer.insert(s,mask);
         end;
        current_ppu^.writeentry(id);
        p:=hcontainer;
      end;


    procedure writeunitas(const s : string;unittable : punitsymtable;only_crc : boolean);
      begin
         Message1(unit_u_ppu_write,s);

       { create unit flags }
         with Current_Module^ do
          begin
{$ifdef GDB}
            if cs_gdb_dbx in aktglobalswitches then
             flags:=flags or uf_has_dbx;
{$endif GDB}
            if target_os.endian=endian_big then
             flags:=flags or uf_big_endian;
            if cs_browser in aktmoduleswitches then
             flags:=flags or uf_has_browser;
            if cs_local_browser in aktmoduleswitches then
             flags:=flags or uf_local_browser;
          end;

{$ifdef Test_Double_checksum_write}
        If only_crc then
          Assign(CRCFile,s+'.INT')
        else
          Assign(CRCFile,s+'.IMP');
        Rewrite(CRCFile);
{$endif def Test_Double_checksum_write}
       { open ppufile }
         current_ppu:=new(pppufile,init(s));
         current_ppu^.crc_only:=only_crc;
         if not current_ppu^.create then
           Message(unit_f_ppu_cannot_write);

{$ifdef Test_Double_checksum}
         if only_crc then
           begin
              new(current_ppu^.crc_test);
              new(current_ppu^.crc_test2);
           end
         else
           begin
             current_ppu^.crc_test:=Current_Module^.crc_array;
             current_ppu^.crc_index:=Current_Module^.crc_size;
             current_ppu^.crc_test2:=Current_Module^.crc_array2;
             current_ppu^.crc_index2:=Current_Module^.crc_size2;
           end;
{$endif def Test_Double_checksum}

         current_ppu^.change_endian:=source_os.endian<>target_os.endian;
       { write symbols and definitions }
         unittable^.writeasunit;

       { flush to be sure }
         current_ppu^.flush;
       { create and write header }
         current_ppu^.header.size:=current_ppu^.size;
         current_ppu^.header.checksum:=current_ppu^.crc;
         current_ppu^.header.interface_checksum:=current_ppu^.interface_crc;
         current_ppu^.header.compiler:=wordversion;
         current_ppu^.header.cpu:=word(target_cpu);
         current_ppu^.header.target:=word(target_info.target);
         current_ppu^.header.flags:=current_module^.flags;
         If not only_crc then
           current_ppu^.writeheader;
       { save crc in current_module also }
         current_module^.crc:=current_ppu^.crc;
         current_module^.interface_crc:=current_ppu^.interface_crc;
         if only_crc then
          begin
{$ifdef Test_Double_checksum}
            Current_Module^.crc_array:=current_ppu^.crc_test;
            current_ppu^.crc_test:=nil;
            Current_Module^.crc_size:=current_ppu^.crc_index2;
            Current_Module^.crc_array2:=current_ppu^.crc_test2;
            current_ppu^.crc_test2:=nil;
            Current_Module^.crc_size2:=current_ppu^.crc_index2;
{$endif def Test_Double_checksum}
            closecurrentppu;
          end;
{$ifdef Test_Double_checksum_write}
        close(CRCFile);
{$endif Test_Double_checksum_write}
      end;


    procedure readusedmacros;
      var
        hs : string;
        mac : pmacro;
        was_defined_at_startup,
        was_used : boolean;
      begin
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           was_defined_at_startup:=boolean(current_ppu^.getbyte);
           was_used:=boolean(current_ppu^.getbyte);
           mac:=pmacro(current_scanner^.macros^.search(hs));
           if assigned(mac) then
             begin
{$ifndef EXTDEBUG}
           { if we don't have the sources why tell }
              if current_module^.sources_avail then
{$endif ndef EXTDEBUG}
               if (not was_defined_at_startup) and
                  was_used and
                  mac^.defined_at_startup then
                Message2(unit_h_cond_not_set_in_last_compile,hs,current_module^.mainsource^);
             end
           else { not assigned }
             if was_defined_at_startup and
                was_used then
              Message2(unit_h_cond_not_set_in_last_compile,hs,current_module^.mainsource^);
         end;
      end;

    procedure readsourcefiles;
      var
        temp,hs       : string;
        temp_dir      : string;
        main_dir      : string;
        incfile_found,
        main_found,
        is_main       : boolean;
        ppufiletime,
        source_time   : longint;
        hp            : pinputfile;
      begin
        ppufiletime:=getnamedfiletime(current_module^.ppufilename^);
        current_module^.sources_avail:=true;
        is_main:=true;
        main_dir:='';
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           temp_dir:='';
           if (current_module^.flags and uf_in_library)<>0 then
            begin
              current_module^.sources_avail:=false;
              temp:=' library';
            end
           else if pos('Macro ',hs)=1 then
            begin
              { we don't want to find this file }
              { but there is a problem with file indexing !! }
              temp:='';
            end
           else
            begin
              { check the date of the source files }
              Source_Time:=GetNamedFileTime(current_module^.path^+hs);
              incfile_found:=false;
              main_found:=false;
              if Source_Time<>-1 then
                hs:=current_module^.path^+hs
              else
               if not(is_main) then
                begin
                  Source_Time:=GetNamedFileTime(main_dir+hs);
                  if Source_Time<>-1 then
                    hs:=main_dir+hs;
                end;
              if (Source_Time=-1) then
                begin
                  if is_main then
                    temp_dir:=unitsearchpath.FindFile(hs,main_found)
                  else
                    temp_dir:=includesearchpath.FindFile(hs,incfile_found);
                  if incfile_found or main_found then
                   begin
                     hs:=temp_dir+hs;
                     Source_Time:=GetNamedFileTime(hs);
                   end
                end;
              if Source_Time=-1 then
               begin
                 current_module^.sources_avail:=false;
                 temp:=' not found';
               end
              else
               begin
                 if main_found then
                   main_dir:=temp_dir;
                 { time newer? But only allow if the file is not searched
                   in the include path (PFV), else you've problems with
                   units which use the same includefile names }
                 if incfile_found then
                  temp:=' found'
                 else
                  begin
                    temp:=' time '+filetimestring(source_time);
                    if (source_time>ppufiletime) then
                     begin
                       current_module^.do_compile:=true;
                       current_module^.recompile_reason:=rr_sourcenewer;
                       temp:=temp+' *'
                     end;
                  end;
               end;
              new(hp,init(hs));
              { the indexing is wrong here PM }
              current_module^.sourcefiles^.register_file(hp);
            end;
           if is_main then
             begin
               stringdispose(current_module^.mainsource);
               current_module^.mainsource:=stringdup(hs);
             end;
           Message1(unit_u_ppu_source,hs+temp);
           is_main:=false;
         end;
      { check if we want to rebuild every unit, only if the sources are
        available }
        if do_build and current_module^.sources_avail then
          begin
             current_module^.do_compile:=true;
             current_module^.recompile_reason:=rr_build;
          end;
      end;


    procedure readloadunit;
      var
        hs : string;
        intfchecksum,
        checksum : longint;
        in_interface : boolean;
      begin
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           checksum:=current_ppu^.getlongint;
           intfchecksum:=current_ppu^.getlongint;
           in_interface:=(current_ppu^.getbyte<>0);
           current_module^.used_units.concat(new(pused_unit,init_to_load(hs,checksum,intfchecksum,in_interface)));
         end;
      end;


    procedure readlinkcontainer(var p:tlinkcontainer);
      var
        s : string;
        m : longint;
      begin
        while not current_ppu^.endofentry do
         begin
           s:=current_ppu^.getstring;
           m:=current_ppu^.getlongint;
           p.insert(s,m);
         end;
      end;


    procedure load_interface;
      var
        b : byte;
        newmodulename : string;
      begin
       { read interface part }
         repeat
           b:=current_ppu^.readentry;
           case b of
             ibmodulename :
               begin
                 newmodulename:=current_ppu^.getstring;
                 if upper(newmodulename)<>current_module^.modulename^ then
                   Message2(unit_f_unit_name_error,current_module^.realmodulename^,newmodulename);
                 stringdispose(current_module^.modulename);
                 stringdispose(current_module^.realmodulename);
                 current_module^.modulename:=stringdup(upper(newmodulename));
                 current_module^.realmodulename:=stringdup(newmodulename);
               end;
             ibsourcefiles :
               readsourcefiles;
             ibusedmacros :
               readusedmacros;
             ibloadunit :
               readloadunit;
             iblinkunitofiles :
               readlinkcontainer(current_module^.LinkUnitOFiles);
             iblinkunitstaticlibs :
               readlinkcontainer(current_module^.LinkUnitStaticLibs);
             iblinkunitsharedlibs :
               readlinkcontainer(current_module^.LinkUnitSharedLibs);
             iblinkotherofiles :
               readlinkcontainer(current_module^.LinkotherOFiles);
             iblinkotherstaticlibs :
               readlinkcontainer(current_module^.LinkotherStaticLibs);
             iblinkothersharedlibs :
               readlinkcontainer(current_module^.LinkotherSharedLibs);
             ibendinterface :
               break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;


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
{$ifdef GDB}
        storeGlobalTypeCount : pword;
{$endif GDB}
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

       { set the name after because it is set to nil in tstoredsymtable.load !! }
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
           pstoredsymtable(current_module^.localsymtable)^.load_browser;
         end;


    procedure tunitsymtable.writeasunit;
      var
         pu        : pused_unit;
      begin
      { first the unitname }
        current_ppu^.putstring(current_module^.realmodulename^);
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
          pstoredsymtable(current_module^.localsymtable)^.writeas;
      { write all browser section }
        if (current_module^.flags and uf_has_browser)<>0 then
         begin
           write_browser;
           pu:=pused_unit(current_module^.used_units.first);
           while assigned(pu) do
            begin
              pstoredsymtable(pu^.u^.globalsymtable)^.write_browser;
              pu:=pused_unit(pu^.next);
            end;
           current_ppu^.writeentry(ibendbrowser);
         end;
        if ((current_module^.flags and uf_local_browser)<>0) and
           assigned(current_module^.localsymtable) then
          pstoredsymtable(current_module^.localsymtable)^.write_browser;

      { the last entry ibend is written automaticly }
      end;


{$ifdef GDB}
   function tunitsymtable.getnewtypecount : word;

      begin
         if not (cs_gdb_dbx in aktglobalswitches) then
           getnewtypecount:=tsymtable.getnewtypecount
         else
           if symtabletype = staticsymtable then
           getnewtypecount:=tsymtable.getnewtypecount
         else
           begin
              getnewtypecount:=unittypecount;
              inc(unittypecount);
           end;
      end;


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
           foreach({$ifdef FPCPROCVAR}@{$endif}concattypestab);
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

{*****************************************************************************
                             Helper Routines
*****************************************************************************}

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


     procedure duplicatesym(sym:psym);
       var
         st : psymtable;
       begin
         Message1(sym_e_duplicate_id,sym^.realname);
         st:=findunitsymtable(sym^.owner);
         with sym^.fileinfo do
           begin
             if assigned(st) and (st^.unitid<>0) then
               Message2(sym_h_duplicate_id_where,'unit '+st^.name^,tostr(line))
             else
               Message2(sym_h_duplicate_id_where,current_module^.sourcefiles^.get_file_name(fileindex),tostr(line));
           end;
       end;


     procedure identifier_not_found(const s:string);
       begin
         Message1(sym_e_id_not_found,s);
         { show a fatal that you need -S2 or -Sd, but only
           if we just parsed the a token that has m_class }
         if not(m_class in aktmodeswitches) and
            (s=pattern) and
            (tokeninfo^[idtoken].keyword=m_class) then
           Message(parser_f_need_objfpc_or_delphi_mode);
       end;



{*****************************************************************************
                                  Search
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
              srsym:=psym(srsymtable^.speedsearch(s,speedvalue));
              if assigned(srsym) then
                exit
              else
                srsymtable:=srsymtable^.next;
           end;
         if notfounderror then
           begin
              identifier_not_found(s);
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
              srsym:=psym(srsymtable^.search(s));
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
                         identifier_not_found(s);
                    end
                  else
                    identifier_not_found(s);
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
            res:=psym(hsymtab^.search(symbol));
        search_a_symtable:=res;
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
               srsym := psym(symt^.search(st));
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
         globaldef := pdef(ptypesym(srsym)^.restype.def);
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
              sym:=psym(pd^.symtable^.search(n));
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
             pd^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}testfordefaultproperty);
             if assigned(_defaultprop) then
               break;
             pd:=pd^.childof;
          end;
        search_default_property:=_defaultprop;
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
         if not(p^.symtabletype in [unitsymtable,globalsymtable,stt_exceptsymtable]) then
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

   procedure InitSymtable;
     var
       token : ttoken;
     begin
      { Reset symbolstack }
        registerdef:=false;
        read_member:=false;
        symtablestack:=nil;
        systemunit:=nil;
{$ifdef GDB}
        firstglobaldef:=nil;
        lastglobaldef:=nil;
        globaltypecount:=1;
        pglobaltypecount:=@globaltypecount;
{$endif GDB}
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
{$ifdef MEMDEBUG}
       writeln('Manglednames: ',manglenamesize,' bytes');
{$endif}
     end;

end.
{
  $Log$
  Revision 1.22  2000-12-23 19:50:09  peter
    * fixed mem leak with withsymtable

  Revision 1.21  2000/12/10 20:25:32  peter
    * fixed missing typecast

  Revision 1.20  2000/12/10 14:14:51  florian
    * fixed web bug 1203: class fields can be now redefined
      in Delphi mode though I don't like this :/

  Revision 1.19  2000/11/30 22:16:49  florian
    * moved to i386

  Revision 1.18  2000/11/29 00:30:42  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.17  2000/11/28 00:28:07  pierre
   * stabs fixing

  Revision 1.1.2.8  2000/11/17 11:14:37  pierre
   * one more class stabs fix

  Revision 1.16  2000/11/12 22:17:47  peter
    * some realname updates for messages

  Revision 1.15  2000/11/06 15:54:15  florian
    * fixed two bugs to get make cycle work, but it's not enough

  Revision 1.14  2000/11/04 14:25:22  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.13  2000/11/01 23:04:38  peter
    * tprocdef.fullprocname added for better casesensitve writing of
      procedures

  Revision 1.12  2000/10/31 22:02:52  peter
    * symtable splitted, no real code changes

  Revision 1.1.2.7  2000/10/16 19:43:04  pierre
   * trying to correct class stabss once more

  Revision 1.11  2000/10/15 07:47:53  peter
    * unit names and procedure names are stored mixed case

  Revision 1.10  2000/10/14 10:14:53  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.9  2000/10/01 19:48:25  peter
    * lot of compile updates for cg11

  Revision 1.8  2000/09/24 15:06:29  peter
    * use defines.inc

  Revision 1.7  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/21 11:27:45  pierre
   * fix the stabs problems

  Revision 1.5  2000/08/20 14:58:41  peter
    * give fatal if objfpc/delphi mode things are found (merged)

  Revision 1.1.2.6  2000/08/20 14:56:46  peter
    * give fatal if objfpc/delphi mode things are found

  Revision 1.4  2000/08/16 18:33:54  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.3  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

}
