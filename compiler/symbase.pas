{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

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
unit symbase;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,
       { symtable }
       symconst
       ;

{************************************************
           Some internal constants
************************************************}

   const
       hasharraysize    = 256;
       indexgrowsize    = 64;

{$ifdef GDB}
       memsizeinc = 2048; { for long stabstrings }
{$endif GDB}


{************************************************
            Needed forward pointers
************************************************}

    type
       tsymtable = class;

{************************************************
               TSymtableEntry
************************************************}

      tsymtableentry = class(TNamedIndexItem)
         owner : tsymtable;
      end;


{************************************************
                 TDefEntry
************************************************}

      tdefentry = class(tsymtableentry)
         deftype : tdeftype;
      end;


{************************************************
                   TSymEntry
************************************************}

      { this object is the base for all symbol objects }
      tsymentry = class(tsymtableentry)
         typ : tsymtyp;
      end;


{************************************************
                 TSymtable
************************************************}

       tsearchhasharray = array[0..hasharraysize-1] of tsymentry;
       psearchhasharray = ^tsearchhasharray;

       tsymtable = class
{$ifdef EXTDEBUG}
       private
          procedure dumpsym(p : TNamedIndexItem;arg:pointer);
{$endif EXTDEBUG}
       public
          name      : pstring;
          realname  : pstring;
          datasize  : longint;
          symindex,
          defindex  : TIndexArray;
          symsearch : Tdictionary;
          next      : tsymtable;
          defowner  : tdefentry; { for records and objects }
          { only used for parameter symtable to determine the offset relative }
          { to the frame pointer and for local inline }
          address_fixup : longint;
          symtabletype  : tsymtabletype;
          { each symtable gets a number }
          unitid        : word;
          { level of symtable, used for nested procedures }
          symtablelevel : byte;
          dataalignment : byte;
          constructor Create(const s:string);
          destructor  destroy;override;
          procedure clear;virtual;
          function  rename(const olds,news : stringid):tsymentry;
          procedure foreach(proc2call : tnamedindexcallback;arg:pointer);
          procedure foreach_static(proc2call : tnamedindexstaticcallback;arg:pointer);
          procedure insert(sym : tsymentry);virtual;
          procedure replace(oldsym,newsym:tsymentry);
          procedure insertvardata(sym : tsymentry);virtual;abstract;
          procedure insertconstdata(sym : tsymentry);virtual;abstract;
          function  search(const s : stringid) : tsymentry;
          function  speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;virtual;
          procedure registerdef(p : tdefentry);
{$ifdef EXTDEBUG}
          procedure dump;
{$endif EXTDEBUG}
          function  getdefnr(l : longint) : tdefentry;
          function  getsymnr(l : longint) : tsymentry;
{$ifdef GDB}
          function getnewtypecount : word; virtual;
{$endif GDB}
       end;

{************************************************
                    TDeref
************************************************}

      tderef = class
        dereftype : tdereftype;
        index     : word;
        next      : tderef;
        constructor create(typ:tdereftype;i:word);
        destructor  destroy;override;
      end;


    var
       registerdef : boolean;      { true, when defs should be registered }

       defaultsymtablestack : tsymtable;  { symtablestack after default units have been loaded }
       symtablestack     : tsymtable;     { linked list of symtables }
       aktrecordsymtable : tsymtable;     { current record read from ppu symtable }
       aktstaticsymtable : tsymtable;     { current static for local ppu symtable }
       aktlocalsymtable  : tsymtable;     { current proc local for local ppu symtable }


implementation

    uses
       verbose;

{****************************************************************************
                                TSYMTABLE
****************************************************************************}

    constructor tsymtable.Create(const s:string);
      begin
         if s<>'' then
          begin
            name:=stringdup(upper(s));
            realname:=stringdup(s);
          end
         else
          begin
            name:=nil;
            realname:=nil;
          end;
         symtabletype:=abstractsymtable;
         symtablelevel:=0;
         defowner:=nil;
         next:=nil;
         symindex:=tindexarray.create(indexgrowsize);
         defindex:=TIndexArray.create(indexgrowsize);
         symsearch:=tdictionary.create;
         symsearch.noclear:=true;
         unitid:=0;
         address_fixup:=0;
         datasize:=0;
         dataalignment:=1;
      end;


    destructor tsymtable.destroy;
      begin
        stringdispose(name);
        stringdispose(realname);
        symindex.destroy;
        defindex.destroy;
        { symsearch can already be disposed or set to nil for withsymtable }
        if assigned(symsearch) then
         begin
           symsearch.destroy;
           symsearch:=nil;
         end;
      end;


{$ifdef EXTDEBUG}
    procedure tsymtable.dumpsym(p : TNamedIndexItem;arg:pointer);
      begin
        writeln(p.name);
      end;


    procedure tsymtable.dump;
      begin
        if assigned(name) then
          writeln('Symtable ',name^)
        else
          writeln('Symtable <not named>');
        symsearch.foreach({$ifdef FPCPROCVAR}@{$endif}dumpsym,nil);
      end;
{$endif EXTDEBUG}


    procedure tsymtable.registerdef(p : tdefentry);
      begin
         defindex.insert(p);
         { set def owner and indexnb }
         p.owner:=self;
      end;


    procedure tsymtable.foreach(proc2call : tnamedindexcallback;arg:pointer);
      begin
        symindex.foreach(proc2call,arg);
      end;


    procedure tsymtable.foreach_static(proc2call : tnamedindexstaticcallback;arg:pointer);
      begin
        symindex.foreach_static(proc2call,arg);
      end;


{***********************************************
                Table Access
***********************************************}

    procedure tsymtable.clear;
      begin
         symindex.clear;
         defindex.clear;
      end;


    procedure tsymtable.insert(sym:tsymentry);
      begin
         sym.owner:=self;
         { insert in index and search hash }
         symindex.insert(sym);
         symsearch.insert(sym);
      end;


    procedure tsymtable.replace(oldsym,newsym:tsymentry);
      begin
         { Replace the entry in the dictionary, this checks
           the name }
         if not symsearch.replace(oldsym,newsym) then
           internalerror(200209061);
         { replace in index }
         symindex.replace(oldsym,newsym);
         { set owner of new symb }
         newsym.owner:=self;
      end;


    function tsymtable.search(const s : stringid) : tsymentry;
      begin
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function tsymtable.speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;
      begin
        speedsearch:=tsymentry(symsearch.speedsearch(s,speedvalue));
      end;


    function tsymtable.rename(const olds,news : stringid):tsymentry;
      begin
        rename:=tsymentry(symsearch.rename(olds,news));
      end;


    function tsymtable.getsymnr(l : longint) : tsymentry;
      var
        hp : tsymentry;
      begin
        hp:=tsymentry(symindex.search(l));
        if hp=nil then
         internalerror(10999);
        getsymnr:=hp;
      end;


    function tsymtable.getdefnr(l : longint) : tdefentry;
      var
        hp : tdefentry;
      begin
        hp:=tdefentry(defindex.search(l));
        if hp=nil then
         internalerror(10998);
        getdefnr:=hp;
      end;


{$ifdef GDB}
    function tsymtable.getnewtypecount : word;
      begin
        getnewtypecount:=0;
      end;
{$endif GDB}


{****************************************************************************
                               TDeref
****************************************************************************}

    constructor tderef.create(typ:tdereftype;i:word);
      begin
        dereftype:=typ;
        index:=i;
        next:=nil;
      end;


    destructor tderef.destroy;
      begin
      end;

end.
{
  $Log$
  Revision 1.12  2003-04-27 11:21:34  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.11  2003/04/27 07:29:51  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.10  2002/12/07 14:27:09  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.9  2002/10/02 20:51:59  peter
    * tsymtable.dump to dump the names in a symtable to stdout

  Revision 1.8  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.7  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.6  2002/05/18 13:34:18  peter
    * readded missing revisions

  Revision 1.5  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.3  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

}
