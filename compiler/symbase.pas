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
unit symbase;

{$i defines.inc}

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
       public
          name      : pstring;
          realname  : pstring;
          symtabletype : tsymtabletype;
          { each symtable gets a number }
          unitid    : word{integer give range check errors PM};
          datasize  : longint;
          dataalignment : longint;
          symindex,
          defindex  : TIndexArray;
          symsearch : Tdictionary;
          next      : tsymtable;
          defowner  : tdefentry; { for records and objects }
          { only used for parameter symtable to determine the offset relative }
          { to the frame pointer and for local inline }
          address_fixup : longint;
          { this saves all definition to allow a proper clean up }
          { separate lexlevel from symtable type }
          symtablelevel : byte;
          constructor Create(const s:string);
          destructor  destroy;override;
          procedure clear;virtual;
          function  rename(const olds,news : stringid):tsymentry;
          procedure foreach(proc2call : tnamedindexcallback);
          procedure foreach_static(proc2call : tnamedindexstaticcallback);
          procedure insert(sym : tsymentry);virtual;
          function  search(const s : stringid) : tsymentry;
          function  speedsearch(const s : stringid;speedvalue : cardinal) : tsymentry;virtual;
          procedure registerdef(p : tdefentry);
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


    procedure tsymtable.registerdef(p : tdefentry);
      begin
         defindex.insert(p);
         { set def owner and indexnb }
         p.owner:=self;
      end;


    procedure tsymtable.foreach(proc2call : tnamedindexcallback);
      begin
        symindex.foreach(proc2call);
      end;


    procedure tsymtable.foreach_static(proc2call : tnamedindexstaticcallback);
      begin
        symindex.foreach_static(proc2call);
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
  Revision 1.2  2001-04-13 01:22:15  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.1  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

}
