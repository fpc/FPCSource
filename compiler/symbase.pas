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
       cutils,cobjects,
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

{************************************************
            Needed forward pointers
************************************************}

    type
       psymtable = ^tsymtable;

{************************************************
               TSymtableEntry
************************************************}

      psymtableentry = ^tsymtableentry;
      tsymtableentry = object(tnamedindexobject)
         owner : psymtable;
      end;


{************************************************
                 TDefEntry
************************************************}
      pdefentry = ^tdefentry;
      tdefentry = object(tsymtableentry)
         deftype : tdeftype;
      end;


{************************************************
                   TSymEntry
************************************************}

      { this object is the base for all symbol objects }
      psymentry = ^tsymentry;
      tsymentry = object(tsymtableentry)
         typ : tsymtyp;
      end;


{************************************************
                 TSymtable
************************************************}

       tsearchhasharray = array[0..hasharraysize-1] of psymentry;
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
          defowner  : pdefentry; { for records and objects }
          { only used for parameter symtable to determine the offset relative }
          { to the frame pointer and for local inline }
          address_fixup : longint;
          { this saves all definition to allow a proper clean up }
          { separate lexlevel from symtable type }
          symtablelevel : byte;
          constructor init(t : tsymtabletype);
          destructor  done;virtual;
          procedure clear;virtual;
          function  rename(const olds,news : stringid):psymentry;
          procedure foreach(proc2call : tnamedindexcallback);
          procedure insert(sym : psymentry);virtual;
          function  search(const s : stringid) : psymentry;
          function  speedsearch(const s : stringid;speedvalue : longint) : psymentry;virtual;
          procedure registerdef(p : pdefentry);
          function  getdefnr(l : longint) : pdefentry;
          function  getsymnr(l : longint) : psymentry;
{$ifdef GDB}
          function getnewtypecount : word; virtual;
{$endif GDB}
       end;

{************************************************
                    TDeref
************************************************}

      pderef = ^tderef;
      tderef = object
        dereftype : tdereftype;
        index     : word;
        next      : pderef;
        constructor init(typ:tdereftype;i:word);
        destructor  done;
      end;


    var
       registerdef : boolean;      { true, when defs should be registered }

       defaultsymtablestack : psymtable;  { symtablestack after default units have been loaded }
       symtablestack     : psymtable;     { linked list of symtables }
       aktrecordsymtable : psymtable;     { current record read from ppu symtable }
       aktstaticsymtable : psymtable;     { current static for local ppu symtable }
       aktlocalsymtable  : psymtable;     { current proc local for local ppu symtable }


implementation

    uses
       verbose;

{****************************************************************************
                                TSYMTABLE
****************************************************************************}

    constructor tsymtable.init(t : tsymtabletype);
      begin
         symtabletype:=t;
         defowner:=nil;
         new(symindex,init(indexgrowsize));
         new(defindex,init(indexgrowsize));
         new(symsearch,init);
         symsearch^.noclear:=true;
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


    procedure tsymtable.registerdef(p : pdefentry);
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
                Table Access
***********************************************}

    procedure tsymtable.clear;
      begin
         symindex^.clear;
         defindex^.clear;
      end;


    procedure tsymtable.insert(sym:psymentry);
      begin
         sym^.owner:=@self;
         { insert in index and search hash }
         symindex^.insert(sym);
         symsearch^.insert(sym);
      end;


    function tsymtable.search(const s : stringid) : psymentry;
      begin
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function tsymtable.speedsearch(const s : stringid;speedvalue : longint) : psymentry;
      begin
        speedsearch:=psymentry(symsearch^.speedsearch(s,speedvalue));
      end;


    function tsymtable.rename(const olds,news : stringid):psymentry;
      begin
        rename:=psymentry(symsearch^.rename(olds,news));
      end;


    function tsymtable.getsymnr(l : longint) : psymentry;
      var
        hp : psymentry;
      begin
        hp:=psymentry(symindex^.search(l));
        if hp=nil then
         internalerror(10999);
        getsymnr:=hp;
      end;


    function tsymtable.getdefnr(l : longint) : pdefentry;
      var
        hp : pdefentry;
      begin
        hp:=pdefentry(defindex^.search(l));
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

    constructor tderef.init(typ:tdereftype;i:word);
      begin
        dereftype:=typ;
        index:=i;
        next:=nil;
      end;


    destructor tderef.done;
      begin
      end;





end.
{
  $Log$
  Revision 1.1  2000-10-31 22:02:51  peter
    * symtable splitted, no real code changes

}
