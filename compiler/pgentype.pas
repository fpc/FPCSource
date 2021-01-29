{
    Copyright (c) 2015 by Sven Barth

    Contains different types that are used in the context of parsing generics.

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
unit pgentype;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,
  symtype,symbase;

const
  inline_specialization_block_types = [bt_type,bt_var_type,bt_const_type,bt_body];

type
  tspecializationstate = record
    oldsymtablestack   : tsymtablestack;
    oldextendeddefs    : tfphashobjectlist;
    oldgenericdummysyms: tfphashobjectlist;
  end;

  tspecializationcontext=class
  public
    paramlist : tfpobjectlist;
    poslist : tfplist;
    prettyname : ansistring;
    specializename : ansistring;
    genname : string;
    sym : tsym;
    symtable : tsymtable;
    constructor create;
    destructor destroy;override;
  end;


implementation

constructor tspecializationcontext.create;
begin
  paramlist:=tfpobjectlist.create(false);
  poslist:=tfplist.create;
end;

destructor tspecializationcontext.destroy;
var
  i : longint;
begin
  paramlist.free;
  for i:=0 to poslist.count-1 do
    dispose(pfileposinfo(poslist[i]));
  poslist.free;
  inherited destroy;
end;

end.

