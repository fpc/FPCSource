{
    Copyright (c) 2009 by Jonas Maebe

    This unit implements some Objective-C helper routines at the node tree
    level.

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

{$i fpcdefs.inc}

unit objcutil;

interface

uses
  node,
  symdef;

function objcsuperclassnode(def: tobjectdef): tnode;

implementation

uses
  pass_1,
  verbose,
  symtable,symconst,
  nbas,nmem,ncal,nld;

function objcsuperclassnode(def: tobjectdef): tnode;
  var
    block: tblocknode;
    statements: tstatementnode;
    para: tcallparanode;
  begin
    { only valid for Objective-C classes }
    if not is_objcclass(def) then
      internalerror(2009032903);
    block:=internalstatements(statements);
    para:=ccallparanode.create(cloadvmtaddrnode.create(ctypenode.create(def)),nil);
    addstatement(statements,ccallnode.createinternfromunit('OBJC1','CLASS_GETSUPERCLASS',para));
    typecheckpass(block);
    result:=block;
  end;

end.
