{
    Copyright (c) 2011 by Jonas Maebe

    Generates nodes for typed constant declarations

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
unit njvmtcon;

{$i fpcdefs.inc}

interface

    uses
      node,
      symdef,
      ngtcon;


    type
      tjvmtypedconstbuilder = class(tnodetreetypedconstbuilder)
       protected
        procedure tc_emit_setdef(def: tsetdef; var node: tnode);override;
      end;

implementation

    uses
      njvmcon;


    procedure tjvmtypedconstbuilder.tc_emit_setdef(def: tsetdef; var node: tnode);
      begin
        { indicate that set constant nodes have to be transformed into
          constructors here }
        if node.nodetype=setconstn then
          tjvmsetconstnode(node).setconsttype:=sct_construct;
        inherited tc_emit_setdef(def,node);
      end;

begin
  ctypedconstbuilder:=tjvmtypedconstbuilder;
end.
