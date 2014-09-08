{
    Copyright (c) 1998-2011 by Florian Klaempfl, Jonas Maebe

    Generates i8086 assembler for typed constant declarations

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
unit n8086tcon;

{$i fpcdefs.inc}

interface

    uses
      node,symdef,ngtcon;


    type

      { ti8086typedconstbuilder }

      ti8086typedconstbuilder = class(tasmlisttypedconstbuilder)
       protected
        procedure tc_emit_pointerdef(def: tpointerdef; var node: tnode);override;
      end;


implementation

uses
  ncnv,defcmp,defutil,aasmtai,symcpu;

    { ti8086typedconstbuilder }

    procedure ti8086typedconstbuilder.tc_emit_pointerdef(def: tpointerdef; var node: tnode);
      var
        hp: tnode;
      begin
        { remove equal typecasts for pointer/nil addresses }
        if (node.nodetype=typeconvn) then
          with Ttypeconvnode(node) do
            if (left.nodetype in [addrn,niln]) and equal_defs(def,node.resultdef) then
              begin
                hp:=left;
                left:=nil;
                node.free;
                node:=hp;
              end;
        if node.nodetype=niln then
          begin
            if is_farpointer(def) or is_hugepointer(def) then
              list.concat(Tai_const.Create_32bit(0))
            else
              list.concat(Tai_const.Create_16bit(0));
          end
        else
          inherited tc_emit_pointerdef(def, node);
      end;

begin
  ctypedconstbuilder:=ti8086typedconstbuilder;
end.

