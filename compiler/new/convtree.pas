{
    $Id$
    Copyright (c) 1999 by Florian Klaempfl

    Converts the old tree nodes into the new OOP nodest
    This unit is necessary to interface the new code generator
    with the old parser

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
unit convtree;

  interface

    uses
       tree;

    function convtree2node(p : ptree) : pnode;

  implementation

    uses
       verbose;

    function convtree2node(p : ptree) : pnode;

      var
         node : pnode;

      begin
         case p^.treetype of
           blockn:
             node:=new(pblocknode,init);
           else internalerror(13751);
         end;
         disposetree(p);
         convtree2node:=node;
      end;

end.
{
  $Log$
  Revision 1.1  1999-01-13 22:52:37  florian
    + YES, finally the new code generator is compilable, but it doesn't run yet :(

}