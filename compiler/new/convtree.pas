{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
       verbose,nstatmnt,nmem;

    function convtree2node(p : ptree) : pnode;

      function doconv(p : ptree) : pnode;

        var
           node : pnode;

        begin
           if assigned(p) then
             begin
                case p^.treetype of
                  blockn:
                    node:=new(pblocknode,init(doconv(p^.left)));
                  assignn:
                    node:=new(passignmentnode,init(doconv(p^.left),
                      doconv(p^.right)));
                  statementn:
                    node:=new(pstatementnode,init(doconv(p^.left),
                      doconv(p^.right)));
                  loadn:
                    node:=new(ploadnode,init(p^.symtableentry,p^.symtable));
                  else internalerror(1209993);
                end;
                doconv:=node;
             end
           else
             doconv:=nil;
        end;

      begin
         convtree2node:=doconv(p);
         disposetree(p);
      end;

end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:08  michael
  + Initial import

  Revision 1.6  2000/01/07 01:14:53  peter
    * updated copyright to 2000

  Revision 1.5  1999/09/14 11:16:09  florian
    * only small updates to work with the current compiler

  Revision 1.4  1999/01/24 22:32:35  florian
    * well, more changes, especially parts of secondload ported

  Revision 1.3  1999/01/23 23:29:47  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.2  1999/01/19 10:19:04  florian
    * bug with mul. of dwords fixed, reported by Alexander Stohr
    * some changes to compile with TP
    + small enhancements for the new code generator

  Revision 1.1  1999/01/13 22:52:37  florian
    + YES, finally the new code generator is compilable, but it doesn't run yet :(

}