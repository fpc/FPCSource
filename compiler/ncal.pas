{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for add nodes

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
unit ncal;

  interface

    uses
       node,symtable;

    type
       tcallnode = class(tbinarynode)
          { the symbol containing the definition of the procedure }
          { to call                                               }
          symtableprocentry : pprocsym;
          { the symtable containing symtableprocentry }
          symtableproc : psymtable;
          { the definition of the procedure to call }
          procdefinition : pabstractprocdef;
          methodpointer : tnode;
          constructor create(v : pprocsym;st : psymtable);
       end;

    function gencallnode(v : pprocsym;st : psymtable) : tnode;

    var
       ccallnode : class of tcallnode;

  implementation

    function gencallnode(v : pprocsym;st : psymtable) : tnode;

      begin
         gencallnode:=ccallnode.create(v,st);
      end;

    constructor tcallnode.create(v : pprocsym;st : psymtable);

      begin
         inherited create(calln,nil,nil);
         symtableprocentry:=v;
         symtableproc:=st;
         include(flags,nf_return_value_used);
         methodpointer:=nil;
         procdefinition:=nil;
      end;

begin
   ccallnode:=tcallnode;
end.
{
  $Log$
  Revision 1.1  2000-09-20 20:52:16  florian
    * initial revision

}