{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Type checking and register allocation for math nodes

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
       tmoddivnode = class(tbinopnode)
       end;

       tshlshrnode = class(tbinopnode)
       end;

       tunaryminusnode = class(tunarynode)
         constructor create(expr : tnode);virtual;
       end;

       tnotnode = class(tunarynode)
         constructor create(expr : tnode);virtual;
       end;

    var
       cmoddivnode : class of tmoddivnode;
       cshlshrnode : class of tshlshrnode;
       cunaryminusnode : class of tunaryminusnode;
       cnotnode : class of cnotnode;

  implementation

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}

{****************************************************************************
                              TSHLSHRNODE
 ****************************************************************************}

{****************************************************************************
                            TUNARYMINUSNODE
 ****************************************************************************}
    constructor tnotnode.create(expr : tnode);

      begin
         inherited create(notn,expr);
      end;

{****************************************************************************
                               TNOTNODE
 ****************************************************************************}

    constructor tnotnode.create(expr : tnode);

      begin
         inherited create(notn,expr);
      end;

begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
{
  $Log$
  Revision 1.1  2000-09-20 21:35:12  florian
    * initial revision

}