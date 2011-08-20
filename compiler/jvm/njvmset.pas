{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM bytecode for in set/case nodes

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
unit njvmset;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,nset,ncgset;

    type
      tjvmcasenode = class(tcgcasenode)
         function pass_1: tnode; override;
      end;


implementation

    uses
      symconst,symdef,
      pass_1,
      ncnv;


{*****************************************************************************
                            TJVMCASENODE
*****************************************************************************}

    function tjvmcasenode.pass_1: tnode;
      begin
        { convert case expression to an integer in case it's an enum, since
          enums are class instances in the JVM. All labels are stored as
          ordinal values, so it doesn't matter that we change the type }
        if left.resultdef.typ=enumdef then
          inserttypeconv_explicit(left,s32inttype);
        result:=inherited pass_1;
      end;



begin
   ccasenode:=tjvmcasenode;
end.
