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
      tjvminnode = class(tcginnode)
         function pass_1: tnode; override;
      end;

      tjvmcasenode = class(tcgcasenode)
         function pass_1: tnode; override;
      end;


implementation

    uses
      symconst,symdef,
      pass_1,
      ncal,ncnv,ncon,nmem,
      njvmcon,
      cgbase;

{*****************************************************************************
                             TJVMINNODE
*****************************************************************************}

    function tjvminnode.pass_1: tnode;
      var
        setparts: Tsetparts;
        numparts: byte;
        use_small: boolean;
        isenum: boolean;
      begin
        { before calling "inherited pass_1", so that in case left is an enum
          constant it's not yet translated into a class instance }
        isenum:=left.resultdef.typ=enumdef;
        { if we can use jumps, don't transform the set constant and (if
          applicable) the value to be tested }
        if checkgenjumps(setparts,numparts,use_small) then
          begin
            if right.nodetype=setconstn then
              tjvmsetconstnode(right).setconsttype:=sct_notransform;
            if isenum then
              if (left.nodetype=ordconstn) then
                tjvmordconstnode(left).enumconstok:=true
              else
                { not very clean, since we now have "longint in enumset", but
                  the code generator doesn't really mind }
                inserttypeconv_explicit(left,s32inttype);
          end;
        result:=inherited pass_1;
        if assigned(result) then
          exit;
        { in case of jumps let the regular code handle it }
        if expectloc=LOC_JUMP then
          exit;
        { otherwise call set helper }
        right:=caddrnode.create_internal(right);
        include(right.flags,nf_typedaddr);
        if isenum then
          begin
            inserttypeconv_explicit(left,java_jlenum);
            inserttypeconv_explicit(right,java_juenumset);
          end
        else
          begin
            inserttypeconv_explicit(left,s32inttype);
            inserttypeconv_explicit(right,java_jubitset);
          end;
        result:=ccallnode.createinternmethod(right,'CONTAINS',ccallparanode.create(left,nil));
        right:=nil;
        left:=nil;
      end;


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
   cinnode:=tjvminnode;
   ccasenode:=tjvmcasenode;
end.
