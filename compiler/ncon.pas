{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for constants

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
unit ncon;
interface

    uses
      node;

    type
       trealconstnode = class(tnode)
          value_real : bestreal;
          lab_real : pasmlabel;
          // !!!!!!! needs at least create, getcopy
          function pass_1 : tnode;override;
       end;

       tfixconstnode = class(tnode)
          value_fix: longint;
          // !!!!!!! needs at least create, getcopy
          function pass_1 : tnode;override;
       end;

       tordconstnode = class(tnode)
          value : TConstExprInt;
          // !!!!!!! needs at least create, getcopy
          function pass_1 : tnode;override;
       end;

       tpointerconstnode = class(tnode)
          value : TPointerOrd;
          // !!!!!!! needs at least create, getcopy
          function pass_1 : tnode;override;
       end;

       tstringconstnode = class(tnode)
          value_str : pchar;
          length : longint;
          lab_str : pasmlabel;
          stringtype : tstringtype
          // !!!!!!! needs at least create, getcopy, destroy
          function pass_1 : tnode;override;
       end;

       tsetconstnode = class(tnode)
          value_set : pconstset;
          lab_set : pasmlabel
          // !!!!!!! needs at least create,  getcopy
          function pass_1 : tnode;override;
       end;

       tnilnode = class(tnode)
          // !!!!!!! needs at least create
          function pass_1 : tnode;override;
       end;

implementation

    uses
      cobjects,verbose,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,pass_1,cpubase;

{*****************************************************************************
                             TREALCONSTNODE
*****************************************************************************}

    function tpointerconstnode.pass_1 : tnode;
      begin
         if (value_real=1.0) or (value_real=0.0) then
           begin
              location.loc:=LOC_FPU;
              registersfpu:=1;
           end
         else
           location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TFIXCONSTNODE
*****************************************************************************}

    function tpointerconstnode.pass_1 : tnode;
      begin
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                              TORDCONSTNODE
*****************************************************************************}

    function tpointerconstnode.pass_1 : tnode;
      begin
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                            TPOINTERCONSTNODE
*****************************************************************************}

    function tpointerconstnode.pass_1 : tnode;
      begin
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TSTRINGCONSTNODE
*****************************************************************************}

    function tstringconstnode.pass_1 : tnode;
      begin
{        if cs_ansistrings in aktlocalswitches then
          resulttype:=cansistringdef
         else
          resulttype:=cshortstringdef; }
        case stringtype of
          st_shortstring :
            resulttype:=cshortstringdef;
          st_ansistring :
            resulttype:=cansistringdef;
          st_widestring :
            resulttype:=cwidestringdef;
          st_longstring :
            resulttype:=clongstringdef;
        end;
        location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TSETCONSTNODE
*****************************************************************************}

    function tsetconstnode.pass_1 : tnode;
      begin
         location.loc:=LOC_MEM;
      end;

{*****************************************************************************
                               TNILNODE
*****************************************************************************}

    function tnilnode.pass_1 : tnode;
      begin
        resulttype:=voidpointerdef;
        location.loc:=LOC_MEM;
      end;

end.
{
  $Log$
  Revision 1.1  2000-09-22 21:44:48  florian
    + initial revision

}
