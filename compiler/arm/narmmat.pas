{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate ARM assembler for math nodes

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
unit narmmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tarmnotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,
      defutil,
      cgbase,cgobj,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,cginfo,
      ncgutil,cgcpu,cg64f32,rgobj;

{*****************************************************************************
                               TARMNOTNODE
*****************************************************************************}

    procedure tarmnotnode.second_boolean;
      var
        hl : tasmlabel;
        ins : taicpu;
      begin
        { if the location is LOC_JUMP, we do the secondpass after the
          labels are allocated
        }
        if left.expectloc=LOC_JUMP then
          begin
            hl:=truelabel;
            truelabel:=falselabel;
            falselabel:=hl;
            secondpass(left);
            maketojumpbool(exprasmlist,left,lr_load_regvars);
            hl:=truelabel;
            truelabel:=falselabel;
            falselabel:=hl;
            location.loc:=LOC_JUMP;
          end
        else
          begin
            secondpass(left);
            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_CREFERENCE :
                begin
                  location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
                  ins:=taicpu.op_reg_reg(A_MVN,left.location.register,left.location.register);
                  ins.oppostfix:=PF_S;
                  exprasmlist.concat(ins);
                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_EQ;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;


begin
   cnotnode:=tarmnotnode;
end.
{
  $Log$
  Revision 1.3  2003-08-27 00:27:56  florian
    + same procedure as very day: today's work on arm
}
