{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate 680x0 assembler for math nodes

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
unit n68kmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat;

    type


      tm68knotnode = class(tnotnode)
         procedure pass_2;override;
      end;
      

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defbase,
      cginfo,cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,paramgr,
      tgobj,ncgutil,cgobj,rgobj,rgcpu,cgcpu,cg64f32;




{*****************************************************************************
                               TM68KNOTNODE
*****************************************************************************}

    procedure tm68knotnode.pass_2;
      var
         hl : tasmlabel;
         opsize : tcgsize;
      begin
         opsize:=def_cgsize(resulttype.def);
         if is_boolean(resulttype.def) then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.location.loc<>LOC_JUMP then
             secondpass(left);

            case left.location.loc of
              LOC_JUMP :
                begin
                  location_reset(location,LOC_JUMP,OS_NO);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(left);
                  maketojumpbool(exprasmlist,left,lr_load_regvars);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  location_release(exprasmlist,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_CONSTANT,
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  location_force_reg(exprasmlist,left.location,def_cgsize(resulttype.def),true);
                  exprasmlist.concat(taicpu.op_reg(A_TST,tcgsize2opsize[opsize],left.location.register));
                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
             else
                internalerror(200203224);
            end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_64,false);
              cg64.a_op64_loc_reg(exprasmlist,OP_NOT,location,
                joinreg64(location.registerlow,location.registerhigh));
           end
         else
          begin
            secondpass(left);
            location_copy(location,left.location);
            location_force_reg(exprasmlist,location,opsize,false);
            cg.a_op_reg_reg(exprasmlist,OP_NOT,opsize,location.register,location.register);
          end;
      end;



begin
   cnotnode:=tm68knotnode;
end.
{
  $Log$
  Revision 1.3  2002-08-15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.2  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.1  2002/08/14 19:16:34  carl
    + m68k type conversion nodes
    + started some mathematical nodes
    * out of bound references should now be handled correctly

}
