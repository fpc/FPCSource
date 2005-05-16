{
    $Id: ncpumat.pas,v 1.23 2005/02/14 17:13:10 peter Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SPARC assembler for math nodes

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
unit ncpumat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tSparcmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tSparcshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tSparcnotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,
      aasmbase,aasmcpu,aasmtai,
      defutil,
      cgbase,cgobj,pass_2,
      ncon,
      cpubase,
      ncgutil,cgcpu,cgutils;

{*****************************************************************************
                             TSparcMODDIVNODE
*****************************************************************************}

    procedure tSparcmoddivnode.pass_2;
      const
                    { signed   overflow }
        divops: array[boolean, boolean] of tasmop =
          ((A_UDIV,A_UDIVcc),(A_SDIV,A_SDIVcc));
      var
         power      : longint;
         op         : tasmop;
         tmpreg,
         numerator,
         divider,
         resultreg  : tregister;
         overflowlabel : tasmlabel;
         ai : taicpu;
      begin
         secondpass(left);
         secondpass(right);
         location_copy(location,left.location);

         { put numerator in register }
         location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
         location_copy(location,left.location);
         numerator := location.register;

         if (nodetype = modn) then
           resultreg := cg.GetIntRegister(exprasmlist,OS_INT)
         else
           begin
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.loc := LOC_REGISTER;
                 location.register := cg.GetIntRegister(exprasmlist,OS_INT);
               end;
             resultreg := location.register;
           end;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value,power) then
           begin
             tmpreg:=cg.GetIntRegister(exprasmlist,OS_INT);
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_INT,31,numerator,tmpreg);
             { if signed, tmpreg=right value-1, otherwise 0 }
             cg.a_op_const_reg(exprasmlist,OP_AND,OS_INT,tordconstnode(right).value-1,tmpreg);
             { add to the left value }
             cg.a_op_reg_reg(exprasmlist,OP_ADD,OS_INT,tmpreg,numerator);
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_INT,aword(power),numerator,resultreg);
           end
         else
           begin
             { load divider in a register if necessary }
             location_force_reg(exprasmlist,right.location,
               def_cgsize(right.resulttype.def),true);
             divider := right.location.register;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { And on Sparc, the only way to catch a div-by-0 is by checking  }
             { the overflow flag (JM)                                       }

             { Fill %y with the -1 or 0 depending on the highest bit }
             if is_signed(left.resulttype.def) then
               begin
                 tmpreg:=cg.GetIntRegister(exprasmlist,OS_INT);
                 exprasmlist.concat(taicpu.op_reg_const_reg(A_SRA,numerator,31,tmpreg));
                 exprasmlist.concat(taicpu.op_reg_reg(A_MOV,tmpreg,NR_Y));
               end
             else
               exprasmlist.concat(taicpu.op_reg_reg(A_MOV,NR_G0,NR_Y));
             { wait 3 instructions slots before we can read %y }
             exprasmlist.concat(taicpu.op_none(A_NOP));
             exprasmlist.concat(taicpu.op_none(A_NOP));
             exprasmlist.concat(taicpu.op_none(A_NOP));

             op := divops[is_signed(right.resulttype.def),
                          cs_check_overflow in aktlocalswitches];
             exprasmlist.concat(taicpu.op_reg_reg_reg(op,numerator,divider,resultreg));

             if (nodetype = modn) then
               begin
                 objectlibrary.getlabel(overflowlabel);
                 ai:=taicpu.op_cond_sym(A_Bxx,C_O,overflowlabel);
                 ai.delayslot_annulled:=true;
                 exprasmlist.concat(ai);
                 exprasmlist.concat(taicpu.op_reg(A_NOT,resultreg));
                 cg.a_label(exprasmlist,overflowlabel);
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_SMUL,resultreg,divider,resultreg));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,numerator,resultreg,resultreg));
               end;
           end;
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        cg.g_overflowcheck(exprasmlist,Location,ResultType.Def);
      end;


{*****************************************************************************
                             TSparcSHLRSHRNODE
*****************************************************************************}

    function TSparcShlShrNode.first_shlshr64bitint:TNode;
      begin
        { 64bit without constants need a helper }
        if is_64bit(left.resulttype.def) and
           (right.nodetype<>ordconstn) then
          begin
            result:=inherited first_shlshr64bitint;
            exit;
          end;

        result := nil;
      end;


    procedure tSparcshlshrnode.pass_2;
      var
        hregister,resultreg,hregister1,
        hreg64hi,hreg64lo : tregister;
        op : topcg;
        shiftval: aword;
      begin
        { 64bit without constants need a helper, and is
          already replaced in pass1 }
        if is_64bit(left.resulttype.def) and
           (right.nodetype<>ordconstn) then
          internalerror(200405301);

        secondpass(left);
        secondpass(right);
        if is_64bit(left.resulttype.def) then
          begin
            location_reset(location,LOC_REGISTER,OS_64);

            { load left operator in a register }
            location_force_reg(exprasmlist,left.location,OS_64,false);
            hreg64hi:=left.location.register64.reghi;
            hreg64lo:=left.location.register64.reglo;

            shiftval := tordconstnode(right).value and 63;
            if shiftval > 31 then
              begin
                if nodetype = shln then
                  begin
                    cg.a_load_const_reg(exprasmlist,OS_32,0,hreg64hi);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval and 31,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_load_const_reg(exprasmlist,OS_32,0,hreg64lo);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval and 31,hreg64hi,hreg64hi);
                  end;
                location.register64.reglo:=hreg64hi;
                location.register64.reghi:=hreg64lo;
              end
            else
              begin
                hregister:=cg.getintregister(exprasmlist,OS_32);
                if nodetype = shln then
                  begin
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,32-shiftval,hreg64lo,hregister);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval,hreg64hi,hreg64hi);
                    cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hregister,hreg64hi,hreg64hi);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,32-shiftval,hreg64hi,hregister);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval,hreg64lo,hreg64lo);
                    cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hregister,hreg64lo,hreg64lo);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval,hreg64hi,hreg64hi);
                  end;
                location.register64.reghi:=hreg64hi;
                location.register64.reglo:=hreg64lo;
              end;
          end
        else
          begin
            { load left operators in a register }
            location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
            location_copy(location,left.location);
            resultreg := location.register;
            hregister1 := location.register;
            if (location.loc = LOC_CREGISTER) then
              begin
                location.loc := LOC_REGISTER;
                resultreg := cg.GetIntRegister(exprasmlist,OS_INT);
                location.register := resultreg;
              end;
            { determine operator }
            if nodetype=shln then
              op:=OP_SHL
            else
              op:=OP_SHR;
            { shifting by a constant directly coded: }
            if (right.nodetype=ordconstn) then
              begin
                if tordconstnode(right).value and 31<>0 then
                  cg.a_op_const_reg_reg(exprasmlist,op,OS_32,tordconstnode(right).value and 31,hregister1,resultreg)
              end
            else
              begin
                { load shift count in a register if necessary }
                location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
                cg.a_op_reg_reg_reg(exprasmlist,op,OS_32,right.location.register,hregister1,resultreg);
              end;
          end;
      end;


{*****************************************************************************
                               TSPARCNOTNODE
*****************************************************************************}

    procedure tsparcnotnode.second_boolean;
      var
        hl : tasmlabel;
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
                  exprasmlist.concat(taicpu.op_reg_const_reg(A_SUBcc,left.location.register,0,NR_G0));
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;


begin
   cmoddivnode:=tSparcmoddivnode;
   cshlshrnode:=tSparcshlshrnode;
   cnotnode:=tSparcnotnode;
end.
{
  $Log: ncpumat.pas,v $
  Revision 1.23  2005/02/14 17:13:10  peter
    * truncate log

}
