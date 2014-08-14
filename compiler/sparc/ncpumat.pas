{
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
         procedure pass_generate_code;override;
      end;

      tSparcshlshrnode = class(tcgshlshrnode)
         procedure second_64bit;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tSparcnotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

      tsparcunaryminusnode = class(tcgunaryminusnode)
         procedure second_float; override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,hlcgobj,pass_2,procinfo,
      ncon,
      cpubase,
      ncgutil,cgcpu,cgutils;

{*****************************************************************************
                             TSparcMODDIVNODE
*****************************************************************************}

    procedure tSparcmoddivnode.pass_generate_code;
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
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         location.register:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);

         { put numerator in register }
         hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
         numerator := left.location.register;
         resultreg := location.register;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value.svalue,power) and
            (not (cs_check_overflow in current_settings.localswitches)) then
           begin
             if is_signed(left.resultdef) Then
               begin
                 tmpreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);
                 cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SAR,OS_INT,31,numerator,tmpreg);
                 { if signed, tmpreg=right value-1, otherwise 0 }
                 cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,tordconstnode(right).value.svalue-1,tmpreg);
                 { add to the left value }
                 cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_INT,numerator,tmpreg);
                 cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SAR,OS_INT,aword(power),tmpreg,resultreg);
               end
             else
               cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,aword(power),numerator,resultreg);
           end
         else
           begin
             { load divider in a register if necessary }
             divider:=NR_NO;
             if (right.location.loc<>LOC_CONSTANT) or
                (right.location.value<simm13lo) or
                (right.location.value>simm13hi) then
               begin
                 hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,
                   right.resultdef,right.resultdef,true);
                 divider:=right.location.register;
               end;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { And on Sparc, the only way to catch a div-by-0 is by checking  }
             { the overflow flag (JM)                                       }

             { Fill %y with the -1 or 0 depending on the highest bit }
             if is_signed(left.resultdef) then
               begin
                 tmpreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_const_reg(A_SRA,numerator,31,tmpreg));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOV,tmpreg,NR_Y));
               end
             else
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOV,NR_G0,NR_Y));
             { wait 3 instructions slots before we can read %y }
             current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
             current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
             current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));

             op := divops[is_signed(right.resultdef),
                          cs_check_overflow in current_settings.localswitches];
             if (divider<>NR_NO) then
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,numerator,divider,resultreg))
             else
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_const_reg(op,numerator,right.location.value,resultreg));

             if (nodetype = modn) then
               begin
                 current_asmdata.getjumplabel(overflowlabel);
                 ai:=taicpu.op_cond_sym(A_Bxx,C_VS,overflowlabel);
                 ai.delayslot_annulled:=true;
                 current_asmdata.CurrAsmList.concat(ai);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_NOT,resultreg));
                 cg.a_label(current_asmdata.CurrAsmList,overflowlabel);
                 if (divider<>NR_NO) then
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SMUL,resultreg,divider,resultreg))
                 else
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_const_reg(A_SMUL,resultreg,right.location.value,resultreg));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUB,numerator,resultreg,resultreg));
               end;
           end;
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        cg.g_overflowcheck(current_asmdata.CurrAsmList,Location,resultdef);
      end;


{*****************************************************************************
                             TSparcSHLRSHRNODE
*****************************************************************************}

    function TSparcShlShrNode.first_shlshr64bitint:TNode;
      begin
        { 64bit without constants need a helper }
        if is_64bit(left.resultdef) and
           (right.nodetype<>ordconstn) then
          begin
            result:=inherited first_shlshr64bitint;
            exit;
          end;

        result := nil;
      end;


    procedure tSparcshlshrnode.second_64bit;
      var
        hregister,hreg64hi,hreg64lo : tregister;
        op : topcg;
        shiftval: aword;
      const
        ops: array [boolean] of topcg = (OP_SHR,OP_SHL);
      begin
        { 64bit without constants need a helper, and is
          already replaced in pass1 }
        if (right.nodetype<>ordconstn) then
          internalerror(200405301);

        location_reset(location, LOC_REGISTER, def_cgsize(resultdef));

        { load left operator in a register }
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
        hreg64hi:=left.location.register64.reghi;
        hreg64lo:=left.location.register64.reglo;

        shiftval := tordconstnode(right).value.svalue and 63;
        op := ops[nodetype=shln];
        location.register64.reglo:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_32);
        location.register64.reghi:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_32);

        { Emitting "left shl 1" as "left+left" is twice shorter }
        if (nodetype=shln) and (shiftval=1) then
          cg64.a_op64_reg_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_64,left.location.register64,left.location.register64,location.register64)
        else if shiftval > 31 then
          begin
            if nodetype = shln then
              begin
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reglo);
                { if shiftval and 31 = 0, it will optimize to MOVE }
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, shiftval and 31, hreg64lo, location.register64.reghi);
              end
            else
              begin
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reghi);
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, shiftval and 31, hreg64hi, location.register64.reglo);
              end;
          end
        else
          begin
            hregister := cg.getintregister(current_asmdata.CurrAsmList, OS_32);

            cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, op, OS_32, shiftval, hreg64hi, location.register64.reghi);
            cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, op, OS_32, shiftval, hreg64lo, location.register64.reglo);
            if shiftval <> 0 then
              begin
                if nodetype = shln then
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, 32-shiftval, hreg64lo, hregister);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_32, hregister, location.register64.reghi, location.register64.reghi);
                  end
                else
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, 32-shiftval, hreg64hi, hregister);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_32, hregister, location.register64.reglo, location.register64.reglo);
                  end;
              end;
          end;
      end;


{*****************************************************************************
                               TSPARCNOTNODE
*****************************************************************************}

    procedure tsparcnotnode.second_boolean;
      begin
        if not handle_locjump then
          begin
            secondpass(left);
            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE,
              LOC_SUBSETREG, LOC_CSUBSETREG,
              LOC_SUBSETREF, LOC_CSUBSETREF:
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
                  if is_64bit(left.resultdef) then
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ORcc,
                      left.location.register64.reglo,left.location.register64.reghi,NR_G0))
                  else
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_const_reg(A_SUBcc,left.location.register,0,NR_G0));
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;


{*****************************************************************************
                                   TSPARCUNARYMINUSNODE
*****************************************************************************}

    procedure tsparcunaryminusnode.second_float;
      begin
        secondpass(left);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        case location.size of
          OS_F32:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEGs,left.location.register,location.register));
          OS_F64:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEGd,left.location.register,location.register));
          OS_F128:
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEGq,left.location.register,location.register));
        else
          internalerror(2013030501);
        end;
      end;

begin
   cmoddivnode:=tSparcmoddivnode;
   cshlshrnode:=tSparcshlshrnode;
   cnotnode:=tSparcnotnode;
   cunaryminusnode:=tsparcunaryminusnode;
end.
