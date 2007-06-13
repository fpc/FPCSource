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

      tSparcshlshrnode = class(tshlshrnode)
         procedure pass_generate_code;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tSparcnotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,pass_2,procinfo,
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
         location_copy(location,left.location);

         { put numerator in register }
         location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
         location_copy(location,left.location);
         numerator := location.register;

         if (nodetype = modn) then
           resultreg := cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT)
         else
           begin
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.loc := LOC_REGISTER;
                 location.register := cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);
               end;
             resultreg := location.register;
           end;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value.svalue,power) then
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
             location_force_reg(current_asmdata.CurrAsmList,right.location,
               def_cgsize(right.resultdef),true);
             divider := right.location.register;

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
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,numerator,divider,resultreg));

             if (nodetype = modn) then
               begin
                 current_asmdata.getjumplabel(overflowlabel);
                 ai:=taicpu.op_cond_sym(A_Bxx,C_O,overflowlabel);
                 ai.delayslot_annulled:=true;
                 current_asmdata.CurrAsmList.concat(ai);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_NOT,resultreg));
                 cg.a_label(current_asmdata.CurrAsmList,overflowlabel);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SMUL,resultreg,divider,resultreg));
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


    procedure tSparcshlshrnode.pass_generate_code;
      var
        hregister,resultreg,hregister1,
        hreg64hi,hreg64lo : tregister;
        op : topcg;
        shiftval: aword;
      begin
        { 64bit without constants need a helper, and is
          already replaced in pass1 }
        if is_64bit(left.resultdef) and
           (right.nodetype<>ordconstn) then
          internalerror(200405301);

        secondpass(left);
        secondpass(right);
        if is_64bit(left.resultdef) then
          begin
            location_reset(location,LOC_REGISTER,OS_64);

            { load left operator in a register }
            location_force_reg(current_asmdata.CurrAsmList,left.location,OS_64,false);
            hreg64hi:=left.location.register64.reghi;
            hreg64lo:=left.location.register64.reglo;

            shiftval := tordconstnode(right).value.svalue and 63;
            if shiftval > 31 then
              begin
                if nodetype = shln then
                  begin
                    cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,hreg64hi);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval and 31,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,hreg64lo);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval and 31,hreg64hi,hreg64hi);
                  end;
                location.register64.reglo:=hreg64hi;
                location.register64.reghi:=hreg64lo;
              end
            else
              begin
                hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                if nodetype = shln then
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,32-shiftval,hreg64lo,hregister);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64hi,hreg64hi);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hregister,hreg64hi,hreg64hi);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,32-shiftval,hreg64hi,hregister);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64lo,hreg64lo);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hregister,hreg64lo,hreg64lo);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64hi,hreg64hi);
                  end;
                location.register64.reghi:=hreg64hi;
                location.register64.reglo:=hreg64lo;
              end;
          end
        else
          begin
            { load left operators in a register }
            location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
            location_copy(location,left.location);
            resultreg := location.register;
            hregister1 := location.register;
            if (location.loc = LOC_CREGISTER) then
              begin
                location.loc := LOC_REGISTER;
                resultreg := cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);
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
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,OS_32,tordconstnode(right).value.svalue and 31,hregister1,resultreg)
              end
            else
              begin
                { load shift count in a register if necessary }
                location_force_reg(current_asmdata.CurrAsmList,right.location,def_cgsize(right.resultdef),true);
                cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,OS_32,right.location.register,hregister1,resultreg);
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
            hl:=current_procinfo.CurrTrueLabel;
            current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
            current_procinfo.CurrFalseLabel:=hl;
            secondpass(left);
            maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
            hl:=current_procinfo.CurrTrueLabel;
            current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
            current_procinfo.CurrFalseLabel:=hl;
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
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE,
              LOC_SUBSETREG, LOC_CSUBSETREG,
              LOC_SUBSETREF, LOC_CSUBSETREF:
                begin
                  location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const_reg(A_SUBcc,left.location.register,0,NR_G0));
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
