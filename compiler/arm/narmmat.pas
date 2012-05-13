{
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
      tarmmoddivnode = class(tmoddivnode)
        function first_moddivint: tnode;override;
        procedure pass_generate_code;override;
      end;

      tarmnotnode = class(tcgnotnode)
        procedure second_boolean;override;
      end;

      tarmunaryminusnode = class(tcgunaryminusnode)
        procedure second_float;override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,constexp,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,hlcgobj,cgutils,
      pass_2,procinfo,
      ncon,
      cpubase,cpuinfo,
      ncgutil,cgcpu,
      nadd,pass_1,symdef;

{*****************************************************************************
                             TARMMODDIVNODE
*****************************************************************************}

    function tarmmoddivnode.first_moddivint: tnode;
      var
        power  : longint;
      begin
        if (right.nodetype=ordconstn) and
          (nodetype=divn) and
          (ispowerof2(tordconstnode(right).value,power) or
           (tordconstnode(right).value=1) or
           (tordconstnode(right).value=int64(-1))
          ) and
          not(is_64bitint(resultdef)) then
          result:=nil
        else if (current_settings.cputype in [cpu_armv7m]) and
          (nodetype=divn) and
          not(is_64bitint(resultdef)) then
          result:=nil
        else if (current_settings.cputype in [cpu_armv7m]) and
          (nodetype=modn) and
          not(is_64bitint(resultdef)) then
          begin
            if (right.nodetype=ordconstn) and
              ispowerof2(tordconstnode(right).value,power) and
              (tordconstnode(right).value<=256) and
              (tordconstnode(right).value>0) then
              result:=caddnode.create(andn,left,cordconstnode.create(tordconstnode(right).value-1,sinttype,false))
            else
              begin
                result:=caddnode.create(subn,left,caddnode.create(muln,right.getcopy, cmoddivnode.Create(divn,left.getcopy,right.getcopy)));
                right:=nil;
              end;
            left:=nil;
          end
        else
          result:=inherited first_moddivint;
      end;


    procedure tarmmoddivnode.pass_generate_code;
      var
        power  : longint;
        numerator,
        helper1,
        helper2,
        resultreg  : tregister;
        size       : Tcgsize;
        so : tshifterop;

       procedure genOrdConstNodeDiv;
         begin
           if tordconstnode(right).value=0 then
             internalerror(2005061701)
           else if tordconstnode(right).value=1 then
             cg.a_load_reg_reg(current_asmdata.CurrAsmList, OS_INT, OS_INT, numerator, resultreg)
           else if (tordconstnode(right).value = int64(-1)) then
             begin
               // note: only in the signed case possible..., may overflow
               current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_MVN,
                 resultreg,numerator),toppostfix(ord(cs_check_overflow in current_settings.localswitches)*ord(PF_S))));
             end
           else if ispowerof2(tordconstnode(right).value,power) then
             begin
               if (is_signed(right.resultdef)) then
                 begin
                    helper1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    helper2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    shifterop_reset(so);
                    so.shiftmode:=SM_ASR;
                    so.shiftimm:=31;
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_shifterop(A_MOV,helper1,numerator,so));
                    shifterop_reset(so);
                    so.shiftmode:=SM_LSR;
                    so.shiftimm:=32-power;
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_shifterop(A_ADD,helper2,numerator,helper1,so));
                    shifterop_reset(so);
                    so.shiftmode:=SM_ASR;
                    so.shiftimm:=power;
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_shifterop(A_MOV,resultreg,helper2,so));
                  end
               else
                 cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,power,numerator,resultreg)
             end;
         end;

{
       procedure genOrdConstNodeMod;
         var
             modreg, maskreg, tempreg : tregister;
         begin
             if (tordconstnode(right).value = 0) then begin
                 internalerror(2005061702);
             end
             else if (abs(tordconstnode(right).value.svalue) = 1) then
             begin
                // x mod +/-1 is always zero
                cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 0, resultreg);
             end
             else if (ispowerof2(tordconstnode(right).value, power)) then
             begin
                 if (is_signed(right.resultdef)) then begin

                     tempreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
                     maskreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
                     modreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);

                     cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, abs(tordconstnode(right).value.svalue)-1, modreg);
                     cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, 31, numerator, maskreg);
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, numerator, modreg, tempreg);

                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC, maskreg, maskreg, modreg));
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBFIC, modreg, tempreg, 0));
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBFE, modreg, modreg, modreg));
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, modreg, maskreg, maskreg);
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_INT, maskreg, tempreg, resultreg);
                 end else begin
                     cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, tordconstnode(right).value.svalue-1, numerator, resultreg);
                 end;
             end else begin
                 genOrdConstNodeDiv();
                 cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_MUL, OS_INT, tordconstnode(right).value.svalue, resultreg, resultreg);
                 cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT, resultreg, numerator, resultreg);
             end;
         end;
}

      begin
        secondpass(left);
        secondpass(right);

        if (current_settings.cputype in [cpu_armv7m]) and
           (nodetype=divn) and
           not(is_64bitint(resultdef)) then
          begin
            size:=def_cgsize(left.resultdef);
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

            location_copy(location,left.location);
            location.loc := LOC_REGISTER;
            location.register := cg.getintregister(current_asmdata.CurrAsmList,size);
            resultreg:=location.register;

            if (right.nodetype=ordconstn) and
               ((tordconstnode(right).value=1) or
                (tordconstnode(right).value=int64(-1)) or
                (tordconstnode(right).value=0) or
                ispowerof2(tordconstnode(right).value,power)) then
              begin
                numerator:=left.location.register;

                genOrdConstNodeDiv;
              end
            else
              begin
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,left.resultdef,true);

                if is_signed(left.resultdef) or
                   is_signed(right.resultdef) then
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_IDIV,OS_INT,right.location.register,left.location.register,location.register)
                else
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_DIV,OS_INT,right.location.register,left.location.register,location.register);
              end;
          end
        else
          begin
            location_copy(location,left.location);

            { put numerator in register }
            size:=def_cgsize(left.resultdef);
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,
              left.resultdef,left.resultdef,true);
            location_copy(location,left.location);
            numerator:=location.register;
            resultreg:=location.register;
            if location.loc=LOC_CREGISTER then
              begin
                location.loc := LOC_REGISTER;
                location.register := cg.getintregister(current_asmdata.CurrAsmList,size);
                resultreg:=location.register;
              end
            else if (nodetype=modn) or (right.nodetype=ordconstn) then
              begin
                // for a modulus op, and for const nodes we need the result register
                // to be an extra register
                resultreg:=cg.getintregister(current_asmdata.CurrAsmList,size);
              end;

            if right.nodetype=ordconstn then
              begin
                if nodetype=divn then
                  genOrdConstNodeDiv
                else
    //              genOrdConstNodeMod;
              end;

            location.register:=resultreg;
          end;

        { unsigned division/module can only overflow in case of division by zero }
        { (but checking this overflow flag is more convoluted than performing a  }
        {  simple comparison with 0)                                             }
        if is_signed(right.resultdef) then
          cg.g_overflowcheck(current_asmdata.CurrAsmList,location,resultdef);
      end;

{*****************************************************************************
                               TARMNOTNODE
*****************************************************************************}

    procedure tarmnotnode.second_boolean;
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
              LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE,
              LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF :
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,left.location.register,0));
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_EQ;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;

{*****************************************************************************
                               TARMUNARYMINUSNODE
*****************************************************************************}

    procedure tarmunaryminusnode.second_float;
      var
        op: tasmop;
      begin
        secondpass(left);
        case current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
              location:=left.location;
              current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_const(A_RSF,
                location.register,left.location.register,0),
                cgsize2fpuoppostfix[def_cgsize(resultdef)]));
            end;
          fpu_vfpv2,
          fpu_vfpv3,
          fpu_vfpv3_d16:
            begin
              location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,true);
              location:=left.location;
              if (left.location.loc=LOC_CMMREGISTER) then
                location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
              if (location.size=OS_F32) then
                op:=A_FNEGS
              else
                op:=A_FNEGD;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,
                location.register,left.location.register));
            end;
          else
            internalerror(2009112602);
        end;
      end;


begin
  cmoddivnode:=tarmmoddivnode;
  cnotnode:=tarmnotnode;
  cunaryminusnode:=tarmunaryminusnode;
end.
