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
        function pass_1: tnode; override;
        procedure second_float;override;
      end;

      tarmshlshrnode = class(tcgshlshrnode)
         procedure second_64bit;override;
         function first_shlshr64bitint: tnode; override;
      end;

implementation

    uses
      globtype,
      cutils,verbose,globals,constexp,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      symtype,symconst,symtable,
      cgbase,cgobj,hlcgobj,cgutils,
      pass_2,procinfo,
      ncon,ncnv,ncal,ninl,
      cpubase,cpuinfo,
      ncgutil,
      nadd,pass_1,symdef;

{*****************************************************************************
                             TARMMODDIVNODE
*****************************************************************************}

    function tarmmoddivnode.first_moddivint: tnode;
      var
        power  : longint;
      begin
        if not(cs_check_overflow in current_settings.localswitches) and
           (right.nodetype=ordconstn) and
           (nodetype=divn) and
           (ispowerof2(tordconstnode(right).value,power) or
            (tordconstnode(right).value=1) or
            (tordconstnode(right).value=int64(-1))
           ) and
           not(is_64bitint(resultdef)) then
          result:=nil
        else if ((GenerateThumbCode) and (CPUARM_HAS_THUMB_IDIV in cpu_capabilities[current_settings.cputype])) and
          (nodetype=divn) and
          not(is_64bitint(resultdef)) then
          result:=nil
        else if ((GenerateThumbCode) and (CPUARM_HAS_THUMB_IDIV in cpu_capabilities[current_settings.cputype])) and
          (nodetype=modn) and
          not(is_64bitint(resultdef)) then
          begin
            if (right.nodetype=ordconstn) and
              ispowerof2(tordconstnode(right).value,power) and
              (tordconstnode(right).value<=256) and
              (tordconstnode(right).value>0) then
              result:=caddnode.create_internal(andn,left,cordconstnode.create(tordconstnode(right).value-1,sinttype,false))
            else
              begin
                result:=caddnode.create_internal(subn,left,caddnode.create_internal(muln,right,cmoddivnode.Create(divn,left.getcopy,right.getcopy)));
                right:=nil;
              end;
            left:=nil;
            firstpass(result);
          end
        else if (nodetype=modn) and
          (is_signed(left.resultdef)) and
          (right.nodetype=ordconstn) and
          (tordconstnode(right).value=2) then
          begin
            // result:=(0-(left and 1)) and (1+(sarlongint(left,31) shl 1))
            result:=caddnode.create_internal(andn,caddnode.create_internal(subn,cordconstnode.create(0,sinttype,false),caddnode.create_internal(andn,left,cordconstnode.create(1,sinttype,false))),
                                         caddnode.create_internal(addn,cordconstnode.create(1,sinttype,false),
                                                              cshlshrnode.create(shln,cinlinenode.create(in_sar_x_y,false,ccallparanode.create(cordconstnode.create(31,sinttype,false),ccallparanode.Create(left.getcopy,nil))),cordconstnode.create(1,sinttype,false))));
            left:=nil;
            firstpass(result);
          end
        else
          result:=inherited first_moddivint;

        { we may not change the result type here }
        if assigned(result) and (torddef(result.resultdef).ordtype<>torddef(resultdef).ordtype) then
          inserttypeconv(result,resultdef);
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
               if cs_check_overflow in current_settings.localswitches then
                 cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);

               current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_MVN,
                 resultreg,numerator),toppostfix(ord(cs_check_overflow in current_settings.localswitches)*ord(PF_S))));
             end
           else if ispowerof2(tordconstnode(right).value,power) then
             begin
               if (is_signed(right.resultdef)) then
                 begin
                    helper1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    helper2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    if power = 1 then
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,numerator,helper1)
                    else
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SAR,OS_INT,31,numerator,helper1);
                    if GenerateThumbCode then
                      begin
                        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_INT,32-power,helper1);
                        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD,helper2,numerator,helper1));
                      end
                    else
                      begin
                        shifterop_reset(so);
                        so.shiftmode:=SM_LSR;
                        so.shiftimm:=32-power;
                        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_shifterop(A_ADD,helper2,numerator,helper1,so));
                      end;
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SAR,OS_INT,power,helper2,resultreg);
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

        if ((GenerateThumbCode) and (CPUARM_HAS_THUMB_IDIV in cpu_capabilities[current_settings.cputype])) and
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

            if left.location.loc<>LOC_JUMP then
              internalerror(2012081305);

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
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
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

    function tarmunaryminusnode.pass_1: tnode;
      var
        procname: string[31];
        fdef : tdef;
      begin
        if (current_settings.fputype<>fpu_fpv4_s16) or
          (tfloatdef(resultdef).floattype=s32real) then
          exit(inherited pass_1);

        result:=nil;
        firstpass(left);
        if codegenerror then
          exit;

        if (left.resultdef.typ=floatdef) then
          begin
            case tfloatdef(resultdef).floattype of
              s64real:
                begin
                  procname:='float64_sub';
                  fdef:=search_system_type('FLOAT64').typedef;
                end;
              else
                internalerror(2005082801);
            end;
            result:=ctypeconvnode.create_internal(ccallnode.createintern(procname,ccallparanode.create(
              ctypeconvnode.create_internal(left,fDef),
              ccallparanode.create(ctypeconvnode.create_internal(crealconstnode.create(0,resultdef),fdef),nil))),resultdef);

            left:=nil;
          end
        else
          begin
            if (left.resultdef.typ=floatdef) then
              expectloc:=LOC_FPUREGISTER
             else if (left.resultdef.typ=orddef) then
               expectloc:=LOC_REGISTER;
          end;
      end;

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
              hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
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
          fpu_fpv4_s16:
            begin
              hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
              location:=left.location;
              if (left.location.loc=LOC_CMMREGISTER) then
                location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
              current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_VNEG,
                location.register,left.location.register), PF_F32));
            end
          else
            internalerror(2009112602);
        end;
      end;

    function tarmshlshrnode.first_shlshr64bitint: tnode;
      begin
        if GenerateThumbCode or GenerateThumb2Code then
          result:=inherited
        else
          result := nil;
      end;

    procedure tarmshlshrnode.second_64bit;
      var
        hreg64hi,hreg64lo,shiftreg:Tregister;
        v : TConstExprInt;
        l1,l2,l3:Tasmlabel;
        so: tshifterop;

      procedure emit_instr(p: tai);
        begin
          current_asmdata.CurrAsmList.concat(p);
        end;

      {Reg1 gets shifted and moved into reg2, and is set to zero afterwards}
      procedure shift_more_than_32(reg1, reg2: TRegister; shiftval: Byte ; sm: TShiftMode);
        begin
          shifterop_reset(so); so.shiftimm:=shiftval - 32; so.shiftmode:=sm;
          emit_instr(taicpu.op_reg_reg_shifterop(A_MOV, reg2, reg1, so));
          emit_instr(taicpu.op_reg_const(A_MOV, reg1, 0));
        end;

      procedure shift_less_than_32(reg1, reg2: TRegister; shiftval: Byte; shiftright: boolean);
        begin
          shifterop_reset(so); so.shiftimm:=shiftval;
          if shiftright then so.shiftmode:=SM_LSR else so.shiftmode:=SM_LSL;
          emit_instr(taicpu.op_reg_reg_shifterop(A_MOV, reg1, reg1, so));

          if shiftright then so.shiftmode:=SM_LSL else so.shiftmode:=SM_LSR;
          so.shiftimm:=32-shiftval;
          emit_instr(taicpu.op_reg_reg_reg_shifterop(A_ORR, reg1, reg1, reg2, so));

          if shiftright then so.shiftmode:=SM_LSR else so.shiftmode:=SM_LSL;
          so.shiftimm:=shiftval;
          emit_instr(taicpu.op_reg_reg_shifterop(A_MOV, reg2, reg2, so));
        end;

      procedure shift_by_variable(reg1, reg2, shiftval: TRegister; shiftright: boolean);
        var
          shiftval2:TRegister;
        begin
          shifterop_reset(so);
          shiftval2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);

          cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);

          {Do we shift more than 32 bits?}
          emit_instr(setoppostfix(taicpu.op_reg_reg_const(A_RSB, shiftval2, shiftval, 32), PF_S));

          {This part cares for 32 bits and more}
          emit_instr(setcondition(taicpu.op_reg_reg_const(A_SUB, shiftval2, shiftval, 32), C_MI));
          if shiftright then so.shiftmode:=SM_LSR else so.shiftmode:=SM_LSL;
          so.rs:=shiftval2;
          emit_instr(setcondition(taicpu.op_reg_reg_shifterop(A_MOV, reg2, reg1, so), C_MI));

          {Less than 32 bits}
          so.rs:=shiftval;
          emit_instr(setcondition(taicpu.op_reg_reg_shifterop(A_MOV, reg2, reg2, so), C_PL));
          if shiftright then so.shiftmode:=SM_LSL else so.shiftmode:=SM_LSR;
          so.rs:=shiftval2;
          emit_instr(setcondition(taicpu.op_reg_reg_reg_shifterop(A_ORR, reg2, reg2, reg1, so), C_PL));

          cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);

          {Final adjustments}
          if shiftright then so.shiftmode:=SM_LSR else so.shiftmode:=SM_LSL;
          so.rs:=shiftval;
          emit_instr(taicpu.op_reg_reg_shifterop(A_MOV, reg1, reg1, so));
        end;

      begin
        if GenerateThumbCode or GenerateThumb2Code then
        begin
          inherited;
          exit;
        end;

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

        { load left operator in a register }
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
        hreg64hi:=left.location.register64.reghi;
        hreg64lo:=left.location.register64.reglo;
        location.register64.reghi:=hreg64hi;
        location.register64.reglo:=hreg64lo;

        { shifting by a constant directly coded: }
        if (right.nodetype=ordconstn) then
          begin
            v:=Tordconstnode(right).value and 63;
            {Single bit shift}
            if v = 1 then
              if nodetype=shln then
                begin
                  {Shift left by one by 2 simple 32bit additions}
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                  emit_instr(setoppostfix(taicpu.op_reg_reg_reg(A_ADD, hreg64lo, hreg64lo, hreg64lo), PF_S));
                  emit_instr(taicpu.op_reg_reg_reg(A_ADC, hreg64hi, hreg64hi, hreg64hi));
                  cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                end
              else
                begin
                  {Shift right by first shifting hi by one and then using RRX (rotate right extended), which rotates through the carry}
                  shifterop_reset(so); so.shiftmode:=SM_LSR; so.shiftimm:=1;
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                  emit_instr(setoppostfix(taicpu.op_reg_reg_shifterop(A_MOV, hreg64hi, hreg64hi, so), PF_S));
                  so.shiftmode:=SM_RRX; so.shiftimm:=0; {RRX does NOT have a shift amount}
                  emit_instr(taicpu.op_reg_reg_shifterop(A_MOV, hreg64lo, hreg64lo, so));
                  cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                end
            {A 32bit shift just replaces a register and clears the other}
            else if v = 32 then
              begin
                if nodetype=shln then
                  emit_instr(taicpu.op_reg_const(A_MOV, hreg64hi, 0))
                else
                  emit_instr(taicpu.op_reg_const(A_MOV, hreg64lo, 0));
                location.register64.reghi:=hreg64lo;
                location.register64.reglo:=hreg64hi;
              end
            {Shift LESS than 32}
            else if (v < 32) and (v > 1) then
              if nodetype=shln then
                shift_less_than_32(hreg64hi, hreg64lo, v.uvalue, false)
              else
                shift_less_than_32(hreg64lo, hreg64hi, v.uvalue, true)
            {More than 32}
            else if v > 32 then
              if nodetype=shln then
                shift_more_than_32(hreg64lo, hreg64hi, v.uvalue, SM_LSL)
              else
                shift_more_than_32(hreg64hi, hreg64lo, v.uvalue, SM_LSR);
          end
        else
          begin
            { force right operators in a register }
            hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,resultdef,false);
            if nodetype = shln then
              shift_by_variable(hreg64lo,hreg64hi,right.location.register, false)
            else
              shift_by_variable(hreg64hi,hreg64lo,right.location.register, true);
          end;
      end;


begin
  cmoddivnode:=tarmmoddivnode;
  cnotnode:=tarmnotnode;
  cunaryminusnode:=tarmunaryminusnode;
  cshlshrnode:=tarmshlshrnode;
end.
