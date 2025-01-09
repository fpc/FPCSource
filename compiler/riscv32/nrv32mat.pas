{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Risc-V32 assembler for math nodes

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
unit nrv32mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat, ncgmat,
      cgbase;

    type
      trv32moddivnode = class(tcgmoddivnode)
        procedure emit_div_reg_reg(signed: boolean; denum, num: tregister); override;
        procedure emit_mod_reg_reg(signed: boolean; denum, num: tregister); override;
        function first_moddivint: tnode; override;
      end;

      trv32shlshrnode = class(tcgshlshrnode)
        function first_shlshr64bitint: tnode;override;
        procedure second_64bit;override;
      end;

      trv32unaryminusnode = class(trvunaryminusnode)
      end;

      trv32notnode = class(tcgnotnode)
        procedure second_boolean; override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgutils,cgobj,hlcgobj,pass_2,
      cpubase,cpuinfo,
      ncon,procinfo,
      ncgutil,cgcpu;

    procedure trv32notnode.second_boolean;
      var
        tlabel, flabel: tasmlabel;
      begin
        secondpass(left);
        if not handle_locjump then
          begin
            case left.location.loc of
              LOC_FLAGS :
                begin
                  Internalerror(2016060601);
                  //location_copy(location,left.location);
                  //inverse_flags(location.resflags);
                end;
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE,
              LOC_SUBSETREG, LOC_CSUBSETREG,
              LOC_SUBSETREF, LOC_CSUBSETREF:
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

                  location_reset(location,LOC_REGISTER,OS_INT);
                  location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,s32inttype);

                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTIU,location.register,left.location.register,1));
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;

    procedure trv32moddivnode.emit_div_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_DIV
        else
          op:=A_DIVU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;

    procedure trv32moddivnode.emit_mod_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_REM
        else
          op:=A_REMU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    function trv32moddivnode.first_moddivint: tnode;
      begin
        if (not is_64bitint(resultdef)) and
           (CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype]) then
          Result:=nil
        else
          result:=inherited;
      end;


    function trv32shlshrnode.first_shlshr64bitint: tnode;
      begin
        result := nil;
      end;


    procedure trv32shlshrnode.second_64bit;
      var
        v : TConstExprInt;
        lreg, resreg: TRegister64;

      procedure emit_instr(p: tai);
        begin
          current_asmdata.CurrAsmList.concat(p);
        end;

      var
        tmpreg1, tmpreg2: TRegister;
        less32, finished: TAsmLabel;

      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
        location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);

        { load left operator in a register }
        if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) or
           (left.location.size<>OS_64) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);

        lreg:=left.location.register64;
        resreg:=location.register64;

        { shifting by a constant directly coded: }
        if right.nodetype=ordconstn then
          begin
            v:=tordconstnode(right).value and 63;
            if v >= 32 then
              begin
                if nodetype=shln then
                  begin
                    emit_instr(taicpu.op_reg_const(A_LI, resreg.reglo,0));
                    emit_instr(taicpu.op_reg_reg_const(A_SLLI,resreg.reghi,lreg.reglo,v.uvalue-32));
                  end
                else
                  begin
                    emit_instr(taicpu.op_reg_const(A_LI, resreg.reghi,0));
                    emit_instr(taicpu.op_reg_reg_const(A_SRLI,resreg.reglo,lreg.reghi,v.uvalue-32));
                  end
              end
            else if (v < 32) then
              if nodetype=shln then
                begin
                  tmpreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                  emit_instr(taicpu.op_reg_reg_const(A_SRLI,tmpreg1,lreg.reglo,32-v.uvalue));
                  emit_instr(taicpu.op_reg_reg_const(A_SLLI,resreg.reglo,lreg.reglo,v.uvalue));
                  emit_instr(taicpu.op_reg_reg_const(A_SLLI,resreg.reghi,lreg.reghi,v.uvalue));
                  emit_instr(taicpu.op_reg_reg_reg(A_OR,resreg.reghi,resreg.reghi,tmpreg1));
                end
              else
                begin
                  tmpreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                  emit_instr(taicpu.op_reg_reg_const(A_SLLI,tmpreg1,lreg.reghi,32-v.uvalue));
                  emit_instr(taicpu.op_reg_reg_const(A_SRLI,resreg.reglo,lreg.reglo,v.uvalue));
                  emit_instr(taicpu.op_reg_reg_const(A_SRLI,resreg.reghi,lreg.reghi,v.uvalue));
                  emit_instr(taicpu.op_reg_reg_reg(A_OR,resreg.reglo,resreg.reglo,tmpreg1));
                end
              else
                Internalerror(2024072601);
          end
        else
          begin
            { force right operator into a register }
            if not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) or
               (right.location.size<>OS_32) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,u32inttype,true);

            current_asmdata.getjumplabel(less32);
            current_asmdata.getjumplabel(finished);
            tmpreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            tmpreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);

            if nodetype = shln then
              begin
                emit_instr(taicpu.op_reg_reg_const(A_ADDI,tmpreg1,right.location.register64.reglo,-32));
                emit_instr(taicpu.op_reg_sym(A_BLTZ,tmpreg1,less32));
                emit_instr(taicpu.op_reg_const(A_LI,resreg.reglo,0));
                emit_instr(taicpu.op_reg_reg_reg(A_SLL,resreg.reghi,lreg.reglo,tmpreg1));
                cg.a_jmp_always(current_asmdata.CurrAsmList,finished);
                cg.a_label(current_asmdata.CurrAsmList,less32);
                { simple case were we know where the bit ends up, usefull when bitmasks are created }
                if (left.nodetype=ordconstn) and (tordconstnode(left).value=1) then
                  begin
                    emit_instr(taicpu.op_reg_reg_reg(A_SLL,resreg.reglo,lreg.reglo,right.location.register64.reglo));
                    emit_instr(taicpu.op_reg_const(A_LI,resreg.reghi,0));
                  end
                else
                  begin
                    emit_instr(taicpu.op_reg_const(A_LI,tmpreg1,31));
                    emit_instr(taicpu.op_reg_reg_const(A_SRLI,tmpreg2,lreg.reglo,1));
                    emit_instr(taicpu.op_reg_reg_reg(A_SUB,tmpreg1,tmpreg1,right.location.register64.reglo));
                    emit_instr(taicpu.op_reg_reg_reg(A_SLL,resreg.reglo,lreg.reglo,right.location.register64.reglo));
                    emit_instr(taicpu.op_reg_reg_reg(A_SRL,tmpreg2,tmpreg2,tmpreg1));
                    emit_instr(taicpu.op_reg_reg_reg(A_SLL,resreg.reghi,lreg.reghi,right.location.register64.reglo));
                    emit_instr(taicpu.op_reg_reg_reg(A_OR,resreg.reghi,resreg.reghi,tmpreg2));
                  end;
              end
            else
              begin
                emit_instr(taicpu.op_reg_reg_const(A_ADDI,tmpreg1,right.location.register64.reglo,-32));
                emit_instr(taicpu.op_reg_sym(A_BLTZ,tmpreg1,less32));
                emit_instr(taicpu.op_reg_const(A_LI,resreg.reghi,0));
                emit_instr(taicpu.op_reg_reg_reg(A_SRL,resreg.reglo,lreg.reghi,tmpreg1));
                cg.a_jmp_always(current_asmdata.CurrAsmList,finished);
                cg.a_label(current_asmdata.CurrAsmList,less32);
                emit_instr(taicpu.op_reg_const(A_LI,tmpreg1,31));
                emit_instr(taicpu.op_reg_reg_const(A_SLLI,tmpreg2,lreg.reghi,1));
                emit_instr(taicpu.op_reg_reg_reg(A_SUB,tmpreg1,tmpreg1,right.location.register64.reglo));
                emit_instr(taicpu.op_reg_reg_reg(A_SRL,resreg.reglo,lreg.reglo,right.location.register64.reglo));
                emit_instr(taicpu.op_reg_reg_reg(A_SLL,tmpreg2,tmpreg2,tmpreg1));
                emit_instr(taicpu.op_reg_reg_reg(A_SRL,resreg.reghi,lreg.reghi,right.location.register64.reglo));
                emit_instr(taicpu.op_reg_reg_reg(A_OR,resreg.reglo,resreg.reglo,tmpreg2));
              end;
            cg.a_label(current_asmdata.CurrAsmList,finished);
          end;
      end;


begin
   cmoddivnode:=trv32moddivnode;
   cshlshrnode:=trv32shlshrnode;
   cunaryminusnode:=trv32unaryminusnode;
   cnotnode:=trv32notnode;
end.

