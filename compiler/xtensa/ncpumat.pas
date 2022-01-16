{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Xtensa assembler for math nodes

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
      cgbase,node,nmat,ncgmat;

    type
      tcpumoddivnode = class(tcgmoddivnode)
        function first_moddivint: tnode; override;
        procedure emit_div_reg_reg(signed: boolean; denum, num: tregister); override;
        procedure emit_mod_reg_reg(signed: boolean; denum, num: tregister); override;
      end;

      tcpunotnode = class(tcgnotnode)
        procedure second_boolean;override;
        function pass_1: tnode;override;
      end;

      tcpuunaryminusnode = class(tcgunaryminusnode)
        function pass_1: tnode; override;
        procedure second_float;override;
      end;

      tcpushlshrnode = class(tcgshlshrnode)
        procedure second_64bit;override;
        function pass_1: tnode;override;
      end;

implementation

    uses
      globtype,compinnr,
      cutils,verbose,globals,constexp,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      symtype,symconst,symtable,
      cgobj,hlcgobj,cgutils,
      pass_2,procinfo,
      ncon,ncnv,ncal,ninl,
      cpubase,cpuinfo,
      ncgutil,
      nadd,pass_1,symdef;

{*****************************************************************************
                             TCPUMODDIVNODE
*****************************************************************************}

    procedure tcpumoddivnode.emit_div_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_QUOS
        else
          op:=A_QUOU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    procedure tcpumoddivnode.emit_mod_reg_reg(signed: boolean; denum, num: tregister);
      var
        op: TAsmOp;
      begin
        if signed then
          op:=A_REMS
        else
          op:=A_REMU;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,num,num,denum));
      end;


    function tcpumoddivnode.first_moddivint: tnode;
      begin
        if (not is_64bitint(resultdef)) and
           (CPUXTENSA_HAS_DIV in cpu_capabilities[current_settings.cputype]) then
          Result:=nil
        else
          result:=inherited;
      end;


{*****************************************************************************
                               TCPUNOTNODE
*****************************************************************************}

    function tcpunotnode.pass_1 : tnode;
      begin
        result:=nil;
        firstpass(left);
        expectloc:=LOC_REGISTER;
      end;


    procedure tcpunotnode.second_boolean;
      var
        tmpreg, hreg1, hreg2, hreg3: TRegister;
        instr: taicpu;
      begin
        secondpass(left);

        if is_64bit(resultdef) then
          begin
            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,resultdef,resultdef,false);
            hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_INT,left.location.register64.reglo,left.location.register64.reghi,hreg1);
            hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,0,hreg2);
            hreg3:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            if is_cbool(resultdef) then
              cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,-1,hreg3)
            else
              cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,1,hreg3);
            location_reset(location, LOC_REGISTER, def_cgsize(resultdef));
            location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            if is_cbool(resultdef) then
              begin
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reglo,hreg3,hreg1);
                instr.condition:=C_EQZ;
                current_asmdata.CurrAsmList.concat(instr);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reghi,hreg3,hreg1);
                instr.condition:=C_EQZ;
                current_asmdata.CurrAsmList.concat(instr);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reglo,hreg2,hreg1);
                instr.condition:=C_NEZ;
                current_asmdata.CurrAsmList.concat(instr);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reghi,hreg2,hreg1);
                instr.condition:=C_NEZ;
                current_asmdata.CurrAsmList.concat(instr);
              end
            else
              begin
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,0,location.register64.reghi);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reglo,hreg3,hreg1);
                instr.condition:=C_EQZ;
                current_asmdata.CurrAsmList.concat(instr);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register64.reglo,hreg2,hreg1);
                instr.condition:=C_NEZ;
                current_asmdata.CurrAsmList.concat(instr);
              end
          end
        else
          begin
            location:=left.location;
            hlcg.location_force_reg(current_asmdata.CurrAsmList,location,resultdef,resultdef,false);
            if is_cbool(resultdef) then
              begin
                { normalize }
                hreg3:=cg.getintregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
                cg.a_load_const_reg(current_asmdata.CurrAsmList,def_cgsize(resultdef),-1,hreg3);
                instr:=taicpu.op_reg_reg_reg(A_MOV,location.register,hreg3,location.register);
                instr.condition:=C_NEZ;
                current_asmdata.CurrAsmList.concat(instr);

                cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,def_cgsize(resultdef), location.register, location.register)
              end
            else
              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,def_cgsize(resultdef),1, location.register, location.register)
          end;
      end;

{*****************************************************************************
                               TXTENSAUNARYMINUSNODE
*****************************************************************************}

    function tcpuunaryminusnode.pass_1: tnode;
      var
        procname: string[31];
        fdef : tdef;
      begin
        Result:=nil;
        if (current_settings.fputype=fpu_soft) and
           (left.resultdef.typ=floatdef) then
          begin
            result:=nil;
            firstpass(left);
            expectloc:=LOC_REGISTER;
            exit;
          end;

        result:=nil;
        firstpass(left);
        if codegenerror then
          exit;

        expectloc:=LOC_REGISTER;
      end;

    procedure tcpuunaryminusnode.second_float;
      var
        ai : taicpu;
      begin
        secondpass(left);
        if (current_settings.fputype=fpu_soft) or (tfloatdef(left.resultdef).floattype<>s32real) or
          not(FPUXTENSA_SINGLE in fpu_capabilities[current_settings.fputype]) then
          begin
            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            if location.size in [OS_64,OS_S64,OS_F64] then
              begin
                location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
              end
            else
              location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);

            case location.size of
              OS_32:
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_32,tcgint($80000000),left.location.register,location.register);
              OS_64:
                begin
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_32,tcgint($80000000),left.location.registerhi,location.registerhi);
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.register64.reglo,location.register64.reglo);
                end;
            else
              internalerror(2014033102);
            end;
          end
        else
          begin
            if not(left.location.loc in [LOC_CFPUREGISTER,LOC_FPUREGISTER]) then
              hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,false);
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
            ai:=taicpu.op_reg_reg(A_NEG,location.register,left.location.register);
            ai.oppostfix := PF_S;
            current_asmdata.CurrAsmList.Concat(ai);
          end;
      end;


    function tcpushlshrnode.pass_1 : tnode;
      begin
        { the xtensa code generator can handle 64 bit shifts by constants directly }
        if is_constintnode(right) and is_64bit(resultdef) and
          (((nodetype=shln) and (tordconstnode(right).value>=0) and (tordconstnode(right).value<=16)) or
           ((nodetype=shrn) and (tordconstnode(right).value>0) and (tordconstnode(right).value<16)) or
           (tordconstnode(right).value=32)) then
          begin
            result:=nil;
            firstpass(left);
            firstpass(right);
            if codegenerror then
              exit;

            expectloc:=LOC_REGISTER;
          end
        else
          Result:=inherited pass_1;
      end;


    procedure tcpushlshrnode.second_64bit;
      var
        op: topcg;
        opsize: TCgSize;
        opdef: tdef;
        shiftval: longint;
        hcountreg: TRegister;
      begin
        { determine operator }
        case nodetype of
          shln: op:=OP_SHL;
          shrn: op:=OP_SHR;
          else
            internalerror(2020082208);
        end;
        opsize:=left.location.size;
        opdef:=left.resultdef;

        if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) or
          { location_force_reg can be also used to change the size of a register }
          (left.location.size<>opsize) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,opdef,true);
        location_reset(location,LOC_REGISTER,opsize);
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
        location.registerhi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);

        { shifting by a constant directly coded: }
        if right.nodetype=ordconstn then
          begin
             { shl/shr must "wrap around", so use ... and 31 }
             { In TP, "byte/word shl 16 = 0", so no "and 15" in case of
               a 16 bit ALU }
             if tcgsize2size[opsize]<=4 then
               shiftval:=tordconstnode(right).value.uvalue and 31
             else
               shiftval:=tordconstnode(right).value.uvalue and 63;
             cg64.a_op64_const_reg_reg(current_asmdata.CurrAsmList,op,location.size,
               shiftval,left.location.register64,location.register64)
          end
        else
          begin
            internalerror(2020082204);
            { load right operators in a register - this
               is done since most target cpu which will use this
               node do not support a shift count in a mem. location (cec)
             }
             hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,sinttype,true);
             hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,opdef,right.location.register,left.location.register,location.register);
          end;
        { shl/shr nodes return the same type as left, which can be different
          from opdef }
        if opdef<>resultdef then
          begin
            hcountreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
            hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,opdef,resultdef,location.register,hcountreg);
            location.register:=hcountreg;
          end;
      end;

begin
  cmoddivnode:=tcpumoddivnode;
  cnotnode:=tcpunotnode;
  cunaryminusnode:=tcpuunaryminusnode;
  cshlshrnode:=tcpushlshrnode;
end.
