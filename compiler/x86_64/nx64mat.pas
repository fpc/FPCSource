{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86-64 assembler for math nodes

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
unit nx64mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,nx86mat;

    type
      tx8664moddivnode = class(tmoddivnode)
         procedure pass_generate_code;override;
      end;

      tx8664shlshrnode = class(tshlshrnode)
         procedure pass_generate_code;override;
      end;

      tx8664unaryminusnode = class(tx86unaryminusnode)
      end;

      tx8664notnode = class(tx86notnode)
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmdata,defutil,
      pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      cgbase,cgutils,cga,cgobj,hlcgobj,cgx86,
      ncgutil;

{*****************************************************************************
                             TX8664MODDIVNODE
*****************************************************************************}

    procedure tx8664moddivnode.pass_generate_code;
      var
        hreg1,hreg2,rega,regd:Tregister;
        power:longint;
        op:Tasmop;
        cgsize:TCgSize;
        opsize:topsize;
      begin
        secondpass(left);
        if codegenerror then
          exit;
        secondpass(right);
        if codegenerror then
          exit;

        { put numerator in register }
        cgsize:=def_cgsize(resultdef);
        opsize:=TCGSize2OpSize[cgsize];
        case cgsize of
          OS_S64,OS_64:
            begin
              rega:=NR_RAX;
              regd:=NR_RDX;
            end;
          OS_S32,OS_32:
            begin
              rega:=NR_EAX;
              regd:=NR_EDX;
            end;
          else
            internalerror(2013102702);
        end;

        location_reset(location,LOC_REGISTER,cgsize);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
        hreg1:=left.location.register;

        if (nodetype=divn) and (right.nodetype=ordconstn) and
           ispowerof2(int64(tordconstnode(right).value),power) then
          begin
            { for signed numbers, the numerator must be adjusted before the
              shift instruction, but not wih unsigned numbers! Otherwise,
              "Cardinal($ffffffff) div 16" overflows! (JM) }
            if is_signed(left.resultdef) Then
              begin
                  { use a sequence without jumps, saw this in
                    comp.compilers (JM) }
                  { no jumps, but more operations }
                  hreg2:=cg.getintregister(current_asmdata.CurrAsmList,cgsize);
                  emit_reg_reg(A_MOV,opsize,hreg1,hreg2);
                  {If the left value is signed, hreg2=$ffffffff, otherwise 0.}
                  emit_const_reg(A_SAR,opsize,63,hreg2);
                  {If signed, hreg2=right value-1, otherwise 0.}
                  { (don't use emit_const_reg, because if value>high(longint)
                     then it must first be loaded into a register) }
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,cgsize,tordconstnode(right).value-1,hreg2);
                  { add to the left value }
                  emit_reg_reg(A_ADD,opsize,hreg2,hreg1);
                  { do the shift }
                  emit_const_reg(A_SAR,opsize,power,hreg1);
              end
            else
              emit_const_reg(A_SHR,opsize,power,hreg1);
            location.register:=hreg1;
          end
        else
          begin
            {Bring denominator to a register.}
            cg.getcpuregister(current_asmdata.CurrAsmList,rega);
            emit_reg_reg(A_MOV,opsize,hreg1,rega);
            cg.getcpuregister(current_asmdata.CurrAsmList,regd);
            {Sign extension depends on the left type.}
            if is_signed(left.resultdef) then
              case left.resultdef.size of
                8:
                  emit_none(A_CQO,S_NO);
                4:
                  emit_none(A_CDQ,S_NO);
                else
                  internalerror(2013102701);
              end
            else
              emit_reg_reg(A_XOR,opsize,regd,regd);

            {Division depends on the right type.}
            if is_signed(right.resultdef) then
              op:=A_IDIV
            else
              op:=A_DIV;

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              emit_ref(op,opsize,right.location.reference)
            else if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_reg(op,opsize,right.location.register)
            else
              begin
                hreg1:=cg.getintregister(current_asmdata.CurrAsmList,right.location.size);
                hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,right.resultdef,right.location,hreg1);
                emit_reg(op,opsize,hreg1);
              end;

            { Copy the result into a new register. Release R/EAX & R/EDX.}
            cg.ungetcpuregister(current_asmdata.CurrAsmList,regd);
            cg.ungetcpuregister(current_asmdata.CurrAsmList,rega);
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,cgsize);
            if nodetype=divn then
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,cgsize,cgsize,rega,location.register)
            else
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,cgsize,cgsize,regd,location.register);
          end;
      end;


{*****************************************************************************
                             TX8664SHLRSHRNODE
*****************************************************************************}


    procedure tx8664shlshrnode.pass_generate_code;
      var
        op : Tasmop;
        opsize : tcgsize;
        mask : aint;
      begin
        secondpass(left);
        secondpass(right);

        { determine operator }
        if nodetype=shln then
          op:=A_SHL
        else
          op:=A_SHR;

        { special treatment of 32bit values for backwards compatibility }
        { mul optimizations require to keep the sign (FK) }
        if left.resultdef.size<=4 then
          begin
            if is_signed(left.resultdef) then
              opsize:=OS_S32
            else
              opsize:=OS_32;
            mask:=31;
          end
        else
          begin
            if is_signed(left.resultdef) then
              opsize:=OS_S64
            else
              opsize:=OS_64;
            mask:=63;
          end;

        { load left operators in a register }
        location_copy(location,left.location);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,cgsize_orddef(opsize),false);

        { shifting by a constant directly coded: }
        if (right.nodetype=ordconstn) then
          emit_const_reg(op,tcgsize2opsize[opsize],tordconstnode(right).value and mask,location.register)
        else
          begin
            { load right operators in a RCX }
            cg.getcpuregister(current_asmdata.CurrAsmList,NR_RCX);
            hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,osuinttype,right.location,NR_RCX);

            { right operand is in ECX }
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_RCX);
            emit_reg_reg(op,tcgsize2opsize[opsize],NR_CL,location.register);
          end;
      end;


begin
   cunaryminusnode:=tx8664unaryminusnode;
   cmoddivnode:=tx8664moddivnode;
   cshlshrnode:=tx8664shlshrnode;
   cnotnode:=tx8664notnode;
end.
