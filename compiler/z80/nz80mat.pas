{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Z80 assembler for math nodes

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
unit nz80mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type

      { tz80notnode }

      tz80notnode = class(tcgnotnode)
      protected
        procedure second_boolean;override;
      end;


implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,
      ncgutil,cgobj,cgutils,
      hlcgobj;

{*****************************************************************************
                                tz80notnode
*****************************************************************************}


    procedure tz80notnode.second_boolean;
      var
        i: Integer;
      begin
        if not handle_locjump then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            secondpass(left);

            if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=left.location.resflags;
                  inverse_flags(location.resflags);
                end;
(*              LOC_CREFERENCE,
              LOC_REFERENCE:
                begin
 {$if defined(cpu32bitalu)}
                  if is_64bit(resultdef) then
                    begin
                      hreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_32);
                      tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,left.location.reference);
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.reference,hreg);
                      inc(left.location.reference.offset,4);
                      cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.reference,hreg);
                    end
                  else
 {$elseif defined(cpu16bitalu)}
                  if is_64bit(resultdef) then
                    begin
                      hreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_16);
                      tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,left.location.reference);
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,left.location.reference,hreg);
                      inc(left.location.reference.offset,2);
                      cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_16,left.location.reference,hreg);
                      inc(left.location.reference.offset,2);
                      cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_16,left.location.reference,hreg);
                      inc(left.location.reference.offset,2);
                      cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_16,left.location.reference,hreg);
                    end
                  else if is_32bit(resultdef) then
                    begin
                      hreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_16);
                      tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,left.location.reference);
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,left.location.reference,hreg);
                      inc(left.location.reference.offset,2);
                      cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_16,left.location.reference,hreg);
                    end
                  else
 {$endif}
                    emit_const_ref(A_CMP, TCGSize2Opsize[opsize], 0, left.location.reference);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;*)
              LOC_CONSTANT,
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_SUBSETREG,
              LOC_CSUBSETREG,
              LOC_SUBSETREF,
              LOC_CSUBSETREF :
                begin
                  if tcgsize2size[def_cgsize(left.resultdef)]<>tcgsize2size[def_cgsize(resultdef)] then
                    internalerror(2020042209);
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
                  if tcgsize2size[def_cgsize(left.resultdef)]=1 then
                    begin
                      cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,left.location.register,NR_A);
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_OR,NR_A,NR_A));
                      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                    end
                  else
                    begin
                      cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,left.location.register,NR_A);
                      for i:=1 to tcgsize2size[def_cgsize(left.resultdef)]-1 do
                        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_OR,NR_A,cg.GetOffsetReg64(left.location.register,left.location.registerhi,i)));
                      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                    end;
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
              else
                internalerror(2020042208);
            end;
          end;
      end;


begin
   cnotnode:=tz80notnode;
end.
