{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate JVM code for math nodes

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
unit njvmmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tjvmmoddivnode = class(tmoddivnode)
         procedure pass_generate_code;override;
      end;

      tjvmshlshrnode = class(tshlshrnode)
         procedure pass_generate_code;override;
      end;

      tjvmnotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgobj,pass_2,procinfo,
      ncon,
      cpubase,
      hlcgobj,hlcgcpu,cgutils;

{*****************************************************************************
                             tjvmmoddivnode
*****************************************************************************}

    procedure tjvmmoddivnode.pass_generate_code;
      var
        op: topcg;
        isu32int: boolean;
      begin
         secondpass(left);
         secondpass(right);
         location_reset(location,LOC_REGISTER,left.location.size);
         location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);


        if nodetype=divn then
          begin
            { TODO: overflow checking in case of high(longint) or high(int64) div -1 }
            thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
            if is_signed(resultdef) then
              op:=OP_IDIV
            else
              op:=OP_DIV;
            thlcgjvm(hlcg).a_op_loc_stack(current_asmdata.CurrAsmList,op,right.resultdef,right.location)
          end
        else
          begin
            { must be handled via a helper }
            if torddef(resultdef).ordtype=u64bit then
              internalerror(2011010416);
            if (torddef(resultdef).ordtype<>u32bit) then
              begin
                isu32int:=false;
                thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
                thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);
              end
            else
              begin
                isu32int:=true;
                if left.location.loc=LOC_CONSTANT then
                  thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s64inttype,left.location.value,R_INTREGISTER)
                else
                  begin
                    thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
                    thlcgjvm(hlcg).resize_stack_int_val(current_asmdata.CurrAsmList,OS_32,OS_S64,false);
                  end;
                if right.location.loc=LOC_CONSTANT then
                  thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s64inttype,right.location.value,R_INTREGISTER)
                else
                  begin
                    thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);
                    thlcgjvm(hlcg).resize_stack_int_val(current_asmdata.CurrAsmList,OS_32,OS_S64,false);
                  end;
              end;
            if isu32int or
               (torddef(resultdef).ordtype=s64bit) then
              begin
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_lrem));
                thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
              end
            else
              begin
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_irem));
                thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
              end;
            if isu32int then
              thlcgjvm(hlcg).resize_stack_int_val(current_asmdata.CurrAsmList,OS_S64,OS_32,false);
          end;
         thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


{*****************************************************************************
                             tjvmshlshrnode
*****************************************************************************}

    procedure tjvmshlshrnode.pass_generate_code;
      var
        op : topcg;
      begin
        secondpass(left);
        secondpass(right);
        location_reset(location,LOC_REGISTER,left.location.size);
        location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);

        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        if nodetype=shln then
          op:=OP_SHL
        else
          op:=OP_SHR;
        thlcgjvm(hlcg).a_op_loc_stack(current_asmdata.CurrAsmList,op,right.resultdef,right.location);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


{*****************************************************************************
                               tjvmnotnode
*****************************************************************************}

    procedure tjvmnotnode.second_boolean;
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
            hlcg.maketojumpbool(current_asmdata.CurrAsmList,left);
            hl:=current_procinfo.CurrTrueLabel;
            current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
            current_procinfo.CurrFalseLabel:=hl;
            location.loc:=LOC_JUMP;
          end
        else
          begin
            secondpass(left);
            case left.location.loc of
              LOC_REGISTER, LOC_CREGISTER,
              LOC_REFERENCE, LOC_CREFERENCE:
                begin
                  location_reset(location,LOC_REGISTER,left.location.size);
                  location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
                  thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
                  thlcgjvm(hlcg).a_op_reg_stack(current_asmdata.CurrAsmList,OP_NOT,left.resultdef,NR_NO);
                  thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
                end;
              else
                internalerror(2011010417);
            end;
          end;
      end;


begin
   cmoddivnode:=tjvmmoddivnode;
   cshlshrnode:=tjvmshlshrnode;
   cnotnode:=tjvmnotnode;
end.
