{
    Copyright (c) 2000-2011 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the JVM

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
unit njvmadd;

{$i fpcdefs.inc}

interface

    uses
       cgbase,
       node,ncgadd,cpubase;

    type

       { njvmadd }

       tjvmaddnode = class(tcgaddnode)
       protected
          function pass_1: tnode;override;

          function cmpnode2signedtopcmp: TOpCmp;

          procedure second_generic_compare;

          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
          procedure second_cmpordinal;override;
       end;

  implementation

    uses
      systems,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      hlcgobj,hlcgcpu,cgutils,
      cpupara,
      ncon,nset,nadd,
      cgobj;

{*****************************************************************************
                               tjvmaddnode
*****************************************************************************}

    function tjvmaddnode.pass_1: tnode;
      begin
        result:=inherited pass_1;
        if expectloc=LOC_FLAGS then
          expectloc:=LOC_JUMP;
      end;


    function tjvmaddnode.cmpnode2signedtopcmp: TOpCmp;
      begin
        case nodetype of
          gtn: result:=OC_GT;
          gten: result:=OC_GTE;
          ltn: result:=OC_LT;
          lten: result:=OC_LTE;
          equaln: result:=OC_EQ;
          unequaln: result:=OC_NE;
          else
            internalerror(2011010412);
        end;
      end;


    procedure tjvmaddnode.second_generic_compare;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;
        location_reset(location,LOC_JUMP,OS_NO);

        if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
          hlcg.a_cmp_loc_reg_label(current_asmdata.CurrAsmList,left.resultdef,cmpnode2signedtopcmp,left.location,right.location.register,current_procinfo.CurrTrueLabel)
        else case left.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            hlcg.a_cmp_reg_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpnode2signedtopcmp,left.location.register,right.location,current_procinfo.CurrTrueLabel);
          LOC_REFERENCE,LOC_CREFERENCE:
            hlcg.a_cmp_ref_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpnode2signedtopcmp,left.location.reference,right.location,current_procinfo.CurrTrueLabel);
          LOC_CONSTANT:
            hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpnode2signedtopcmp,left.location.value,right.location,current_procinfo.CurrTrueLabel);
          else
            internalerror(2011010413);
        end;
        hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
      end;


    procedure tjvmaddnode.second_addfloat;
      var
        op : TAsmOp;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);

        case nodetype of
          addn :
            begin
              if location.size=OS_F64 then
                op:=a_dadd
              else
                op:=a_fadd;
            end;
          muln :
            begin
              if location.size=OS_F64 then
                op:=a_dmul
              else
                op:=a_fmul;
            end;
          subn :
            begin
              if location.size=OS_F64 then
                op:=a_dsub
              else
                op:=a_fsub;
            end;
          slashn :
            begin
              if location.size=OS_F64 then
                op:=a_ddiv
              else
                op:=a_fdiv;
            end;
          else
            internalerror(2011010402);
        end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(op));
        thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1+ord(location.size=OS_F64));
        { could be optimized in the future by keeping the results on the stack,
          if we add code to swap the operands when necessary (a_swap for
          singles, store/load/load for doubles since there is no swap for
          2-slot elements -- also adjust expectloc in that case! }
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


    procedure tjvmaddnode.second_cmpfloat;
      var
        op : tasmop;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;
        location_reset(location,LOC_JUMP,OS_NO);

        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);

        { compares two floating point values and puts 1/0/-1 on stack depending
          on whether value1 >/=/< value2 }
        if left.location.size=OS_F64 then
          { make sure that comparisons with NaNs always return false for </> }
          if nodetype in [ltn,lten] then
            op:=a_dcmpg
          else
            op:=a_dcmpl
        else if nodetype in [ltn,lten] then
          op:=a_fcmpg
        else
          op:=a_fcmpl;
        current_asmdata.CurrAsmList.concat(taicpu.op_none(op));
        thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,(1+ord(left.location.size=OS_F64))*2-1);

        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcmp2if[cmpnode2signedtopcmp],current_procinfo.CurrTrueLabel));
        thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
      end;


    procedure tjvmaddnode.second_cmpboolean;
      begin
        second_generic_compare;
      end;


    procedure tjvmaddnode.second_cmpsmallset;
      begin
        if (nodetype in [equaln,unequaln]) then
          begin
            second_generic_compare;
            exit;
          end;
        case nodetype of
          lten,gten:
            begin
              pass_left_right;
              If (not(nf_swapped in flags) and
                  (nodetype=lten)) or
                 ((nf_swapped in flags) and
                  (nodetype=gten)) then
                swapleftright;
              location_reset(location,LOC_JUMP,OS_NO);
              // now we have to check whether left >= right:
              // (right and not(left)=0)
              thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
              thlcgjvm(hlcg).a_op_reg_stack(current_asmdata.CurrAsmList,OP_NOT,left.resultdef,NR_NO);
              thlcgjvm(hlcg).a_op_loc_stack(current_asmdata.CurrAsmList,OP_AND,right.resultdef,right.location);
              current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_ifeq,current_procinfo.CurrTrueLabel));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
            end;
          else
            internalerror(2011010414);
        end;
      end;


    procedure tjvmaddnode.second_cmp64bit;
      begin
        second_generic_compare;
      end;


    procedure tjvmaddnode.second_cmpordinal;
      begin
        second_generic_compare;
      end;

begin
  caddnode:=tjvmaddnode;
end.
