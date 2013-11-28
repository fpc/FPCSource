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

       { tjvmaddnode }

       tjvmaddnode = class(tcgaddnode)
          function pass_1: tnode;override;
       protected
          function jvm_first_addset: tnode;

          procedure second_generic_compare(unsigned: boolean);

          procedure pass_left_right;override;
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmp64bit;override;
          procedure second_add64bit; override;
          procedure second_cmpordinal;override;
       end;

  implementation

    uses
      systems,
      cutils,verbose,constexp,globtype,
      symconst,symtable,symdef,
      paramgr,procinfo,pass_1,
      aasmtai,aasmdata,aasmcpu,defutil,
      hlcgobj,hlcgcpu,cgutils,
      cpupara,
      nbas,ncon,nset,nadd,ncal,ncnv,ninl,nld,nmat,nmem,
      njvmcon,
      cgobj;

{*****************************************************************************
                               tjvmaddnode
*****************************************************************************}

    function tjvmaddnode.pass_1: tnode;
      begin
        { special handling for enums: they're classes in the JVM -> get their
          ordinal value to compare them (do before calling inherited pass_1,
          because pass_1 will convert enum constants from ordinals into class
          instances) }
        if (left.resultdef.typ=enumdef) and
           (right.resultdef.typ=enumdef) then
          begin
            { enums can only be compared at this stage (add/sub is only allowed
              in constant expressions) }
            if not is_boolean(resultdef) then
              internalerror(2011062603);
            inserttypeconv_explicit(left,s32inttype);
            inserttypeconv_explicit(right,s32inttype);
          end;
        { special handling for sets: all sets are JUBitSet/JUEnumSet on the JVM
          target to ease interoperability with Java code }
        if left.resultdef.typ=setdef then
          begin
            result:=jvm_first_addset;
            exit;
          end;
        { special handling for comparing a dynamic array to nil: dynamic arrays
          can be empty on the jvm target and not be different from nil at the
          same time (array of 0 elements) -> change into length check }
        if is_dynamic_array(left.resultdef) and
           (right.nodetype=niln) then
          begin
           result:=caddnode.create(nodetype,cinlinenode.create(in_length_x,false,left),genintconstnode(0));
           left:=nil;
           exit;
          end;
        if is_dynamic_array(right.resultdef) and
           (left.nodetype=niln) then
          begin
            result:=caddnode.create(nodetype,cinlinenode.create(in_length_x,false,right),genintconstnode(0));
            right:=nil;
            exit;
          end;
        result:=inherited pass_1;
        if expectloc=LOC_FLAGS then
          expectloc:=LOC_JUMP;
      end;


    function tjvmaddnode.jvm_first_addset: tnode;

      procedure call_set_helper_paras(const n : string; isenum: boolean; paras: tcallparanode);
        var
          block: tblocknode;
          stat: tstatementnode;
          temp: ttempcreatenode;
        begin
          result:=ccallnode.createinternmethod(left,'CLONE',nil);
          if isenum then
            inserttypeconv_explicit(result,java_juenumset)
          else
            inserttypeconv_explicit(result,java_jubitset);
          if isenum then
            begin
              { all enum instance methods return a boolean, while we are
                interested in the resulting set }
              block:=internalstatements(stat);
              temp:=ctempcreatenode.create(java_juenumset,4,tt_persistent,true);
              addstatement(stat,temp);
              addstatement(stat,cassignmentnode.create(
                ctemprefnode.create(temp),result));
              addstatement(stat,ccallnode.createinternmethod(
                ctemprefnode.create(temp),n,paras));
              addstatement(stat,ctempdeletenode.create_normal_temp(temp));
              addstatement(stat,ctemprefnode.create(temp));
              result:=block;
            end
          else
            result:=ccallnode.createinternmethod(result,n,paras);
        end;

      procedure call_set_helper(const n: string; isenum: boolean);
        begin
          call_set_helper_paras(n,isenum,ccallparanode.create(right,nil));
        end;

      var
        procname: string;
        tmpn: tnode;
        paras: tcallparanode;
        isenum: boolean;
      begin
        isenum:=
          (assigned(tsetdef(left.resultdef).elementdef) and
           (tsetdef(left.resultdef).elementdef.typ=enumdef)) or
          ((right.nodetype=setelementn) and
           (tsetelementnode(right).left.resultdef.typ=enumdef)) or
          ((right.resultdef.typ=setdef) and
           assigned(tsetdef(right.resultdef).elementdef) and
           (tsetdef(right.resultdef).elementdef.typ=enumdef));
        { don't destroy optimization opportunity }
        if not((nodetype=addn) and
               (right.nodetype=setelementn) and
               is_emptyset(left)) then
          begin
            left:=caddrnode.create_internal(left);
            include(left.flags,nf_typedaddr);
            if isenum then
              begin
                inserttypeconv_explicit(left,java_juenumset);
                if right.resultdef.typ=setdef then
                  begin
                    right:=caddrnode.create_internal(right);
                    include(right.flags,nf_typedaddr);
                    inserttypeconv_explicit(right,java_juenumset);
                  end;
              end
            else
              begin
                inserttypeconv_explicit(left,java_jubitset);
                if right.resultdef.typ=setdef then
                  begin
                    right:=caddrnode.create_internal(right);
                    include(right.flags,nf_typedaddr);
                    inserttypeconv_explicit(right,java_jubitset);
                  end;
              end;
          end
        else
          tjvmsetconstnode(left).setconsttype:=sct_notransform;
        firstpass(left);
        firstpass(right);
        case nodetype of
          equaln,unequaln,lten,gten:
            begin
              case nodetype of
                equaln,unequaln:
                  procname:='EQUALS';
                lten,gten:
                  begin
                    { (left <= right) = (right >= left) }
                    if nodetype=lten then
                      begin
                        tmpn:=left;
                        left:=right;
                        right:=tmpn;
                      end;
                      procname:='CONTAINSALL'
                    end;
                end;
              result:=ccallnode.createinternmethod(left,procname,ccallparanode.create(right,nil));
              { for an unequaln, we have to negate the result of equals }
              if nodetype=unequaln then
                result:=cnotnode.create(result);
            end;
          addn:
            begin
              { optimize first loading of a set }
              if (right.nodetype=setelementn) and
                  is_emptyset(left) then
                begin
                  paras:=nil;
                  procname:='OF';
                  if isenum then
                    begin
                      inserttypeconv_explicit(tsetelementnode(right).left,tenumdef(tsetelementnode(right).left.resultdef).getbasedef.classdef);
                      result:=cloadvmtaddrnode.create(ctypenode.create(java_juenumset));
                    end
                  else
                    begin
                      { for boolean, char, etc }
                      inserttypeconv_explicit(tsetelementnode(right).left,s32inttype);
                      result:=cloadvmtaddrnode.create(ctypenode.create(java_jubitset));
                    end;
                  paras:=ccallparanode.create(tsetelementnode(right).left,nil);
                  tsetelementnode(right).left:=nil;
                  if assigned(tsetelementnode(right).right) then
                    begin
                      procname:='RANGE';
                      if isenum then
                        begin
                          inserttypeconv_explicit(tsetelementnode(right).right,tenumdef(tsetelementnode(right).right.resultdef).getbasedef.classdef);
                        end
                      else
                        begin
                          inserttypeconv_explicit(tsetelementnode(right).right,s32inttype);
                        end;
                      paras:=ccallparanode.create(tsetelementnode(right).right,paras);
                      tsetelementnode(right).right:=nil;
                    end;
                  right.free;
                  result:=ccallnode.createinternmethod(result,procname,paras)
                end
              else
                begin
                  if right.nodetype=setelementn then
                    begin
                      paras:=nil;
                      { get a copy of left to add to }
                      procname:='ADD';
                      if isenum then
                        begin
                          inserttypeconv_explicit(tsetelementnode(right).left,tenumdef(tsetelementnode(right).left.resultdef).getbasedef.classdef);
                        end
                      else
                        begin
                          { for boolean, char, etc }
                          inserttypeconv_explicit(tsetelementnode(right).left,s32inttype);
                        end;
                      paras:=ccallparanode.create(tsetelementnode(right).left,paras);
                      tsetelementnode(right).left:=nil;
                      if assigned(tsetelementnode(right).right) then
                        begin
                          procname:='ADDALL';
                          { create a set containing the range via the class
                            factory method, then add all of its elements }
                          if isenum then
                            begin
                              inserttypeconv_explicit(tsetelementnode(right).right,tenumdef(tsetelementnode(right).right.resultdef).getbasedef.classdef);
                              tmpn:=cloadvmtaddrnode.create(ctypenode.create(java_juenumset));
                            end
                          else
                            begin
                              inserttypeconv_explicit(tsetelementnode(right).right,s32inttype);
                              tmpn:=cloadvmtaddrnode.create(ctypenode.create(java_jubitset));
                            end;
                          paras:=ccallparanode.create(ccallnode.createinternmethod(tmpn,'RANGE',ccallparanode.create(tsetelementnode(right).right,paras)),nil);
                          tsetelementnode(right).right:=nil;
                        end;
                      call_set_helper_paras(procname,isenum,paras);
                    end
                  else
                    call_set_helper('ADDALL',isenum)
                end
            end;
          subn:
            call_set_helper('REMOVEALL',isenum);
          symdifn:
            if isenum then
              begin
                { "s1 xor s2" is the same as "(s1 + s2) - (s1 * s2)"
                  -> call helper to prevent double evaluations }
                result:=ccallnode.createintern('fpc_enumset_symdif',
                  ccallparanode.create(right,ccallparanode.create(left,nil)));
                left:=nil;
                right:=nil;
              end
            else
              call_set_helper('SYMDIF',isenum);
          muln:
            call_set_helper('RETAINALL',isenum)
          else
            internalerror(2011062807);
        end;
        { convert helper result back to original set type for further expression
          evaluation }
        if not is_boolean(resultdef) then
          begin
            inserttypeconv_explicit(result,getpointerdef(resultdef));
            result:=cderefnode.create(result);
          end;
        { left and right are reused as parameters }
        left:=nil;
        right:=nil;
      end;


    procedure tjvmaddnode.second_generic_compare(unsigned: boolean);
      var
        cmpop: TOpCmp;
      begin
        pass_left_right;
        { swap the operands to make it easier for the optimizer to optimize
          the operand stack slot reloading in case both are in a register }
        if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
           (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          swapleftright;
        cmpop:=cmpnode2topcmp(unsigned);
        if (nf_swapped in flags) then
          cmpop:=swap_opcmp(cmpop);
        location_reset(location,LOC_JUMP,OS_NO);

        if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
          hlcg.a_cmp_loc_reg_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location,left.location.register,current_procinfo.CurrTrueLabel)
        else case right.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            hlcg.a_cmp_reg_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.register,left.location,current_procinfo.CurrTrueLabel);
          LOC_REFERENCE,LOC_CREFERENCE:
            hlcg.a_cmp_ref_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.reference,left.location,current_procinfo.CurrTrueLabel);
          LOC_CONSTANT:
            hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.value,left.location,current_procinfo.CurrTrueLabel);
          else
            internalerror(2011010413);
        end;
        hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
      end;

    procedure tjvmaddnode.pass_left_right;
      begin
        swapleftright;
        inherited pass_left_right;
      end;


    procedure tjvmaddnode.second_addfloat;
      var
        op : TAsmOp;
        commutative : boolean;
      begin
        pass_left_right;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);

        commutative:=false;
        case nodetype of
          addn :
            begin
              if location.size=OS_F64 then
                op:=a_dadd
              else
                op:=a_fadd;
              commutative:=true;
            end;
          muln :
            begin
              if location.size=OS_F64 then
                op:=a_dmul
              else
                op:=a_fmul;
              commutative:=true;
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

        { swap the operands to make it easier for the optimizer to optimize
          the operand stack slot reloading (non-commutative operations must
          always be in the correct order though) }
        if (commutative and
            (left.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
            (right.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER])) or
           (not commutative and
            (nf_swapped in flags)) then
          swapleftright;

        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);

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
        cmpop: TOpCmp;
      begin
        pass_left_right;
        { swap the operands to make it easier for the optimizer to optimize
          the operand stack slot reloading in case both are in a register }
        if (left.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
           (right.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
          swapleftright;
        cmpop:=cmpnode2topcmp(false);
        if (nf_swapped in flags) then
          cmpop:=swap_opcmp(cmpop);
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

        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcmp2if[cmpop],current_procinfo.CurrTrueLabel));
        thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        hlcg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
      end;


    procedure tjvmaddnode.second_cmpboolean;
      begin
        second_generic_compare(true);
      end;


    procedure tjvmaddnode.second_cmp64bit;
      begin
        second_generic_compare(not is_signed(left.resultdef));
      end;


    procedure tjvmaddnode.second_add64bit;
      begin
        second_opordinal;
      end;


    procedure tjvmaddnode.second_cmpordinal;
      begin
        second_generic_compare(not is_signed(left.resultdef));
      end;

begin
  caddnode:=tjvmaddnode;
end.
