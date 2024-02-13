{
    Copyright (c) 2014 Jonas Maebe

    Code generation for add nodes on AArch64

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
unit ncpuadd;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd,cpubase;

    type
       taarch64addnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
          function  GetFPUResFlags:TResFlags;
       protected
          function use_fma : boolean;override;
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmpsmallset;override;
          procedure second_cmpordinal;override;
          procedure second_addordinal;override;
          procedure second_add64bit; override;
          procedure second_cmp64bit; override;
       public
          function use_generic_mul32to64: boolean; override;
          function pass_1 : tnode;override;
       end;

  implementation

    uses
      systems,symconst,symtype,symdef,
      globals,globtype,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,cgcpu,cgutils,
      cpupara,
      ncon,nset,nadd,nmat,
      hlcgobj, ncgutil,cgobj,pass_2;

{*****************************************************************************
                               taarch64addnode
*****************************************************************************}

    function taarch64addnode.use_fma : boolean;
      begin
        Result:=true;
      end;


    function taarch64addnode.GetResFlags(unsigned:Boolean):TResFlags;
      begin
        case NodeType of
          equaln:
            GetResFlags:=F_EQ;
          unequaln:
            GetResFlags:=F_NE;
          else
            if not(unsigned) then
              begin
                if nf_swapped in flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_GT;
                    lten:
                      GetResFlags:=F_GE;
                    gtn:
                      GetResFlags:=F_LT;
                    gten:
                      GetResFlags:=F_LE;
                    else
                      internalerror(2014082010);
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_LT;
                    lten:
                      GetResFlags:=F_LE;
                    gtn:
                      GetResFlags:=F_GT;
                    gten:
                      GetResFlags:=F_GE;
                    else
                      internalerror(2014082011);
                  end;
              end
            else
              begin
                if nf_swapped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_HI;
                    lten:
                      GetResFlags:=F_HS;
                    gtn:
                      GetResFlags:=F_LO;
                    gten:
                      GetResFlags:=F_LS;
                    else
                      internalerror(2014082012);
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_LO;
                    lten:
                      GetResFlags:=F_LS;
                    gtn:
                      GetResFlags:=F_HI;
                    gten:
                      GetResFlags:=F_HS;
                    else
                      internalerror(2014082013);
                  end;
              end;
        end;
      end;


    function taarch64addnode.GetFPUResFlags:TResFlags;
      begin
        case NodeType of
          equaln:
            result:=F_EQ;
          unequaln:
            result:=F_NE;
          else
            begin
              if nf_swapped in Flags then
                case NodeType of
                  ltn:
                    result:=F_GT;
                  lten:
                    result:=F_GE;
                  gtn:
                    result:=F_LO;
                  gten:
                    result:=F_LS;
                  else
                    internalerror(2014082014);
                end
              else
                case NodeType of
                  ltn:
                    result:=F_LO;
                  lten:
                    result:=F_LS;
                  gtn:
                    result:=F_GT;
                  gten:
                    result:=F_GE;
                  else
                    internalerror(2014082015);
                end;
            end;
        end;
      end;


    procedure taarch64addnode.second_addfloat;
      var
        op : TAsmOp;
      begin
        pass_left_right;
        if nf_swapped in flags then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,right.location,right.resultdef,true);

        location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
        location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);

        case nodetype of
          addn :
            begin
              op:=A_FADD;
            end;
          muln :
            begin
              op:=A_FMUL;
            end;
          subn :
            begin
              op:=A_FSUB;
            end;
          slashn :
            begin
              op:=A_FDIV;
            end;
          else
            internalerror(200306014);
        end;

        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
           location.register,left.location.register,right.location.register));
        cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
      end;


    procedure taarch64addnode.second_cmpfloat;
      begin
        pass_left_right;
        if nf_swapped in flags then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_mmregscalar(current_asmdata.CurrAsmList,right.location,right.resultdef,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getfpuresflags;

        { signalling compare so we can get exceptions }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCMPE,
             left.location.register,right.location.register));
        cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
      end;


    procedure taarch64addnode.second_cmpboolean;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        if right.location.loc=LOC_CONSTANT then
          begin
            if right.location.value>=0 then
              Tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,A_CMP,left.location.size,left.location.register,right.location.value,NR_XZR,NR_NO,false,false)
            else
              { avoid overflow if value=low(int64) }
{$push}{$r-}{$q-}
              Tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,A_CMN,left.location.size,left.location.register,-right.location.value,NR_XZR,NR_NO,false,false)
{$pop}
          end
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
      end;


    procedure taarch64addnode.second_cmpsmallset;
      var
        tmpreg : tregister;
        op: tasmop;
      begin
        pass_left_right;

        location_reset(location,LOC_FLAGS,OS_NO);

        force_reg_left_right(true,true);

        if right.location.loc=LOC_CONSTANT then
          begin
            { when doing a cmp/cmn on 32 bit, we care whether the *lower 32 bit*
              is a positive/negative value -> sign extend }
            if not(right.location.size in [OS_64,OS_S64]) then
              right.location.value:=longint(right.location.value);
            if right.location.value>=0 then
              op:=A_CMP
            else
              op:=A_CMN;
          end
        else
          { for DFA }
          op:=A_NONE;

        case nodetype of
          equaln,
          unequaln:
            begin
              if right.location.loc=LOC_CONSTANT then
                tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,op,def_cgsize(resultdef),left.location.register,abs(right.location.value),NR_XZR,NR_NO,false,false)
              else
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              location.resflags:=getresflags(true);
            end;
          lten,
          gten:
            begin
              if (not(nf_swapped in flags) and
                  (nodetype=lten)) or
                 ((nf_swapped in flags) and
                  (nodetype=gten)) then
                swapleftright;
              { we can't handle left as a constant yet }
              if left.location.loc=LOC_CONSTANT then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,left.location.size);
              if right.location.loc=LOC_CONSTANT then
                begin
                  hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,resultdef,right.location.value,left.location.register,tmpreg);
                  tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,op,def_cgsize(resultdef),tmpreg,abs(right.location.value),NR_XZR,NR_NO,false,false)
                end
              else
                begin
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_AND,tmpreg,left.location.register,right.location.register));
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,tmpreg,right.location.register));
                end;
              location.resflags:=F_EQ;
            end;
          else
            internalerror(2012042701);
        end;
      end;


    procedure taarch64addnode.second_cmpordinal;
      var
        unsigned : boolean;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        if right.location.loc = LOC_CONSTANT then
          begin
            if right.location.value>=0 then
              Tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,A_CMP,left.location.size,left.location.register,right.location.value,NR_XZR,NR_NO,false,false)
            else
{$push}{$r-}{$q-}
              Tcgaarch64(cg).handle_reg_imm12_reg(current_asmdata.CurrAsmList,A_CMN,left.location.size,left.location.register,-right.location.value,NR_XZR,NR_NO,false,false)
{$pop}
          end
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);
      end;


    procedure taarch64addnode.second_addordinal;
      const
        multops: array[boolean] of TAsmOp = (A_SMULL,A_UMULL);
      var
        unsigned: boolean;
        logical_not_op: TAsmOp;
      begin
        { 32x32->64 multiplication }
        if (nodetype=muln) and
           is_32bit(left.resultdef) and
           is_32bit(right.resultdef) and
           is_64bit(resultdef) then
          begin
            unsigned:=not(is_signed(left.resultdef)) or
                      not(is_signed(right.resultdef));
            pass_left_right;
            force_reg_left_right(true,true);
            { force_reg_left_right can leave right as a LOC_CONSTANT (we can't
              say "a constant register is okay, but an ordinal constant isn't) }
            if right.location.loc=LOC_CONSTANT then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(multops[unsigned],location.register,left.location.register,right.location.register));
            Exit;
          end
        else if (cs_opt_level2 in current_settings.optimizerswitches) then
          begin
            { Can we turn "x and (not y)" into an ANDN instruction instead? }
            if (nodetype in [andn, orn, xorn]) and
              (
                (
                  (left.nodetype = notn) and
                  (tnotnode(left).left.location.loc <> LOC_CONSTANT)
                ) or
                (
                  (right.nodetype = notn) and
                  (tnotnode(right).left.location.loc <> LOC_CONSTANT)
                )
              ) then
              begin
                { BIC only supports the second operand being inverted; however,
                  since we're dealing with ordinals, there won't be any Boolean
                  shortcutting, so we can safely swap the parameters }

                if (left.nodetype = notn) and (tnotnode(left).left.location.loc <> LOC_CONSTANT) then
                  { If the left node is "not" but is inverting a constant, then
                    the right node must also be a "not", but with a non-constant
                    input }
                  swapleftright;

                secondpass(left);

                { Skip the not node completely }
                Include(right.flags, nf_do_not_execute);
                secondpass(tnotnode(right).left);

                { allocate registers }
                if not (tnotnode(right).left.location.loc in [LOC_REGISTER, LOC_CREGISTER]) then
                  hlcg.location_force_reg(
                    current_asmdata.CurrAsmList,
                    tnotnode(right).left.location,
                    tnotnode(right).left.resultdef,
                    tnotnode(right).left.resultdef,
                    // tnotnode(right).resultdef, { In case the "not" node does some implicit typecasting }
                    false
                  );

                if not (left.location.loc in [LOC_REGISTER, LOC_CREGISTER]) then
                  hlcg.location_force_reg(
                    current_asmdata.CurrAsmList,
                    left.location,
                    left.resultdef,
                    left.resultdef,
                    false
                  );

                set_result_location_reg;

                case nodetype of
                  andn:
                    logical_not_op := A_BIC;
                  orn:
                    logical_not_op := A_ORN;
                  xorn:
                    logical_not_op := A_EON;
                  else
                    InternalError(2022102130);
                end;

                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(logical_not_op,location.register,left.location.register,tnotnode(right).left.location.register));

                { We have to make sure trailing bits are cut off for unsigned
                  extensions since it will be full of 1s, so do this by
                  downsizing the register from 32-bit to the target size }
                if (def_cgsize(resultdef) in [OS_8, OS_16]) then
                  hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,torddef(u32inttype),resultdef,self.location.register,self.location.register);

                { Overflow can't happen with bic/orn/eon }
                Exit;
              end;
          end;

        { Default behaviour }
        inherited second_addordinal;
      end;


    procedure taarch64addnode.second_add64bit;
      begin
        second_addordinal;
      end;


    procedure taarch64addnode.second_cmp64bit;
      begin
        second_cmpordinal;
      end;


    function taarch64addnode.use_generic_mul32to64: boolean;
      begin
        result:=false;
      end;

    function taarch64addnode.pass_1: tnode;
      begin
        Result:=inherited pass_1;
        { if the result is not nil, a new node has been generated and the current node will be discarted }
        if Result=nil then
          begin
            if left.resultdef.typ=floatdef then
              if needs_check_for_fpu_exceptions then
                Include(current_procinfo.flags,pi_do_call);
          end;
      end;


begin
  caddnode:=taarch64addnode;
end.
