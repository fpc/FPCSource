{
    Copyright (c) 2000-2006 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the LoongArch64

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
       node,nadd,ncgadd,cpubase;

    type
      tloongarch64addnode = class(tcgaddnode)
      private
        procedure Cmp(signed,is_smallset: boolean);
      protected
        procedure second_cmpsmallset;override;
        procedure second_cmpordinal;override;
        procedure second_cmp64bit; override;

        procedure second_addordinal; override;
        procedure second_add64bit; override;

        procedure pass_left_and_right;

        procedure second_addfloat;override;
        procedure second_cmpfloat;override;
      public
        function use_generic_mul32to64: boolean; override;
        function pass_1 : tnode;override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,
      cpupara,cgcpu,cgutils,procinfo,
      ncon,nset,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,hlcgobj;


    procedure tloongarch64addnode.Cmp(signed,is_smallset: boolean);
      var
        flabel,tlabel: tasmlabel;
        op, opi: TAsmOp;
        allow_constant : boolean;
      begin
        pass_left_right;

        allow_constant:=(not is_smallset) or not (nodetype in [lten,gten]);

        force_reg_left_right(true,allow_constant);

        if nf_swapped in flags then
          swapleftright;

        location_reset(location,LOC_REGISTER,OS_INT);
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);

        if signed then op:=A_SLT else op:=A_SLTU;
        if signed then opi:=A_SLTI else opi:=A_SLTUI;

        case nodetype of
          equaln:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 (not is_uimm12(right.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if right.location.loc=LOC_CONSTANT then
                if right.location.value = 0 then
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,left.location.register,location.register)
                else
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_XORI,location.register,left.location.register,right.location.value))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_XOR,location.register,left.location.register,right.location.register));
              current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTUI,location.register,location.register,1));
            end;
          unequaln:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 (not is_uimm12(right.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if right.location.loc=LOC_CONSTANT then
                if right.location.value = 0 then
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,left.location.register,location.register)
                else
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_XORI,location.register,left.location.register,right.location.value))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_XOR,location.register,left.location.register,right.location.register));
              current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_SLTU,location.register,NR_R0,location.register));
            end;
          ltn:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 (not is_simm12(right.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if right.location.loc=LOC_CONSTANT then
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(opi,location.register,left.location.register,right.location.value))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,right.location.register));
            end;
          gtn:
            begin
              if not (right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if (left.location.loc=LOC_CONSTANT) and
                 (not is_simm12(left.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if left.location.loc=LOC_CONSTANT then
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(opi,location.register,right.location.register,left.location.value))
              else
                current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,location.register,right.location.register,left.location.register));
            end;

          lten:
            begin
              if not (right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

              if (left.location.loc=LOC_CONSTANT) and
                 (not is_simm12(left.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
              if is_smallset then
                begin
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_ANDN,location.register,left.location.register,right.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTUI,location.register,location.register,1));
                end
              else
                begin
                  if left.location.loc=LOC_CONSTANT then
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(opi,location.register,right.location.register,left.location.value))
                  else
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,location.register,right.location.register,left.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_XORI,location.register,location.register,1));
                end;
            end;
          gten:
            begin
              if not (left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

              if (right.location.loc=LOC_CONSTANT) and
                 (not is_simm12(right.location.value)) then
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
              if is_smallset then
                begin
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_ANDN,location.register,right.location.register,left.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_SLTUI,location.register,location.register,1));
                end
              else
                begin
                   if right.location.loc=LOC_CONSTANT then
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(opi,location.register,left.location.register,right.location.value))
                  else
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,right.location.register));
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_const(A_XORI,location.register,location.register,1));
                end;
            end;
        else
          Internalerror(2022111946);
        end;
      end;


    { Smallset means the one all bits in another one. }
    procedure tloongarch64addnode.second_cmpsmallset;
      begin
        Cmp(false,true);
      end;


    procedure tloongarch64addnode.second_cmpordinal;
      var
        unsigned: Boolean;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        Cmp(not unsigned,false);
      end;


    procedure tloongarch64addnode.second_cmp64bit;
      var
        unsigned: Boolean;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        Cmp(not unsigned,false);
      end;


    procedure tloongarch64addnode.second_addordinal;
      const
        multops: array[boolean] of TAsmOp = (A_MULW_D_W,A_MULW_D_WU);
      var
        unsigned: boolean;
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
          end
        else
          inherited second_addordinal;
      end;


    procedure tloongarch64addnode.second_add64bit;
      begin
        second_addordinal;
      end;


    procedure tloongarch64addnode.pass_left_and_right;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;

        secondpass(left);
        secondpass(right);
      end;


    procedure tloongarch64addnode.second_addfloat;
      var
        op    : TAsmOp;
        cmpop,
        singleprec: boolean;
      begin
        pass_left_and_right;
        if (nf_swapped in flags) then
          swapleftright;

        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);

        cmpop:=false;
        singleprec:=tfloatdef(left.resultdef).floattype=s32real;
        case nodetype of
          addn :
            if singleprec then
              op:=A_FADD_S
            else
              op:=A_FADD_D;
          muln :
            if singleprec then
              op:=A_FMUL_S
            else
            op:=A_FMUL_D;
          subn :
            if singleprec then
              op:=A_FSUB_S
            else
              op:=A_FSUB_D;
          slashn :
            if singleprec then
              op:=A_FDIV_S
            else
             op:=A_FDIV_D;
          equaln:
            begin
              if singleprec then
                op:=A_FCMP_CEQ_S
              else
                op:=A_FCMP_CEQ_D;
              cmpop:=true;
            end;
          unequaln:
            begin
              if singleprec then
                op:=A_FCMP_CUNE_S
              else
                op:=A_FCMP_CUNE_D;
              cmpop:=true;
            end;
          ltn:
            begin
              if singleprec then
                op:=A_FCMP_SLT_S
              else
                op:=A_FCMP_SLT_D;
              cmpop:=true;
            end;
          lten:
            begin
              if singleprec then
                op:=A_FCMP_SLE_S
              else
                op:=A_FCMP_SLE_D;
              cmpop:=true;
            end;
          gtn:
            begin
              if singleprec then
                op:=A_FCMP_SGT_S
              else
                op:=A_FCMP_SGT_D;
              cmpop:=true;
            end;
          gten:
            begin
              if singleprec then
                op:=A_FCMP_SGE_S
              else
                op:=A_FCMP_SGE_D;
              cmpop:=true;
            end;
          else
            internalerror(2022111947);
        end;

        if cmpop then
          begin
            { TODO This should be like mips, but... }
            { location_reset(location, LOC_FLAGS, OS_NO); }
            { location.register := cg.getfpuregister(current_asmdata.CurrAsmList,location.size); }
            location_reset(location,LOC_REGISTER,OS_8);
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,NR_FCC0,left.location.register,right.location.register));
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOVCF2GR,location.register,NR_FCC0));
            cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
          end
        else
          begin
            location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
            location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,right.location.register));
            cg.maybe_check_for_fpu_exception(current_asmdata.CurrAsmList);
          end;
      end;

    procedure tloongarch64addnode.second_cmpfloat;
      begin
        second_addfloat;
      end;

    function tloongarch64addnode.use_generic_mul32to64: boolean;
      begin
        result:=false;
      end;

    function tloongarch64addnode.pass_1: tnode;
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
  caddnode := tloongarch64addnode;
end.
