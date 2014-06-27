{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the Motorola 680x0 family

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
unit n68kadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,ncgadd,cpubase,cgbase;


    type
       t68kaddnode = class(tcgaddnode)
       private
          function cmp64_lt(left_reg,right_reg:tregister64):tregister;
          function cmp64_le(left_reg,right_reg:tregister64):tregister;
          function cmp64_eq(left_reg,right_reg:tregister64):tregister;
          function cmp64_ne(left_reg,right_reg:tregister64):tregister;
          function cmp64_ltu(left_reg,right_reg:tregister64):tregister;
          function cmp64_leu(left_reg,right_reg:tregister64):tregister;

          function getresflags(unsigned: boolean) : tresflags;
          function getres64_register(unsigned:boolean;left_reg,right_reg:tregister64):tregister;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpordinal;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
       public
          function pass_1:tnode;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,symtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cpuinfo,pass_1,pass_2,regvars,
      cpupara,cgutils,procinfo,
      ncon,nset,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,hlcgobj,cg64f32;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function t68kaddnode.cmp64_lt(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64_1,labelcmp64_2 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        current_asmdata.getjumplabel(labelcmp64_1);
        current_asmdata.getjumplabel(labelcmp64_2);

        { check whether left_reg.reghi is less than right_reg.reghi }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,right_reg.reghi,left_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_LT,S_NO,labelcmp64_2));

        { are left_reg.reghi and right_reg.reghi equal? }
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64_1));

        { is left_reg.reglo less than right_reg.reglo? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,right_reg.reglo,left_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_2));

        current_asmdata.currasmlist.concat(Taicpu.op_sym(A_BRA,S_NO,labelcmp64_1));
        cg.a_label(current_asmdata.currasmlist,labelcmp64_2);

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64_1);
        result:=tmpreg;
      end;

    function t68kaddnode.cmp64_le(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64_1,labelcmp64_2 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        current_asmdata.getjumplabel(labelcmp64_1);
        current_asmdata.getjumplabel(labelcmp64_2);

        { check whether right_reg.reghi is less than left_reg.reghi }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reghi,right_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_LT,S_NO,labelcmp64_1));

        { are left_reg.reghi and right_reg.reghi equal? }
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64_2));

        { is right_reg.reglo less than left_reg.reglo? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reglo,right_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_1));

        cg.a_label(current_asmdata.currasmlist,labelcmp64_2);

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64_1);
        result:=tmpreg;
      end;

    function t68kaddnode.cmp64_eq(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);
        current_asmdata.getjumplabel(labelcmp64);

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        { is the high order longword equal? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reghi,right_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64));

        { is the low order longword equal? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reglo,right_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64));

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64);
        result:=tmpreg;
      end;

    function t68kaddnode.cmp64_ne(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);
        current_asmdata.getjumplabel(labelcmp64);

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        { is the high order longword equal? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reghi,right_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64));

        { is the low order longword equal? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reglo,right_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64));

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64);
        result:=tmpreg;
      end;

    function t68kaddnode.cmp64_ltu(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64_1,labelcmp64_2 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        current_asmdata.getjumplabel(labelcmp64_1);
        current_asmdata.getjumplabel(labelcmp64_2);

        { check whether left_reg.reghi is less than right_reg.reghi }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,right_reg.reghi,left_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_2));

        { are left_reg.reghi and right_reg.reghi equal? }
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64_1));

        { is left_reg.reglo less than right_reg.reglo? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,right_reg.reglo,left_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_2));

        current_asmdata.currasmlist.concat(Taicpu.op_sym(A_BRA,S_NO,labelcmp64_1));
        cg.a_label(current_asmdata.currasmlist,labelcmp64_2);

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64_1);
        result:=tmpreg;
      end;

    function t68kaddnode.cmp64_leu(left_reg,right_reg:tregister64):tregister;
      var
        labelcmp64_1,labelcmp64_2 : tasmlabel;
        tmpreg : tregister;
      begin
        tmpreg:=cg.getintregister(current_asmdata.currasmlist,OS_INT);

        { load the value for "false" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,0,tmpreg);

        current_asmdata.getjumplabel(labelcmp64_1);
        current_asmdata.getjumplabel(labelcmp64_2);

        { check whether right_reg.reghi is less than left_reg.reghi }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reghi,right_reg.reghi));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_1));

        { are left_reg.reghi and right_reg.reghi equal? }
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_NE,S_NO,labelcmp64_2));

        { is right_reg.reglo less than left_reg.reglo? }
        current_asmdata.currasmlist.concat(taicpu.op_reg_reg(A_CMP,S_L,left_reg.reglo,right_reg.reglo));
        current_asmdata.currasmlist.concat(taicpu.op_cond_sym(A_BXX,C_CS,S_NO,labelcmp64_1));

        cg.a_label(current_asmdata.currasmlist,labelcmp64_2);

        { load the value for "true" }
        cg.a_load_const_reg(current_asmdata.currasmlist,OS_INT,1,tmpreg);

        cg.a_label(current_asmdata.currasmlist,labelcmp64_1);
        result:=tmpreg;
      end;

    function t68kaddnode.getresflags(unsigned : boolean) : tresflags;
      begin
         case nodetype of
           equaln : getresflags:=F_E;
           unequaln : getresflags:=F_NE;
          else
           if not(unsigned) then
             begin
                if nf_swapped in flags then
                  case nodetype of
                     ltn : getresflags:=F_G;
                     lten : getresflags:=F_GE;
                     gtn : getresflags:=F_L;
                     gten : getresflags:=F_LE;
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_L;
                     lten : getresflags:=F_LE;
                     gtn : getresflags:=F_G;
                     gten : getresflags:=F_GE;
                  end;
             end
           else
             begin
                if nf_swapped in flags then
                  case nodetype of
                     ltn : getresflags:=F_A;
                     lten : getresflags:=F_AE;
                     gtn : getresflags:=F_B;
                     gten : getresflags:=F_BE;
                  end
                else
                  case nodetype of
                     ltn : getresflags:=F_B;
                     lten : getresflags:=F_BE;
                     gtn : getresflags:=F_A;
                     gten : getresflags:=F_AE;
                  end;
             end;
         end;
      end;

    function t68kaddnode.getres64_register(unsigned:boolean;left_reg,right_reg:tregister64):tregister;
      begin
        case nodetype of
          equaln:
            result:=cmp64_eq(left_reg,right_reg);
          unequaln:
            result:=cmp64_ne(left_reg,right_reg);
          else
            if not unsigned then
            begin
              if nf_swapped in flags then
                case nodetype of
                  ltn:
                    result:=cmp64_lt(right_reg,left_reg);
                  lten:
                    result:=cmp64_le(right_reg,left_reg);
                  gtn:
                    result:=cmp64_lt(left_reg,right_reg);
                  gten:
                    result:=cmp64_le(left_reg,right_reg);
                end
              else
                case nodetype of
                  ltn:
                    result:=cmp64_lt(left_reg,right_reg);
                  lten:
                    result:=cmp64_le(left_reg,right_reg);
                  gtn:
                    result:=cmp64_lt(right_reg,left_reg);
                  gten:
                    result:=cmp64_le(right_reg,left_reg);
                end;
            end
            else
            begin
              if nf_swapped in Flags then
                case nodetype of
                  ltn:
                    result:=cmp64_ltu(right_reg,left_reg);
                  lten:
                    result:=cmp64_leu(right_reg,left_reg);
                  gtn:
                    result:=cmp64_ltu(left_reg,right_reg);
                  gten:
                    result:=cmp64_leu(left_reg,right_reg);
                end
              else
                case nodetype of
                  ltn:
                    result:=cmp64_ltu(left_reg,right_reg);
                  lten:
                    result:=cmp64_leu(left_reg,right_reg);
                  gtn:
                    result:=cmp64_ltu(right_reg,left_reg);
                  gten:
                    result:=cmp64_leu(right_reg,left_reg);
                end;
            end;
        end;
      end;

{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure t68kaddnode.second_addfloat;
      var
        op    : TAsmOp;
      begin
        pass_left_right;

        case nodetype of
          addn :
            op:=A_FADD;
          muln :
            op:=A_FMUL;
          subn :
            op:=A_FSUB;
          slashn :
            op:=A_FDIV;
          else
            internalerror(200403182);
        end;

        // get the operands in the correct order, there are no special cases
        // here, everything is register-based
        if nf_swapped in flags then
          swapleftright;

        // put both operands in a register
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

        // initialize de result
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        if left.location.loc = LOC_FPUREGISTER then
          location.register := left.location.register
        else if right.location.loc = LOC_FPUREGISTER then
          location.register := right.location.register
        else
          location.register := cg.getfpuregister(current_asmdata.CurrAsmList,location.size);

        // emit the actual operation
        {
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
            location.register,left.location.register,
            right.location.register))
        }
      end;


    procedure t68kaddnode.second_cmpfloat;
      begin
        pass_left_right;

{
        if (nf_swapped in flags) then
          swapleftright;
}
        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
{
        if nodetype in [equaln,unequaln] then
          current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMF,
             left.location.register,right.location.register),
             cgsize2fpuoppostfix[def_cgsize(resultdef)]))
        else
          current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMFE,
             left.location.register,right.location.register),
             cgsize2fpuoppostfix[def_cgsize(resultdef)]));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(false);
}
      end;




{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure t68kaddnode.second_cmpsmallset;
     var
       tmpreg : tregister;
     begin
       pass_left_right;

       location_reset(location,LOC_FLAGS,OS_NO);

       if (not(nf_swapped in flags) and
           (nodetype = lten)) or
          ((nf_swapped in flags) and
           (nodetype = gten)) then
         swapleftright;

       { Try to keep right as a constant }
       if right.location.loc<>LOC_CONSTANT then
         hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
       hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

       case nodetype of
         equaln,
         unequaln:
           begin
             if right.location.loc=LOC_CONSTANT then
               current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.value,left.location.register))
             else
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,right.location.register,left.location.register));
             if nodetype=equaln then
               location.resflags:=F_E
             else
               location.resflags:=F_NE;
           end;
         lten,
         gten:
           begin
             tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
             if right.location.loc=LOC_CONSTANT then
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_MOVE,S_L,right.location.value,tmpreg));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_AND,S_L,tmpreg,left.location.register));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,tmpreg,left.location.register));
               end
             else
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOVE,S_L,right.location.register,tmpreg));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_AND,S_L,tmpreg,left.location.register));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,S_L,tmpreg,left.location.register));
               end;
             location.resflags:=F_E;
           end;
         else
           internalerror(2013092701);
       end;
     end;


{*****************************************************************************
                                Ordinals
*****************************************************************************}

    procedure t68kaddnode.second_cmpordinal;
     var
      unsigned : boolean;
      useconst : boolean;
      tmpreg : tregister;
      opsize : topsize;
      cmpsize : tcgsize;
     begin
       pass_left_right;
       { set result location }
       location_reset(location,LOC_JUMP,OS_NO);

       { ToDo : set "allowconstants" to True, but this seems to upset Coldfire
                a bit for the CMP instruction => check manual and implement
                exception accordingly below }
       { load values into registers (except constants) }
       force_reg_left_right(true, false);

       { determine if the comparison will be unsigned }
       unsigned:=not(is_signed(left.resultdef)) or
                   not(is_signed(right.resultdef));

        // get the constant on the right if there is one
        if (left.location.loc = LOC_CONSTANT) then
          swapleftright;
        // can we use an immediate, or do we have to load the
        // constant in a register first?
        if (right.location.loc = LOC_CONSTANT) then
          begin
{$ifdef extdebug}
            if (right.location.size in [OS_64,OS_S64]) and (hi(right.location.value64)<>0) and ((hi(right.location.value64)<>-1) or unsigned) then
              internalerror(2002080301);
{$endif extdebug}
            if (nodetype in [equaln,unequaln]) then
              if (unsigned and
                  (right.location.value > high(word))) or
                 (not unsigned and
                  (longint(right.location.value) < low(smallint)) or
                   (longint(right.location.value) > high(smallint))) then
                { we can then maybe use a constant in the 'othersigned' case
                 (the sign doesn't matter for // equal/unequal)}
                unsigned := not unsigned;

            if (unsigned and
                ((right.location.value) <= high(word))) or
               (not(unsigned) and
                (longint(right.location.value) >= low(smallint)) and
                (longint(right.location.value) <= high(smallint))) then
               useconst := true
            else
              begin
                useconst := false;
                tmpreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,
                  aword(right.location.value),tmpreg);
               end
          end
        else
          useconst := false;
        location.loc := LOC_FLAGS;
        location.resflags := getresflags(unsigned);
        if tcgsize2size[right.location.size]=tcgsize2size[left.location.size] then
          cmpsize:=left.location.size
        else
          { ToDo : zero/sign extend??? }
          if tcgsize2size[right.location.size]<tcgsize2size[left.location.size] then
            cmpsize:=left.location.size
          else
            cmpsize:=right.location.size;
        opsize:=tcgsize2opsize[cmpsize];
        if opsize=S_NO then
          internalerror(2013090301);
        { Attention: The RIGHT(!) operand is substracted from and must be a
                     register! }
        if (right.location.loc = LOC_CONSTANT) then
          if useconst then
            current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,opsize,
              longint(right.location.value),left.location.register))
          else
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,opsize,
                tmpreg,left.location.register));
            end
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,opsize,
            right.location.register,left.location.register));
     end;


    function t68kaddnode.pass_1:tnode;
      var
        ld,rd : tdef;
      begin
        result:=inherited pass_1;

        { for 64 bit operations we return the resulting value in a register }
        if not assigned(result) then
          begin
            rd:=right.resultdef;
            ld:=left.resultdef;
            if (nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) and
                (
                  ((ld.typ=orddef) and (torddef(ld).ordtype in [u64bit,s64bit,scurrency])) or
                  ((rd.typ=orddef) and (torddef(rd).ordtype in [u64bit,s64bit,scurrency]))
                ) then
              expectloc:=LOC_REGISTER;
          end;
      end;


{*****************************************************************************
                                64-bit
*****************************************************************************}

    procedure t68kaddnode.second_cmp64bit;
      var
        unsigned : boolean;
        tmp_left_reg : tregister;
      begin
        pass_left_right;
        force_reg_left_right(false,false);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        location_reset(location,LOC_REGISTER,OS_INT);
        location.register:=getres64_register(unsigned,left.location.register64,right.location.register64);

        { keep the below code for now, as we could optimize the =/<> code later
          on based on it }

      // writeln('second_cmp64bit');
//      pass_left_right;


//     load_left_right(true,false);
(*
        case nodetype of
          ltn,lten,
          gtn,gten:
           begin
             emit_cmp64_hi;
             firstjmp64bitcmp;
             emit_cmp64_lo;
             secondjmp64bitcmp;
           end;
          equaln,unequaln:
           begin
             // instead of doing a complicated compare, do
             // (left.hi xor right.hi) or (left.lo xor right.lo)
             // (somewhate optimized so that no superfluous 'mr's are
             //  generated)
                  if (left.location.loc = LOC_CONSTANT) then
                    swapleftright;
                  if (right.location.loc = LOC_CONSTANT) then
                    begin
                      if left.location.loc = LOC_REGISTER then
                        begin
                          tempreg64.reglo := left.location.register64.reglo;
                          tempreg64.reghi := left.location.register64.reghi;
                        end
                      else
                        begin
                          if (aword(right.location.valueqword) <> 0) then
                            tempreg64.reglo := cg.getintregister(current_asmdata.CurrAsmList)
                          else
                            tempreg64.reglo := left.location.register64.reglo;
                          if ((right.location.valueqword shr 32) <> 0) then
                            tempreg64.reghi := cg.getintregister(current_asmdata.CurrAsmList)
                          else
                            tempreg64.reghi := left.location.register64.reghi;
                        end;

                      if (aword(right.location.valueqword) <> 0) then
                        { negative values can be handled using SUB, }
                        { positive values < 65535 using XOR.        }
                        if (longint(right.location.valueqword) >= -32767) and
                           (longint(right.location.valueqword) < 0) then
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,
                            aword(right.location.valueqword),
                            left.location.register64.reglo,tempreg64.reglo)
                        else
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_INT,
                            aword(right.location.valueqword),
                            left.location.register64.reglo,tempreg64.reglo);

                      if ((right.location.valueqword shr 32) <> 0) then
                        if (longint(right.location.valueqword shr 32) >= -32767) and
                           (longint(right.location.valueqword shr 32) < 0) then
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.register64.reghi,tempreg64.reghi)
                        else
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.register64.reghi,tempreg64.reghi);
                    end
                  else
                    begin
                       tempreg64.reglo := cg.getintregister(current_asmdata.CurrAsmList);
                       tempreg64.reghi := cg.getintregister(current_asmdata.CurrAsmList);
                       cg64.a_op64_reg_reg_reg(current_asmdata.CurrAsmList,OP_XOR,
                         left.location.register64,right.location.register64,
                         tempreg64);
                    end;

                  cg.a_reg_alloc(current_asmdata.CurrAsmList,R_0);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR_,R_0,
                    tempreg64.reglo,tempreg64.reghi));
                  cg.a_reg_dealloc(current_asmdata.CurrAsmList,R_0);
                  if (tempreg64.reglo <> left.location.register64.reglo) then
                    cg.ungetregister(current_asmdata.CurrAsmList,tempreg64.reglo);
                  if (tempreg64.reghi <> left.location.register64.reghi) then
                    cg.ungetregister(current_asmdata.CurrAsmList,tempreg64.reghi);

                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags := getresflags;
                end;
              else
                internalerror(2002072803);
            end;


        { set result location }
        { (emit_compare sets it to LOC_FLAGS for compares, so set the }
        {  real location only now) (JM)                               }
        if cmpop and
           not(nodetype in [equaln,unequaln]) then
          location_reset(location,LOC_JUMP,OS_NO);
*)
  //     location_reset(location,LOC_JUMP,OS_NO);
       // writeln('second_cmp64_exit');
     end;


begin
   caddnode:=t68kaddnode;
end.
