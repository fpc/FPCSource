{
    Copyright (c) 2008 by Florian Klaempfl

    Code generation for add nodes on the spc32

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
unit nspc32add;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd,cpubase;

    type

       { Tspc32AddNode }

       Tspc32AddNode = class(tcgaddnode)
       private
         function  GetResFlags(unsigned:Boolean):TResFlags;
       protected
         function pass_1 : tnode;override;
         procedure second_cmpordinal;override;
         procedure second_cmpsmallset;override;
         procedure second_cmp64bit;override;
         procedure second_cmp;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgbase,cgutils,cgcpu,
      cpuinfo,pass_1,pass_2,regvars,procinfo,
      cpupara,
      ncon,nset,nadd,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32,
      hlcgobj;

{*****************************************************************************
                               Tspc32AddNode
*****************************************************************************}

    function Tspc32AddNode.GetResFlags(unsigned: Boolean): TResFlags;
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
                      GetResFlags:=F_NotPossible;
                    lten:
                      GetResFlags:=F_NotPossible;
                    gtn:
                      GetResFlags:=F_LT;
                    gten:
                      GetResFlags:=F_LE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_LT;
                    lten:
                      GetResFlags:=F_LE;
                    gtn:
                      GetResFlags:=F_NotPossible;
                    gten:
                      GetResFlags:=F_NotPossible;
                  end;
              end
            else
              begin
                if nf_swapped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_NotPossible;
                    lten:
                      GetResFlags:=F_NotPossible;
                    gtn:
                      GetResFlags:=F_B;
                    gten:
                      GetResFlags:=F_BE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_B;
                    lten:
                      GetResFlags:=F_BE;
                    gtn:
                      GetResFlags:=F_NotPossible;
                    gten:
                      GetResFlags:=F_NotPossible;
                  end;
              end;
        end;
      end;


    procedure Tspc32AddNode.second_cmpsmallset;
      var
        tmpreg : tregister;
        b: byte;
      begin
        pass_left_right;

        location_reset(location,LOC_FLAGS,OS_NO);

        if (not(nf_swapped in flags) and
            (nodetype = lten)) or
           ((nf_swapped in flags) and
            (nodetype = gten)) then
          swapleftright;

        (* Try to keep right as a constant *)
        //if (right.location.loc <> LOC_CONSTANT) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

        case nodetype of
          equaln,
          unequaln:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_LD,left.location.register));
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              if right.location.loc = LOC_CONSTANT then
                current_asmdata.CurrAsmList.concat(taicpu.op_const(A_SUB,right.location.value))
              else
                current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_SUB,right.location.register));
              if nodetype = equaln then
                location.resflags:=F_EQ
              else
                location.resflags:=F_NE;
            end;
          lten,
          gten:
            begin
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
              if right.location.loc = LOC_CONSTANT then
                begin
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,right.location.value,left.location.register,tmpreg);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_LD,tmpreg));
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                  current_asmdata.CurrAsmList.concat(taicpu.op_const(A_SUB,right.location.value))
                  //current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,tmpreg,right.location.value));
                end
              else
                begin
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,right.location.register,left.location.register,tmpreg);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_LD,tmpreg));
                  cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_SUB,right.location.register))
                  //current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,tmpreg,right.location.register));
                end;
              location.resflags:=F_EQ;
            end;
          else
            internalerror(2004012401);
        end;
      end;


    procedure Tspc32AddNode.second_cmp;
      var
        unsigned : boolean;
        tmpreg1,tmpreg2 : tregister;
        i : longint;
      begin
        pass_left_right;
        force_reg_left_right(true,false);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        if getresflags(unsigned)=F_NotPossible then
          swapleftright;

        current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_LD,left.location.register));
        cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_SUB,right.location.register));

        if left.location.size in [OS_S64,OS_64] then
          begin
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_LD,left.location.registerhi));
            current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_SBC,right.location.registerhi));
          end;

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);
      end;


    procedure Tspc32AddNode.second_cmp64bit;
      begin
        second_cmp;
      end;


    function Tspc32AddNode.pass_1: tnode;
      var
        unsigned: Boolean;
      begin
        result:=inherited pass_1;

        if not(assigned(result)) then
          begin
            unsigned:=not(is_signed(left.resultdef)) or
              not(is_signed(right.resultdef));

            if is_64bit(left.resultdef) and
              ((nodetype in [equaln,unequaln]) or
               (unsigned and (nodetype in [ltn,lten,gtn,gten]))
              ) then
              expectloc:=LOC_FLAGS;
          end;
        { handling boolean expressions }
        if not(assigned(result)) and
           (
             not(is_boolean(left.resultdef)) or
             not(is_boolean(right.resultdef)) or
             is_dynamic_array(left.resultdef)
           ) then
          expectloc:=LOC_FLAGS;
      end;


    procedure Tspc32AddNode.second_cmpordinal;
      begin
        second_cmp;
      end;

begin
  caddnode:=tspc32addnode;
end.
