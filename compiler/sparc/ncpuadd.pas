{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the SPARC

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
       tsparcaddnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
          function  GetFPUResFlags:TResFlags;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
          procedure second_cmpordinal;override;
          procedure second_addordinal;override;
       public
          function use_generic_mul32to64: boolean; override;
       end;

  implementation

    uses
      systems,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,cgcpu,cgutils,
      cpupara,
      ncon,nset,nadd,
      ncgutil,cgobj;

{*****************************************************************************
                               TSparcAddNode
*****************************************************************************}

    function TSparcAddNode.GetResFlags(unsigned:Boolean):TResFlags;
      begin
        case NodeType of
          equaln:
            GetResFlags:=F_E;
          unequaln:
            GetResFlags:=F_NE;
          else
            if not(unsigned) then
              begin
                if nf_swapped in flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_G;
                    lten:
                      GetResFlags:=F_GE;
                    gtn:
                      GetResFlags:=F_L;
                    gten:
                      GetResFlags:=F_LE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_L;
                    lten:
                      GetResFlags:=F_LE;
                    gtn:
                      GetResFlags:=F_G;
                    gten:
                      GetResFlags:=F_GE;
                  end;
              end
            else
              begin
                if nf_swapped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_A;
                    lten:
                      GetResFlags:=F_AE;
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
                      GetResFlags:=F_A;
                    gten:
                      GetResFlags:=F_AE;
                  end;
              end;
        end;
      end;


    function TSparcAddNode.GetFPUResFlags:TResFlags;
      begin
        case NodeType of
          equaln:
            result:=F_FE;
          unequaln:
            result:=F_FNE;
          else
            begin
              if nf_swapped in Flags then
                case NodeType of
                  ltn:
                    result:=F_FG;
                  lten:
                    result:=F_FGE;
                  gtn:
                    result:=F_FL;
                  gten:
                    result:=F_FLE;
                end
              else
                case NodeType of
                  ltn:
                    result:=F_FL;
                  lten:
                    result:=F_FLE;
                  gtn:
                    result:=F_FG;
                  gten:
                    result:=F_FGE;
                end;
            end;
        end;
      end;


    procedure tsparcaddnode.second_addfloat;
      var
        op : TAsmOp;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,true);
        location_force_fpureg(current_asmdata.CurrAsmList,right.location,(left.location.loc<>LOC_CFPUREGISTER));

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        if left.location.loc<>LOC_CFPUREGISTER then
          location.register:=left.location.register
        else
          location.register:=right.location.register;

        case nodetype of
          addn :
            begin
              if location.size=OS_F64 then
                op:=A_FADDd
              else
                op:=A_FADDs;
            end;
          muln :
            begin
              if location.size=OS_F64 then
                op:=A_FMULd
              else
                op:=A_FMULs;
            end;
          subn :
            begin
              if location.size=OS_F64 then
                op:=A_FSUBd
              else
                op:=A_FSUBs;
            end;
          slashn :
            begin
              if location.size=OS_F64 then
                op:=A_FDIVd
              else
                op:=A_FDIVs;
            end;
          else
            internalerror(200306014);
        end;

        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
           left.location.register,right.location.register,location.register));
      end;


    procedure tsparcaddnode.second_cmpfloat;
      var
        op : tasmop;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,true);
        location_force_fpureg(current_asmdata.CurrAsmList,right.location,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getfpuresflags;

        if left.location.size=OS_F64 then
          op:=A_FCMPd
        else
          op:=A_FCMPs;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,
             left.location.register,right.location.register));
        { Delay slot (can only contain integer operation) }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
      end;


    procedure tsparcaddnode.second_cmpboolean;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_SUBcc,left.location.register,right.location.value,NR_G0)
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,NR_G0));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
      end;


    procedure tsparcaddnode.second_cmpsmallset;
      var
        tmpreg : tregister;
      begin
        pass_left_right;

        location_reset(location,LOC_FLAGS,OS_NO);

        force_reg_left_right(false,false);

        case nodetype of
          equaln,
          unequaln:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,NR_G0));
              location.resflags:=getresflags(true);
            end;
          lten,
          gten:
            begin
              if (not(nf_swapped in flags) and
                  (nodetype = lten)) or
                 ((nf_swapped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,left.location.size);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_AND,left.location.register,right.location.register,tmpreg));
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBcc,tmpreg,right.location.register,NR_G0));
              location.resflags:=F_E;
            end;
          else
            internalerror(2012042701);
        end;
      end;


    procedure tsparcaddnode.second_cmp64bit;
      var
        unsigned   : boolean;

      procedure firstjmp64bitcmp;
        var
           oldnodetype : tnodetype;
        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrFalseLabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrTrueLabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),current_procinfo.CurrFalseLabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrFalseLabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrTrueLabel);
           end;
        end;

      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),current_procinfo.CurrTrueLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                end;
              equaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrFalseLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,current_procinfo.CurrTrueLabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                end;
           end;
        end;

      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        location_reset(location,LOC_JUMP,OS_NO);

        if (right.location.loc<>LOC_CONSTANT) then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi));
            firstjmp64bitcmp;
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
            secondjmp64bitcmp;
          end
        else
          begin
            tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_SUBcc,left.location.register64.reghi,hi(right.location.value64),NR_G0);
            firstjmp64bitcmp;
            tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_SUBcc,left.location.register64.reglo,lo(right.location.value64),NR_G0);
            secondjmp64bitcmp;
          end;
      end;


    procedure tsparcaddnode.second_cmpordinal;
      var
        unsigned : boolean;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_SUBcc,left.location.register,right.location.value,NR_G0)
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,NR_G0));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);
      end;


    const
      multops: array[boolean] of TAsmOp = (A_SMUL, A_UMUL);

    procedure tsparcaddnode.second_addordinal;
      var
        unsigned: boolean;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));
        if (nodetype=muln) and is_64bit(resultdef) then
          begin
            pass_left_right;
            force_reg_left_right(true,false);
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg_reg(multops[unsigned],left.location.register,right.location.register,location.register64.reglo));
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_MOV,NR_Y,location.register64.reghi));
          end
        else
          inherited second_addordinal;
      end;


    function tsparcaddnode.use_generic_mul32to64: boolean;
      begin
        result:=false;
      end;

begin
  caddnode:=tsparcaddnode;
end.
