{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the ARM

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
unit narmadd;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd,cpubase;

    type
       tarmaddnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
       public
          function pass_1 : tnode;override;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpordinal;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      constexp,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgbase,cgutils,cgcpu,
      cpuinfo,pass_1,pass_2,regvars,procinfo,
      cpupara,
      ncon,nset,nadd,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32,
      hlcgobj
      ;

{*****************************************************************************
                               TSparcAddNode
*****************************************************************************}

    function tarmaddnode.GetResFlags(unsigned:Boolean):TResFlags;
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
                  end;
              end
            else
              begin
                if nf_swapped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_HI;
                    lten:
                      GetResFlags:=F_CS;
                    gtn:
                      GetResFlags:=F_CC;
                    gten:
                      GetResFlags:=F_LS;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_CC;
                    lten:
                      GetResFlags:=F_LS;
                    gtn:
                      GetResFlags:=F_HI;
                    gten:
                      GetResFlags:=F_CS;
                  end;
              end;
        end;
      end;


    procedure tarmaddnode.second_addfloat;
      var
        op : TAsmOp;
        singleprec: boolean;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        case current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
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
                  op:=A_ADF;
                muln :
                  op:=A_MUF;
                subn :
                  op:=A_SUF;
                slashn :
                  op:=A_DVF;
                else
                  internalerror(200308313);
              end;

              current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op,
                 location.register,left.location.register,right.location.register),
                 cgsize2fpuoppostfix[def_cgsize(resultdef)]));
            end;
          fpu_vfpv2,
          fpu_vfpv3,
          fpu_vfpv3_d16:
            begin
              { force mmreg as location, left right doesn't matter
                as both will be in a fpureg }
              location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,true);
              location_force_mmregscalar(current_asmdata.CurrAsmList,right.location,true);

              location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
              if left.location.loc<>LOC_CMMREGISTER then
                location.register:=left.location.register
              else if right.location.loc<>LOC_CMMREGISTER then
                location.register:=right.location.register
              else
                location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);

              singleprec:=tfloatdef(left.resultdef).floattype=s32real;
              case nodetype of
                addn :
                  if singleprec then
                    op:=A_FADDS
                  else
                    op:=A_FADDD;
                muln :
                  if singleprec then
                    op:=A_FMULS
                  else
                    op:=A_FMULD;
                subn :
                  if singleprec then
                    op:=A_FSUBS
                  else
                    op:=A_FSUBD;
                slashn :
                  if singleprec then
                    op:=A_FDIVS
                  else
                    op:=A_FDIVD;
                else
                  internalerror(2009111401);
              end;

              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
                 location.register,left.location.register,right.location.register));
            end;
          fpu_soft:
            { this case should be handled already by pass1 }
            internalerror(200308252);
          else
            internalerror(200308251);
        end;
      end;


    procedure tarmaddnode.second_cmpfloat;
      var
        op: TAsmOp;
      begin
        pass_left_right;
        if (nf_swapped in flags) then
          swapleftright;

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);

        case current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              { force fpureg as location, left right doesn't matter
                as both will be in a fpureg }
              location_force_fpureg(current_asmdata.CurrAsmList,left.location,true);
              location_force_fpureg(current_asmdata.CurrAsmList,right.location,true);

              if nodetype in [equaln,unequaln] then
                current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMF,
                   left.location.register,right.location.register),
                   cgsize2fpuoppostfix[def_cgsize(resultdef)]))
              else
                current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMFE,
                   left.location.register,right.location.register),
                   cgsize2fpuoppostfix[def_cgsize(resultdef)]));
            end;
          fpu_vfpv2,
          fpu_vfpv3,
          fpu_vfpv3_d16:
            begin
              location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,true);
              location_force_mmregscalar(current_asmdata.CurrAsmList,right.location,true);

              if (tfloatdef(left.resultdef).floattype=s32real) then
                if nodetype in [equaln,unequaln] then
                  op:=A_FCMPS
                 else
                   op:=A_FCMPES
              else if nodetype in [equaln,unequaln] then
                op:=A_FCMPD
              else
                op:=A_FCMPED;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,
                left.location.register,right.location.register));
              current_asmdata.CurrAsmList.concat(taicpu.op_none(A_FMSTAT));
            end;
          fpu_soft:
            { this case should be handled already by pass1 }
            internalerror(2009112404);
        end;

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(false);
      end;


    procedure tarmaddnode.second_cmpsmallset;
      var
        tmpreg : tregister;
      begin
        pass_left_right;

        location_reset(location,LOC_FLAGS,OS_NO);

        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

        case nodetype of
          equaln:
            begin
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              location.resflags:=F_EQ;
            end;
          unequaln:
            begin
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              location.resflags:=F_NE;
            end;
          lten,
          gten:
            begin
              if (not(nf_swapped in flags) and
                  (nodetype = lten)) or
                 ((nf_swapped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_AND,tmpreg,left.location.register,right.location.register));
              cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,tmpreg,right.location.register));
              location.resflags:=F_EQ;
            end;
          else
            internalerror(2004012401);
        end;
      end;


    procedure tarmaddnode.second_cmp64bit;
      var
        unsigned : boolean;
        oldnodetype : tnodetype;
        dummyreg : tregister;
        l: tasmlabel;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        pass_left_right;

        if (nodetype in [equaln,unequaln]) and
          (left.nodetype=ordconstn) and (tordconstnode(left).value=0) then
          begin
            location_reset(location,LOC_FLAGS,OS_NO);
            location.resflags:=getresflags(unsigned);
            if not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
            dummyreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
            current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(A_ORR,dummyreg,right.location.register64.reglo,right.location.register64.reghi),PF_S));
          end
        else if (nodetype in [equaln,unequaln]) and
          (right.nodetype=ordconstn) and (tordconstnode(right).value=0) then
          begin
            location_reset(location,LOC_FLAGS,OS_NO);
            location.resflags:=getresflags(unsigned);
            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            dummyreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
            cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
            current_asmdata.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(A_ORR,dummyreg,left.location.register64.reglo,left.location.register64.reghi),PF_S));
          end
        else
          begin
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

            { operation requiring proper N, Z and C flags ? }
            if unsigned or (nodetype in [equaln,unequaln]) then
              begin
                location_reset(location,LOC_FLAGS,OS_NO);
                location.resflags:=getresflags(unsigned);
                cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi));
                if current_settings.cputype in cpu_thumb2 then
                  begin
                    current_asmdata.getjumplabel(l);
                    cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,l);
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
                    cg.a_label(current_asmdata.CurrAsmList,l);
                  end
                else
                  current_asmdata.CurrAsmList.concat(setcondition(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo),C_EQ));
              end
            else
            { operation requiring proper N, Z and V flags ? }
              begin
                location_reset(location,LOC_JUMP,OS_NO);
                cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi));
                { the jump the sequence is a little bit hairy }
                case nodetype of
                   ltn,gtn:
                     begin
                        cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(false),current_procinfo.CurrTrueLabel);
                        { cheat a little bit for the negative test }
                        toggleflag(nf_swapped);
                        cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(false),current_procinfo.CurrFalseLabel);
                        cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
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
                        cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                        nodetype:=oldnodetype;
                     end;
                end;
                cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
                { the comparisaion of the low dword have to be
                   always unsigned!                            }
                cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),current_procinfo.CurrTrueLabel);
                cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
              end;
          end;
      end;


    function tarmaddnode.pass_1 : tnode;
      var
        unsigned : boolean;
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
      end;


    procedure tarmaddnode.second_cmpordinal;
      var
        unsigned : boolean;
        tmpreg : tregister;
        b : byte;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));
        cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
        if right.location.loc = LOC_CONSTANT then
          begin
             if is_shifter_const(right.location.value,b) then
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,left.location.register,right.location.value))
             else
               begin
                 tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
                 cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,
                   right.location.value,tmpreg);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,tmpreg));
               end;
          end
        else
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);
      end;

begin
  caddnode:=tarmaddnode;
end.
