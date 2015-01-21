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
          function pass_1: tnode; override;
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
      hlcgobj, ncgutil,cgobj;

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
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,(left.location.loc<>LOC_CFPUREGISTER));

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
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
        hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);

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
        hreg1,hreg2: tregister;

      procedure emit_compare(list:tasmlist; ls,rs:tnode);
        var
          lreg: tregister64;
        begin
          if (ls.location.loc=LOC_CONSTANT) then
            begin
              lreg.reghi:=NR_G0;
              lreg.reglo:=NR_G0;
              if lo(ls.location.value64)<>0 then
                begin
                  lreg.reglo:=cg.GetIntRegister(list,OS_INT);
                  cg.a_load_const_reg(list,OS_INT,lo(ls.location.value64),lreg.reglo);
                end;
              if hi(ls.location.value64)<>0 then
                begin
                  lreg.reghi:=cg.GetIntRegister(list,OS_INT);
                  cg.a_load_const_reg(list,OS_INT,hi(ls.location.value64),lreg.reghi);
                end;
            end
          else
            lreg:=ls.location.register64;

          if (rs.location.loc=LOC_CONSTANT) then
            begin
              tcgsparc(cg).handle_reg_const_reg(list,A_SUBcc,lreg.reglo,lo(rs.location.value64),NR_G0);
              tcgsparc(cg).handle_reg_const_reg(list,A_SUBXcc,lreg.reghi,hi(rs.location.value64),NR_G0);
            end
          else
            begin
              list.concat(taicpu.op_reg_reg_reg(A_SUBcc,lreg.reglo,rs.location.register64.reglo,NR_G0));
              list.concat(taicpu.op_reg_reg_reg(A_SUBXcc,lreg.reghi,rs.location.register64.reghi,NR_G0));
            end;
        end;

      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        location_reset(location,LOC_FLAGS,OS_NO);

        if (nodetype in [equaln,unequaln]) then
          begin
            location.resflags:=getresflags(unsigned);
            if (right.location.loc=LOC_CONSTANT) then
              begin
                if hi(right.location.value64)<>0 then
                  begin
                    hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_XOR,left.location.register64.reghi,hi(right.location.value64),hreg1);
                  end
                else
                  hreg1:=left.location.register64.reghi;

                if lo(right.location.value64)<>0 then
                  begin
                    hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    tcgsparc(cg).handle_reg_const_reg(current_asmdata.CurrAsmList,A_XOR,left.location.register64.reglo,lo(right.location.value64),hreg2);
                  end
                else
                  hreg2:=left.location.register64.reglo;
              end
            else
              begin
                hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_XOR,left.location.register64.reghi,right.location.register64.reghi,hreg1));
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_XOR,left.location.register64.reglo,right.location.register64.reglo,hreg2));
              end;
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ORcc,hreg1,hreg2,NR_G0));
          end
        else
          begin
            { undo possible swapped state }
            if (nf_swapped in flags) then
              swapleftright;
            { Subtracting sides sets N,V and C flags correctly, but not Z flag
              (which ends up depending only on upper dword). So don't use conditions
              that test Z flag:
                             unsigned  signed
              a < b   =>       F_B      F_L
              a >= b  =>       F_AE     F_GE
              a <= b  => swap, F_AE     F_GE
              a > b   => swap, F_B      F_L }
            if (nodetype in [ltn,gten]) then
              begin
                emit_compare(current_asmdata.CurrAsmList,left,right);
                location.resflags:=getresflags(unsigned);
              end
            else if (nodetype in [lten,gtn]) then
              begin
                emit_compare(current_asmdata.CurrAsmList,right,left);
                toggleflag(nf_swapped);
                location.resflags:=getresflags(unsigned);
                toggleflag(nf_swapped);
              end
            else
              InternalError(2014011001);
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


    function tsparcaddnode.pass_1: tnode;
      begin
        result:=inherited pass_1;
        if not assigned(result) then
          begin
            if is_64bitint(left.resultdef) and
              (nodetype in [equaln,unequaln,ltn,gtn,lten,gten]) then
              expectloc:=LOC_FLAGS;
          end;
      end;

begin
  caddnode:=tsparcaddnode;
end.
