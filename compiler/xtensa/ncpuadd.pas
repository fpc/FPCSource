{
    Copyright (c) 2008 by Florian Klaempfl

    Code generation for add nodes on the Xtensa

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
       cgbase,node,ncgadd,cpubase,
       compilerbase;

    type
       TCPUAddNode = class(tcgaddnode)
       private
         procedure pass_left_and_right(ctx:tpassgeneratecodecontext);
         procedure cmp64_le(left_reg, right_reg: TRegister64; unsigned: boolean; ctx:tpassgeneratecodecontext);
         procedure cmp64_lt(left_reg, right_reg: TRegister64; unsigned: boolean; ctx:tpassgeneratecodecontext);
       protected
         function pass_1 : tnode;override;
         function first_addfloat: tnode;override;
         function use_generic_mul32to64: boolean;override;
         function use_generic_mul64bit: boolean;override;
         procedure second_addordinal(ctx:tpassgeneratecodecontext);override;
         procedure second_cmpordinal(ctx:tpassgeneratecodecontext);override;
         procedure second_cmpsmallset(ctx:tpassgeneratecodecontext);override;
         procedure second_cmp64bit(ctx:tpassgeneratecodecontext);override;
         procedure second_add64bit(ctx:tpassgeneratecodecontext);override;
         procedure second_cmpfloat(ctx:tpassgeneratecodecontext);override;
         procedure second_addfloat(ctx:tpassgeneratecodecontext);override;
         procedure second_cmp(ctx:tpassgeneratecodecontext);
         function use_fma: boolean;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgutils,cgcpu,
      cpuinfo,pass_1,pass_2,pass_2_context,procinfo,
      cpupara,
      ncon,nset,nadd,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32,
      nodehelper,compiler;

{*****************************************************************************
                               TCPUAddNode
*****************************************************************************}

    function TCPUAddNode.use_fma : boolean;
      begin
        Result:=is_single(left.resultdef) and is_single(right.resultdef) and
          (FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]);
      end;


    procedure TCPUAddNode.second_addordinal(ctx:tpassgeneratecodecontext);
      var
        ophigh: tasmop;
      begin
        { this is only true, if the CPU supports 32x32 -> 64 bit MUL, see the relevant method }
        if (nodetype=muln) and is_64bit(resultdef) then
          begin
            if not(is_signed(left.resultdef)) or
               not(is_signed(right.resultdef)) then
              ophigh:=A_MULUH
            else
              ophigh:=A_MULSH;

            pass_left_right(ctx);

            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            if not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

            { initialize the result }
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

            location.register64.reglo:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
            location.register64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);

            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULL,location.register64.reglo,left.location.register,right.location.register));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(ophigh,location.register64.reghi,left.location.register,right.location.register));
          end
        else
          Inherited;
      end;


    procedure TCPUAddNode.second_cmpsmallset(ctx:tpassgeneratecodecontext);
      var
        tmpreg : tregister;
        truelab, falselab: TAsmLabel;
      begin
        pass_left_right(ctx);

        if (not(nf_swapped in flags) and
            (nodetype = lten)) or
           ((nf_swapped in flags) and
            (nodetype = gten)) then
          swapleftright;

        ctx.CurrAsmList.AsmData.getjumplabel(truelab);
        ctx.CurrAsmList.AsmData.getjumplabel(falselab);

        location_reset_jump(location,truelab,falselab);
        force_reg_left_right(false,false,ctx);

        case nodetype of
          equaln:
            begin
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_EQ,left.location.register,right.location.register,location.truelabel);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
            end;
          unequaln:
            begin
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left.location.register,right.location.register,location.truelabel);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
            end;
          lten,
          gten:
            begin
              tmpreg:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);
              ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_AND,OS_32,left.location.register,right.location.register,tmpreg);
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_EQ,tmpreg,right.location.register,location.truelabel);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
            end;
          else
            internalerror(2020082401);
        end;
      end;


    procedure TCPUAddNode.second_cmp(ctx:tpassgeneratecodecontext);
      var
        cond: TOpCmp;
        instr: taicpu;
        truelab, falselab: TAsmLabel;
      begin
        pass_left_right(ctx);

        ctx.CurrAsmList.AsmData.getjumplabel(truelab);
        ctx.CurrAsmList.AsmData.getjumplabel(falselab);

        location_reset_jump(location,truelab,falselab);

        ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,cgsize_orddef(OS_INT),true);

        if is_signed(left.resultdef) then
          case nodetype of
            equaln:   cond:=OC_EQ;
            unequaln: cond:=OC_NE;
            ltn:      cond:=OC_LT;
            lten:     cond:=OC_LTE;
            gtn:      cond:=OC_GT;
            gten:     cond:=OC_GTE;
          else
            internalerror(2020030801);
          end
        else
          case nodetype of
            equaln:   cond:=OC_EQ;
            unequaln: cond:=OC_NE;
            ltn:      cond:=OC_B;
            lten:     cond:=OC_BE;
            gtn:      cond:=OC_A;
            gten:     cond:=OC_AE;
          else
            internalerror(2020030803);
          end;

        if (right.nodetype=ordconstn) and not(nf_swapped in flags) then
          ctx.cg.a_cmp_const_reg_label(ctx.CurrAsmList,OS_INT,cond,right.location.value,left.location.register,location.truelabel)
        else
          begin
            if not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,cgsize_orddef(OS_INT),true);

            if nf_swapped in flags then
               ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,cond,left.location.register,right.location.register,location.truelabel)
             else
               ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,cond,right.location.register,left.location.register,location.truelabel);
          end;
        ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
      end;


    const
      cmpops: array[boolean] of TOpCmp = (OC_LT,OC_B);

    procedure TCPUAddNode.cmp64_lt(left_reg, right_reg: TRegister64;unsigned: boolean;ctx:tpassgeneratecodecontext);
      begin
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,cmpops[unsigned],right_reg.reghi,left_reg.reghi,location.truelabel);
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_B,right_reg.reglo,left_reg.reglo,location.truelabel);
        ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
      end;


    procedure TCPUAddNode.cmp64_le(left_reg, right_reg: TRegister64;unsigned: boolean;ctx:tpassgeneratecodecontext);
      begin
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,cmpops[unsigned],left_reg.reghi,right_reg.reghi,location.falselabel);
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
        ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_B,left_reg.reglo,right_reg.reglo,location.falselabel);
        ctx.cg.a_jmp_always(ctx.CurrAsmList,location.truelabel);
      end;


    procedure TCPUAddNode.second_cmp64bit(ctx:tpassgeneratecodecontext);
      var
        truelabel,
        falselabel: tasmlabel;
        unsigned: boolean;
        left_reg,right_reg: TRegister64;
      begin
        ctx.CurrAsmList.AsmData.getjumplabel(truelabel);
        ctx.CurrAsmList.AsmData.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        pass_left_right(ctx);
        force_reg_left_right(true,true,ctx);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        left_reg:=left.location.register64;
        { force_reg_left_right might leave right as LOC_CONSTANT, however, we cannot take advantage of this yet }
        if right.location.loc=LOC_CONSTANT then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
        right_reg:=right.location.register64;

        case NodeType of
          equaln:
            begin
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.falselabel);
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.falselabel);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,location.truelabel);
            end;
          unequaln:
            begin
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,location.truelabel);
              ctx.cg.a_cmp_reg_reg_label(ctx.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,location.truelabel);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
            end;
        else
          if nf_swapped in flags then
            case NodeType of
              ltn:
                cmp64_lt(right_reg, left_reg,unsigned,ctx);
              lten:
                cmp64_le(right_reg, left_reg,unsigned,ctx);
              gtn:
                cmp64_lt(left_reg, right_reg,unsigned,ctx);
              gten:
                cmp64_le(left_reg, right_reg,unsigned,ctx);
              else
                internalerror(2020082202);
            end
          else
            case NodeType of
              ltn:
                cmp64_lt(left_reg, right_reg,unsigned,ctx);
              lten:
                cmp64_le(left_reg, right_reg,unsigned,ctx);
              gtn:
                cmp64_lt(right_reg, left_reg,unsigned,ctx);
              gten:
                cmp64_le(right_reg, left_reg,unsigned,ctx);
              else
                internalerror(2020082203);
            end;
        end;
      end;


    function TCPUAddNode.pass_1 : tnode;
      begin
        result:=inherited pass_1;
        if not(assigned(result)) and (nodetype in [equaln,unequaln,ltn,lten,gtn,gten]) and
          not((FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) and
            is_single(left.resultdef) and (nodetype<>slashn)) then
          expectloc:=LOC_JUMP;
{$ifdef dummy}
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
{$endif dummy}
      end;


    procedure TCPUAddNode.second_cmpordinal(ctx:tpassgeneratecodecontext);
      begin
        second_cmp(ctx);
      end;


    procedure TCPUAddNode.pass_left_and_right(ctx:tpassgeneratecodecontext);
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;

        secondpass(left,ctx);
        secondpass(right,ctx);
      end;


    function TCPUAddNode.first_addfloat: tnode;
      begin
        result := nil;

        if (FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) and
          (tfloatdef(left.resultdef).floattype=s32real) and (nodetype<>slashn) then
          begin
            if nodetype in [equaln,unequaln,lten,ltn,gten,gtn] then
              expectloc:=LOC_FLAGS
            else
              expectloc:=LOC_FPUREGISTER;

            if compiler.globals.needs_check_for_fpu_exceptions then
              Include(compiler.current_procinfo.flags,pi_do_call);
          end
        else
          result:=first_addfloat_soft;
      end;


    function TCPUAddNode.use_generic_mul32to64: boolean;
      begin
        result:=not(CPUXTENSA_HAS_MUL32HIGH in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]) or needoverflowcheck;
      end;


    function TCPUAddNode.use_generic_mul64bit: boolean;
      begin
        result:=needoverflowcheck or
          (cs_opt_size in compiler.globals.current_settings.optimizerswitches) or
          not(CPUXTENSA_HAS_MUL32HIGH in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]);
      end;


    procedure TCPUAddNode.second_addfloat(ctx:tpassgeneratecodecontext);
      var
        op    : TAsmOp;
        cmpop,
        singleprec , inv: boolean;
        ai : taicpu;
      begin
        pass_left_and_right(ctx);
        if (nf_swapped in flags) then
          swapleftright;

        ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,left.location,left.resultdef,true);
        ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,right.location,right.resultdef,true);

        cmpop:=false;
        inv:=false;
        case nodetype of
          addn :
            op:=A_ADD;
          muln :
            op:=A_MUL;
          subn :
            op:=A_SUB;
          unequaln:
            begin
              op:=A_OEQ;
              cmpop:=true;
              inv:=true;
            end;
          equaln:
            begin
              op:=A_OEQ;
              cmpop:=true;
            end;
          ltn:
            begin
              op:=A_OLT;
              cmpop:=true;
            end;
          lten:
            begin
              op:=A_OLE;
              cmpop:=true;
            end;
          gtn:
            begin
              op:=A_OLT;
              swapleftright;
              cmpop:=true;
            end;
          gten:
            begin
              op:=A_OLE;
              swapleftright;
              cmpop:=true;
            end;
          else
            internalerror(2020032601);
        end;

        { initialize de result }
        if cmpop then
          begin
            if CPUXTENSA_HAS_BOOLEAN_OPTION in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype] then
              begin
                location_reset(location,LOC_FLAGS,OS_NO);
                location.resflags.register:=NR_B0;
                location.resflags.flag:=F_NZ;
              end
            else
              Internalerror(2020070402);
          end
        else
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
           location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);
         end;

        { emit the actual operation }
        if cmpop then
          begin
            ctx.cg.getcpuregister(ctx.CurrAsmList,location.resflags.register);
            ai:=taicpu.op_reg_reg_reg(op,location.resflags.register,left.location.register,right.location.register);
            ai.oppostfix:=PF_S;
            ctx.CurrAsmList.concat(ai);
            ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);

            if inv then
              location.resflags.flag:=F_Z;
          end
        else
          begin
            ai:=taicpu.op_reg_reg_reg(op,location.register,left.location.register,right.location.register);
            ai.oppostfix := PF_S;
            ctx.CurrAsmList.concat(ai);
            ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
          end;
      end;


    procedure TCPUAddNode.second_cmpfloat(ctx:tpassgeneratecodecontext);
      begin
        second_addfloat(ctx);
      end;


    procedure TCPUAddNode.second_add64bit(ctx:tpassgeneratecodecontext);
      var
        unsigned: Boolean;
        tmpreg: tregister;
      begin
        if nodetype=muln then
          begin
            pass_left_right(ctx);
            unsigned:=((left.resultdef.typ=orddef) and
                       (torddef(left.resultdef).ordtype=u64bit)) or
                      ((right.resultdef.typ=orddef) and
                       (torddef(right.resultdef).ordtype=u64bit));
            force_reg_left_right(true,true,ctx);

            { force_reg_left_right might leave right as LOC_CONSTANT, however, we cannot take advantage of this yet }
            if right.location.loc=LOC_CONSTANT then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
            location.register64.reglo:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
            location.register64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
            tmpreg:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULL,location.register64.reglo,left.location.register64.reglo,right.location.register64.reglo));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULUH,location.register64.reghi,left.location.register64.reglo,right.location.register64.reglo));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULL,tmpreg,left.location.register64.reglo,right.location.register64.reghi));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD,location.register64.reghi,location.register64.reghi,tmpreg));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULL,tmpreg,left.location.register64.reghi,right.location.register64.reglo));
            ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD,location.register64.reghi,location.register64.reghi,tmpreg));
          end
        else
          Inherited;
      end;


begin
  caddnode:=tcpuaddnode;
end.

