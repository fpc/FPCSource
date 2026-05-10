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
       node,ncgadd,cpubase,
       compilerbase;

    type
       tarmaddnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
          function  GetFpuResFlags:TResFlags;
       public
          function use_fma : boolean;override;
          function pass_1 : tnode;override;
          function use_generic_mul32to64: boolean; override;
          function use_generic_mul64bit: boolean; override;
       protected
          function first_addfloat: tnode; override;
          procedure second_addordinal(ctx:tpassgeneratecodecontext);override;
          procedure second_addfloat(ctx:tpassgeneratecodecontext);override;
          procedure second_cmpfloat(ctx:tpassgeneratecodecontext);override;
          procedure second_cmpordinal(ctx:tpassgeneratecodecontext);override;
          procedure second_cmpsmallset(ctx:tpassgeneratecodecontext);override;
          procedure second_cmp64bit(ctx:tpassgeneratecodecontext);override;
          procedure second_add64bit(ctx:tpassgeneratecodecontext);override;
       end;

  implementation

    uses
      globtype,verbose,globals,systems,
      constexp,symdef,symtable,symtype,symconst,
      aasmbase,aasmdata,aasmcpu,
      defutil,htypechk,cgbase,cgutils,
      cpuinfo,pass_1,pass_2,pass_2_context,procinfo,
      ncon,nadd,ncnv,ncal,nmat,
      ncgutil,cgobj,cgcpu,
      nodehelper,
      compiler
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
                    else
                      internalerror(201408203);
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
                      internalerror(201408204);
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
                    else
                      internalerror(201408205);
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
                    else
                      internalerror(201408206);
                  end;
              end;
        end;
      end;


    function tarmaddnode.GetFpuResFlags:TResFlags;
      begin
        if nf_swapped in Flags then
          internalerror(2014042001);
        case NodeType of
          equaln:
            result:=F_EQ;
          unequaln:
            result:=F_NE;
          ltn:
            result:=F_MI;
          lten:
            result:=F_LS;
          gtn:
            result:=F_GT;
          gten:
            result:=F_GE;
          else
            internalerror(201408207);
        end;
      end;


    function tarmaddnode.use_fma : boolean;
      begin
       Result:=FPUARM_HAS_FMA in fpu_capabilities[compiler.globals.current_settings.fputype];
      end;


    procedure tarmaddnode.second_addfloat(ctx:tpassgeneratecodecontext);
      var
        op : TAsmOp;
        singleprec: boolean;
        pf: TOpPostfix;
      begin
        pass_left_right(ctx);
        if (nf_swapped in flags) then
          swapleftright;

        case compiler.globals.current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              { force fpureg as location, left right doesn't matter
                as both will be in a fpureg }
              ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,right.location,right.resultdef,true);

              location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
              location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);

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

              ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op,
                 location.register,left.location.register,right.location.register),
                 cgsize2fpuoppostfix[def_cgsize(resultdef)]));
            end;
          fpu_soft:
            { this case should be handled already by pass1 }
            internalerror(2003082503);
          else if FPUARM_HAS_VFP_DOUBLE in fpu_capabilities[compiler.globals.current_settings.fputype] then
            begin
              { force mmreg as location, left right doesn't matter
                as both will be in a fpureg }
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,right.location,right.resultdef,true);

              location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
              location.register:=ctx.cg.getmmregister(ctx.CurrAsmList,location.size);

              singleprec:=tfloatdef(left.resultdef).floattype=s32real;
              if singleprec then
                pf:=PF_F32
              else
                pf:=PF_F64;
              case nodetype of
                addn :
                  op:=A_VADD;
                muln :
                  op:=A_VMUL;
                subn :
                  op:=A_VSUB;
                slashn :
                  op:=A_VDIV;
                else
                  internalerror(2009111401);
              end;

              ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op,
                 location.register,left.location.register,right.location.register),pf));
              ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
            end
          else if FPUARM_HAS_VFP_EXTENSION in fpu_capabilities[compiler.globals.current_settings.fputype] then
            begin
              { force mmreg as location, left right doesn't matter
                as both will be in a fpureg }
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,right.location,right.resultdef,true);

              location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
              location.register:=ctx.cg.getmmregister(ctx.CurrAsmList,location.size);

              case nodetype of
                addn :
                  op:=A_VADD;
                muln :
                  op:=A_VMUL;
                subn :
                  op:=A_VSUB;
                slashn :
                  op:=A_VDIV;
                else
                  internalerror(2009111404);
              end;

              ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(op, location.register,left.location.register,right.location.register), PF_F32));
              ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
            end
          else
            internalerror(200308251);
        end;
      end;


    procedure tarmaddnode.second_cmpfloat(ctx:tpassgeneratecodecontext);
      var
        op: TAsmOp;
        pf: TOpPostfix;
      begin
        pass_left_right(ctx);
        if (nf_swapped in flags) then
          swapleftright;

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(false);

        case compiler.globals.current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              { force fpureg as location, left right doesn't matter
                as both will be in a fpureg }
              ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,right.location,right.resultdef,true);

              ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
              if nodetype in [equaln,unequaln] then
                ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMF,
                   left.location.register,right.location.register),
                   cgsize2fpuoppostfix[def_cgsize(resultdef)]))
              else
                ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_CMFE,
                   left.location.register,right.location.register),
                   cgsize2fpuoppostfix[def_cgsize(resultdef)]));
            end;
          else if FPUARM_HAS_VFP_DOUBLE in fpu_capabilities[compiler.globals.current_settings.fputype] then
            begin
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,right.location,right.resultdef,true);

              if nodetype in [equaln,unequaln] then
                op:=A_VCMP
              else
                op:=A_VCMPE;

              if (tfloatdef(left.resultdef).floattype=s32real) then
                pf:=PF_F32
              else
                pf:=PF_F64;

              ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(op,
                left.location.register,right.location.register), pf));
              ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
              ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
              ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_VMRS,NR_APSR_nzcv,NR_FPSCR));
              location.resflags:=GetFpuResFlags;
            end
          else if FPUARM_HAS_VFP_EXTENSION in fpu_capabilities[compiler.globals.current_settings.fputype] then
            begin
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,left.location,left.resultdef,true);
              ctx.hlcg.location_force_mmregscalar(ctx.CurrAsmList,right.location,right.resultdef,true);

              if nodetype in [equaln,unequaln] then
                op:=A_VCMP
              else
                op:=A_VCMPE;

              ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(op,
                left.location.register,right.location.register),PF_F32));
              ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
              ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
              ctx.CurrAsmList.Concat(taicpu.op_reg_reg(A_VMRS, NR_APSR_nzcv, NR_FPSCR));
            end
          else
            { this case should be handled already by pass1 }
            internalerror(2009112404);
        end;
      end;


    procedure tarmaddnode.second_cmpsmallset(ctx:tpassgeneratecodecontext);
      var
        tmpreg : tregister;
        b: byte;
      begin
        pass_left_right(ctx);

        location_reset(location,LOC_FLAGS,OS_NO);

        if (not(nf_swapped in flags) and
            (nodetype = lten)) or
           ((nf_swapped in flags) and
            (nodetype = gten)) then
          swapleftright;

        (* Try to keep right as a constant *)
        if (right.location.loc <> LOC_CONSTANT) or
          not(is_shifter_const(right.location.value, b)) or
          ((GenerateThumbCode) and not(is_thumb_imm(right.location.value))) then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
        ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

        case nodetype of
          equaln,
          unequaln:
            begin
              ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
              if right.location.loc = LOC_CONSTANT then
                ctx.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,left.location.register,right.location.value))
              else
                ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              if nodetype = equaln then
                location.resflags:=F_EQ
              else
                location.resflags:=F_NE;
            end;
          lten,
          gten:
            begin
              tmpreg:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);
              if right.location.loc = LOC_CONSTANT then
                begin
                  ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_AND,OS_32,right.location.value,left.location.register,tmpreg);
                  ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                  ctx.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,tmpreg,right.location.value));
                end
              else
                begin
                  ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_AND,OS_32,left.location.register,right.location.register,tmpreg);
                  ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                  ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,tmpreg,right.location.register));
                end;
              location.resflags:=F_EQ;
            end;
          else
            internalerror(2004012401);
        end;
      end;


    procedure tarmaddnode.second_cmp64bit(ctx:tpassgeneratecodecontext);
      var
        unsigned : boolean;
        oldnodetype : tnodetype;
        dummyreg : tregister;
        truelabel, falselabel: tasmlabel;
        l: tasmlabel;
      const
        lt_zero_swapped: array[boolean] of tnodetype = (ltn, gtn);
      begin
        truelabel:=nil;
        falselabel:=nil;
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        pass_left_right(ctx);

        { pass_left_right moves possible consts to the right, the only
          remaining case with left consts (currency) can take this path too (KB) }
        if (right.nodetype=ordconstn) and
           (tordconstnode(right).value=0) and
           ((nodetype in [equaln,unequaln]) or
            (not(GenerateThumbCode) and is_signed(left.resultdef) and (nodetype = lt_zero_swapped[nf_swapped in Flags]))
           ) then
          begin
            location_reset(location,LOC_FLAGS,OS_NO);
            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

            ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
            { Optimize for the common case of int64 < 0 }
            if nodetype in [ltn, gtn] then
              begin
                {Just check for the MSB in reghi to be set or not, this is independent from nf_swapped}
                location.resflags:=F_NE;
                ctx.CurrAsmList.concat(taicpu.op_reg_const(A_TST,left.location.register64.reghi, aint($80000000)));
              end
            else
              begin
                location.resflags:=getresflags(unsigned);
                dummyreg:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);

                if GenerateThumbCode then
                  ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,left.location.register64.reglo,left.location.register64.reghi,dummyreg)
                else
                  ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg_reg(A_ORR,dummyreg,left.location.register64.reglo,left.location.register64.reghi),PF_S));
              end;
          end
        else
          begin
            ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,true);

            { operation requiring proper N, Z and C flags ? }
            if unsigned or (nodetype in [equaln,unequaln]) then
              begin
                location_reset(location,LOC_FLAGS,OS_NO);
                location.resflags:=getresflags(unsigned);
                ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi));
                if GenerateThumbCode or GenerateThumb2Code then
                  begin
                    current_asmdata.getjumplabel(l);
                    ctx.cg.a_jmp_flags(ctx.CurrAsmList,F_NE,l);
                    ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
                    ctx.cg.a_label(ctx.CurrAsmList,l);
                  end
                else
                  ctx.CurrAsmList.concat(setcondition(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo),C_EQ));
              end
            else
            { operation requiring proper N, Z and V flags ? }
              begin
                current_asmdata.getjumplabel(truelabel);
                current_asmdata.getjumplabel(falselabel);
                location_reset_jump(location,truelabel,falselabel);
                ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi));
                { the jump the sequence is a little bit hairy }
                case nodetype of
                   ltn,gtn:
                     begin
                        ctx.cg.a_jmp_flags(ctx.CurrAsmList,getresflags(false),location.truelabel);
                        { cheat a little bit for the negative test }
                        toggleflag(nf_swapped);
                        ctx.cg.a_jmp_flags(ctx.CurrAsmList,getresflags(false),location.falselabel);
                        ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                        toggleflag(nf_swapped);
                     end;
                   lten,gten:
                     begin
                        oldnodetype:=nodetype;
                        if nodetype=lten then
                          nodetype:=ltn
                        else
                          nodetype:=gtn;
                        ctx.cg.a_jmp_flags(ctx.CurrAsmList,getresflags(unsigned),location.truelabel);
                        { cheat for the negative test }
                        if nodetype=ltn then
                          nodetype:=gtn
                        else
                          nodetype:=ltn;
                        ctx.cg.a_jmp_flags(ctx.CurrAsmList,getresflags(unsigned),location.falselabel);
                        ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                        nodetype:=oldnodetype;
                     end;
                   else
                     ;
                end;
                ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
                { the comparison of the low dword have to be
                   always unsigned!                            }
                ctx.cg.a_jmp_flags(ctx.CurrAsmList,getresflags(true),location.truelabel);
                ctx.cg.a_jmp_always(ctx.CurrAsmList,location.falselabel);
                ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
              end;
          end;
      end;

    procedure tarmaddnode.second_add64bit(ctx:tpassgeneratecodecontext);
      var
        asmList : TAsmList;
        ll,rl,res : TRegister64;
        tmpreg: TRegister;
      begin
        if (nodetype in [muln]) then
          begin
            asmList := ctx.CurrAsmList;
            pass_left_right(ctx);
            force_reg_left_right(true, (left.location.loc<>LOC_CONSTANT) and (right.location.loc<>LOC_CONSTANT), ctx);
            set_result_location_reg(ctx);

            { shortcuts to register64s }
            ll:=left.location.register64;
            rl:=right.location.register64;
            res:=location.register64;

            tmpreg := ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
            asmList.concat(taicpu.op_reg_reg_reg(A_MUL,tmpreg,ll.reglo,rl.reghi));
            asmList.concat(taicpu.op_reg_reg_reg_reg(A_UMULL,res.reglo,res.reghi,rl.reglo,ll.reglo));
            tbasecgarm(ctx.cg).safe_mla(asmList,tmpreg,rl.reglo,ll.reghi,tmpreg);
            asmList.concat(taicpu.op_reg_reg_reg(A_ADD,res.reghi,tmpreg,res.reghi));
          end
        else
          inherited;
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
            if (left.resultdef.typ=floatdef) and
              ([FPUARM_HAS_VFP_EXTENSION,FPUARM_HAS_VFP_DOUBLE]*fpu_capabilities[compiler.globals.current_settings.fputype]<>[]) and
              compiler.globals.needs_check_for_fpu_exceptions then
              Include(compiler.current_procinfo.flags,pi_do_call);
          end;
      end;


    function tarmaddnode.first_addfloat: tnode;
      begin
        result := nil;

        if (FPUARM_HAS_VFP_EXTENSION in fpu_capabilities[compiler.globals.current_settings.fputype]) and
           not(FPUARM_HAS_VFP_DOUBLE in fpu_capabilities[compiler.globals.current_settings.fputype]) then
          begin
            case tfloatdef(left.resultdef).floattype of
              s32real:
                ;
              s64real:
                result:=first_addfloat_soft;
              else
                internalerror(2019050933);
            end;
          end
        else
          result:=inherited first_addfloat;
      end;


    procedure tarmaddnode.second_cmpordinal(ctx:tpassgeneratecodecontext);
      var
        unsigned : boolean;
        tmpreg : tregister;
        b : byte;
      begin
        pass_left_right(ctx);
        force_reg_left_right(true,true,ctx);

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));
        ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
        if right.location.loc = LOC_CONSTANT then
          begin
             if (not(GenerateThumbCode) and is_shifter_const(right.location.value,b)) or
                ((GenerateThumbCode) and is_thumb_imm(right.location.value)) then
               ctx.CurrAsmList.concat(taicpu.op_reg_const(A_CMP,left.location.register,right.location.value))
             else
               begin
                 tmpreg:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);
                 ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,
                   right.location.value,tmpreg);
                 ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,tmpreg));
               end;
          end
        else
          ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);
      end;

    const
      multops: array[boolean] of TAsmOp = (A_SMULL, A_UMULL);

    procedure tarmaddnode.second_addordinal(ctx:tpassgeneratecodecontext);
      var
        unsigned: boolean;
      begin
        if (nodetype=muln) and
           is_64bit(resultdef) and
           not(GenerateThumbCode) and
           (CPUARM_HAS_UMULL in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]) then
          begin
            pass_left_right(ctx);
            force_reg_left_right(true, false, ctx);
            set_result_location_reg(ctx);
            unsigned:=not(is_signed(left.resultdef)) or
                      not(is_signed(right.resultdef));
            ctx.CurrAsmList.Concat(
              taicpu.op_reg_reg_reg_reg(multops[unsigned], location.register64.reglo, location.register64.reghi,
                                        left.location.register,right.location.register));
          end
        else
          inherited;
      end;

    function tarmaddnode.use_generic_mul32to64: boolean;
      begin
        result:=GenerateThumbCode or not(CPUARM_HAS_UMULL in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]);
      end;

    function tarmaddnode.use_generic_mul64bit: boolean;
      begin
        result:=GenerateThumbCode or
          not(CPUARM_HAS_UMULL in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]) or
          needoverflowcheck;
      end;

begin
  caddnode:=tarmaddnode;
end.
