{
    Copyright (c) 2000-2002 by the FPC development team

    Code generation for add nodes (generic version)

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
unit ncgadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,cgbase;

    type
       tcgaddnode = class(taddnode)
{          function pass_1: tnode; override;}
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
         protected
          { call secondpass for both left and right }
          procedure pass_left_right(ctx:tpassgeneratecodecontext); virtual;
          { set the register of the result location }
          procedure set_result_location_reg(ctx:tpassgeneratecodecontext);
          { load left and right nodes into registers }
          procedure force_reg_left_right(allow_swap,allow_constant:boolean;ctx:tpassgeneratecodecontext); virtual;

          function cmpnode2topcmp(unsigned: boolean): TOpCmp;

          procedure second_opfloat(ctx:tpassgeneratecodecontext);
          procedure second_opboolean(ctx:tpassgeneratecodecontext);
          procedure second_opsmallset(ctx:tpassgeneratecodecontext);
          procedure second_op64bit(ctx:tpassgeneratecodecontext);
          procedure second_opordinal(ctx:tpassgeneratecodecontext);

          procedure second_addstring(ctx:tpassgeneratecodecontext);virtual;
          procedure second_addfloat(ctx:tpassgeneratecodecontext);virtual;abstract;
          procedure second_addboolean(ctx:tpassgeneratecodecontext);virtual;
          procedure second_addsmallset(ctx:tpassgeneratecodecontext);virtual;
          procedure second_addsmallsetelement(ctx:tpassgeneratecodecontext);virtual;
{$ifdef x86}
{$ifdef SUPPORT_MMX}
          procedure second_opmmx(ctx:tpassgeneratecodecontext);virtual;abstract;
{$endif SUPPORT_MMX}
{$endif x86}
          procedure second_opvector(ctx:tpassgeneratecodecontext);virtual;abstract;
          procedure second_add64bit(ctx:tpassgeneratecodecontext);virtual;
          procedure second_addordinal(ctx:tpassgeneratecodecontext);virtual;
          procedure second_cmpfloat(ctx:tpassgeneratecodecontext);virtual;abstract;
          procedure second_cmpboolean(ctx:tpassgeneratecodecontext);virtual;
          procedure second_cmpsmallset(ctx:tpassgeneratecodecontext);virtual;abstract;
          procedure second_cmp64bit(ctx:tpassgeneratecodecontext);virtual;abstract;
          procedure second_cmpordinal(ctx:tpassgeneratecodecontext);virtual;abstract;

          function needoverflowcheck: boolean;
       end;

  implementation

    uses
      globtype,systemstypes,systems,compiler,
      verbose,globals,
      symconst,symdef,
      aasmbase,aasmdata,defutil,
      pass_2,pass_2_context,tgobj,
      nutils,nset,ncgutil,cgobj,cgutils,
      nodehelper
      ;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure tcgaddnode.pass_left_right(ctx:tpassgeneratecodecontext);
{$if defined(x86) and not defined(llvm)}
      var
        tmpreg     : tregister;
        pushedfpu  : boolean;
{$endif x86 and not llvm}
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
          swapleftright;

        secondpass(left,ctx);
        if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,resultdef,false);
{$if defined(x86) and not defined(llvm)}
        { are too few registers free? }
        pushedfpu:=false;
        if (left.location.loc=LOC_FPUREGISTER) and
           (node_resources_fpu(right)>=maxfpuregs) then
          begin
            ctx.hlcg.location_force_mem(ctx.CurrAsmList,left.location,left.resultdef);
            pushedfpu:=true;
          end;
{$endif x86 and not llvm}

        secondpass(right,ctx);
        if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,resultdef,false);
{$if defined(x86) and not defined(llvm)}
        if pushedfpu then
          begin
            if use_vectorfpu(left.resultdef) then
              begin
                tmpreg := ctx.cg.getmmregister(ctx.CurrAsmList,left.location.size);
                ctx.hlcg.a_loadmm_loc_reg(ctx.CurrAsmList,left.resultdef,left.resultdef,left.location,tmpreg,mms_movescalar);
                ctx.tg.location_freetemp(ctx.CurrAsmList,left.location);
                location_reset(left.location,LOC_MMREGISTER,left.location.size);
                left.location.register:=tmpreg;
              end
            else
              begin
                tmpreg := ctx.cg.getfpuregister(ctx.CurrAsmList,left.location.size);
                ctx.cg.a_loadfpu_loc_reg(ctx.CurrAsmList,left.location.size,left.location,tmpreg);
                ctx.tg.location_freetemp(ctx.CurrAsmList,left.location);
                location_reset(left.location,LOC_FPUREGISTER,left.location.size);
                left.location.register := tmpreg;
                { left operand is now on top of the stack, instead of the right one! }
                if (right.location.loc=LOC_FPUREGISTER) then
                  toggleflag(nf_swapped);
              end;
          end;
{$endif x86 and not llvm}
      end;


    procedure tcgaddnode.set_result_location_reg(ctx:tpassgeneratecodecontext);
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
        if location.size in [OS_64,OS_S64] then
          begin
            location.register64.reglo := ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
            location.register64.reghi := ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
          end
        else
{$endif}
          location.register := ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);
      end;


    procedure tcgaddnode.force_reg_left_right(allow_swap,allow_constant:boolean;ctx:tpassgeneratecodecontext);
      begin
        if (left.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (left.location.loc in [LOC_CONSTANT,LOC_CREGISTER])
              ) then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
        if (right.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (right.location.loc in [LOC_CONSTANT,LOC_CREGISTER]) and
               (left.location.loc<>LOC_CONSTANT)
              ) then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,false);

        { Left is always a register, right can be register or constant }
        if left.location.loc=LOC_CONSTANT then
          begin
            { when it is not allowed to swap we have a constant on
              left, that will give problems }
            if not allow_swap then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false)
            else
              swapleftright;
          end;
      end;


    function tcgaddnode.cmpnode2topcmp(unsigned: boolean): TOpCmp;
      begin
        if unsigned then
          case nodetype of
            gtn:      result:=OC_A;
            gten:     result:=OC_AE;
            ltn:      result:=OC_B;
            lten:     result:=OC_BE;
            equaln:   result:=OC_EQ;
            unequaln: result:=OC_NE;
          else
            internalerror(2011010412);
          end
        else
          case nodetype of
            gtn:      result:=OC_GT;
            gten:     result:=OC_GTE;
            ltn:      result:=OC_LT;
            lten:     result:=OC_LTE;
            equaln:   result:=OC_EQ;
            unequaln: result:=OC_NE;
          else
            internalerror(2011010403);
          end
      end;

{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure tcgaddnode.second_opsmallset(ctx:tpassgeneratecodecontext);
      begin
        { when a setdef is passed, it has to be a smallset }
        if not(
               ((left.nodetype=setelementn) or is_smallset(left.resultdef)) and
               ((right.nodetype=setelementn) or is_smallset(right.resultdef))
              ) then
          internalerror(200203302);
        if (left.nodetype=setelementn) or (right.nodetype=setelementn) then
          second_addsmallsetelement(ctx)
        else if nodetype in [equaln,unequaln,gtn,gten,lten,ltn] then
          second_cmpsmallset(ctx)
        else
          second_addsmallset(ctx);
      end;


    procedure tcgaddnode.second_addsmallset(ctx:tpassgeneratecodecontext);
      var
        cgop    : TOpCg;
        opdone  : boolean;
      begin
        opdone := false;
        pass_left_right(ctx);
        force_reg_left_right(true,true,ctx);
        set_result_location_reg(ctx);
        case nodetype of
          addn :
            cgop:=OP_OR;
          symdifn :
            cgop:=OP_XOR;
          muln :
            cgop:=OP_AND;
          subn :
            begin
              cgop:=OP_AND;
              if (not(nf_swapped in flags)) then
                if (right.location.loc=LOC_CONSTANT) then
                  right.location.value := not(right.location.value)
                else
                  opdone := true
              else if (left.location.loc=LOC_CONSTANT) then
                left.location.value := not(left.location.value)
              else
                 begin
                   swapleftright;
                   opdone := true;
                 end;
              if opdone then
                begin
                  if (right.location.size<>left.location.size) or
                     (location.size<>left.location.size) then
                    internalerror(2010123001);
                  { make sure that location.register is different from
                    left.location.register, since right will overwrite it
                    and we'll use left afterwards }
                  if (right.location.loc=LOC_REGISTER) then
                    location.register:=right.location.register
                  else
                    location.register:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);
                  { make sure we don't modify left/right.location, because we told
                    force_reg_left_right above that they can be constant }
                  ctx.hlcg.a_op_reg_reg(ctx.CurrAsmList,OP_NOT,resultdef,right.location.register,location.register);
                  if left.location.loc = LOC_CONSTANT then
                    ctx.hlcg.a_op_const_reg(ctx.CurrAsmList,OP_AND,resultdef,left.location.value,location.register)
                  else
                    ctx.hlcg.a_op_reg_reg(ctx.CurrAsmList,OP_AND,resultdef,left.location.register,location.register);
                end;
            end;
          else
            internalerror(2002072701);
        end;

        if not opdone then
          begin
            // these are all commutative operations
            if (left.location.loc = LOC_CONSTANT) then
              swapleftright;
            if (right.location.loc = LOC_CONSTANT) then
              ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,cgop,resultdef,
                right.location.value,left.location.register,
                location.register)
            else
              ctx.hlcg.a_op_reg_reg_reg(ctx.CurrAsmList,cgop,resultdef,
                right.location.register,left.location.register,
                location.register);
          end;
      end;


    procedure tcgaddnode.second_addsmallsetelement(ctx:tpassgeneratecodecontext);
      var
        tmpreg : tregister;
        mask,
        setbase : aint;
        cgop    : TOpCg;
      begin
        if nodetype<>addn then
          internalerror(20080302);
        { no range support for smallsets }
        if assigned(tsetelementnode(right).right) then
          internalerror(20080303);
        pass_left_right(ctx);
        { setelementn is a special case, it must be on right }
        if (nf_swapped in flags) and
           (left.nodetype=setelementn) then
          swapleftright;
        force_reg_left_right(false,false,ctx);
        set_result_location_reg(ctx);
        setbase:=tsetdef(left.resultdef).setbase;
        if (right.location.loc = LOC_CONSTANT) then
          begin
            if (compiler.target.info.endian=endian_big) then
              mask:=aint((aword(1) shl (resultdef.size*8-1)) shr aword(right.location.value-setbase))
            else
              mask:=aint(1 shl (right.location.value-setbase));
            ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,OP_OR,resultdef,
              mask,left.location.register,location.register);
          end
        else
          begin
            if (compiler.target.info.endian=endian_big) then
              begin
                mask:=aint((aword(1) shl (resultdef.size*8-1)));
                cgop:=OP_SHR
              end
            else
              begin
                mask:=1;
                cgop:=OP_SHL
              end;
            tmpreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);
            ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,resultdef,mask,tmpreg);
            ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,resultdef,true);
            register_maybe_adjust_setbase(ctx.hlcg,ctx.CurrAsmList,resultdef,right.location,setbase);
            ctx.hlcg.a_op_reg_reg(ctx.CurrAsmList,cgop,resultdef,
              right.location.register,tmpreg);
            if left.location.loc <> LOC_CONSTANT then
              ctx.hlcg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,resultdef,tmpreg,
                  left.location.register,location.register)
            else
              ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,OP_OR,resultdef,
                  left.location.value,tmpreg,location.register);
          end;
      end;


{*****************************************************************************
                                Boolean
*****************************************************************************}

    procedure tcgaddnode.second_opboolean(ctx:tpassgeneratecodecontext);
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmpboolean(ctx)
        else
          second_addboolean(ctx);
      end;


    procedure tcgaddnode.second_addboolean(ctx:tpassgeneratecodecontext);
      var
        cgop    : TOpCg;
        truelabel, falselabel : tasmlabel;
        oldflowcontrol : tflowcontrol;
      begin
        { And,Or will only evaluate from left to right only the
          needed nodes unless full boolean evaluation is enabled }
        if (nodetype in [orn,andn]) and
           (not(cs_full_boolean_eval in compiler.globals.current_settings.localswitches) or
            (anf_short_bool in addnodeflags)) then
          begin
            case nodetype of
              andn :
                begin
                   secondpass(left,ctx);
                   ctx.hlcg.maketojumpbool(ctx.CurrAsmList,left);
                   ctx.hlcg.a_label(ctx.CurrAsmList,left.location.truelabel);
                   ctx.CurrAsmList.AsmData.getjumplabel(truelabel);
                   location_reset_jump(location,truelabel,left.location.falselabel);
                end;
              orn :
                begin
                   secondpass(left,ctx);
                   ctx.hlcg.maketojumpbool(ctx.CurrAsmList,left);
                   ctx.hlcg.a_label(ctx.CurrAsmList,left.location.falselabel);
                   ctx.CurrAsmList.AsmData.getjumplabel(falselabel);
                   location_reset_jump(location,left.location.truelabel,falselabel);
                end;
              else
                internalerror(200307044);
            end;
            { these jumps mean we're now in a flow control construct }
            oldflowcontrol:=flowcontrol;
            include(flowcontrol,fc_inflowcontrol);

            secondpass(right,ctx);
            { jump to the same labels as the left side, since the andn/orn
              merges the results of left and right }
            ctx.hlcg.maketojumpboollabels(ctx.CurrAsmList,right,location.truelabel,location.falselabel);

            flowcontrol:=oldflowcontrol+(flowcontrol-[fc_inflowcontrol]);
          end
        else
          begin
            pass_left_right(ctx);
            force_reg_left_right(false,true,ctx);
            set_result_location_reg(ctx);

            case nodetype of
              xorn :
                cgop:=OP_XOR;
              orn :
                cgop:=OP_OR;
              andn :
                cgop:=OP_AND;
              else
                 internalerror(200203247);
            end;
{$if not defined(cpu64bitalu) and not defined(cpuhighleveltarget)}
            if right.location.size in [OS_64,OS_S64] then
              begin
                if right.location.loc <> LOC_CONSTANT then
                  ctx.cg64.a_op64_reg_reg_reg(ctx.CurrAsmList,cgop,location.size,
                     left.location.register64,right.location.register64,
                     location.register64)
                else
                  ctx.cg64.a_op64_const_reg_reg(ctx.CurrAsmList,cgop,location.size,
                     right.location.value,left.location.register64,
                     location.register64);
              end
            else
{$endif cpu64bitalu}
              begin
                if right.location.loc <> LOC_CONSTANT then
                  ctx.hlcg.a_op_reg_reg_reg(ctx.CurrAsmList,cgop,resultdef,
                     left.location.register,right.location.register,
                     location.register)
                else
                  ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,cgop,resultdef,
                     right.location.value,left.location.register,
                     location.register);
              end;
         end;
      end;


{*****************************************************************************
                                64-bit
*****************************************************************************}

    procedure tcgaddnode.second_op64bit(ctx:tpassgeneratecodecontext);
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmp64bit(ctx)
        else
          second_add64bit(ctx);
      end;


    procedure tcgaddnode.second_add64bit(ctx:tpassgeneratecodecontext);
      var
        op         : TOpCG;
        checkoverflow : boolean;
        ovloc : tlocation;
      begin
        ovloc.loc:=LOC_VOID;

        pass_left_right(ctx);
        force_reg_left_right(false,true,ctx);
        set_result_location_reg(ctx);

        { assume no overflow checking is required }
        checkoverflow := false;
        case nodetype of
          addn :
             begin
                op:=OP_ADD;
                checkoverflow:=true;
             end;
          subn :
             begin
                op:=OP_SUB;
                checkoverflow:=true;
             end;
          xorn:
            op:=OP_XOR;
          orn:
            op:=OP_OR;
          andn:
            op:=OP_AND;
          muln:
            begin
              { should be handled in pass_1 (JM) }
              internalerror(200109051);
            end;
          else
            internalerror(2002072705);
        end;

        checkoverflow:=
          checkoverflow and
          needoverflowcheck;

{$if defined(cpu64bitalu) or defined(cpuhighleveltarget)}
        case nodetype of
          xorn,orn,andn,addn:
            begin
              if (right.location.loc = LOC_CONSTANT) then
                ctx.hlcg.a_op_const_reg_reg(ctx.CurrAsmList,op,resultdef,right.location.value,
                  left.location.register,location.register)
              else
                ctx.hlcg.a_op_reg_reg_reg(ctx.CurrAsmList,op,resultdef,right.location.register,
                  left.location.register,location.register);
            end;
          subn:
            begin
              if (nf_swapped in flags) then
                swapleftright;

              if left.location.loc <> LOC_CONSTANT then
                begin
                  if right.location.loc <> LOC_CONSTANT then
                    // reg64 - reg64
                    ctx.hlcg.a_op_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                      right.location.register,left.location.register,location.register,
                      checkoverflow,ovloc)
                  else
                    // reg64 - const64
                    ctx.hlcg.a_op_const_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                      right.location.value,left.location.register,location.register,
                      checkoverflow,ovloc);
                end
              else
                begin
                  // const64 - reg64
                  ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
                  ctx.hlcg.a_op_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                    right.location.register,left.location.register,location.register,
                    checkoverflow,ovloc);
                end;
            end;
          else
            internalerror(2002072803);
        end;
{$else cpu64bitalu or cpuhighleveltarget}
        case nodetype of
          xorn,orn,andn,addn:
            begin
              if (right.location.loc = LOC_CONSTANT) then
                ctx.cg64.a_op64_const_reg_reg_checkoverflow(ctx.CurrAsmList,op,location.size,right.location.value64,
                  left.location.register64,location.register64,
                  checkoverflow,ovloc)
              else
                ctx.cg64.a_op64_reg_reg_reg_checkoverflow(ctx.CurrAsmList,op,location.size,right.location.register64,
                  left.location.register64,location.register64,
                  checkoverflow,ovloc);
            end;
          subn:
            begin
              if (nf_swapped in flags) then
                swapleftright;

              if left.location.loc <> LOC_CONSTANT then
                begin
                  if right.location.loc <> LOC_CONSTANT then
                    // reg64 - reg64
                    ctx.cg64.a_op64_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,location.size,
                      right.location.register64,left.location.register64,
                      location.register64,
                      checkoverflow,ovloc)
                  else
                    // reg64 - const64
                    ctx.cg64.a_op64_const_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,location.size,
                      right.location.value64,left.location.register64,
                      location.register64,
                      checkoverflow,ovloc)
                end
              else
                begin
                  // const64 - reg64
                  ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
                  ctx.cg64.a_op64_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,location.size,
                    right.location.register64,left.location.register64,
                    location.register64,
                    checkoverflow,ovloc);
                end;
            end;
          else
            internalerror(2002072804);
        end;
{$endif cpu64bitalu or cpuhighleveltarget}

        { emit overflow check if enabled }
        if checkoverflow then
           ctx.hlcg.g_overflowcheck_loc(ctx.CurrAsmList,Location,resultdef,ovloc);
      end;


{*****************************************************************************
                                Strings
*****************************************************************************}

    procedure tcgaddnode.second_addstring(ctx:tpassgeneratecodecontext);
      begin
        { this should already be handled in pass1 }
        internalerror(2002072402);
      end;


{*****************************************************************************
                                Floats
*****************************************************************************}

    procedure tcgaddnode.second_opfloat(ctx:tpassgeneratecodecontext);
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmpfloat(ctx)
        else
          second_addfloat(ctx);
      end;


{*****************************************************************************
                                Ordinals
*****************************************************************************}

    procedure tcgaddnode.second_opordinal(ctx:tpassgeneratecodecontext);
      begin
         if (nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) then
           second_cmpordinal(ctx)
         else
           second_addordinal(ctx);
      end;


    procedure tcgaddnode.second_addordinal(ctx:tpassgeneratecodecontext);
      var
        unsigned,
        checkoverflow : boolean;
        cgop   : topcg;
        tmpreg : tregister;
        ovloc : tlocation;
      begin
        ovloc.loc:=LOC_VOID;

        pass_left_right(ctx);
        force_reg_left_right(true,true,ctx);
        set_result_location_reg(ctx);

        { determine if the comparison will be unsigned }
        unsigned:=not(is_signed(left.resultdef)) or
                    not(is_signed(right.resultdef));

        { assume no overflow checking is require }
        checkoverflow := false;

        case nodetype of
          addn:
            begin
              cgop:=OP_ADD;
              checkoverflow:=true;
            end;
          xorn :
            begin
              cgop:=OP_XOR;
            end;
          orn :
            begin
              cgop:=OP_OR;
            end;
          andn:
            begin
              cgop:=OP_AND;
            end;
          muln:
            begin
              checkoverflow:=true;
              if unsigned then
                cgop:=OP_MUL
              else
                cgop:=OP_IMUL;
            end;
          subn :
            begin
              checkoverflow:=true;
              cgop:=OP_SUB;
            end;
          else
            internalerror(2013120104);
        end;

       checkoverflow:=
         checkoverflow and
          (left.resultdef.typ<>pointerdef) and
          (right.resultdef.typ<>pointerdef) and
          (cs_check_overflow in compiler.globals.current_settings.localswitches) and not(nf_internal in flags);

       if nodetype<>subn then
        begin
          if (right.location.loc<>LOC_CONSTANT) then
            ctx.hlcg.a_op_reg_reg_reg_checkoverflow(ctx.CurrAsmList,cgop,resultdef,
               left.location.register,right.location.register,
               location.register,checkoverflow,ovloc)
          else
            ctx.hlcg.a_op_const_reg_reg_checkoverflow(ctx.CurrAsmList,cgop,resultdef,
               right.location.value,left.location.register,
               location.register,checkoverflow,ovloc);
        end
      else  { subtract is a special case since its not commutative }
        begin
          if (nf_swapped in flags) then
            swapleftright;
          if left.location.loc<>LOC_CONSTANT then
            begin
              if right.location.loc<>LOC_CONSTANT then
                ctx.hlcg.a_op_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                    right.location.register,left.location.register,
                    location.register,checkoverflow,ovloc)
              else
                ctx.hlcg.a_op_const_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                  right.location.value,left.location.register,
                  location.register,checkoverflow,ovloc);
            end
          else
            begin
              tmpreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,resultdef);
              ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,resultdef,
                left.location.value,tmpreg);
              ctx.hlcg.a_op_reg_reg_reg_checkoverflow(ctx.CurrAsmList,OP_SUB,resultdef,
                right.location.register,tmpreg,location.register,checkoverflow,ovloc);
            end;
        end;

        { emit overflow check if required }
        if checkoverflow then
          ctx.hlcg.g_overflowcheck_loc(ctx.CurrAsmList,Location,resultdef,ovloc);
      end;


    procedure tcgaddnode.second_cmpboolean(ctx:tpassgeneratecodecontext);
      begin
        second_cmpordinal(ctx);
      end;

    function tcgaddnode.needoverflowcheck: boolean;
      begin
        result:=
          (cs_check_overflow in compiler.globals.current_settings.localswitches) and
          (left.resultdef.typ<>pointerdef) and
          (right.resultdef.typ<>pointerdef) and
          not(nf_internal in flags);
      end;


{*****************************************************************************
                                pass_generate_code;
*****************************************************************************}

    procedure tcgaddnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      begin
        case left.resultdef.typ of
          orddef :
            begin
              { handling boolean expressions }
              if is_boolean(left.resultdef) and
                 is_boolean(right.resultdef) then
                second_opboolean(ctx)
{$ifndef cpu64bitalu}
              { 64bit operations }
              else if is_64bit(left.resultdef) then
                second_op64bit(ctx)
{$endif cpu64bitalu}
              else
                second_opordinal(ctx);
            end;
          stringdef :
            begin
              second_addstring(ctx);
            end;
          setdef :
            begin
              if is_smallset(tsetdef(left.resultdef)) then
                second_opsmallset(ctx)
              else
                internalerror(200109041);
            end;
          arraydef :
            begin
              { support dynarr=nil }
              if is_dynamic_array(left.resultdef) then
                second_opordinal(ctx)
              else
                if (cs_support_vectors in compiler.globals.current_settings.globalswitches) and
                   is_vector(left.resultdef) then
                  second_opvector(ctx)
{$ifdef SUPPORT_MMX}
              else
                if is_mmx_able_array(left.resultdef) then
                  second_opmmx(ctx)
{$endif SUPPORT_MMX}
              else
                internalerror(200306016);
            end;
          floatdef :
            second_opfloat(ctx);
          else
            second_opordinal(ctx);
        end;
      end;

begin
   caddnode:=tcgaddnode;
end.
