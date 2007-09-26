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
       node,nadd,cpubase;

    type
       tcgaddnode = class(taddnode)
{          function pass_1: tnode; override;}
          procedure pass_generate_code;override;
         protected
          { call secondpass for both left and right }
          procedure pass_left_right;
          { set the register of the result location }
          procedure set_result_location_reg;
          { load left and right nodes into registers }
          procedure force_reg_left_right(allow_swap,allow_constant:boolean);

          procedure second_opfloat;
          procedure second_opboolean;
          procedure second_opsmallset;
          procedure second_op64bit;
          procedure second_opordinal;

          procedure second_addstring;virtual;
          procedure second_addfloat;virtual;abstract;
          procedure second_addboolean;virtual;
          procedure second_addsmallset;virtual;
{$ifdef x86}
{$ifdef SUPPORT_MMX}
          procedure second_opmmxset;virtual;abstract;
          procedure second_opmmx;virtual;abstract;
{$endif SUPPORT_MMX}
{$endif x86}
          procedure second_opvector;virtual;abstract;
          procedure second_add64bit;virtual;
          procedure second_addordinal;virtual;
          procedure second_cmpfloat;virtual;abstract;
          procedure second_cmpboolean;virtual;
          procedure second_cmpsmallset;virtual;abstract;
          procedure second_cmp64bit;virtual;abstract;
          procedure second_cmpordinal;virtual;abstract;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,defutil,
      cgbase,procinfo,pass_2,tgobj,
      nutils,ncon,nset,ncgutil,cgobj,cgutils
      ;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure tcgaddnode.pass_left_right;
      var
        tmpreg     : tregister;
        isjump,
        pushedfpu  : boolean;
        otl,ofl    : tasmlabel;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
          swapleftright;

        isjump:=(left.expectloc=LOC_JUMP);
        if isjump then
          begin
             otl:=current_procinfo.CurrTrueLabel;
             current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
             ofl:=current_procinfo.CurrFalseLabel;
             current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
          end;
        secondpass(left);
        if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
          location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(resultdef),false);
        if isjump then
          begin
            current_procinfo.CurrTrueLabel:=otl;
            current_procinfo.CurrFalseLabel:=ofl;
          end;

        { are too few registers free? }
        pushedfpu:=false;
{$ifdef i386}
        if (left.location.loc=LOC_FPUREGISTER) and
           (node_resources_fpu(right)>=maxfpuregs) then
          begin
            location_force_mem(current_asmdata.CurrAsmList,left.location);
            pushedfpu:=true;
          end;
{$endif i386}

        isjump:=(right.expectloc=LOC_JUMP);
        if isjump then
          begin
             otl:=current_procinfo.CurrTrueLabel;
             current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
             ofl:=current_procinfo.CurrFalseLabel;
             current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
          end;
        secondpass(right);
        if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
          location_force_reg(current_asmdata.CurrAsmList,right.location,def_cgsize(resultdef),false);
        if isjump then
          begin
            current_procinfo.CurrTrueLabel:=otl;
            current_procinfo.CurrFalseLabel:=ofl;
          end;
        if pushedfpu then
          begin
{$ifdef x86}
            if use_sse(left.resultdef) then
              begin
                tmpreg := cg.getmmregister(current_asmdata.CurrAsmList,left.location.size);
                cg.a_loadmm_loc_reg(current_asmdata.CurrAsmList,left.location.size,left.location,tmpreg,mms_movescalar);
                location_freetemp(current_asmdata.CurrAsmList,left.location);
                location_reset(left.location,LOC_MMREGISTER,left.location.size);
                left.location.register := tmpreg;
              end
            else
{$endif x86}
              begin
                tmpreg := cg.getfpuregister(current_asmdata.CurrAsmList,left.location.size);
                cg.a_loadfpu_loc_reg(current_asmdata.CurrAsmList,left.location.size,left.location,tmpreg);
                location_freetemp(current_asmdata.CurrAsmList,left.location);
                location_reset(left.location,LOC_FPUREGISTER,left.location.size);
                left.location.register := tmpreg;
{$ifdef x86}
                { left operand is now on top of the stack, instead of the right one! }
                toggleflag(nf_swapped);
{$endif x86}
              end;
          end;
      end;


    procedure tcgaddnode.set_result_location_reg;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$ifdef x86}
        if left.location.loc=LOC_REGISTER then
          begin
            if TCGSize2Size[left.location.size]<>TCGSize2Size[location.size] then
              internalerror(200307041);
{$ifndef cpu64bit}
            if location.size in [OS_64,OS_S64] then
              begin
                location.register64.reglo := left.location.register64.reglo;
                location.register64.reghi := left.location.register64.reghi;
              end
            else
{$endif}
              location.register := left.location.register;
          end
        else
         if right.location.loc=LOC_REGISTER then
          begin
            if TCGSize2Size[right.location.size]<>TCGSize2Size[location.size] then
              internalerror(200307042);
{$ifndef cpu64bit}
            if location.size in [OS_64,OS_S64] then
              begin
                location.register64.reglo := right.location.register64.reglo;
                location.register64.reghi := right.location.register64.reghi;
              end
            else
{$endif}
              location.register := right.location.register;
          end
        else
{$endif}
          begin
{$ifndef cpu64bit}
            if location.size in [OS_64,OS_S64] then
              begin
                location.register64.reglo := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                location.register64.reghi := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              end
            else
{$endif}
            location.register := cg.getintregister(current_asmdata.CurrAsmList,location.size);
          end;
      end;


    procedure tcgaddnode.force_reg_left_right(allow_swap,allow_constant:boolean);
      begin
        if (left.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (left.location.loc in [LOC_CONSTANT,LOC_CREGISTER])
              ) then
          location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,false);
        if (right.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (right.location.loc in [LOC_CONSTANT,LOC_CREGISTER]) and
               (left.location.loc<>LOC_CONSTANT)
              ) then
          location_force_reg(current_asmdata.CurrAsmList,right.location,right.location.size,false);

        { Left is always a register, right can be register or constant }
        if left.location.loc=LOC_CONSTANT then
          begin
            { when it is not allowed to swap we have a constant on
              left, that will give problems }
            if not allow_swap then
              internalerror(200307043);
            swapleftright;
          end;
      end;


{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure tcgaddnode.second_opsmallset;
      begin
        { when a setdef is passed, it has to be a smallset }
        if is_varset(left.resultdef) or
          is_varset(right.resultdef) then
          internalerror(200203302);

        if nodetype in [equaln,unequaln,gtn,gten,lten,ltn] then
          second_cmpsmallset
        else
          second_addsmallset;
      end;


    procedure tcgaddnode.second_addsmallset;
      var
        tmpreg : tregister;
        mask,
        setbase : aint;

        cgop    : TOpCg;
        opdone  : boolean;
      begin
        opdone := false;

        pass_left_right;
        force_reg_left_right(true,true);

        { setelementn is a special case, it must be on right.
          We need an extra check if left is a register because the
          default case can skip the register loading when the
          setelementn is in a register (PFV) }
        if (nf_swapped in flags) and
           (left.nodetype=setelementn) then
          swapleftright;
        if (right.nodetype=setelementn) and
           (left.location.loc<>LOC_REGISTER) then
          location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,false);

        set_result_location_reg;
        if (left.resultdef.typ=setdef) then
          setbase:=tsetdef(left.resultdef).setbase
        else
          setbase:=tsetdef(right.resultdef).setbase;

        case nodetype of
          addn :
            begin
              { are we adding set elements ? }
              if right.nodetype=setelementn then
                begin
                  { no range support for smallsets! }
                  if assigned(tsetelementnode(right).right) then
                   internalerror(43244);
                  if (right.location.loc = LOC_CONSTANT) then
                    begin
                      if (target_info.endian=endian_big) then
                        mask:=aint((aword(1) shl (resultdef.size*8-1)) shr aword(right.location.value-setbase))
                      else
                        mask:=aint(1 shl (right.location.value-setbase));
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_OR,location.size,
                        mask,left.location.register,location.register);
                    end
                  else
                    begin
                      if (target_info.endian=endian_big) then
                        begin
                          mask:=aint((aword(1) shl (resultdef.size*8-1)));
                          cgop:=OP_SHR
                        end
                      else
                        begin
                          mask:=1;
                          cgop:=OP_SHL
                        end;
                      tmpreg := cg.getintregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,mask,tmpreg);
                      location_force_reg(current_asmdata.CurrAsmList,right.location,location.size,true);
                      register_maybe_adjust_setbase(current_asmdata.CurrAsmList,right.location,setbase);
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList,cgop,location.size,
                        right.location.register,tmpreg);
                      if left.location.loc <> LOC_CONSTANT then
                        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,location.size,tmpreg,
                            left.location.register,location.register)
                      else
                        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_OR,location.size,
                            left.location.value,tmpreg,location.register);
                    end;
                  opdone := true;
                end
              else
                cgop := OP_OR;
            end;
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
                  if left.location.loc = LOC_CONSTANT then
                    begin
                      tmpreg := cg.getintregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,
                        left.location.value,tmpreg);
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,location.size,right.location.register,right.location.register);
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_AND,location.size,right.location.register,tmpreg);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,location.size,tmpreg,location.register);
                    end
                  else
                    begin
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,right.location.size,right.location.register,right.location.register);
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_AND,left.location.size,right.location.register,left.location.register);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register);
                    end;
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
              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,cgop,location.size,
                right.location.value,left.location.register,
                location.register)
            else
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,cgop,location.size,
                right.location.register,left.location.register,
                location.register);
          end;
      end;


{*****************************************************************************
                                Boolean
*****************************************************************************}

    procedure tcgaddnode.second_opboolean;
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmpboolean
        else
          second_addboolean;
      end;


    procedure tcgaddnode.second_addboolean;
      var
        cgop    : TOpCg;
        otl,ofl : tasmlabel;
        oldflowcontrol : tflowcontrol;
      begin
        { And,Or will only evaluate from left to right only the
          needed nodes unless full boolean evaluation is enabled }
        if (nodetype in [orn,andn]) and
           (not(cs_full_boolean_eval in current_settings.localswitches) or
            (nf_short_bool in flags)) then
          begin
            location_reset(location,LOC_JUMP,OS_NO);
            case nodetype of
              andn :
                begin
                   otl:=current_procinfo.CurrTrueLabel;
                   current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                   secondpass(left);
                   maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
                   cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                   current_procinfo.CurrTrueLabel:=otl;
                end;
              orn :
                begin
                   ofl:=current_procinfo.CurrFalseLabel;
                   current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
                   secondpass(left);
                   maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
                   cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                   current_procinfo.CurrFalseLabel:=ofl;
                end;
              else
                internalerror(200307044);
            end;
            { these jumps mean we're now in a flow control construct }
            oldflowcontrol:=flowcontrol;
            include(flowcontrol,fc_inflowcontrol);

            secondpass(right);
            maketojumpbool(current_asmdata.CurrAsmList,right,lr_load_regvars);

            flowcontrol:=oldflowcontrol+(flowcontrol-[fc_inflowcontrol]);
          end
        else
          begin
            pass_left_right;
            force_reg_left_right(false,true);
            set_result_location_reg;

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

            if right.location.loc <> LOC_CONSTANT then
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,cgop,location.size,
                 left.location.register,right.location.register,
                 location.register)
            else
              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,cgop,location.size,
                 right.location.value,left.location.register,
                 location.register);
         end;
      end;


{*****************************************************************************
                                64-bit
*****************************************************************************}

    procedure tcgaddnode.second_op64bit;
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmp64bit
        else
          second_add64bit;
      end;


    procedure tcgaddnode.second_add64bit;
      var
        op         : TOpCG;
        checkoverflow : boolean;
        ovloc : tlocation;
      begin
        ovloc.loc:=LOC_VOID;

        pass_left_right;
        force_reg_left_right(false,true);
        set_result_location_reg;

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
          (left.resultdef.typ<>pointerdef) and
          (right.resultdef.typ<>pointerdef);

{$ifdef cpu64bit}
        case nodetype of
          xorn,orn,andn,addn:
            begin
              if (right.location.loc = LOC_CONSTANT) then
                cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,location.size,right.location.value,
                  left.location.register,location.register)
              else
                cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,location.size,right.location.register,
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
                    cg.a_op_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                      right.location.register,left.location.register,location.register,
                      checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
                  else
                    // reg64 - const64
                    cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                      right.location.value,left.location.register,location.register,
                      checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
                end
              else
                begin
                  // const64 - reg64
                  location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,true);
                  cg.a_op_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                    right.location.register,left.location.register,location.register,
                    checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
                end;
            end;
          else
            internalerror(2002072803);
        end;
{$else cpu64bit}
        case nodetype of
          xorn,orn,andn,addn:
            begin
              if (right.location.loc = LOC_CONSTANT) then
                cg64.a_op64_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,op,location.size,right.location.value64,
                  left.location.register64,location.register64,
                  checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
              else
                cg64.a_op64_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,op,location.size,right.location.register64,
                  left.location.register64,location.register64,
                  checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
            end;
          subn:
            begin
              if (nf_swapped in flags) then
                swapleftright;

              if left.location.loc <> LOC_CONSTANT then
                begin
                  if right.location.loc <> LOC_CONSTANT then
                    // reg64 - reg64
                    cg64.a_op64_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                      right.location.register64,left.location.register64,
                      location.register64,
                      checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
                  else
                    // reg64 - const64
                    cg64.a_op64_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                      right.location.value64,left.location.register64,
                      location.register64,
                      checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
                end
              else
                begin
                  // const64 - reg64
                  location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,true);
                  cg64.a_op64_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                    right.location.register64,left.location.register64,
                    location.register64,
                    checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
                end;
            end;
          else
            internalerror(2002072803);
        end;
{$endif cpu64bit}

        { emit overflow check if enabled }
        if checkoverflow then
           cg.g_overflowcheck_loc(current_asmdata.CurrAsmList,Location,resultdef,ovloc);
      end;


{*****************************************************************************
                                Strings
*****************************************************************************}

    procedure tcgaddnode.second_addstring;
      begin
        { this should already be handled in pass1 }
        internalerror(2002072402);
      end;


{*****************************************************************************
                                Floats
*****************************************************************************}

    procedure tcgaddnode.second_opfloat;
      begin
        if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
          second_cmpfloat
        else
          second_addfloat;
      end;


{*****************************************************************************
                                Ordinals
*****************************************************************************}

    procedure tcgaddnode.second_opordinal;
      begin
         if (nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) then
           second_cmpordinal
         else
           second_addordinal;
      end;


    procedure tcgaddnode.second_addordinal;
      var
        unsigned,
        checkoverflow : boolean;
        cgop   : topcg;
        tmpreg : tregister;
        ovloc : tlocation;
      begin
        ovloc.loc:=LOC_VOID;

        pass_left_right;
        force_reg_left_right(false,true);
        set_result_location_reg;

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
        end;

       checkoverflow:=
         checkoverflow and
          (left.resultdef.typ<>pointerdef) and
          (right.resultdef.typ<>pointerdef);

       if nodetype<>subn then
        begin
          if (right.location.loc<>LOC_CONSTANT) then
            cg.a_op_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,cgop,location.size,
               left.location.register,right.location.register,
               location.register,checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
          else
            cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,cgop,location.size,
               right.location.value,left.location.register,
               location.register,checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
        end
      else  { subtract is a special case since its not commutative }
        begin
          if (nf_swapped in flags) then
            swapleftright;
          if left.location.loc<>LOC_CONSTANT then
            begin
              if right.location.loc<>LOC_CONSTANT then
                cg.a_op_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                    right.location.register,left.location.register,
                    location.register,checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc)
              else
                cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                  right.location.value,left.location.register,
                  location.register,checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
            end
          else
            begin
              tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
              cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,
                left.location.value,tmpreg);
              cg.a_op_reg_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,location.size,
                right.location.register,tmpreg,location.register,checkoverflow and (cs_check_overflow in current_settings.localswitches),ovloc);
            end;
        end;

        { emit overflow check if required }
        if checkoverflow then
          cg.g_overflowcheck_loc(current_asmdata.CurrAsmList,Location,resultdef,ovloc);
      end;


    procedure tcgaddnode.second_cmpboolean;
      begin
        second_cmpordinal;
      end;


{*****************************************************************************
                                pass_generate_code;
*****************************************************************************}

    procedure tcgaddnode.pass_generate_code;
      begin
        case left.resultdef.typ of
          orddef :
            begin
              { handling boolean expressions }
              if is_boolean(left.resultdef) and
                 is_boolean(right.resultdef) then
                second_opboolean
              { 64bit operations }
              else if is_64bit(left.resultdef) then
                second_op64bit
              else
                second_opordinal;
            end;
          stringdef :
            begin
              second_addstring;
            end;
          setdef :
            begin
              {Normalsets are already handled in pass1 if mmx
               should not be used.}
              if is_varset(tsetdef(left.resultdef)) then
                begin
{$ifdef SUPPORT_MMX}
                {$ifdef i386}
                  if cs_mmx in current_settings.localswitches then
                    second_opmmxset
                  else
                {$endif}
{$endif SUPPORT_MMX}
                    internalerror(200109041);
                end
              else
                second_opsmallset;
            end;
          arraydef :
            begin
              { support dynarr=nil }
              if is_dynamic_array(left.resultdef) then
                second_opordinal
              else
                if (cs_support_vectors in current_settings.globalswitches) and
                   is_vector(left.resultdef) then
                  second_opvector
{$ifdef SUPPORT_MMX}
              else
                if is_mmx_able_array(left.resultdef) then
                  second_opmmx
{$endif SUPPORT_MMX}
              else
                internalerror(200306016);
            end;
          floatdef :
            second_opfloat;
          else
            second_opordinal;
        end;
      end;

begin
   caddnode:=tcgaddnode;
end.
