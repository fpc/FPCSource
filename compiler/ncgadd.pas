{
    $Id$
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
          procedure pass_2;override;
         protected
          { call secondpass for both left and right }
          procedure pass_left_right;
          { set the register of the result location }
          procedure set_result_location_reg;
          { load left and right nodes into registers }
          procedure force_reg_left_right(allow_swap,allow_constant:boolean);
          { free used registers, except result location }
          procedure release_reg_left_right;

          procedure second_opfloat;
          procedure second_opboolean;
          procedure second_opsmallset;
          procedure second_op64bit;
          procedure second_opordinal;

          procedure second_addfloat;virtual;abstract;
          procedure second_addboolean;virtual;
          procedure second_addsmallset;virtual;
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
      aasmbase,aasmtai,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,regvars,
      ncon,nset,ncgutil,tgobj,rgobj,cgobj,
{$ifdef cpu64bit}
      cg64f64
{$else cpu64bit}
      cg64f32
{$endif cpu64bit}
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

        isjump:=(left.location.loc=LOC_JUMP);
        if isjump then
          begin
             otl:=truelabel;
             objectlibrary.getlabel(truelabel);
             ofl:=falselabel;
             objectlibrary.getlabel(falselabel);
          end;
        secondpass(left);
        if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
          location_force_reg(exprasmlist,left.location,def_cgsize(resulttype.def),false);
        if isjump then
          begin
            truelabel:=otl;
            falselabel:=ofl;
          end;

        { are too few registers free? }
        if left.location.loc=LOC_FPUREGISTER then
          pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
        else
          pushedfpu:=false;
        isjump:=(right.location.loc=LOC_JUMP);
        if isjump then
          begin
             otl:=truelabel;
             objectlibrary.getlabel(truelabel);
             ofl:=falselabel;
             objectlibrary.getlabel(falselabel);
          end;
        secondpass(right);
        if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
          location_force_reg(exprasmlist,right.location,def_cgsize(resulttype.def),false);
        if isjump then
          begin
            truelabel:=otl;
            falselabel:=ofl;
          end;
        if pushedfpu then
          begin
            tmpreg := rg.getregisterfpu(exprasmlist,left.location.size);
            cg.a_loadfpu_loc_reg(exprasmlist,left.location,tmpreg);
            location_reset(left.location,LOC_FPUREGISTER,left.location.size);
            left.location.register := tmpreg;
          end;
      end;


    procedure tcgaddnode.set_result_location_reg;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
        if left.location.loc=LOC_REGISTER then
          begin
            if TCGSize2Size[left.location.size]<>TCGSize2Size[location.size] then
              internalerror(200307041);
{$ifndef cpu64bit}
            if location.size in [OS_64,OS_S64] then
              begin
                location.registerlow := left.location.registerlow;
                location.registerhigh := left.location.registerhigh;
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
                location.registerlow := right.location.registerlow;
                location.registerhigh := right.location.registerhigh;
              end
            else
{$endif}
              location.register := right.location.register;
          end
        else
          begin
{$ifndef cpu64bit}
            if location.size in [OS_64,OS_S64] then
              begin
                location.registerlow := rg.getregisterint(exprasmlist,OS_INT);
                location.registerhigh := rg.getregisterint(exprasmlist,OS_INT);
              end
            else
{$endif}
              location.register := rg.getregisterint(exprasmlist,location.size);
          end;
      end;


    procedure tcgaddnode.force_reg_left_right(allow_swap,allow_constant:boolean);
      begin
        if (left.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (left.location.loc=LOC_CONSTANT)
              ) then
          location_force_reg(exprasmlist,left.location,left.location.size,false);
        if (right.location.loc<>LOC_REGISTER) and
           not(
               allow_constant and
               (right.location.loc=LOC_CONSTANT) and
               (left.location.loc<>LOC_CONSTANT)
              ) then
          location_force_reg(exprasmlist,right.location,right.location.size,false);
        { Left is always a register, right can be register or constant }
        if left.location.loc<>LOC_REGISTER then
          begin
            { when it is not allowed to swap we have a constant on
              left, that will give problems }
            if not allow_swap then
              internalerror(200307041);
            swapleftright;
          end;
      end;


    procedure tcgaddnode.release_reg_left_right;
      begin
        if (right.location.loc in [LOC_REGISTER,LOC_FPUREGISTER]) and
           not(
               (location.loc = right.location.loc) and
               (location.register=right.location.register)
              ) then
          location_release(exprasmlist,right.location);
        if (left.location.loc in [LOC_REGISTER,LOC_FPUREGISTER]) and
           not(
               (location.loc = left.location.loc) and
               (location.register=left.location.register)
              ) then
          location_release(exprasmlist,left.location);
      end;


{*****************************************************************************
                                Smallsets
*****************************************************************************}

    procedure tcgaddnode.second_opsmallset;
      begin
        { when a setdef is passed, it has to be a smallset }
        if ((left.resulttype.def.deftype=setdef) and
            (tsetdef(left.resulttype.def).settype<>smallset)) or
           ((right.resulttype.def.deftype=setdef) and
            (tsetdef(right.resulttype.def).settype<>smallset)) then
          internalerror(200203301);

        if nodetype in [equaln,unequaln,gtn,gten,lten,ltn] then
          second_cmpsmallset
        else
          second_addsmallset;
      end;


    procedure tcgaddnode.second_addsmallset;
      var
        cgop   : TOpCg;
        tmpreg : tregister;
        opdone : boolean;
      begin
        opdone := false;

        pass_left_right;
        force_reg_left_right(true,true);

        { setelementn is a special case, it must be on right.
          We need an extra check if left is a register because the
          default case can skip the register loading when the
          setelementn is in a register (PFV) }
        if (nf_swaped in flags) and
           (left.nodetype=setelementn) then
          swapleftright;
        if (right.nodetype=setelementn) and
           (left.location.loc<>LOC_REGISTER) then
          location_force_reg(exprasmlist,left.location,left.location.size,false);

        set_result_location_reg;

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
                    cg.a_op_const_reg_reg(exprasmlist,OP_OR,location.size,
                      aword(1 shl aword(right.location.value)),
                      left.location.register,location.register)
                  else
                    begin
                      tmpreg := rg.getregisterint(exprasmlist,location.size);
                      cg.a_load_const_reg(exprasmlist,location.size,1,tmpreg);
                      cg.a_op_reg_reg(exprasmlist,OP_SHL,location.size,
                        right.location.register,tmpreg);
                      if left.location.loc <> LOC_CONSTANT then
                        cg.a_op_reg_reg_reg(exprasmlist,OP_OR,location.size,tmpreg,
                            left.location.register,location.register)
                      else
                        cg.a_op_const_reg_reg(exprasmlist,OP_OR,location.size,
                            aword(left.location.value),tmpreg,location.register);
                      rg.ungetregisterint(exprasmlist,tmpreg);
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
              if (not(nf_swaped in flags)) then
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
                      tmpreg := rg.getregisterint(exprasmlist,location.size);
                      cg.a_load_const_reg(exprasmlist,location.size,
                        aword(left.location.value),tmpreg);
                      cg.a_op_reg_reg(exprasmlist,OP_NOT,location.size,right.location.register,right.location.register);
                      cg.a_op_reg_reg(exprasmlist,OP_AND,location.size,right.location.register,tmpreg);
                      cg.a_load_reg_reg(exprasmlist,OS_INT,location.size,tmpreg,location.register);
                      rg.ungetregisterint(exprasmlist,tmpreg);
                    end
                  else
                    begin
                      cg.a_op_reg_reg(exprasmlist,OP_NOT,right.location.size,right.location.register,right.location.register);
                      cg.a_op_reg_reg(exprasmlist,OP_AND,left.location.size,right.location.register,left.location.register);
                      cg.a_load_reg_reg(exprasmlist,left.location.size,location.size,left.location.register,location.register);
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
              cg.a_op_const_reg_reg(exprasmlist,cgop,location.size,
                aword(right.location.value),left.location.register,
                location.register)
            else
              cg.a_op_reg_reg_reg(exprasmlist,cgop,location.size,
                right.location.register,left.location.register,
                location.register);
          end;

        release_reg_left_right;
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
      begin
        { And,Or will only evaluate from left to right only the
          needed nodes unless full boolean evaluation is enabled }
        if (nodetype in [orn,andn]) and
           not(cs_full_boolean_eval in aktlocalswitches) then
          begin
            location_reset(location,LOC_JUMP,OS_NO);
            case nodetype of
              andn :
                begin
                   otl:=truelabel;
                   objectlibrary.getlabel(truelabel);
                   secondpass(left);
                   maketojumpbool(exprasmlist,left,lr_load_regvars);
                   cg.a_label(exprasmlist,truelabel);
                   truelabel:=otl;
                end;
              orn :
                begin
                   ofl:=falselabel;
                   objectlibrary.getlabel(falselabel);
                   secondpass(left);
                   maketojumpbool(exprasmlist,left,lr_load_regvars);
                   cg.a_label(exprasmlist,falselabel);
                   falselabel:=ofl;
                end;
              else
                internalerror(200307044);
            end;
            secondpass(right);
            maketojumpbool(exprasmlist,right,lr_load_regvars);
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
              cg.a_op_reg_reg_reg(exprasmlist,cgop,location.size,
                 left.location.register,right.location.register,
                 location.register)
            else
              cg.a_op_const_reg_reg(exprasmlist,cgop,location.size,
                 aword(right.location.value),left.location.register,
                 location.register);
         end;

        release_reg_left_right;
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
      begin
        pass_left_right;
        force_reg_left_right(false,(cs_check_overflow in aktlocalswitches) and
                                   (nodetype in [addn,subn]));
        set_result_location_reg;

        { assume no overflow checking is required }
        checkoverflow := false;
        case nodetype of
          addn :
             begin
                op:=OP_ADD;
                checkoverflow := true;
             end;
          subn :
             begin
                op:=OP_SUB;
                checkoverflow := true;
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

        case nodetype of
          xorn,orn,andn,addn:
            begin
              if (right.location.loc = LOC_CONSTANT) then
                cg64.a_op64_const_reg_reg(exprasmlist,op,right.location.valueqword,
                  left.location.register64,location.register64)
              else
                cg64.a_op64_reg_reg_reg(exprasmlist,op,right.location.register64,
                  left.location.register64,location.register64);
            end;
          subn:
            begin
              if (nf_swaped in flags) then
                swapleftright;

              if left.location.loc <> LOC_CONSTANT then
                begin
                  if right.location.loc <> LOC_CONSTANT then
                    // reg64 - reg64
                    cg64.a_op64_reg_reg_reg(exprasmlist,OP_SUB,
                      right.location.register64,left.location.register64,
                      location.register64)
                  else
                    // reg64 - const64
                    cg64.a_op64_const_reg_reg(exprasmlist,OP_SUB,
                      right.location.valueqword,left.location.register64,
                      location.register64)
                end
              else
                begin
                  // const64 - reg64
                  location_force_reg(exprasmlist,left.location,left.location.size,true);
                  cg64.a_op64_reg_reg_reg(exprasmlist,OP_SUB,
                    right.location.register64,left.location.register64,
                    location.register64);
                end;
            end;
          else
            internalerror(2002072803);
        end;

        { emit overflow check if enabled }
        if checkoverflow then
           cg.g_overflowcheck(exprasmlist,Location,ResultType.Def);

        release_reg_left_right;
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
      begin
        pass_left_right;
        force_reg_left_right(false,(cs_check_overflow in aktlocalswitches) and
                                   (nodetype in [addn,subn,muln]));
        set_result_location_reg;

        { determine if the comparison will be unsigned }
        unsigned:=not(is_signed(left.resulttype.def)) or
                    not(is_signed(right.resulttype.def));

        { assume no overflow checking is require }
        checkoverflow := false;

        case nodetype of
          addn:
            begin
              cgop := OP_ADD;
              checkoverflow := true;
            end;
          xorn :
            begin
              cgop := OP_XOR;
            end;
          orn :
            begin
              cgop := OP_OR;
            end;
          andn:
            begin
              cgop := OP_AND;
            end;
          muln:
            begin
              checkoverflow := true;
              if unsigned then
                cgop := OP_MUL
              else
                cgop := OP_IMUL;
            end;
          subn :
            begin
              checkoverflow := true;
              cgop := OP_SUB;
            end;
        end;

       if nodetype <> subn then
        begin
          if (right.location.loc <> LOC_CONSTANT) then
            cg.a_op_reg_reg_reg(exprasmlist,cgop,location.size,
               left.location.register,right.location.register,
               location.register)
          else
            cg.a_op_const_reg_reg(exprasmlist,cgop,location.size,
               aword(right.location.value),left.location.register,
               location.register);
        end
      else  { subtract is a special case since its not commutative }
        begin
          if (nf_swaped in flags) then
            swapleftright;
          if left.location.loc <> LOC_CONSTANT then
            begin
              if right.location.loc <> LOC_CONSTANT then
                cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,location.size,
                    right.location.register,left.location.register,
                    location.register)
              else
                cg.a_op_const_reg_reg(exprasmlist,OP_SUB,location.size,
                    aword(right.location.value),left.location.register,
                    location.register);
            end
          else
            begin
              tmpreg := rg.getregisterint(exprasmlist,location.size);
              cg.a_load_const_reg(exprasmlist,location.size,
                aword(left.location.value),tmpreg);
              cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,location.size,
                right.location.register,tmpreg,location.register);
              rg.ungetregisterint(exprasmlist,tmpreg);
            end;
        end;

        { emit overflow check if required }
        if checkoverflow then
          cg.g_overflowcheck(exprasmlist,Location,ResultType.Def);

        release_reg_left_right;
      end;


    procedure tcgaddnode.second_cmpboolean;
      begin
         second_cmpordinal;
      end;


{*****************************************************************************
                                pass_2
*****************************************************************************}

    procedure tcgaddnode.pass_2;
      begin
        case left.resulttype.def.deftype of
          orddef :
            begin
              { handling boolean expressions }
              if is_boolean(left.resulttype.def) and
                 is_boolean(right.resulttype.def) then
                second_opboolean
              { 64bit operations }
              else if is_64bit(left.resulttype.def) then
                second_op64bit
              else
                second_opordinal;
            end;
          stringdef :
            begin
              { this should already be handled in pass1 }
              internalerror(2002072402);
            end;
          setdef :
            begin
              { normalsets are already handled in pass1 }
              if (tsetdef(left.resulttype.def).settype<>smallset) then
                internalerror(200109041);
              second_opsmallset;
            end;
          arraydef :
            begin
{$ifdef SUPPORT_MMX}
              if is_mmx_able_array(left.resulttype.def) then
                second_opmmx;
{$endif SUPPORT_MMX}
              { only mmx arrays are possible }
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
{
  $Log$
  Revision 1.20  2003-10-01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.19  2003/09/14 21:57:08  jonas
    * fixed release_reg_left_right for fpu registers

  Revision 1.18  2003/09/14 21:34:16  peter
    * fix setelementn support
    * fix loading of flags

  Revision 1.17  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.16  2003/09/03 11:18:36  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.15.2.2  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.15.2.1  2003/08/27 20:23:55  peter
    * remove old ra code

  Revision 1.15  2003/07/08 21:24:59  peter
    * sparc fixes

  Revision 1.14  2003/07/06 17:44:12  peter
    * cleanup and first sparc implementation

  Revision 1.13  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.12  2003/06/10 20:46:17  mazen
  * fixing a general compile problem related to
    cg.g_overflowcheck declaration that has
    changed

  Revision 1.11  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.10  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.9  2003/04/30 22:15:59  florian
    * some 64 bit adaptions in ncgadd
    * x86-64 now uses ncgadd
    * tparamanager.ret_in_acc doesn't return true anymore for a void-def

  Revision 1.8  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.7  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.6  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.5  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.4  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.3  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.2  2002/12/08 15:02:17  carl
    + more fixes

  Revision 1.1  2002/12/07 19:51:35  carl
    + first version (uncompilable!)

}
