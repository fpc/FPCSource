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
unit ncgcadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,cginfo;

    type
       tppcaddnode = class(taddnode)
          function pass_1: tnode; override;
          procedure pass_2;override;
         private
          procedure pass_left_and_right;
          procedure load_left_right(cmpop, load_constants: boolean);
          procedure clear_left_right(cmpop: boolean);
          function  getresflags : tresflags;
          procedure emit_compare(unsigned : boolean);
          procedure second_addfloat;
          procedure second_addboolean;
          procedure second_addsmallset;
          procedure second_add64bit;   { done }
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,regvars,
      cpupara,
      ncon,nset,
      cga,ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;



{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function tcgaddnode.getresflags(unsigned : boolean) : tresflags;
      begin
         case nodetype of
           equaln : getresflags:=F_E;
           unequaln : getresflags:=F_NE;
          else
           if not(unsigned) then
             begin
                if nf_swaped in flags then
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
                if nf_swaped in flags then
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


    procedure tcgaddnode.pass_left_and_right;
      var
        pushedregs : tmaybesave;
        tmpreg     : tregister;
        pushedfpu  : boolean;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;

        secondpass(left);

        { are too few registers free? }
        maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
        if location.loc=LOC_FPUREGISTER then
          pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
        else
          pushedfpu:=false;
        secondpass(right);
        maybe_restore(exprasmlist,left.location,pushedregs);
        if pushedfpu then
          begin
            tmpreg := rg.getregisterfpu(exprasmlist);
            cg.a_loadfpu_loc_reg(exprasmlist,left.location,tmpreg);
            location_reset(left.location,LOC_FPUREGISTER,left.location.size);
            left.location.register := tmpreg;
          end;
      end;


    procedure tcgaddnode.load_left_right(cmpop, load_constants: boolean);

      procedure load_node(var n: tnode);
        begin
          case n.location.loc of
            LOC_REGISTER:
              if not cmpop then
                begin
                  location.register := n.location.register;
                  if is_64bitint(n.resulttype.def) then
                    location.registerhigh := n.location.registerhigh;
                end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                location_force_reg(exprasmlist,n.location,def_cgsize(n.resulttype.def),false);
                if not cmpop then
                  begin
                    location.register := n.location.register;
                    if is_64bitint(n.resulttype.def) then
                      location.registerhigh := n.location.registerhigh;
                  end;
              end;
            LOC_CONSTANT:
              begin
                if load_constants then
                  begin
                    location_force_reg(exprasmlist,n.location,def_cgsize(n.resulttype.def),false);
                    if not cmpop then
                      location.register := n.location.register;
                      if is_64bitint(n.resulttype.def) then
                        location.registerhigh := n.location.registerhigh;
                  end;
              end;
          end;
        end;

      begin
        load_node(left);
        load_node(right);
      end;


{*****************************************************************************
                                AddSmallSet
*****************************************************************************}
    procedure tppcaddnode.second_opsmallset;
      var
       cmpop : boolean;
      begin
        cmpop := false;
        pass_left_and_right;
        
        { when a setdef is passed, it has to be a smallset }
        if ((left.resulttype.def.deftype=setdef) and
            (tsetdef(left.resulttype.def).settype<>smallset)) or
           ((right.resulttype.def.deftype=setdef) and
            (tsetdef(right.resulttype.def).settype<>smallset)) then
         internalerror(200203301);

        if nodetype in [equaln,unequaln,gtn,gten,lte,lten] then
          cmpop := true;

        load_left_right(cmpop,false);

        if cmpop then
            second_cmpsmallset
        else
            second_addsmallset;

        clear_left_right(cmpop);
      end;
      
      
    procedure tcgaddnode.second_cmpsmallset;
     begin
       location_reset(location,LOC_FLAGS,OS_NO);
          
       case nodetype of
          equaln,
          unequaln :
            begin
              {emit_compare(true);}
            end;
          lten,gten:
            begin
(*
              If (not(nf_swaped in flags) and
                  (nodetype = lten)) or
                 ((nf_swaped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              // now we have to check whether left >= right
              tmpreg := cg.get_scratch_reg_int(exprasmlist);
              if left.location.loc = LOC_CONSTANT then
                begin
                  cg.a_op_const_reg_reg(exprasmlist,OP_AND,OS_INT,
                    not(left.location.value),right.location.register,tmpreg);
                  exprasmlist.concat(taicpu.op_reg_const(A_CMPWI,tmpreg,0));
                  // the two instructions above should be folded together by
                  // the peepholeoptimizer
                end
              else
                begin
                  if right.location.loc = LOC_CONSTANT then
                    begin
                      cg.a_load_const_reg(exprasmlist,OS_INT,
                        aword(right.location.value),tmpreg);
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC_,tmpreg,
                        tmpreg,left.location.register));
                    end
                  else
                    exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC_,tmpreg,
                      right.location.register,left.location.register));
                end;
              cg.free_scratch_reg(exprasmlist,tmpreg);
              location.resflags.cr := R_CR0;
              location.resflags.flag := F_EQ;
              opdone := true;*)
            end;
          else
            internalerror(2002072701);
        end;
          
          
     end;
     

    procedure tppcaddnode.second_addsmallset;
      var
        cgop   : TOpCg;
        tmpreg : tregister;
        opdone,
        cmpop  : boolean;
      begin


        opdone := false;
        
        location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))

        if  (location.register = R_NO) then
          location.register := rg.getregisterint(exprasmlist);

        case nodetype of
          addn :
            begin
              if (nf_swaped in flags) and (left.nodetype=setelementn) then
                swapleftright;
              { are we adding set elements ? }
              if right.nodetype=setelementn then
                begin
                  { no range support for smallsets! }
                  if assigned(tsetelementnode(right).right) then
                   internalerror(43244);
                  if (right.location.loc = LOC_CONSTANT) then
                    cg.a_op_const_reg_reg(exprasmlist,OP_OR,OS_INT,
                      aword(1 shl aword(right.location.value)),
                      left.location.register,location.register)
                  else
                    begin
                      tmpreg := cg.get_scratch_reg_int(exprasmlist);
                      cg.a_load_const_reg(exprasmlist,OS_INT,1,tmpreg);
                      cg.a_op_reg_reg(exprasmlist,OP_SHL,OS_INT,
                        right.location.register,tmpreg);
                      if left.location.loc <> LOC_CONSTANT then
                        cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_INT,tmpreg,
                          left.location.register,location.register)
                      else
                        cg.a_op_const_reg_reg(exprasmlist,OP_OR,OS_INT,
                          aword(left.location.value),tmpreg,location.register);
                      cg.free_scratch_reg(exprasmlist,tmpreg);
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
                      tmpreg := cg.get_scratch_reg_int(exprasmlist);
                      cg.a_load_const_reg(exprasmlist,OS_INT,
                        aword(left.location.value),tmpreg);
                      cg.a_op_reg(OP_NOT,OS_INT,right.location.register);
                      cg.a_op_reg_reg(OP_AND,OS_INT,right.location.register,tmpreg);
                      cg.a_load_reg_reg(OS_INT,tmpreg,location.register);
                      cg.free_scratch_reg(exprasmlist,tmpreg);
                    end
                  else
                    begin
                      cg.a_op_reg(OP_NOT,OS_INT,right.location.register);
                      cg.a_op_reg_reg(OP_AND,OS_INT,right.location.register,left.location.register);
                      cg.a_load_reg_reg(OS_INT,left.location.register,location.register);
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
              cg.a_op_const_reg_reg(exprasmlist,cgop,OS_INT,
                aword(right.location.value),left.location.register,
                location.register)
            else
              cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,
                right.location.register,left.location.register,
                location.register);
          end;

      end;

{*****************************************************************************
                                AddBoolean
*****************************************************************************}

    procedure tppcaddnode.second_opboolean
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

      end;

    procedure tppcaddnode.second_cmpboolean;
      begin
      end;
    
    procedure tppcaddnode.second_addboolean;
      var
        cgop      : TOpCg;
        cgsize  : TCgSize;
        cmpop,
        isjump  : boolean;
        otl,ofl : tasmlabel;
        pushedregs : tmaybesave;
      begin

        cmpop:=false;
        if (torddef(left.resulttype.def).typ=bool8bit) or
           (torddef(right.resulttype.def).typ=bool8bit) then
         cgsize:=OS_8
        else
          if (torddef(left.resulttype.def).typ=bool16bit) or
             (torddef(right.resulttype.def).typ=bool16bit) then
           cgsize:=OS_16
        else
           cgsize:=OS_32;
(*
        if (cs_full_boolean_eval in aktlocalswitches) or
           (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
          begin
            if left.nodetype in [ordconstn,realconstn] then
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
             location_force_reg(exprasmlist,left.location,cgsize,false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end;

            maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
            isjump:=(right.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=truelabel;
                 objectlibrary.getlabel(truelabel);
                 ofl:=falselabel;
                 objectlibrary.getlabel(falselabel);
              end;
            secondpass(right);
            maybe_restore(exprasmlist,left.location,pushedregs);
            if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,right.location,cgsize,false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end;

            cmpop := nodetype in [ltn,lten,gtn,gten,equaln,unequaln];

            { set result location }
            if not cmpop then
              location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
             else
              location_reset(location,LOC_FLAGS,OS_NO);

            load_left_right(cmpop,false);

            if (left.location.loc = LOC_CONSTANT) then
              swapleftright;

            { compare the }
            case nodetype of
              ltn,lten,gtn,gten,
              equaln,unequaln :
                begin
                  if (right.location.loc <> LOC_CONSTANT) then
                    exprasmlist.concat(taicpu.op_reg_reg(A_CMPLW,
                      left.location.register,right.location.register))
                  else
                    exprasmlist.concat(taicpu.op_reg_const(A_CMPLWI,
                      left.location.register,longint(right.location.value)));
                  location.resflags := getresflags;
                end;
              else
                begin
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
                    cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,
                      left.location.register,right.location.register,
                      location.register)
                  else
                    cg.a_op_const_reg_reg(exprasmlist,cgop,OS_INT,
                      aword(right.location.value),left.location.register,
                      location.register);
                end;
            end;
         end
        else
         begin
           // just to make sure we free the right registers
           cmpop := true;
           case nodetype of
             andn,
             orn :
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
                     CGMessage(type_e_mismatch);
                 end;
                 secondpass(right);
                 maketojumpbool(exprasmlist,right,lr_load_regvars);
               end;
           end;
         end;*)
        clear_left_right(cmpop);
      end;


{*****************************************************************************
                                Add64bit
*****************************************************************************}

    procedure tcgaddnode.second_op64bit;
     var
       cmpop : boolean;
     begin
        cmpop := false;
        firstcomplex(self);

        pass_left_and_right;
 
        if nodetype in [equaln,unequaln,gtn,gten,lte,lten] then
          cmpop := true;

        if cmpop then
            second_cmp64bit
        else
            second_add64bit;

        clear_left_right(cmpop);
     end;

    procedure tcgaddnode.second_cmp64bit;
     begin
(*        load_left_right(true,false);

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
                          tempreg64.reglo := left.location.registerlow;
                          tempreg64.reghi := left.location.registerhigh;
                        end
                      else
                        begin
                          if (aword(right.location.valueqword) <> 0) then
                            tempreg64.reglo := cg.get_scratch_reg_int(exprasmlist)
                          else
                            tempreg64.reglo := left.location.registerlow;
                          if ((right.location.valueqword shr 32) <> 0) then
                            tempreg64.reghi := cg.get_scratch_reg_int(exprasmlist)
                          else
                            tempreg64.reghi := left.location.registerhigh;
                        end;

                      if (aword(right.location.valueqword) <> 0) then
                        { negative values can be handled using SUB, }
                        { positive values < 65535 using XOR.        }
                        if (longint(right.location.valueqword) >= -32767) and
                           (longint(right.location.valueqword) < 0) then
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            aword(right.location.valueqword),
                            left.location.registerlow,tempreg64.reglo)
                        else
                          cg.a_op_const_reg_reg(exprasmlist,OP_XOR,OS_INT,
                            aword(right.location.valueqword),
                            left.location.registerlow,tempreg64.reglo);

                      if ((right.location.valueqword shr 32) <> 0) then
                        if (longint(right.location.valueqword shr 32) >= -32767) and
                           (longint(right.location.valueqword shr 32) < 0) then
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.registerhigh,tempreg64.reghi)
                        else
                          cg.a_op_const_reg_reg(exprasmlist,OP_XOR,OS_INT,
                            aword(right.location.valueqword shr 32),
                            left.location.registerhigh,tempreg64.reghi);
                    end
                  else
                    begin
                       tempreg64.reglo := cg.get_scratch_reg_int(exprasmlist);
                       tempreg64.reghi := cg.get_scratch_reg_int(exprasmlist);
                       cg64.a_op64_reg_reg_reg(exprasmlist,OP_XOR,
                         left.location.register64,right.location.register64,
                         tempreg64);
                    end;

                  cg.a_reg_alloc(exprasmlist,R_0);
                  exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR_,R_0,
                    tempreg64.reglo,tempreg64.reghi));
                  cg.a_reg_dealloc(exprasmlist,R_0);
                  if (tempreg64.reglo <> left.location.registerlow) then
                    cg.free_scratch_reg(exprasmlist,tempreg64.reglo);
                  if (tempreg64.reghi <> left.location.registerhigh) then
                    cg.free_scratch_reg(exprasmlist,tempreg64.reghi);

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
     end;


    procedure tppcaddnode.second_add64bit;
      var
        op         : TOpCG;
        unsigned   : boolean;

      begin

        unsigned:=((left.resulttype.def.deftype=orddef) and
                   (torddef(left.resulttype.def).typ=u64bit)) or
                  ((right.resulttype.def.deftype=orddef) and
                   (torddef(right.resulttype.def).typ=u64bit));
        case nodetype of
          addn :
              op:=OP_ADD;
          subn :
              op:=OP_SUB;
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

        location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));

        load_left_right(false,(cs_check_overflow in aktlocalswitches) and
            (nodetype in [addn,subn]));

        case nodetype of
          begin
              xorn,orn,andn,addn:
                begin
                  if (location.registerlow = R_NO) then
                    begin
                      location.registerlow := rg.getregisterint(exprasmlist);
                      location.registerhigh := rg.getregisterint(exprasmlist);
                    end;

                  if (left.location.loc = LOC_CONSTANT) then
                    swapleftright;
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
                      if (location.registerlow = R_NO) then
                        begin
                         location.registerlow := rg.getregisterint(exprasmlist);
                         location.registerhigh := rg.getregisterint(exprasmlist);
                      end;
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
                      location_force_reg(exprasmlist,left.location,
                        def_cgsize(left.resulttype.def),true);
                      if (left.location.loc = LOC_REGISTER) then
                        location.register64 := left.location.register64
                      else if (location.registerlow = R_NO) then
                        begin
                         location.registerlow := rg.getregisterint(exprasmlist);
                         location.registerhigh := rg.getregisterint(exprasmlist);
                        end;
                      cg64.a_op64_reg_reg_reg(exprasmlist,OP_SUB,
                        right.location.register64,left.location.register64,
                        location.register64);
                     end;
                end;
              else
                internalerror(2002072803);
            end;
          end
        { emit overflow check if enabled }        
        cg.g_overflowcheck(exprasmlist,self);

      end;


{
  $Log$
  Revision 1.1  2002-12-07 19:51:35  carl
    + first version (uncompilable!)

}