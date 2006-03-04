{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the PowerPC

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
unit nppcadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,ncgadd,cpubase;

    type
       tppcaddnode = class(tcgaddnode)
          function pass_1: tnode; override;
          procedure pass_2;override;
         private
          procedure pass_left_and_right;
          procedure load_left_right(cmpop, load_constants: boolean);
          function  getresflags : tresflags;
          procedure emit_compare(unsigned : boolean);
          procedure second_addfloat;override;
          procedure second_addboolean;override;
          procedure second_addsmallset;override;
{$ifdef SUPPORT_MMX}
          procedure second_addmmx;override;
{$endif SUPPORT_MMX}
          procedure second_add64bit;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,regvars,
      cpupara,cgcpu,cgutils,
      ncon,nset,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;


{*****************************************************************************
                                  Pass 1
*****************************************************************************}

    function tppcaddnode.pass_1: tnode;
      begin
        resulttypepass(left);
        if (nodetype in [equaln,unequaln]) and
           (left.resulttype.def.deftype = orddef) and
           is_64bit(left.resulttype.def) then
          begin
            result := nil;
            firstpass(left);
            firstpass(right);
            expectloc := LOC_FLAGS;
            calcregisters(self,2,0,0);
            exit;
          end;
        result := inherited pass_1;
      end;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure tppcaddnode.pass_left_and_right;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;

        secondpass(left);
        secondpass(right);
      end;


    procedure tppcaddnode.load_left_right(cmpop, load_constants: boolean);

      procedure load_node(var n: tnode);
        begin
          case n.location.loc of
            LOC_REGISTER:
              if not cmpop then
                begin
                  location.register := n.location.register;
                  if is_64bit(n.resulttype.def) then
                    location.register64.reghi := n.location.register64.reghi;
                end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                location_force_reg(exprasmlist,n.location,def_cgsize(n.resulttype.def),false);
                if not cmpop then
                  begin
                    location.register := n.location.register;
                    if is_64bit(n.resulttype.def) then
                      location.register64.reghi := n.location.register64.reghi;
                  end;
              end;
            LOC_CONSTANT:
              begin
                if load_constants then
                  begin
                    location_force_reg(exprasmlist,n.location,def_cgsize(n.resulttype.def),false);
                    if not cmpop then
                      begin
                        location.register := n.location.register;
                        if is_64bit(n.resulttype.def) then
                          location.register64.reghi := n.location.register64.reghi;
                      end;
                  end;
              end;
          end;
        end;

      begin
        load_node(left);
        load_node(right);
        if not(cmpop) and
           (location.register = NR_NO) then
         begin
           location.register := cg.getintregister(exprasmlist,OS_INT);
           if is_64bit(resulttype.def) then
             location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
         end;
      end;


    function tppcaddnode.getresflags : tresflags;
      begin
        if (left.resulttype.def.deftype <> floatdef) then
          result.cr := RS_CR0
        else
          result.cr := RS_CR1;
        case nodetype of
          equaln : result.flag:=F_EQ;
          unequaln : result.flag:=F_NE;
        else
          if nf_swaped in flags then
            case nodetype of
              ltn : result.flag:=F_GT;
              lten : result.flag:=F_GE;
              gtn : result.flag:=F_LT;
              gten : result.flag:=F_LE;
            end
          else
            case nodetype of
              ltn : result.flag:=F_LT;
              lten : result.flag:=F_LE;
              gtn : result.flag:=F_GT;
              gten : result.flag:=F_GE;
            end;
        end
      end;


    procedure tppcaddnode.emit_compare(unsigned: boolean);
      var
        op : tasmop;
        tmpreg : tregister;
        useconst : boolean;
      begin
        // get the constant on the right if there is one
        if (left.location.loc = LOC_CONSTANT) then
          swapleftright;
        // can we use an immediate, or do we have to load the
        // constant in a register first?
        if (right.location.loc = LOC_CONSTANT) then
          begin
{$ifdef dummy}
            if (right.location.size in [OS_64,OS_S64]) and (hi(right.location.value64)<>0) and ((hi(right.location.value64)<>$ffffffff) or unsigned) then
              internalerror(2002080301);
{$endif extdebug}
            if (nodetype in [equaln,unequaln]) then
              if (unsigned and
                  (aword(right.location.value) > high(word))) or
                 (not unsigned and
                  (aint(right.location.value) < low(smallint)) or
                   (aint(right.location.value) > high(smallint))) then
                { we can then maybe use a constant in the 'othersigned' case
                 (the sign doesn't matter for // equal/unequal)}
                unsigned := not unsigned;

            if (unsigned and
                (aword(right.location.value) <= high(word))) or
               (not(unsigned) and
                (aint(right.location.value) >= low(smallint)) and
                (aint(right.location.value) <= high(smallint))) then
               useconst := true
            else
              begin
                useconst := false;
                tmpreg := cg.getintregister(exprasmlist,OS_INT);
                cg.a_load_const_reg(exprasmlist,OS_INT,
                    right.location.value,tmpreg);
               end
          end
        else
          useconst := false;
        location.loc := LOC_FLAGS;
        location.resflags := getresflags;
        if not unsigned then
          if useconst then
            op := A_CMPWI
          else
            op := A_CMPW
        else
          if useconst then
            op := A_CMPLWI
          else
            op := A_CMPLW;

        if (right.location.loc = LOC_CONSTANT) then
          begin
            if useconst then
              exprasmlist.concat(taicpu.op_reg_const(op,left.location.register,longint(right.location.value)))
            else
              exprasmlist.concat(taicpu.op_reg_reg(op,left.location.register,tmpreg));
          end
        else
          exprasmlist.concat(taicpu.op_reg_reg(op,
            left.location.register,right.location.register));
      end;


{*****************************************************************************
                                AddBoolean
*****************************************************************************}

    procedure tppcaddnode.second_addboolean;
      var
        cgop      : TOpCg;
        cgsize  : TCgSize;
        cmpop,
        isjump  : boolean;
        otl,ofl : tasmlabel;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

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

        if (cs_full_boolean_eval in aktlocalswitches) or
           (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
          begin
            if left.nodetype in [ordconstn,realconstn] then
             swapleftright;

            isjump:=(left.expectloc=LOC_JUMP);
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
             end
            else if left.location.loc=LOC_JUMP then
              internalerror(2003122901);

            isjump:=(right.expectloc=LOC_JUMP);
            if isjump then
              begin
                 otl:=truelabel;
                 objectlibrary.getlabel(truelabel);
                 ofl:=falselabel;
                 objectlibrary.getlabel(falselabel);
              end;
            secondpass(right);
            if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,right.location,cgsize,false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end
            else if right.location.loc=LOC_JUMP then
              internalerror(200312292);

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
                      right.location.value,left.location.register,
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
                     internalerror(200403181);
                 end;
                 secondpass(right);
                 maketojumpbool(exprasmlist,right,lr_load_regvars);
               end;
           end;
         end;
      end;


{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure tppcaddnode.second_addfloat;
      var
        op    : TAsmOp;
        cmpop : boolean;
      begin
        pass_left_and_right;

        cmpop:=false;
        case nodetype of
          addn :
            op:=A_FADD;
          muln :
            op:=A_FMUL;
          subn :
            op:=A_FSUB;
          slashn :
            op:=A_FDIV;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
              op:=A_FCMPO;
              cmpop:=true;
            end;
          else
            internalerror(200403182);
        end;

        // get the operands in the correct order, there are no special cases
        // here, everything is register-based
        if nf_swaped in flags then
          swapleftright;

        // put both operands in a register
        location_force_fpureg(exprasmlist,right.location,true);
        location_force_fpureg(exprasmlist,left.location,true);

        // initialize de result
        if not cmpop then
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
            if left.location.loc = LOC_FPUREGISTER then
              location.register := left.location.register
            else if right.location.loc = LOC_FPUREGISTER then
              location.register := right.location.register
            else
              location.register := cg.getfpuregister(exprasmlist,location.size);
          end
        else
         begin
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags := getresflags;
         end;

        // emit the actual operation
        if not cmpop then
          begin
            exprasmlist.concat(taicpu.op_reg_reg_reg(op,
              location.register,left.location.register,
              right.location.register))
          end
        else
          begin
            exprasmlist.concat(taicpu.op_reg_reg_reg(op,
              newreg(R_SPECIALREGISTER,location.resflags.cr,R_SUBNONE),left.location.register,right.location.register))
          end;
      end;

{*****************************************************************************
                                AddSmallSet
*****************************************************************************}

    procedure tppcaddnode.second_addsmallset;
      var
        cgop   : TOpCg;
        tmpreg : tregister;
        opdone,
        cmpop  : boolean;
      begin
        pass_left_and_right;

        { when a setdef is passed, it has to be a smallset }
        if ((left.resulttype.def.deftype=setdef) and
            (tsetdef(left.resulttype.def).settype<>smallset)) or
           ((right.resulttype.def.deftype=setdef) and
            (tsetdef(right.resulttype.def).settype<>smallset)) then
         internalerror(200203301);

        opdone := false;
        cmpop:=nodetype in [equaln,unequaln,lten,gten];

        { set result location }
        if not cmpop then
          location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
         else
          location_reset(location,LOC_FLAGS,OS_NO);

        load_left_right(cmpop,false);

        if not(cmpop) and
           (location.register = NR_NO) then
          location.register := cg.getintregister(exprasmlist,OS_INT);

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
                      aint(aword(1) shl aword(right.location.value)),
                      left.location.register,location.register)
                  else
                    begin
                      tmpreg := cg.getintregister(exprasmlist,OS_INT);
                      cg.a_load_const_reg(exprasmlist,OS_INT,1,tmpreg);
                      cg.a_op_reg_reg(exprasmlist,OP_SHL,OS_INT,
                        right.location.register,tmpreg);
                      if left.location.loc <> LOC_CONSTANT then
                        cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_INT,tmpreg,
                          left.location.register,location.register)
                      else
                        cg.a_op_const_reg_reg(exprasmlist,OP_OR,OS_INT,
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
                      tmpreg := cg.getintregister(exprasmlist,OS_INT);
                      cg.a_load_const_reg(exprasmlist,OS_INT,
                        left.location.value,tmpreg);
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC,
                        location.register,tmpreg,right.location.register));
                    end
                  else
                    exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC,
                      location.register,left.location.register,
                      right.location.register));
                end;
            end;
          equaln,
          unequaln :
            begin
              emit_compare(true);
              opdone := true;
            end;
          lten,gten:
            begin
              If (not(nf_swaped in flags) and
                  (nodetype = lten)) or
                 ((nf_swaped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              // now we have to check whether left >= right
              tmpreg := cg.getintregister(exprasmlist,OS_INT);
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
                        right.location.value,tmpreg);
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC_,tmpreg,
                        tmpreg,left.location.register));
                    end
                  else
                    exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC_,tmpreg,
                      right.location.register,left.location.register));
                end;
              location.resflags.cr := RS_CR0;
              location.resflags.flag := F_EQ;
              opdone := true;
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
                right.location.value,left.location.register,
                location.register)
            else
              cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,
                right.location.register,left.location.register,
                location.register);
          end;
      end;

{*****************************************************************************
                                Add64bit
*****************************************************************************}

    procedure tppcaddnode.second_add64bit;
      var
        op         : TOpCG;
        op1,op2    : TAsmOp;
        cmpop,
        unsigned   : boolean;


      procedure emit_cmp64_hi;

        var
          oldleft, oldright: tlocation;
        begin
          // put the high part of the location in the low part
          location_copy(oldleft,left.location);
          location_copy(oldright,right.location);
          if left.location.loc = LOC_CONSTANT then
            left.location.value64 := left.location.value64 shr 32
          else
            left.location.register64.reglo := left.location.register64.reghi;
          if right.location.loc = LOC_CONSTANT then
            right.location.value64 := right.location.value64 shr 32
          else
            right.location.register64.reglo := right.location.register64.reghi;

          // and call the normal emit_compare
          emit_compare(unsigned);
          location_copy(left.location,oldleft);
          location_copy(right.location,oldright);
        end;


      procedure emit_cmp64_lo;

        begin
          emit_compare(true);
        end;


      procedure firstjmp64bitcmp;

        var
          oldnodetype: tnodetype;
        begin
{$ifdef OLDREGVARS}
           load_all_regvars(exprasmlist);
{$endif OLDREGVARS}
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   cg.a_jmp_flags(exprasmlist,getresflags,truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swaped);
                   cg.a_jmp_flags(exprasmlist,getresflags,falselabel);
                   toggleflag(nf_swaped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(exprasmlist,getresflags,truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(exprasmlist,getresflags,falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                begin
                  nodetype := unequaln;
                  cg.a_jmp_flags(exprasmlist,getresflags,falselabel);
                  nodetype := equaln;
                end;
              unequaln:
                begin
                  cg.a_jmp_flags(exprasmlist,getresflags,truelabel);
                end;
           end;
        end;


      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparison of the low dword always has }
                   { to be always unsigned!                     }
                   cg.a_jmp_flags(exprasmlist,getresflags,truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
              equaln:
                begin
                   nodetype := unequaln;
                   cg.a_jmp_flags(exprasmlist,getresflags,falselabel);
                   cg.a_jmp_always(exprasmlist,truelabel);
                   nodetype := equaln;
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(exprasmlist,getresflags,truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
           end;
        end;


    var
      tempreg64: tregister64;

      begin
        firstcomplex(self);

        pass_left_and_right;

        cmpop:=false;
        unsigned:=((left.resulttype.def.deftype=orddef) and
                   (torddef(left.resulttype.def).typ=u64bit)) or
                  ((right.resulttype.def.deftype=orddef) and
                   (torddef(right.resulttype.def).typ=u64bit));
        case nodetype of
          addn :
            begin
              op:=OP_ADD;
            end;
          subn :
            begin
              op:=OP_SUB;
              if (nf_swaped in flags) then
                swapleftright;
            end;
          ltn,lten,
          gtn,gten,
          equaln,unequaln:
            begin
              op:=OP_NONE;
              cmpop:=true;
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

        if not cmpop then
          location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));

        load_left_right(cmpop,(cs_check_overflow in aktlocalswitches) and
            (nodetype in [addn,subn]));

        if not(cs_check_overflow in aktlocalswitches) or
           not(nodetype in [addn,subn]) then
          begin
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
                          tempreg64.reglo := left.location.register64.reglo;
                          tempreg64.reghi := left.location.register64.reghi;
                        end
                      else
                        begin
                          if (aint(right.location.value64) <> 0) then
                            tempreg64.reglo := cg.getintregister(exprasmlist,OS_32)
                          else
                            tempreg64.reglo := left.location.register64.reglo;
                          if ((right.location.value64 shr 32) <> 0) then
                            tempreg64.reghi := cg.getintregister(exprasmlist,OS_32)
                          else
                            tempreg64.reghi := left.location.register64.reghi;
                        end;

                      if (aint(right.location.value64) <> 0) then
                        { negative values can be handled using SUB, }
                        { positive values < 65535 using XOR.        }
                        if (longint(right.location.value64) >= -32767) and
                           (longint(right.location.value64) < 0) then
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            aint(right.location.value64),
                            left.location.register64.reglo,tempreg64.reglo)
                        else
                          cg.a_op_const_reg_reg(exprasmlist,OP_XOR,OS_INT,
                            aint(right.location.value64),
                            left.location.register64.reglo,tempreg64.reglo);

                      if ((right.location.value64 shr 32) <> 0) then
                        if (longint(right.location.value64 shr 32) >= -32767) and
                           (longint(right.location.value64 shr 32) < 0) then
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            aint(right.location.value64 shr 32),
                            left.location.register64.reghi,tempreg64.reghi)
                        else
                          cg.a_op_const_reg_reg(exprasmlist,OP_XOR,OS_INT,
                            aint(right.location.value64 shr 32),
                            left.location.register64.reghi,tempreg64.reghi);
                    end
                  else
                    begin
                       tempreg64.reglo := cg.getintregister(exprasmlist,OS_INT);
                       tempreg64.reghi := cg.getintregister(exprasmlist,OS_INT);
                       cg64.a_op64_reg_reg_reg(exprasmlist,OP_XOR,location.size,
                         left.location.register64,right.location.register64,
                         tempreg64);
                    end;

                  cg.a_reg_alloc(exprasmlist,NR_R0);
                  exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR_,NR_R0,
                    tempreg64.reglo,tempreg64.reghi));
                  cg.a_reg_dealloc(exprasmlist,NR_R0);

                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags := getresflags;
                end;
              xorn,orn,andn,addn:
                begin
                  if (location.register64.reglo = NR_NO) then
                    begin
                      location.register64.reglo := cg.getintregister(exprasmlist,OS_INT);
                      location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
                    end;

                  if (left.location.loc = LOC_CONSTANT) then
                    swapleftright;
                  if (right.location.loc = LOC_CONSTANT) then
                    cg64.a_op64_const_reg_reg(exprasmlist,op,location.size,right.location.value64,
                      left.location.register64,location.register64)
                  else
                    cg64.a_op64_reg_reg_reg(exprasmlist,op,location.size,right.location.register64,
                      left.location.register64,location.register64);
                end;
              subn:
                begin
                  if left.location.loc <> LOC_CONSTANT then
                    begin
                      if (location.register64.reglo = NR_NO) then
                        begin
                         location.register64.reglo := cg.getintregister(exprasmlist,OS_INT);
                         location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
                      end;
                      if right.location.loc <> LOC_CONSTANT then
                        // reg64 - reg64
                        cg64.a_op64_reg_reg_reg(exprasmlist,OP_SUB,location.size,
                          right.location.register64,left.location.register64,
                          location.register64)
                      else
                        // reg64 - const64
                        cg64.a_op64_const_reg_reg(exprasmlist,OP_SUB,location.size,
                          right.location.value64,left.location.register64,
                          location.register64)
                    end
                  else if ((left.location.value64 shr 32) = 0) then
                    begin
                      if (location.register64.reglo = NR_NO) then
                        begin
                         location.register64.reglo := cg.getintregister(exprasmlist,OS_INT);
                         location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
                      end;
                      if (int64(left.location.value64) >= low(smallint)) and
                         (int64(left.location.value64) <= high(smallint)) then
                        begin
                          // consts16 - reg64
                          exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                            location.register64.reglo,right.location.register64.reglo,
                            left.location.value));
                        end
                      else
                        begin
                          // const32 - reg64
                          location_force_reg(exprasmlist,left.location,
                            OS_32,true);
                          exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBC,
                            location.register64.reglo,left.location.register64.reglo,
                            right.location.register64.reglo));
                        end;
                      exprasmlist.concat(taicpu.op_reg_reg(A_SUBFZE,
                        location.register64.reghi,right.location.register64.reghi));
                    end
                  else if (aint(left.location.value64) = 0) then
                    begin
                      // (const32 shl 32) - reg64
                      if (location.register64.reglo = NR_NO) then
                        begin
                         location.register64.reglo := cg.getintregister(exprasmlist,OS_INT);
                         location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
                      end;
                      exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                        location.register64.reglo,right.location.register64.reglo,0));
                      left.location.value64 := left.location.value64 shr 32;
                      location_force_reg(exprasmlist,left.location,OS_32,true);
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBFE,
                        location.register64.reghi,right.location.register64.reghi,
                        left.location.register));
                    end
                  else
                    begin
                      // const64 - reg64
                      location_force_reg(exprasmlist,left.location,
                        def_cgsize(left.resulttype.def),false);
                      if (left.location.loc = LOC_REGISTER) then
                        location.register64 := left.location.register64
                      else if (location.register64.reglo = NR_NO) then
                        begin
                         location.register64.reglo := cg.getintregister(exprasmlist,OS_INT);
                         location.register64.reghi := cg.getintregister(exprasmlist,OS_INT);
                        end;
                      cg64.a_op64_reg_reg_reg(exprasmlist,OP_SUB,location.size,
                        right.location.register64,left.location.register64,
                        location.register64);
                     end;
                end;
              else
                internalerror(2002072803);
            end;
          end
        else
          begin
            if is_signed(resulttype.def) then
              begin
                case nodetype of
                  addn:
                    begin
                      op1 := A_ADDC;
                      op2 := A_ADDEO;
                    end;
                  subn:
                    begin
                      op1 := A_SUBC;
                      op2 := A_SUBFEO;
                    end;
                  else
                    internalerror(2002072806);
                end
              end
            else
              begin
                case nodetype of
                  addn:
                    begin
                      op1 := A_ADDC;
                      op2 := A_ADDE;
                    end;
                  subn:
                    begin
                      op1 := A_SUBC;
                      op2 := A_SUBFE;
                    end;
                end;
              end;
            exprasmlist.concat(taicpu.op_reg_reg_reg(op1,location.register64.reglo,
              left.location.register64.reglo,right.location.register64.reglo));
            exprasmlist.concat(taicpu.op_reg_reg_reg(op2,location.register64.reghi,
              right.location.register64.reghi,left.location.register64.reghi));
            if not(is_signed(resulttype.def)) then
              if nodetype = addn then
                exprasmlist.concat(taicpu.op_reg_reg(A_CMPLW,location.register64.reghi,left.location.register64.reghi))
              else
                exprasmlist.concat(taicpu.op_reg_reg(A_CMPLW,left.location.register64.reghi,location.register64.reghi));
            cg.g_overflowcheck(exprasmlist,location,resulttype.def);
          end;

        { set result location }
        { (emit_compare sets it to LOC_FLAGS for compares, so set the }
        {  real location only now) (JM)                               }
        if cmpop and
           not(nodetype in [equaln,unequaln]) then
          location_reset(location,LOC_JUMP,OS_NO);
      end;


{*****************************************************************************
                                AddMMX
*****************************************************************************}

{$ifdef SUPPORT_MMX}
    procedure ti386addnode.second_addmmx;
      var
        op         : TAsmOp;
        cmpop      : boolean;
        mmxbase    : tmmxtype;
        hregister  : tregister;
      begin
        pass_left_and_right;

        cmpop:=false;
        mmxbase:=mmx_type(left.resulttype.def);
        case nodetype of
          addn :
            begin
              if (cs_mmx_saturation in aktlocalswitches) then
                begin
                   case mmxbase of
                      mmxs8bit:
                        op:=A_PADDSB;
                      mmxu8bit:
                        op:=A_PADDUSB;
                      mmxs16bit,mmxfixed16:
                        op:=A_PADDSB;
                      mmxu16bit:
                        op:=A_PADDUSW;
                   end;
                end
              else
                begin
                   case mmxbase of
                      mmxs8bit,mmxu8bit:
                        op:=A_PADDB;
                      mmxs16bit,mmxu16bit,mmxfixed16:
                        op:=A_PADDW;
                      mmxs32bit,mmxu32bit:
                        op:=A_PADDD;
                   end;
                end;
            end;
          muln :
            begin
               case mmxbase of
                  mmxs16bit,mmxu16bit:
                    op:=A_PMULLW;
                  mmxfixed16:
                    op:=A_PMULHW;
               end;
            end;
          subn :
            begin
              if (cs_mmx_saturation in aktlocalswitches) then
                begin
                   case mmxbase of
                      mmxs8bit:
                        op:=A_PSUBSB;
                      mmxu8bit:
                        op:=A_PSUBUSB;
                      mmxs16bit,mmxfixed16:
                        op:=A_PSUBSB;
                      mmxu16bit:
                        op:=A_PSUBUSW;
                   end;
                end
              else
                begin
                   case mmxbase of
                      mmxs8bit,mmxu8bit:
                        op:=A_PSUBB;
                      mmxs16bit,mmxu16bit,mmxfixed16:
                        op:=A_PSUBW;
                      mmxs32bit,mmxu32bit:
                        op:=A_PSUBD;
                   end;
                end;
            end;
          xorn:
            op:=A_PXOR;
          orn:
            op:=A_POR;
          andn:
            op:=A_PAND;
          else
            internalerror(200403183);
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_MMXREGISTER) then
         begin
           if (right.location.loc=LOC_MMXREGISTER) then
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              { register variable ? }
              if (left.location.loc=LOC_CMMXREGISTER) then
               begin
                 hregister:=rg.getregistermm(exprasmlist);
                 emit_reg_reg(A_MOVQ,S_NO,left.location.register,hregister);
               end
              else
               begin
                 if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203245);

                 location_release(exprasmlist,left.location);

                 hregister:=rg.getregistermm(exprasmlist);
                 emit_ref_reg(A_MOVQ,S_NO,left.location.reference,hregister);
               end;

              location_reset(left.location,LOC_MMXREGISTER,OS_NO);
              left.location.register:=hregister;
            end;
         end;

        { at this point, left.location.loc should be LOC_MMXREGISTER }
        if right.location.loc<>LOC_MMXREGISTER then
         begin
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if right.location.loc=LOC_CMMXREGISTER then
               begin
                 emit_reg_reg(A_MOVQ,S_NO,right.location.register,R_MM7);
                 emit_reg_reg(op,S_NO,left.location.register,R_MM7);
                 emit_reg_reg(A_MOVQ,S_NO,R_MM7,left.location.register);
               end
              else
               begin
                 if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203247);
                 emit_ref_reg(A_MOVQ,S_NO,right.location.reference,R_MM7);
                 emit_reg_reg(op,S_NO,left.location.register,R_MM7);
                 emit_reg_reg(A_MOVQ,S_NO,R_MM7,left.location.register);
                 location_release(exprasmlist,right.location);
               end;
            end
           else
            begin
              if (right.location.loc=LOC_CMMXREGISTER) then
               begin
                 emit_reg_reg(op,S_NO,right.location.register,left.location.register);
               end
              else
               begin
                 if not(right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203246);
                 emit_ref_reg(op,S_NO,right.location.reference,left.location.register);
                 location_release(exprasmlist,right.location);
               end;
            end;
          end
        else
          begin
            { right.location=LOC_MMXREGISTER }
            if (nodetype=subn) and (nf_swaped in flags) then
             begin
               emit_reg_reg(op,S_NO,left.location.register,right.location.register);
               location_swap(left.location,right.location);
               toggleflag(nf_swaped);
             end
            else
             begin
               emit_reg_reg(op,S_NO,right.location.register,left.location.register);
             end;
          end;

        location_freetemp(exprasmlist,right.location);
        location_release(exprasmlist,right.location);
        if cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;
        set_result_location(cmpop,true);
      end;
{$endif SUPPORT_MMX}


{*****************************************************************************
                                pass_2
*****************************************************************************}

    procedure tppcaddnode.pass_2;
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }
      var
         cgop       : topcg;
         op         : tasmop;
         tmpreg     : tregister;
         hl         : tasmlabel;
         cmpop      : boolean;

         { true, if unsigned types are compared }
         unsigned : boolean;

      begin
         { to make it more readable, string and set (not smallset!) have their
           own procedures }
         case left.resulttype.def.deftype of
           orddef :
             begin
               { handling boolean expressions }
               if is_boolean(left.resulttype.def) and
                  is_boolean(right.resulttype.def) then
                 begin
                   second_addboolean;
                   exit;
                 end
               { 64bit operations }
               else if is_64bit(left.resulttype.def) then
                 begin
                   second_add64bit;
                   exit;
                 end;
             end;
           stringdef :
             begin
               internalerror(2002072402);
               exit;
             end;
           setdef :
             begin
               { normalsets are already handled in pass1 }
               if (tsetdef(left.resulttype.def).settype<>smallset) then
                internalerror(200109041);
               second_addsmallset;
               exit;
             end;
           arraydef :
             begin
{$ifdef SUPPORT_MMX}
               if is_mmx_able_array(left.resulttype.def) then
                begin
                  second_addmmx;
                  exit;
                end;
{$endif SUPPORT_MMX}
             end;
           floatdef :
             begin
               second_addfloat;
               exit;
             end;
         end;

         { defaults }
         cmpop:=nodetype in [ltn,lten,gtn,gten,equaln,unequaln];
         unsigned:=not(is_signed(left.resulttype.def)) or
                   not(is_signed(right.resulttype.def));

         pass_left_and_right;

         { Convert flags to register first }
         { can any of these things be in the flags actually?? (JM) }

         if (left.location.loc = LOC_FLAGS) or
            (right.location.loc = LOC_FLAGS) then
           internalerror(2002072602);

         { set result location }
         if not cmpop then
           location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
          else
           location_reset(location,LOC_FLAGS,OS_NO);

         load_left_right(cmpop, (cs_check_overflow in aktlocalswitches) and
            (nodetype in [addn,subn,muln]));

         if (location.register = NR_NO) and
            not(cmpop) then
           location.register := cg.getintregister(exprasmlist,OS_INT);

         if not(cs_check_overflow in aktlocalswitches) or
            (cmpop) or
            (nodetype in [orn,andn,xorn]) then
           begin
             case nodetype of
               addn, muln, xorn, orn, andn:
                 begin
                   case nodetype of
                     addn:
                       cgop := OP_ADD;
                     muln:
                       if unsigned then
                         cgop := OP_MUL
                       else
                         cgop := OP_IMUL;
                     xorn:
                       cgop := OP_XOR;
                     orn:
                       cgop := OP_OR;
                     andn:
                       cgop := OP_AND;
                   end;
                   if (left.location.loc = LOC_CONSTANT) then
                     swapleftright;
                   if (right.location.loc <> LOC_CONSTANT) then
                     cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,
                       left.location.register,right.location.register,
                       location.register)
                   else
                     cg.a_op_const_reg_reg(exprasmlist,cgop,OS_INT,
                       right.location.value,left.location.register,
                     location.register);
                 end;
               subn:
                 begin
                   if (nf_swaped in flags) then
                     swapleftright;
                   if left.location.loc <> LOC_CONSTANT then
                     if right.location.loc <> LOC_CONSTANT then
                       cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,OS_INT,
                         right.location.register,left.location.register,
                         location.register)
                     else
                       cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                         right.location.value,left.location.register,
                         location.register)
                   else
                     if (longint(left.location.value) >= low(smallint)) and
                        (longint(left.location.value) <= high(smallint)) then
                       begin
                         exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                           location.register,right.location.register,
                           longint(left.location.value)));
                       end
                     else
                       begin
                         tmpreg := cg.getintregister(exprasmlist,OS_INT);
                         cg.a_load_const_reg(exprasmlist,OS_INT,
                           left.location.value,tmpreg);
                         cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,OS_INT,
                           right.location.register,tmpreg,location.register);
                       end;
                 end;
               ltn,lten,gtn,gten,equaln,unequaln :
                 begin
                   emit_compare(unsigned);
                 end;
             end;
           end
         else
           // overflow checking is on and we have an addn, subn or muln
           begin
             if is_signed(resulttype.def) then
               begin
                 case nodetype of
                   addn:
                     op := A_ADDO;
                   subn:
                     begin
                       op := A_SUBO;
                       if (nf_swaped in flags) then
                         swapleftright;
                     end;
                   muln:
                     op := A_MULLWO;
                   else
                     internalerror(2002072601);
                 end;
                 exprasmlist.concat(taicpu.op_reg_reg_reg(op,location.register,
                   left.location.register,right.location.register));
                 cg.g_overflowcheck(exprasmlist,location,resulttype.def);
              end
             else
              begin
                case nodetype of
                  addn:
                    begin
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_ADD,location.register,
                        left.location.register,right.location.register));
                      exprasmlist.concat(taicpu.op_reg_reg(A_CMPLW,location.register,left.location.register));
                      cg.g_overflowcheck(exprasmlist,location,resulttype.def);
                    end;
                  subn:
                    begin
                      if nf_swaped in flags then
                        swapleftright;
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,location.register,
                        left.location.register,right.location.register));
                      exprasmlist.concat(taicpu.op_reg_reg(A_CMPLW,left.location.register,location.register));
                      cg.g_overflowcheck(exprasmlist,location,resulttype.def);
                    end;
                  muln:
                    begin
                      { calculate the upper 32 bits of the product, = 0 if no overflow }
                      cg.a_reg_alloc(exprasmlist,NR_R0);
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULHWU_,NR_R0,
                        left.location.register,right.location.register));
                      cg.a_reg_dealloc(exprasmlist,NR_R0);
                      { calculate the real result }
                      exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULLW,location.register,
                        left.location.register,right.location.register));
                      { g_overflowcheck generates a OC_AE instead of OC_EQ :/ }
                      objectlibrary.getlabel(hl);
                      tcgppc(cg).a_jmp_cond(exprasmlist,OC_EQ,hl);
                      cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
                      cg.a_label(exprasmlist,hl);
                    end;
                end;
              end;
           end;
      end;

begin
   caddnode:=tppcaddnode;
end.
