{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Code generation for add nodes on the i386

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
unit n386add;

{$i defines.inc}

interface

    uses
       node,nadd,cpubase,cginfo;

    type
       ti386addnode = class(taddnode)
          procedure pass_2;override;
         protected
          function  first_addstring : tnode; override;
         private
          procedure pass_left_and_right(var pushedfpu:boolean);
          function  getresflags(unsigned : boolean) : tresflags;
          procedure left_must_be_reg(opsize:TOpSize;noswap:boolean);
          procedure emit_op_right_left(op:TAsmOp;opsize:TOpSize);
          procedure emit_generic_code(op:TAsmOp;opsize:TOpSize;unsigned,extra_not,mboverflow:boolean);
          procedure set_result_location(cmpop,unsigned:boolean);
          procedure second_addstring;
          procedure second_addboolean;
          procedure second_addfloat;
          procedure second_addsmallset;
{$ifdef SUPPORT_MMX}
          procedure second_addmmx;
{$endif SUPPORT_MMX}
          procedure second_add64bit;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasm,types,htypechk,
      cgbase,pass_2,regvars,
      cpuasm,
      ncon,nset,
      tainst,cga,ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

      const
         opsize_2_cgsize : array[S_B..S_L] of tcgsize = (OS_8,OS_16,OS_32);

    procedure ti386addnode.pass_left_and_right(var pushedfpu:boolean);
      var
        pushedregs : tmaybesave;
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
      end;


    function ti386addnode.getresflags(unsigned : boolean) : tresflags;
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


    procedure ti386addnode.left_must_be_reg(opsize:TOpSize;noswap:boolean);
      begin
        { left location is not a register? }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           { if right is register then we can swap the locations }
           if (not noswap) and
              (right.location.loc=LOC_REGISTER) then
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              { maybe we can reuse a constant register when the
                operation is a comparison that doesn't change the
                value of the register }
              location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],(nodetype in [ltn,lten,gtn,gten,equaln,unequaln]));
            end;
          end;
       end;


    procedure ti386addnode.emit_op_right_left(op:TAsmOp;opsize:TOpsize);
      begin
        { left must be a register }
        case right.location.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            exprasmlist.concat(taicpu.op_reg_reg(op,opsize,right.location.register,left.location.register));
          LOC_REFERENCE,
          LOC_CREFERENCE :
            exprasmlist.concat(taicpu.op_ref_reg(op,opsize,right.location.reference,left.location.register));
          LOC_CONSTANT :
            exprasmlist.concat(taicpu.op_const_reg(op,opsize,right.location.value,left.location.register));
          else
            internalerror(200203232);
        end;
      end;


    procedure ti386addnode.set_result_location(cmpop,unsigned:boolean);
      begin
        if cmpop then
         begin
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=getresflags(unsigned);
         end
        else
         location_copy(location,left.location);
      end;


    procedure ti386addnode.emit_generic_code(op:TAsmOp;opsize:TOpSize;unsigned,extra_not,mboverflow:boolean);
      var
        power : longint;
        hl4   : tasmlabel;
      begin
        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { right.location is a LOC_REGISTER }
           { when swapped another result register }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if extra_not then
               emit_reg(A_NOT,S_L,left.location.register);
              emit_reg_reg(op,opsize,left.location.register,right.location.register);
              { newly swapped also set swapped flag }
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              if extra_not then
                emit_reg(A_NOT,S_L,right.location.register);
              emit_reg_reg(op,opsize,right.location.register,left.location.register);
            end;
         end
        else
         begin
           { right.location is not a LOC_REGISTER }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if extra_not then
                emit_reg(A_NOT,opsize,left.location.register);
              rg.getexplicitregisterint(exprasmlist,R_EDI);
              cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
              emit_reg_reg(op,opsize,left.location.register,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.register);
              rg.ungetregisterint(exprasmlist,R_EDI);
            end
           else
            begin
               { Optimizations when right.location is a constant value }
               if (op=A_CMP) and
                  (nodetype in [equaln,unequaln]) and
                  (right.location.loc=LOC_CONSTANT) and
                  (right.location.value=0) then
                 begin
                   emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
                 end
               else
                 if (op=A_ADD) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_INC,opsize,left.location.register);
                  end
               else
                 if (op=A_SUB) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_DEC,opsize,left.location.register);
                  end
               else
                 if (op=A_IMUL) and
                    (right.location.loc=LOC_CONSTANT) and
                    (ispowerof2(right.location.value,power)) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_const_reg(A_SHL,opsize,power,left.location.register);
                  end
               else
                 begin
                   if extra_not then
                     begin
                        rg.getexplicitregisterint(exprasmlist,R_EDI);
                        cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
                        emit_reg(A_NOT,S_L,R_EDI);
                        emit_reg_reg(A_AND,S_L,R_EDI,left.location.register);
                        rg.ungetregisterint(exprasmlist,R_EDI);
                     end
                   else
                     begin
                        emit_op_right_left(op,opsize);
                     end;
                 end;
            end;
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                                   }
        if mboverflow then
         begin
           if cs_check_overflow in aktlocalswitches  then
            begin
              getlabel(hl4);
              if unsigned then
               emitjmp(C_NB,hl4)
              else
               emitjmp(C_NO,hl4);
              cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
              cg.a_label(exprasmlist,hl4);
            end;
         end;
      end;

{*****************************************************************************
                                Addstring
*****************************************************************************}

    { note: if you implemented an fpc_shortstr_concat similar to the    }
    { one in i386.inc, you have to override first_addstring like in     }
    { ti386addnode.first_string and implement the shortstring concat    }
    { manually! The generic routine is different from the i386 one (JM) }
    function ti386addnode.first_addstring : tnode;

      begin
        { special cases for shortstrings, handled in pass_2 (JM) }
        { can't handle fpc_shortstr_compare with compilerproc either because it }
        { returns its results in the flags instead of in eax                    }
        if (((nodetype = addn) and
             is_shortstring(resulttype.def)) or
            ((nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) and
             is_shortstring(left.resulttype.def))) then
          begin
            if nodetype = addn then
              location_reset(location,LOC_CREFERENCE,def_cgsize(resulttype.def))
            else
              location_reset(location,LOC_FLAGS,OS_NO);
            calcregisters(self,0,0,0);
            result := nil;
            exit;
          end;
        { otherwise, use the generic code }
        result := inherited first_addstring;
      end;


    procedure ti386addnode.second_addstring;

      var
        href       : treference;
        cmpop      : boolean;
        pushed     : tpushedsaved;
        regstopush : tregisterset;
      begin
        { string operations are not commutative }
        if nf_swaped in flags then
          swapleftright;
        case tstringdef(left.resulttype.def).string_typ of
           st_shortstring:
             begin
                case nodetype of
                   addn:
                     begin
                        cmpop:=false;
                        secondpass(left);
                        { if str_concat is set in expr
                          s:=s+ ... no need to create a temp string (PM) }
                        { the tempstring can also come from a typeconversion }
                        { or a function result, so simply check for a        }
                        { temp of 256 bytes(JM)                                          }
                        if not(tg.istemp(left.location.reference) and
                               (tg.getsizeoftemp(left.location.reference) = 256)) and
                           not(nf_use_strconcat in flags) then
                          begin
                             tg.gettempofsizereference(exprasmlist,256,href);
                             cg.g_copyshortstring(exprasmlist,left.location.reference,href,255,true,false);
                             { location is released by copyshortstring }
                             location_freetemp(exprasmlist,left.location);

                             location_reset(left.location,LOC_CREFERENCE,def_cgsize(resulttype.def));
                             left.location.reference:=href;
                          end;

                        secondpass(right);

                        { on the right we do not need the register anymore too }
                        { Instead of releasing them already, simply do not }
                        { push them (so the release is in the right place, }
                        { because emitpushreferenceaddr doesn't need extra }
                        { registers) (JM)                                  }
                        regstopush := all_registers;
                        remove_non_regvars_from_loc(right.location,regstopush);
                        rg.saveusedregisters(exprasmlist,pushed,regstopush);
                        { push the maximum possible length of the result }
                        cg.a_paramaddr_ref(exprasmlist,left.location.reference,2);
                        { the optimizer can more easily put the          }
                        { deallocations in the right place if it happens }
                        { too early than when it happens too late (if    }
                        { the pushref needs a "lea (..),edi; push edi")  }
                        location_release(exprasmlist,right.location);
                        cg.a_paramaddr_ref(exprasmlist,right.location.reference,1);
                        rg.saveregvars(exprasmlist,regstopush);
                        cg.a_call_name(exprasmlist,'FPC_SHORTSTR_CONCAT');
                        tg.ungetiftemp(exprasmlist,right.location.reference);
                        cg.g_maybe_loadself(exprasmlist);
                        rg.restoreusedregisters(exprasmlist,pushed);
                        location_copy(location,left.location);
                     end;
                   ltn,lten,gtn,gten,equaln,unequaln :
                     begin
                       cmpop := true;
                       rg.saveusedregisters(exprasmlist,pushed,all_registers);
                       secondpass(left);
                       location_release(exprasmlist,left.location);
                       cg.a_paramaddr_ref(exprasmlist,left.location.reference,2);
                       secondpass(right);
                       location_release(exprasmlist,right.location);
                       cg.a_paramaddr_ref(exprasmlist,right.location.reference,1);
                       rg.saveregvars(exprasmlist,all_registers);
                       cg.a_call_name(exprasmlist,'FPC_SHORTSTR_COMPARE');
                       cg.g_maybe_loadself(exprasmlist);
                       rg.restoreusedregisters(exprasmlist,pushed);
                       location_freetemp(exprasmlist,left.location);
                       location_freetemp(exprasmlist,right.location);
                     end;
                end;
                set_result_location(cmpop,true);
             end;
           else
             { rest should be handled in first pass (JM) }
             internalerror(200108303);
       end;
     end;

{*****************************************************************************
                                AddBoolean
*****************************************************************************}

    procedure ti386addnode.second_addboolean;
      var
        op      : TAsmOp;
        opsize  : TOpsize;
        cmpop,
        isjump  : boolean;
        otl,ofl : tasmlabel;
        pushedregs : tmaybesave;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        cmpop:=false;
        if (torddef(left.resulttype.def).typ=bool8bit) or
           (torddef(right.resulttype.def).typ=bool8bit) then
         opsize:=S_B
        else
          if (torddef(left.resulttype.def).typ=bool16bit) or
             (torddef(right.resulttype.def).typ=bool16bit) then
           opsize:=S_W
        else
           opsize:=S_L;

        if (cs_full_boolean_eval in aktlocalswitches) or
           (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
          begin
            if left.nodetype in [ordconstn,realconstn] then
             swapleftright;

            isjump:=(left.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
              end;
            secondpass(left);
            if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
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
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
              end;
            secondpass(right);
            maybe_restore(exprasmlist,left.location,pushedregs);
            if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end;

            { left must be a register }
            left_must_be_reg(opsize,false);
            { compare the }
            case nodetype of
              ltn,lten,gtn,gten,
              equaln,unequaln :
                begin
                  op:=A_CMP;
                  cmpop:=true;
                end;
              xorn :
                op:=A_XOR;
              orn :
                op:=A_OR;
              andn :
                op:=A_AND;
              else
                internalerror(200203247);
            end;
            emit_op_right_left(op,opsize);
            location_freetemp(exprasmlist,right.location);
            location_release(exprasmlist,right.location);
            if cmpop then
             begin
               location_freetemp(exprasmlist,left.location);
               location_release(exprasmlist,left.location);
             end;
            set_result_location(cmpop,true);
         end
        else
         begin
           case nodetype of
             andn,
             orn :
               begin
                 location_reset(location,LOC_JUMP,OS_NO);
                 case nodetype of
                   andn :
                     begin
                        otl:=truelabel;
                        getlabel(truelabel);
                        secondpass(left);
                        maketojumpbool(exprasmlist,left,lr_load_regvars);
                        cg.a_label(exprasmlist,truelabel);
                        truelabel:=otl;
                     end;
                   orn :
                     begin
                        ofl:=falselabel;
                        getlabel(falselabel);
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
             else
               CGMessage(type_e_mismatch);
           end;
         end;
      end;


{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure ti386addnode.second_addfloat;
      var
        op         : TAsmOp;
        resflags   : tresflags;
        pushedfpu,
        cmpop      : boolean;
      begin
        pass_left_and_right(pushedfpu);

        cmpop:=false;
        case nodetype of
          addn :
            op:=A_FADDP;
          muln :
            op:=A_FMULP;
          subn :
            op:=A_FSUBP;
          slashn :
            op:=A_FDIVP;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
              op:=A_FCOMPP;
              cmpop:=true;
            end;
          else
            CGMessage(type_e_mismatch);
        end;

        if (right.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,
               right.location,R_ST);
           if (right.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
           if (left.location.loc<>LOC_FPUREGISTER) then
            begin
              cg.a_loadfpu_loc_reg(exprasmlist,left.location,R_ST);
              if (left.location.loc <> LOC_CFPUREGISTER) and
                 pushedfpu then
                location_freetemp(exprasmlist,left.location);
            end
           else
            begin
              { left was on the stack => swap }
              toggleflag(nf_swaped);
            end;

           { releases the right reference }
           location_release(exprasmlist,right.location);
         end
        { the nominator in st0 }
        else if (left.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,left.location,R_ST);
           if (left.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
         end
        else
         begin
           { fpu operands are always in the wrong order on the stack }
           toggleflag(nf_swaped);
         end;

        { releases the left reference }
        if (left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          location_release(exprasmlist,left.location);

        { if we swaped the tree nodes, then use the reverse operator }
        if nf_swaped in flags then
          begin
             if (nodetype=slashn) then
               op:=A_FDIVRP
             else if (nodetype=subn) then
               op:=A_FSUBRP;
          end;
        { to avoid the pentium bug
        if (op=FDIVP) and (opt_processors=pentium) then
          cg.a_call_name(exprasmlist,'EMUL_FDIVP')
        else
        }
        { the Intel assemblers want operands }
        if op<>A_FCOMPP then
          begin
             emit_reg_reg(op,S_NO,R_ST,R_ST1);
             dec(trgcpu(rg).fpuvaroffset);
          end
        else
          begin
             emit_none(op,S_NO);
             dec(trgcpu(rg).fpuvaroffset,2);
          end;

        { on comparison load flags }
        if cmpop then
         begin
           if not(R_EAX in rg.unusedregsint) then
             begin
               rg.getexplicitregisterint(exprasmlist,R_EDI);
               emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
             end;
           emit_reg(A_FNSTSW,S_NO,R_AX);
           emit_none(A_SAHF,S_NO);
           if not(R_EAX in rg.unusedregsint) then
             begin
               emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
               rg.ungetregisterint(exprasmlist,R_EDI);
             end;
           if nf_swaped in flags then
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_A;
                    lten : resflags:=F_AE;
                     gtn : resflags:=F_B;
                    gten : resflags:=F_BE;
              end;
            end
           else
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_B;
                    lten : resflags:=F_BE;
                     gtn : resflags:=F_A;
                    gten : resflags:=F_AE;
              end;
            end;
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=resflags;
         end
        else
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
           location.register:=R_ST;
         end;
      end;


{*****************************************************************************
                                AddSmallSet
*****************************************************************************}

    procedure ti386addnode.second_addsmallset;
      var
        opsize : TOpSize;
        op     : TAsmOp;
        cmpop,
        pushedfpu,
        extra_not,
        noswap : boolean;
      begin
        pass_left_and_right(pushedfpu);

        { when a setdef is passed, it has to be a smallset }
        if ((left.resulttype.def.deftype=setdef) and
            (tsetdef(left.resulttype.def).settype<>smallset)) or
           ((right.resulttype.def.deftype=setdef) and
            (tsetdef(right.resulttype.def).settype<>smallset)) then
         internalerror(200203301);

        cmpop:=false;
        noswap:=false;
        extra_not:=false;
        opsize:=S_L;
        case nodetype of
          addn :
            begin
              { this is a really ugly hack!!!!!!!!!! }
              { this could be done later using EDI   }
              { as it is done for subn               }
              { instead of two registers!!!!         }
              { adding elements is not commutative }
              if (nf_swaped in flags) and (left.nodetype=setelementn) then
               swapleftright;
              { are we adding set elements ? }
              if right.nodetype=setelementn then
               begin
                 { no range support for smallsets! }
                 if assigned(tsetelementnode(right).right) then
                  internalerror(43244);
                 { bts requires both elements to be registers }
                 location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
                 location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],true);
                 op:=A_BTS;
                 noswap:=true;
               end
              else
               op:=A_OR;
            end;
          symdifn :
            op:=A_XOR;
          muln :
            op:=A_AND;
          subn :
            begin
              op:=A_AND;
              if (not(nf_swaped in flags)) and
                 (right.location.loc=LOC_CONSTANT) then
                right.location.value := not(right.location.value)
              else if (nf_swaped in flags) and
                      (left.location.loc=LOC_CONSTANT) then
                left.location.value := not(left.location.value)
              else
                extra_not:=true;
            end;
          equaln,
          unequaln :
            begin
              op:=A_CMP;
              cmpop:=true;
            end;
          lten,gten:
            begin
              If (not(nf_swaped in flags) and
                  (nodetype = lten)) or
                 ((nf_swaped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],true);
              emit_op_right_left(A_AND,opsize);
              op:=A_CMP;
              cmpop:=true;
              { warning: ugly hack, we need a JE so change the node to equaln }
              nodetype:=equaln;
            end;
          xorn :
            op:=A_XOR;
          orn :
            op:=A_OR;
          andn :
            op:=A_AND;
          else
            begin
              { no < or > support for sets }
              CGMessage(type_e_mismatch);
            end;
        end;
        { left must be a register }
        left_must_be_reg(opsize,noswap);
        emit_generic_code(op,opsize,true,extra_not,false);
        location_freetemp(exprasmlist,right.location);
        location_release(exprasmlist,right.location);
        if cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;
        set_result_location(cmpop,true);
      end;


{*****************************************************************************
                                Add64bit
*****************************************************************************}

    procedure ti386addnode.second_add64bit;
      var
        op         : TOpCG;
        op1,op2    : TAsmOp;
        opsize     : TOpSize;
        hregister,
        hregister2 : tregister;
        href       : treference;
        hl4        : tasmlabel;
        pushedfpu,
        mboverflow,
        cmpop,
        unsigned   : boolean;

      procedure firstjmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           load_all_regvars(exprasmlist);
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   emitjmp(flags_to_cond(getresflags(unsigned)),truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swaped);
                   emitjmp(flags_to_cond(getresflags(unsigned)),falselabel);
                   toggleflag(nf_swaped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   emitjmp(flags_to_cond(getresflags(unsigned)),truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   emitjmp(flags_to_cond(getresflags(unsigned)),falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                emitjmp(C_NE,falselabel);
              unequaln:
                emitjmp(C_NE,truelabel);
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
                   emitjmp(flags_to_cond(getresflags(true)),truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
              equaln:
                begin
                   emitjmp(C_NE,falselabel);
                   cg.a_jmp_always(exprasmlist,truelabel);
                end;
              unequaln:
                begin
                   emitjmp(C_NE,truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
           end;
        end;

      begin
        firstcomplex(self);

        pass_left_and_right(pushedfpu);

        op1:=A_NONE;
        op2:=A_NONE;
        mboverflow:=false;
        cmpop:=false;
        opsize:=S_L;
        unsigned:=((left.resulttype.def.deftype=orddef) and
                   (torddef(left.resulttype.def).typ=u64bit)) or
                  ((right.resulttype.def.deftype=orddef) and
                   (torddef(right.resulttype.def).typ=u64bit));
        case nodetype of
          addn :
            begin
              op:=OP_ADD;
              mboverflow:=true;
            end;
          subn :
            begin
              op:=OP_SUB;
              op1:=A_SUB;
              op2:=A_SBB;
              mboverflow:=true;
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
            CGMessage(type_e_mismatch);
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              { we can reuse a CREGISTER for comparison }
              if not((left.location.loc=LOC_CREGISTER) and cmpop) then
               begin
                 if (left.location.loc<>LOC_CREGISTER) then
                  begin
                    location_freetemp(exprasmlist,left.location);
                    location_release(exprasmlist,left.location);
                  end;
                 hregister:=rg.getregisterint(exprasmlist);
                 hregister2:=rg.getregisterint(exprasmlist);
                 tcg64f32(cg).a_load64_loc_reg(exprasmlist,left.location,hregister,hregister2);
                 location_reset(left.location,LOC_REGISTER,OS_64);
                 left.location.registerlow:=hregister;
                 left.location.registerhigh:=hregister2;
               end;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { when swapped another result register }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              tcg64f32(cg).a_op64_reg_reg(exprasmlist,op,
                left.location.registerlow,left.location.registerhigh,
                right.location.registerlow,right.location.registerhigh);
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else if cmpop then
            begin
              emit_reg_reg(A_CMP,S_L,right.location.registerhigh,left.location.registerhigh);
              firstjmp64bitcmp;
              emit_reg_reg(A_CMP,S_L,right.location.registerlow,left.location.registerlow);
              secondjmp64bitcmp;
            end
           else
            begin
              tcg64f32(cg).a_op64_reg_reg(exprasmlist,op,
                right.location.registerlow,right.location.registerhigh,
                left.location.registerlow,left.location.registerhigh);
            end;
           location_release(exprasmlist,right.location);
         end
        else
         begin
           { right.location<>LOC_REGISTER }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              rg.getexplicitregisterint(exprasmlist,R_EDI);
              tcg64f32(cg).a_load64low_loc_reg(exprasmlist,right.location,R_EDI);
              emit_reg_reg(op1,opsize,left.location.registerlow,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.registerlow);
              tcg64f32(cg).a_load64high_loc_reg(exprasmlist,right.location,R_EDI);
              { the carry flag is still ok }
              emit_reg_reg(op2,opsize,left.location.registerhigh,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.registerhigh);
              rg.ungetregisterint(exprasmlist,R_EDI);
              if right.location.loc<>LOC_CREGISTER then
               begin
                 location_freetemp(exprasmlist,right.location);
                 location_release(exprasmlist,right.location);
               end;
            end
           else if cmpop then
            begin
              case right.location.loc of
                LOC_CREGISTER :
                  begin
                    emit_reg_reg(A_CMP,S_L,right.location.registerhigh,left.location.registerhigh);
                    firstjmp64bitcmp;
                    emit_reg_reg(A_CMP,S_L,right.location.registerlow,left.location.registerlow);
                    secondjmp64bitcmp;
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    href:=right.location.reference;
                    inc(href.offset,4);
                    emit_ref_reg(A_CMP,S_L,href,left.location.registerhigh);
                    firstjmp64bitcmp;
                    emit_ref_reg(A_CMP,S_L,right.location.reference,left.location.registerlow);
                    secondjmp64bitcmp;
                    cg.a_jmp_always(exprasmlist,falselabel);
                    location_freetemp(exprasmlist,right.location);
                    location_release(exprasmlist,right.location);
                  end;
                LOC_CONSTANT :
                  begin
                    exprasmlist.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.valuehigh,left.location.registerhigh));
                    firstjmp64bitcmp;
                    exprasmlist.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.valuelow,left.location.registerlow));
                    secondjmp64bitcmp;
                  end;
                else
                  internalerror(200203282);
              end;
            end

           else
            begin
              tcg64f32(cg).a_op64_loc_reg(exprasmlist,op,right.location,
                left.location.registerlow,left.location.registerhigh);
              if (right.location.loc<>LOC_CREGISTER) then
               begin
                 location_freetemp(exprasmlist,right.location);
                 location_release(exprasmlist,right.location);
               end;
            end;
         end;

        if (left.location.loc<>LOC_CREGISTER) and cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                              }
        if mboverflow then
         begin
           if cs_check_overflow in aktlocalswitches  then
            begin
              getlabel(hl4);
              if unsigned then
               emitjmp(C_NB,hl4)
              else
               emitjmp(C_NO,hl4);
              cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
              cg.a_label(exprasmlist,hl4);
            end;
         end;

        { we have LOC_JUMP as result }
        if cmpop then
         location_reset(location,LOC_JUMP,OS_NO)
        else
         location_copy(location,left.location);
      end;


{*****************************************************************************
                                AddMMX
*****************************************************************************}

{$ifdef SUPPORT_MMX}
    procedure ti386addnode.second_addmmx;
      var
        op         : TAsmOp;
        pushedfpu,
        cmpop      : boolean;
        mmxbase    : tmmxtype;
        hregister  : tregister;
      begin
        pass_left_and_right(pushedfpu);

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
            CGMessage(type_e_mismatch);
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

    procedure ti386addnode.pass_2;
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }
      var
         popeax,popedx,
         pushedfpu,
         mboverflow,cmpop : boolean;
         op : tasmop;
         power : longint;
         opsize : topsize;

         { true, if unsigned types are compared }
         unsigned : boolean;
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         {is_in_dest : boolean;}
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;

         regstopush: tregisterset;

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
               else if is_64bitint(left.resulttype.def) then
                 begin
                   second_add64bit;
                   exit;
                 end;
             end;
           stringdef :
             begin
               second_addstring;
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
         {is_in_dest:=false;}
         extra_not:=false;
         mboverflow:=false;
         cmpop:=false;
         unsigned:=not(is_signed(left.resulttype.def)) or
                   not(is_signed(right.resulttype.def));
         opsize:=def_opsize(left.resulttype.def);

         pass_left_and_right(pushedfpu);

         if (left.resulttype.def.deftype=pointerdef) or
            (right.resulttype.def.deftype=pointerdef) or

            (is_class_or_interface(right.resulttype.def) and is_class_or_interface(left.resulttype.def)) or

            (left.resulttype.def.deftype=classrefdef) or

            (left.resulttype.def.deftype=procvardef) or

            ((left.resulttype.def.deftype=enumdef) and
             (left.resulttype.def.size=4)) or

            ((left.resulttype.def.deftype=orddef) and
             (torddef(left.resulttype.def).typ in [s32bit,u32bit])) or
            ((right.resulttype.def.deftype=orddef) and
             (torddef(right.resulttype.def).typ in [s32bit,u32bit])) then
          begin
            case nodetype of
              addn :
                begin
                  op:=A_ADD;
                  mboverflow:=true;
                end;
              muln :
                begin
                  if unsigned then
                    op:=A_MUL
                  else
                    op:=A_IMUL;
                  mboverflow:=true;
                end;
              subn :
                begin
                  op:=A_SUB;
                  mboverflow:=true;
                end;
              ltn,lten,
              gtn,gten,
              equaln,unequaln :
                begin
                  op:=A_CMP;
                  cmpop:=true;
                end;
              xorn :
                op:=A_XOR;
              orn :
                op:=A_OR;
              andn :
                op:=A_AND;
              else
                CGMessage(type_e_mismatch);
            end;

            { filter MUL, which requires special handling }
            if op=A_MUL then
             begin
               popeax:=false;
               popedx:=false;
               { here you need to free the symbol first }
               { left.location and right.location must }
               { only be freed when they are really released,  }
               { because the optimizer NEEDS correct regalloc  }
               { info!!! (JM)                                  }
               { the location.register will be filled in later (JM) }
               location_reset(location,LOC_REGISTER,OS_INT);

{$IfNDef NoShlMul}
               if right.nodetype=ordconstn then
                swapleftright;
               If (left.nodetype = ordconstn) and
                  ispowerof2(tordconstnode(left).value, power) and
                  not(cs_check_overflow in aktlocalswitches) then
                 Begin
                   { This release will be moved after the next }
                   { instruction by the optimizer. No need to  }
                   { release left.location, since it's a   }
                   { constant (JM)                             }
                   location_release(exprasmlist,right.location);
                   location.register:=rg.getregisterint(exprasmlist);
                   cg.a_load_loc_reg(exprasmlist,right.location,location.register);
                   cg.a_op_const_reg(exprasmlist,OP_SHL,power,location.register);
                 End
               Else
                Begin
{$EndIf NoShlMul}
                  regstopush := all_registers;
                  remove_non_regvars_from_loc(right.location,regstopush);
                  remove_non_regvars_from_loc(left.location,regstopush);
                  { now, regstopush does NOT contain EAX and/or EDX if they are }
                  { used in either the left or the right location, excepts if   }
                  {they are regvars. It DOES contain them if they are used in   }
                  { another location (JM)                                       }
                  if not(R_EAX in rg.unusedregsint) and
                     (R_EAX in regstopush) then
                   begin
                     emit_reg(A_PUSH,S_L,R_EAX);
                     popeax:=true;
                   end;
                  if not(R_EDX in rg.unusedregsint) and
                     (R_EDX in regstopush) then
                   begin
                     emit_reg(A_PUSH,S_L,R_EDX);
                     popedx:=true;
                   end;
                  { left.location can be R_EAX !!! }
                  rg.getexplicitregisterint(exprasmlist,R_EDI);
                  { load the left value }
                  cg.a_load_loc_reg(exprasmlist,left.location,R_EDI);
                  location_release(exprasmlist,left.location);
                  { allocate EAX }
                  if R_EAX in rg.unusedregsint then
                    exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                  { load he right value }
                  cg.a_load_loc_reg(exprasmlist,right.location,R_EAX);
                  location_release(exprasmlist,right.location);
                  { allocate EAX if it isn't yet allocated (JM) }
                  if (R_EAX in rg.unusedregsint) then
                    exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                  { also allocate EDX, since it is also modified by }
                  { a mul (JM)                                      }
                  if R_EDX in rg.unusedregsint then
                    exprasmList.concat(Tairegalloc.Alloc(R_EDX));
                  emit_reg(A_MUL,S_L,R_EDI);
                  rg.ungetregisterint(exprasmlist,R_EDI);
                  if R_EDX in rg.unusedregsint then
                    exprasmList.concat(Tairegalloc.DeAlloc(R_EDX));
                  if R_EAX in rg.unusedregsint then
                    exprasmList.concat(Tairegalloc.DeAlloc(R_EAX));
                  location.register:=rg.getregisterint(exprasmlist);
                  emit_reg_reg(A_MOV,S_L,R_EAX,location.register);
                  if popedx then
                   emit_reg(A_POP,S_L,R_EDX);
                  if popeax then
                   emit_reg(A_POP,S_L,R_EAX);
{$IfNDef NoShlMul}
                End;
{$endif NoShlMul}
               location_freetemp(exprasmlist,left.location);
               location_freetemp(exprasmlist,right.location);
               exit;
             end;

            { Convert flags to register first }
            if (left.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
            if (right.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],false);

            left_must_be_reg(opsize,false);
            emit_generic_code(op,opsize,unsigned,extra_not,mboverflow);
            location_freetemp(exprasmlist,right.location);
            location_release(exprasmlist,right.location);
            if cmpop and
               (left.location.loc<>LOC_CREGISTER) then
             begin
               location_freetemp(exprasmlist,left.location);
               location_release(exprasmlist,left.location);
             end;
            set_result_location(cmpop,unsigned);
          end

         { 8/16 bit enum,char,wchar types }
         else
          if ((left.resulttype.def.deftype=orddef) and
              (torddef(left.resulttype.def).typ in [uchar,uwidechar])) or
             ((left.resulttype.def.deftype=enumdef) and
              ((left.resulttype.def.size=1) or
               (left.resulttype.def.size=2))) then
           begin
             case nodetype of
               ltn,lten,gtn,gten,
               equaln,unequaln :
                 cmpop:=true;
               else
                 CGMessage(type_e_mismatch);
             end;
             left_must_be_reg(opsize,false);
             emit_op_right_left(A_CMP,opsize);
             location_freetemp(exprasmlist,right.location);
             location_release(exprasmlist,right.location);
             if left.location.loc<>LOC_CREGISTER then
              begin
                location_freetemp(exprasmlist,left.location);
                location_release(exprasmlist,left.location);
              end;
             set_result_location(true,true);
           end
         else
           CGMessage(type_e_mismatch);
      end;

begin
   caddnode:=ti386addnode;
end.
{
  $Log$
  Revision 1.36  2002-05-13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.35  2002/05/12 16:53:17  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.34  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.33  2002/04/05 15:09:13  jonas
    * fixed web bug 1915

  Revision 1.32  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.31  2002/04/02 17:11:35  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.29  2002/03/04 19:10:13  peter
    * removed compiler warnings

  Revision 1.28  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called :=rg.getregisterint(exprasmlist);

  Revision 1.27  2001/12/29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.25  2001/10/12 13:51:51  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.24  2001/09/17 21:29:13  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.23  2001/09/05 15:22:09  jonas
    * made multiplying, dividing and mod'ing of int64 and qword processor
      independent with compilerprocs (+ small optimizations by using shift/and
      where possible)

  Revision 1.22  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.21  2001/09/03 13:27:42  jonas
    * compilerproc implementation of set addition/substraction/...
    * changed the declaration of some set helpers somewhat to accomodate the
      above change
    * i386 still uses the old code for comparisons of sets, because its
      helpers return the results in the flags
    * dummy tc_normal_2_small_set type conversion because I need the original
      resulttype of the set add nodes
    NOTE: you have to start a cycle with 1.0.5!

  Revision 1.20  2001/08/30 15:43:14  jonas
    * converted adding/comparing of strings to compileproc. Note that due
      to the way the shortstring helpers for i386 are written, they are
      still handled by the old code (reason: fpc_shortstr_compare returns
      results in the flags instead of in eax and fpc_shortstr_concat
      has wierd parameter conventions). The compilerproc stuff should work
      fine with the generic implementations though.
    * removed some nested comments warnings

  Revision 1.19  2001/08/29 17:50:45  jonas
    * removed unused var

  Revision 1.18  2001/08/29 12:03:23  jonas
    * fixed wrong regalloc info around FPC_MUL/DIV/MOD_INT64/QWORD calls
    * fixed partial result overwriting with the above calls too

  Revision 1.17  2001/08/26 13:36:55  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.16  2001/07/08 21:00:16  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.15  2001/06/25 14:11:37  jonas
    * fixed set bug discovered by Carl (merged)

  Revision 1.14  2001/06/18 20:36:25  peter
    * -Ur switch (merged)
    * masm fixes (merged)
    * quoted filenames for go32v2 and win32

  Revision 1.13  2001/05/27 14:30:56  florian
    + some widestring stuff added

  Revision 1.12  2001/05/06 17:12:14  jonas
    * fixed an IE10 and another bug with [var1..var2] construct

  Revision 1.11  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.10  2001/04/02 21:20:36  peter
    * resulttype rewrite

  Revision 1.9  2000/12/31 11:14:11  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.8  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.7  2000/12/16 15:56:18  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.6  2000/12/05 11:44:32  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.5  2000/11/29 00:30:45  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/11/13 11:30:56  florian
    * some bugs with interfaces and NIL fixed

  Revision 1.3  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.6  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.4  2000/09/24 15:06:18  peter
    * use defines.inc

  Revision 1.3  2000/09/22 22:42:52  florian
    * more fixes

  Revision 1.2  2000/09/21 12:24:22  jonas
    * small fix to my changes for full boolean evaluation support (moved
      opsize determination for boolean operations back in boolean
      processing block)
    + full boolean evaluation support (from cg386add)

  Revision 1.1  2000/09/20 21:23:32  florian
    * initial revision
}
