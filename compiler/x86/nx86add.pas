{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Common code generation for add nodes on the i386 and x86

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
{
Common code generation for add nodes on the i386 and x86
}
unit nx86add;

{$i fpcdefs.inc}

  interface

    uses
      cgbase,
      cpubase,
      node,nadd,ncgadd;

    type
      tx86addnode = class(tcgaddnode)
        function  getresflags(unsigned : boolean) : tresflags;
        procedure set_result_location(cmpop,unsigned:boolean);
        procedure left_must_be_reg(opsize:TCGSize;noswap:boolean);
        procedure emit_op_right_left(op:TAsmOp;opsize:TOpSize);
        procedure emit_generic_code(op:TAsmOp;opsize:TCgSize;unsigned,extra_not,mboverflow:boolean);

        function  first_addstring : tnode; override;
        procedure pass_2;override;
        procedure second_addfloat;override;
        procedure second_addfloatsse;
        procedure second_addstring;
        procedure second_mul;virtual;abstract;
        procedure pass_left_and_right(var pushedfpu:boolean);
      end;


  implementation

    uses
      globtype,globals,
      verbose,
      cutils,
      aasmbase,aasmtai,aasmcpu,
      cpuinfo,
      symconst,symdef,
      cgobj,cgx86,cga,
      paramgr,
      htypechk,
      pass_2,ncgutil,
      ncon,
      defutil;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure tx86addnode.emit_generic_code(op:TAsmOp;opsize:TCGSize;unsigned,extra_not,mboverflow:boolean);
      var
        power : longint;
        hl4   : tasmlabel;
        r     : Tregister;
      begin
        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { right.location is a LOC_REGISTER }
           { when swapped another result register }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if extra_not then
               emit_reg(A_NOT,TCGSize2Opsize[opsize],left.location.register);
              emit_reg_reg(op,TCGSize2Opsize[opsize],left.location.register,right.location.register);
              { newly swapped also set swapped flag }
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              if extra_not then
                emit_reg(A_NOT,TCGSize2Opsize[opsize],right.location.register);
              if (op=A_ADD) or (op=A_OR) or (op=A_AND) or (op=A_XOR) or (op=A_IMUL) then
                location_swap(left.location,right.location);
              emit_reg_reg(op,TCGSize2Opsize[opsize],right.location.register,left.location.register);
            end;
         end
        else
         begin
           { right.location is not a LOC_REGISTER }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if extra_not then
                emit_reg(A_NOT,TCGSize2Opsize[opsize],left.location.register);
              r:=cg.getintregister(exprasmlist,OS_INT);
              cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,r);
              emit_reg_reg(op,TCGSize2Opsize[opsize],left.location.register,r);
              emit_reg_reg(A_MOV,TCGSize2Opsize[opsize],r,left.location.register);
              cg.ungetregister(exprasmlist,r);
            end
           else
            begin
               { Optimizations when right.location is a constant value }
               if (op=A_CMP) and
                  (nodetype in [equaln,unequaln]) and
                  (right.location.loc=LOC_CONSTANT) and
                  (right.location.value=0) then
                 begin
                   emit_reg_reg(A_TEST,TCGSize2Opsize[opsize],left.location.register,left.location.register);
                 end
               else
                 if (op=A_ADD) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_INC,TCGSize2Opsize[opsize],left.location.register);
                  end
               else
                 if (op=A_SUB) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_DEC,TCGSize2Opsize[opsize],left.location.register);
                  end
               else
                 if (op=A_IMUL) and
                    (right.location.loc=LOC_CONSTANT) and
                    (ispowerof2(right.location.value,power)) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_const_reg(A_SHL,TCGSize2Opsize[opsize],power,left.location.register);
                  end
               else
                 begin
                   if extra_not then
                     begin
                        r:=cg.getintregister(exprasmlist,OS_INT);
                        cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,r);
                        emit_reg(A_NOT,TCGSize2Opsize[opsize],r);
                        emit_reg_reg(A_AND,TCGSize2Opsize[opsize],r,left.location.register);
                        cg.ungetregister(exprasmlist,r);
                     end
                   else
                     begin
                        emit_op_right_left(op,TCGSize2Opsize[opsize]);
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
              objectlibrary.getlabel(hl4);
              if unsigned then
                cg.a_jmp_flags(exprasmlist,F_AE,hl4)
              else
                cg.a_jmp_flags(exprasmlist,F_NO,hl4);
              cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
              cg.a_label(exprasmlist,hl4);
            end;
         end;
      end;

    procedure tx86addnode.left_must_be_reg(opsize:TCGSize;noswap:boolean);
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
              location_force_reg(exprasmlist,left.location,opsize,(nodetype in [ltn,lten,gtn,gten,equaln,unequaln]));
            end;
          end;
       end;


    procedure tx86addnode.emit_op_right_left(op:TAsmOp;opsize:TOpsize);
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


    procedure tx86addnode.set_result_location(cmpop,unsigned:boolean);
      begin
        if cmpop then
         begin
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=getresflags(unsigned);
         end
        else
         location_copy(location,left.location);
      end;


    function tx86addnode.getresflags(unsigned : boolean) : tresflags;
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


{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure tx86addnode.pass_left_and_right(var pushedfpu:boolean);
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;
        secondpass(left);

        { are too few registers free? }
        if location.loc=LOC_FPUREGISTER then
          pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
        else
          pushedfpu:=false;
        secondpass(right);
      end;


    procedure tx86addnode.second_addfloat;
      var
        op         : TAsmOp;
        resflags   : tresflags;
        pushedfpu,
        cmpop      : boolean;
      begin
        if use_sse(resulttype.def) then
          begin
            second_addfloatsse;
            exit;
          end;
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
            internalerror(2003042214);
        end;

        if (right.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,right.location,NR_ST);
           if (right.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
           if (left.location.loc<>LOC_FPUREGISTER) then
            begin
              cg.a_loadfpu_loc_reg(exprasmlist,left.location,NR_ST);
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
           cg.a_loadfpu_loc_reg(exprasmlist,left.location,NR_ST);
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
             emit_reg_reg(op,S_NO,NR_ST,NR_ST1);
             tcgx86(cg).dec_fpu_stack;
          end
        else
          begin
             emit_none(op,S_NO);
             tcgx86(cg).dec_fpu_stack;
             tcgx86(cg).dec_fpu_stack;
          end;

        { on comparison load flags }
        if cmpop then
         begin
           cg.getexplicitregister(exprasmlist,NR_AX);
           emit_reg(A_FNSTSW,S_NO,NR_AX);
           emit_none(A_SAHF,S_NO);
           cg.ungetregister(exprasmlist,NR_AX);
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
           location.register:=NR_ST;
         end;
      end;


    procedure tx86addnode.second_addfloatsse;
      var
        op : topcg;
      begin
        pass_left_right;
        if (nf_swaped in flags) then
          swapleftright;

        case nodetype of
          addn :
            op:=OP_ADD;
          muln :
            op:=OP_MUL;
          subn :
            op:=OP_SUB;
          slashn :
            op:=OP_DIV;
          else
            internalerror(200312231);
        end;

        location_reset(location,LOC_MMREGISTER,def_cgsize(resulttype.def));
        { we can use only right as left operand if the operation is commutative }
        if (right.location.loc=LOC_MMREGISTER) and (op in [OP_ADD,OP_MUL]) then
          begin
            location.register:=right.location.register;
            { force floating point reg. location to be written to memory,
              we don't force it to mm register because writing to memory
              allows probably shorter code because there is no direct fpu->mm register
              copy instruction
            }
            if left.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER] then
              location_force_mem(exprasmlist,left.location);
            cg.a_opmm_loc_reg(exprasmlist,op,location.size,left.location,location.register,mms_movescalar);
            location_release(exprasmlist,left.location);
          end
        else
          begin
            location_force_mmregscalar(exprasmlist,left.location,false);
            location.register:=left.location.register;
            { force floating point reg. location to be written to memory,
              we don't force it to mm register because writing to memory
              allows probably shorter code because there is no direct fpu->mm register
              copy instruction
            }
            if right.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER] then
              location_force_mem(exprasmlist,right.location);
            cg.a_opmm_loc_reg(exprasmlist,op,location.size,right.location,location.register,mms_movescalar);
            location_release(exprasmlist,right.location);
          end;
      end;

{*****************************************************************************
                                Addstring
*****************************************************************************}

    { note: if you implemented an fpc_shortstr_concat similar to the    }
    { one in i386.inc, you have to override first_addstring like in     }
    { ti386addnode.first_string and implement the shortstring concat    }
    { manually! The generic routine is different from the i386 one (JM) }
    function tx86addnode.first_addstring : tnode;

      begin
        { special cases for shortstrings, handled in pass_2 (JM) }
        { can't handle fpc_shortstr_compare with compilerproc either because it }
        { returns its results in the flags instead of in eax                    }
        if (nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) and
           is_shortstring(left.resulttype.def) and
           not(((left.nodetype=stringconstn) and (str_length(left)=0)) or
              ((right.nodetype=stringconstn) and (str_length(right)=0))) then
         begin
           expectloc:=LOC_FLAGS;
           calcregisters(self,0,0,0);
           result := nil;
           exit;
         end;
        { otherwise, use the generic code }
        result := inherited first_addstring;
      end;


    procedure tx86addnode.second_addstring;
      var
        paraloc1,
        paraloc2   : tparalocation;
        hregister1,
        hregister2 : tregister;
      begin
        { string operations are not commutative }
        if nf_swaped in flags then
          swapleftright;
        case tstringdef(left.resulttype.def).string_typ of
           st_shortstring:
             begin
                case nodetype of
                   ltn,lten,gtn,gten,equaln,unequaln :
                     begin
                       paraloc1:=paramanager.getintparaloc(pocall_default,1);
                       paraloc2:=paramanager.getintparaloc(pocall_default,2);
                       { process parameters }
                       secondpass(left);
                       location_release(exprasmlist,left.location);
                       if paraloc2.loc=LOC_REGISTER then
                         begin
                           hregister2:=cg.getaddressregister(exprasmlist);
                           cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,hregister2);
                         end
                       else
                         begin
                           paramanager.allocparaloc(exprasmlist,paraloc2);
                           cg.a_paramaddr_ref(exprasmlist,left.location.reference,paraloc2);
                         end;
                       secondpass(right);
                       location_release(exprasmlist,right.location);
                       if paraloc1.loc=LOC_REGISTER then
                         begin
                           hregister1:=cg.getaddressregister(exprasmlist);
                           cg.a_loadaddr_ref_reg(exprasmlist,right.location.reference,hregister1);
                         end
                       else
                         begin
                           paramanager.allocparaloc(exprasmlist,paraloc1);
                           cg.a_paramaddr_ref(exprasmlist,right.location.reference,paraloc1);
                         end;
                       { push parameters }
                       if paraloc1.loc=LOC_REGISTER then
                         begin
                           cg.ungetregister(exprasmlist,hregister2);
                           paramanager.allocparaloc(exprasmlist,paraloc2);
                           cg.a_param_reg(exprasmlist,OS_ADDR,hregister2,paraloc2);
                         end;
                       if paraloc2.loc=LOC_REGISTER then
                         begin
                           cg.ungetregister(exprasmlist,hregister1);
                           paramanager.allocparaloc(exprasmlist,paraloc1);
                           cg.a_param_reg(exprasmlist,OS_ADDR,hregister1,paraloc1);
                         end;
                       paramanager.freeparaloc(exprasmlist,paraloc1);
                       paramanager.freeparaloc(exprasmlist,paraloc2);
                       cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                       cg.a_call_name(exprasmlist,'FPC_SHORTSTR_COMPARE');
                       cg.deallocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                       location_freetemp(exprasmlist,left.location);
                       location_freetemp(exprasmlist,right.location);
                     end;
                end;
                set_result_location(true,true);
             end;
           else
             { rest should be handled in first pass (JM) }
             internalerror(200108303);
       end;
     end;


{*****************************************************************************
                                pass_2
*****************************************************************************}

    procedure tx86addnode.pass_2;
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }
      var
         pushedfpu,
         mboverflow,cmpop : boolean;
         op : tasmop;
         opsize : tcgsize;

         { true, if unsigned types are compared }
         unsigned : boolean;
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         {is_in_dest : boolean;}
         { true, if for sets subtractions the extra not should generated }
         extra_not : boolean;

      begin
         { to make it more readable, string and set have their own procedures }
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
{$ifndef x86_64}
               { 64bit operations }
               else if is_64bit(left.resulttype.def) then
                 begin
                   second_add64bit;
                   exit;
                 end
{$endif x86_64}
               ;
             end;
           stringdef :
             begin
               second_addstring;
               exit;
             end;
           setdef :
             begin
              {Normalsets are already handled in pass1 if mmx
               should not be used.}
              if (tsetdef(left.resulttype.def).settype<>smallset) then
                begin
{$ifdef MMXSET}
                  if cs_mmx in aktlocalswitches then
                    second_addmmxset
                  else
{$endif MMXSET}
                    internalerror(200109041);
                end
              else
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
         opsize:=def_cgsize(left.resulttype.def);

         pass_left_and_right(pushedfpu);

         if (left.resulttype.def.deftype=pointerdef) or
            (right.resulttype.def.deftype=pointerdef) or

            (is_class_or_interface(right.resulttype.def) and is_class_or_interface(left.resulttype.def)) or

            (left.resulttype.def.deftype=classrefdef) or

            (left.resulttype.def.deftype=procvardef) or

{$ifdef x86_64}
            ((left.resulttype.def.deftype=enumdef) and
             (left.resulttype.def.size in [4,8])) or

            ((left.resulttype.def.deftype=orddef) and
             (torddef(left.resulttype.def).typ in [s32bit,u32bit,s64bit,u64bit])) or
            ((right.resulttype.def.deftype=orddef) and
             (torddef(right.resulttype.def).typ in [s32bit,u32bit,s64bit,u64bit])) then
{$else x86_64}

            ((left.resulttype.def.deftype=enumdef) and
             (left.resulttype.def.size=4)) or

            ((left.resulttype.def.deftype=orddef) and
             (torddef(left.resulttype.def).typ in [s32bit,u32bit])) or
            ((right.resulttype.def.deftype=orddef) and
             (torddef(right.resulttype.def).typ in [s32bit,u32bit])) then
{$endif x86_64}
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
                internalerror(200304229);
            end;

            { filter MUL, which requires special handling }
            if op=A_MUL then
             begin
               second_mul;
               exit;
             end;

            { Convert flags to register first }
            if (left.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,left.location,opsize,false);
            if (right.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,right.location,opsize,false);

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
                 internalerror(2003042210);
             end;
             left_must_be_reg(opsize,false);
             emit_op_right_left(A_CMP,TCGSize2Opsize[opsize]);
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
           internalerror(2003042211);
      end;

begin
   caddnode:=tx86addnode;
end.
{
  $Log$
  Revision 1.6  2004-01-20 12:59:37  florian
    * common addnode code for x86-64 and i386

  Revision 1.5  2003/12/26 13:19:16  florian
    * rtl and compiler compile with -Cfsse2

  Revision 1.4  2003/12/26 00:32:22  florian
    + fpu<->mm register conversion

  Revision 1.3  2003/12/25 01:07:09  florian
    + $fputype directive support
    + single data type operations with sse unit
    * fixed more x86-64 stuff

  Revision 1.2  2003/12/23 14:38:07  florian
    + second_floataddsse implemented

  Revision 1.1  2003/10/13 01:58:04  florian
    * some ideas for mm support implemented
}
