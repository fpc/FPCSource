{*****************************************************************************}
{ File                   : naddcpu.pas                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\07\14                                         }
{ Last modification date : 2002\07\26                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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
    along with this program; IF not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
UNIT naddcpu;
{$INCLUDE fpcdefs.inc}
INTERFACE
USES
  node,nadd,cpubase,cginfo;
TYPE
  TSparcAddNode=CLASS(TAddNode)
    PROCEDURE pass_2;OVERRIDE;
  PRIVATE
    FUNCTION GetResFlags(unsigned:Boolean):TResFlags;
    PROCEDURE left_must_be_reg(OpSize:TOpSize;NoSwap:Boolean);
    PROCEDURE emit_generic_code(op:TAsmOp;OpSize:TOpSize;unsigned,extra_not,mboverflow:Boolean);
    PROCEDURE emit_op_right_left(op:TAsmOp;OpSize:TOpsize);
    PROCEDURE set_result_location(cmpOp,unsigned:Boolean);
  END;
IMPLEMENTATION
USES
  globtype,systems,
  cutils,verbose,globals,
  symconst,symdef,paramgr,
  aasmbase,aasmtai,aasmcpu,defbase,htypechk,
  cgbase,pass_2,regvars,
  cpupara,
  ncon,nset,
  cga,ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;
CONST
  opsize_2_cgSize:ARRAY[S_B..S_L]OF TCgSize=(OS_8,OS_16,OS_32);
FUNCTION TSparcAddNode.GetResFlags(unsigned:Boolean):TResFlags;
  BEGIN
    CASE NodeType OF
      equaln:
        GetResFlags:=F_E;
      unequaln:
        GetResFlags:=F_NE;
      ELSE
        IF NOT(unsigned)
        THEN
          BEGIN
            IF nf_swaped IN flags
            THEN
              CASE NodeType OF
                ltn:
                  GetResFlags:=F_G;
                lten:
                  GetResFlags:=F_GE;
                gtn:
                  GetResFlags:=F_L;
                gten:
                  GetResFlags:=F_LE;
              END
            ELSE
              CASE NodeType OF
                ltn:
                  GetResFlags:=F_L;
                lten:
                  GetResFlags:=F_LE;
                gtn:
                  GetResFlags:=F_G;
                gten:
                  GetResFlags:=F_GE;
              END;
          END
        ELSE
          BEGIN
            IF nf_swaped IN Flags
            THEN
              CASE NodeType OF
                ltn:
                  GetResFlags:=F_A;
                lten:
                  GetResFlags:=F_AE;
                gtn:
                  GetResFlags:=F_B;
                gten:
                  GetResFlags:=F_BE;
              END
            ELSE
              CASE NodeType OF
                ltn:
                  GetResFlags:=F_B;
                lten:
                  GetResFlags:=F_BE;
                gtn:
                  GetResFlags:=F_A;
                gten:
                  GetResFlags:=F_AE;
              END;
          END;
    END;
  END;
PROCEDURE TSparcAddNode.left_must_be_reg(OpSize:TOpSize;NoSwap:Boolean);
  BEGIN
    IF(left.location.loc<>LOC_REGISTER)
    THEN{left location is not a register}
      BEGIN
        IF(NOT NoSwap)AND(right.location.loc=LOC_REGISTER)
        THEN{right is register so we can swap the locations}
          BEGIN
            location_swap(left.location,right.location);
            toggleflag(nf_swaped);
          END
        ELSE
          BEGIN
{maybe we can reuse a constant register when the operation is a comparison that
doesn't change the value of the register}
            location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],(nodetype IN [ltn,lten,gtn,gten,equaln,unequaln]));
          END;
      END;
   END;
PROCEDURE TSparcAddNode.emit_generic_code(op:TAsmOp;OpSize:TOpSize;unsigned,extra_not,mboverflow:Boolean);
  VAR
    power:LongInt;
    hl4:TAsmLabel;
  BEGIN
    { at this point, left.location.loc should be LOC_REGISTER }
    IF right.location.loc=LOC_REGISTER
    THEN
      BEGIN
           { right.location is a LOC_REGISTER }
           { when swapped another result register }
        IF(nodetype=subn)AND(nf_swaped IN flags)
        THEN
          BEGIN
            IF extra_not
            THEN
              emit_reg(A_NOT,S_L,left.location.register);
              emit_reg_reg(op,opsize,left.location.register,right.location.register);
            { newly swapped also set swapped flag }
            location_swap(left.location,right.location);
            toggleflag(nf_swaped);
          END
        ELSE
          BEGIN
            IF extra_not
            THEN
              emit_reg(A_NOT,S_L,right.location.register);
            emit_reg_reg(op,opsize,right.location.register,left.location.register);
          END;
      END
    ELSE
      BEGIN
        { right.location is not a LOC_REGISTER }
        IF(nodetype=subn)AND(nf_swaped IN flags)
        THEN
          BEGIN
            IF extra_not
            THEN
              emit_reg(A_NOT,opsize,left.location.register);
//          rg.getexplicitregisterint(exprasmlist,R_EDI);
//          cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
//          emit_reg_reg(op,opsize,left.location.register,R_EDI);
//          emit_reg_reg(A_MOV,opsize,R_EDI,left.location.register);
//          rg.ungetregisterint(exprasmlist,R_EDI);
          END
        ELSE
          BEGIN
            { Optimizations when right.location is a constant value }
            IF(op=A_CMP)AND(nodetype IN [equaln,unequaln])AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=0)
            THEN
              BEGIN
//                emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
              END
            ELSE IF(op=A_ADD)AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=1)AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              BEGIN
                emit_reg(A_INC,opsize,left.location.register);
              END
            ELSE IF(op=A_SUB)AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=1)AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              BEGIN
                emit_reg(A_DEC,opsize,left.location.register);
              END
            ELSE IF(op=A_SMUL)AND(right.location.loc=LOC_CONSTANT)AND(ispowerof2(right.location.value,power))AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              BEGIN
                emit_const_reg(A_SLL,opsize,power,left.location.register);
              END
            ELSE
              BEGIN
                IF extra_not
                THEN
                  BEGIN
//                  rg.getexplicitregisterint(exprasmlist,R_EDI);
//                  cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
//                  emit_reg(A_NOT,S_L,R_EDI);
//                  emit_reg_reg(A_AND,S_L,R_EDI,left.location.register);
//                  rg.ungetregisterint(exprasmlist,R_EDI);
                  END
                ELSE
                  BEGIN
                    emit_op_right_left(op,opsize);
                  END;
              END;
          END;
      END;
    { only in case of overflow operations }
    { produce overflow code }
    { we must put it here directly, because sign of operation }
    { is in unsigned VAR!!                                   }
    IF mboverflow
    THEN
      BEGIN
        IF cs_check_overflow IN aktlocalswitches
        THEN
          BEGIN
      //      getlabel(hl4);
            IF unsigned
            THEN
              emitjmp(C_NB,hl4)
            ELSE
              emitjmp(C_NO,hl4);
            cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
            cg.a_label(exprasmlist,hl4);
          END;
       END;
  END;
PROCEDURE TSparcAddNode.emit_op_right_left(op:TAsmOp;OpSize:TOpsize);
  BEGIN
    {left must be a register}
    CASE right.location.loc OF
      LOC_REGISTER,LOC_CREGISTER:
        exprasmlist.concat(taicpu.op_reg_reg(op,opsize,right.location.register,left.location.register));
      LOC_REFERENCE,LOC_CREFERENCE :
        exprasmlist.concat(taicpu.op_ref_reg(op,opsize,right.location.reference,left.location.register));
      LOC_CONSTANT:
        exprasmlist.concat(taicpu.op_const_reg(op,opsize,right.location.value,left.location.register));
    ELSE
      InternalError(200203232);
    END;
  END;
PROCEDURE TSparcAddNode.set_result_location(cmpOp,unsigned:Boolean);
  BEGIN
    IF cmpOp
    THEN
      BEGIN
        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=GetResFlags(unsigned);
      END
    ELSE
      location_copy(location,left.location);
  END;
PROCEDURE TSparcAddNode.pass_2;
{is also being used for "xor", and "mul", "sub", or and comparative operators}
  VAR
    popeax,popedx,pushedfpu,mboverflow,cmpop:Boolean;
    op:TAsmOp;
    power:LongInt;
    OpSize:TOpSize;
    unsigned:Boolean;{true, if unsigned types are compared}
         { is_in_dest if the result is put directly into }
         { the resulting refernce or varregister }
         {is_in_dest : boolean;}
         { true, if for sets subtractions the extra not should generated }
    extra_not:Boolean;
  BEGIN
{to make it more readable, string and set (not smallset!) have their own
procedures }
    CASE left.resulttype.def.deftype OF
      orddef:
        BEGIN
          IF is_boolean(left.resulttype.def)AND is_boolean(right.resulttype.def)
          THEN{handling boolean expressions}
            BEGIN
              InternalError(20020726);//second_addboolean;
              exit;
            END
          ELSE IF is_64bitint(left.resulttype.def)
          THEN{64bit operations}
            BEGIN
              InternalError(20020726);//second_add64bit;
              exit;
            END;
        END;
      stringdef:
        BEGIN
          InternalError(20020726);//second_addstring;
          exit;
        END;
      setdef:
        BEGIN
          {normalsets are already handled in pass1}
          IF(tsetdef(left.resulttype.def).settype<>smallset)
          THEN
            internalerror(200109041);
          InternalError(20020726);//second_addsmallset;
          exit;
        END;
      arraydef :
        BEGIN
{$ifdef SUPPORT_MMX}
               if is_mmx_able_array(left.resulttype.def) then
                begin
                  InternalError(20020726);//second_addmmx;
                  exit;
                end;
{$endif SUPPORT_MMX}
        END;
      floatdef :
        BEGIN
          InternalError(20020726);//second_addfloat;
          exit;
        END;
    END;
{defaults}
    {is_in_dest:=false;}
    extra_not:=false;
    mboverflow:=false;
    cmpop:=false;
    unsigned:=not(is_signed(left.resulttype.def))or not(is_signed(right.resulttype.def));
    opsize:=def_opsize(left.resulttype.def);
    //pass_left_and_right(pushedfpu);
    IF(left.resulttype.def.deftype=pointerdef)OR
      (right.resulttype.def.deftype=pointerdef) or
      (is_class_or_interface(right.resulttype.def) and is_class_or_interface(left.resulttype.def)) or
      (left.resulttype.def.deftype=classrefdef) or
      (left.resulttype.def.deftype=procvardef) or
      ((left.resulttype.def.deftype=enumdef)and(left.resulttype.def.size=4)) or
      ((left.resulttype.def.deftype=orddef)and(torddef(left.resulttype.def).typ in [s32bit,u32bit])) or
      ((right.resulttype.def.deftype=orddef)and(torddef(right.resulttype.def).typ in [s32bit,u32bit]))
    THEN
      BEGIN
        CASE NodeType OF
          addn:
            BEGIN
              op:=A_ADD;
              mboverflow:=true;
            END;
          muln:
            BEGIN
              IF unsigned
              THEN
                op:=A_UMUL
              ELSE
                op:=A_SMUL;
              mboverflow:=true;
            END;
          subn:
            BEGIN
              op:=A_SUB;
              mboverflow:=true;
            END;
          ltn,lten,
          gtn,gten,
          equaln,unequaln:
            BEGIN
              op:=A_CMP;
              cmpop:=true;
            END;
          xorn:
            op:=A_XOR;
          orn:
            op:=A_OR;
          andn:
            op:=A_AND;
        ELSE
          CGMessage(type_e_mismatch);
        END;
   { filter MUL, which requires special handling }
        IF op=A_UMUL
        THEN
          BEGIN
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
            IF right.nodetype=ordconstn
            THEN
              swapleftright;
            IF(left.nodetype=ordconstn)and
              ispowerof2(tordconstnode(left).value, power)and
              not(cs_check_overflow in aktlocalswitches)
            THEN
              BEGIN
                   { This release will be moved after the next }
                   { instruction by the optimizer. No need to  }
                   { release left.location, since it's a   }
                   { constant (JM)                             }
                location_release(exprasmlist,right.location);
                location.register:=rg.getregisterint(exprasmlist);
                cg.a_load_loc_reg(exprasmlist,right.location,location.register);
                cg.a_op_const_reg(exprasmlist,OP_SHL,power,location.register);
              END
            ELSE
              BEGIN
{$EndIf NoShlMul}
{In SPARC there is no push/pop mechanism. There is a windowing mechanism using
 SAVE and RESTORE instructions.}
                //regstopush:=all_registers;
                //remove_non_regvars_from_loc(right.location,regstopush);
                //remove_non_regvars_from_loc(left.location,regstopush);
                {left.location can be R_EAX !!!}
//              rg.GetExplicitRegisterInt(exprasmlist,R_EDI);
                {load the left value}
//                cg.a_load_loc_reg(exprasmlist,left.location,R_EDI);
//                location_release(exprasmlist,left.location);
                  { allocate EAX }
//                if R_EAX in rg.unusedregsint then
//                  exprasmList.concat(tai_regalloc.Alloc(R_EAX));
                  { load he right value }
//                cg.a_load_loc_reg(exprasmlist,right.location,R_EAX);
//                location_release(exprasmlist,right.location);
                  { allocate EAX if it isn't yet allocated (JM) }
//                if (R_EAX in rg.unusedregsint) then
//                  exprasmList.concat(tai_regalloc.Alloc(R_EAX));
                  { also allocate EDX, since it is also modified by }
                  { a mul (JM)                                      }
{                if R_EDX in rg.unusedregsint then
                    exprasmList.concat(tai_regalloc.Alloc(R_EDX));
                  emit_reg(A_MUL,S_L,R_EDI);
                  rg.ungetregisterint(exprasmlist,R_EDI);
                  if R_EDX in rg.unusedregsint then
                    exprasmList.concat(tai_regalloc.DeAlloc(R_EDX));
                  if R_EAX in rg.unusedregsint then
                    exprasmList.concat(tai_regalloc.DeAlloc(R_EAX));
                  location.register:=rg.getregisterint(exprasmlist);
                  emit_reg_reg(A_MOV,S_L,R_EAX,location.register);
                  if popedx then
                   emit_reg(A_POP,S_L,R_EDX);
                  if popeax then
                   emit_reg(A_POP,S_L,R_EAX);}
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

            left_must_be_reg(OpSize,false);
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
{         else
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
           CGMessage(type_e_mismatch);}
      end;
BEGIN
  cAddNode:=TSparcAddNode;
END.
