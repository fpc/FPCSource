{******************************************************************************
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
    procedure pass_2;override;
  PRIVATE
    FUNCTION GetResFlags(unsigned:Boolean):TResFlags;
    procedure left_must_be_reg(OpSize:TOpSize;NoSwap:Boolean);
    procedure emit_generic_code(op:TAsmOp;OpSize:TOpSize;unsigned,extra_not,mboverflow:Boolean);
    procedure emit_op_right_left(op:TAsmOp);
    procedure pass_left_and_right;
    procedure set_result_location(cmpOp,unsigned:Boolean);
  end;
implementation
uses
  globtype,systems,
  cutils,verbose,globals,
  symconst,symdef,paramgr,
  aasmbase,aasmtai,aasmcpu,defbase,htypechk,
  cgbase,pass_2,regvars,
  cpupara,
  ncon,nset,
  cga,ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;
const
  opsize_2_cgSize:array[S_B..S_L]of TCgSize=(OS_8,OS_16,OS_32);
function TSparcAddNode.GetResFlags(unsigned:Boolean):TResFlags;
  begin
    case NodeType of
      equaln:
        GetResFlags:=F_E;
      unequaln:
        GetResFlags:=F_NE;
      else
        if not(unsigned)
        then
          if nf_swaped IN flags
          then
            case NodeType of
              ltn:
                GetResFlags:=F_G;
              lten:
                GetResFlags:=F_GE;
              gtn:
                GetResFlags:=F_L;
              gten:
                GetResFlags:=F_LE;
            end
          else
            case NodeType of
              ltn:
                GetResFlags:=F_L;
              lten:
                GetResFlags:=F_LE;
              gtn:
                GetResFlags:=F_G;
              gten:
                GetResFlags:=F_GE;
            end
        else
          if nf_swaped IN Flags
          then
            case NodeType of
              ltn:
                GetResFlags:=F_A;
              lten:
                GetResFlags:=F_AE;
              gtn:
                GetResFlags:=F_B;
              gten:
                GetResFlags:=F_BE;
            end
          else
            case NodeType of
              ltn:
                GetResFlags:=F_B;
              lten:
                GetResFlags:=F_BE;
              gtn:
                GetResFlags:=F_A;
              gten:
                GetResFlags:=F_AE;
            end;
    end;
  end;
procedure TSparcAddNode.left_must_be_reg(OpSize:TOpSize;NoSwap:Boolean);
  begin
    if(left.location.loc=LOC_REGISTER)
    then
      exit;
  {left location is not a register}
    if(not NoSwap)and(right.location.loc=LOC_REGISTER)
    then{right is register so we can swap the locations}
      begin
        location_swap(left.location,right.location);
        toggleflag(nf_swaped);
      end
    else
      begin
{maybe we can reuse a constant register when the operation is a comparison that
doesn't change the value of the register}
        location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],(nodetype in [ltn,lten,gtn,gten,equaln,unequaln]));
      end;
  end;
procedure TSparcAddNode.emit_generic_code(op:TAsmOp;OpSize:TOpSize;unsigned,extra_not,mboverflow:Boolean);
  VAR
    power:LongInt;
    hl4:TAsmLabel;
  begin
    { at this point, left.location.loc should be LOC_REGISTER }
    if right.location.loc=LOC_REGISTER
    then
      begin
        { right.location is a LOC_REGISTER }
        { when swapped another result register }
        if(nodetype=subn)and(nf_swaped in flags)
        then
          begin
            if extra_not
            then
              emit_reg(A_NOT,S_L,left.location.register);
            exprasmList.concat(Taicpu.Op_reg_reg_reg(Op,right.location.register,left.location.register,right.location.register));
            { newly swapped also set swapped flag }
            location_swap(left.location,right.location);
            toggleflag(nf_swaped);
          end
        else
          begin
            if extra_not
            then
              emit_reg(A_NOT,S_L,right.location.register);
           // emit_reg_reg(op,opsize,right.location.register,left.location.register);
            exprasmList.concat(Taicpu.Op_reg_reg_reg(Op,right.location.register,left.location.register,right.location.register));
          end;
      end
    ELSE
      begin
        { right.location is not a LOC_REGISTER }
        IF(nodetype=subn)AND(nf_swaped IN flags)
        THEN
          begin
            IF extra_not
            THEN
              emit_reg(A_NOT,opsize,left.location.register);
//          rg.getexplicitregisterint(exprasmlist,R_EDI);
//          cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
//          emit_reg_reg(op,opsize,left.location.register,R_EDI);
//          emit_reg_reg(A_MOV,opsize,R_EDI,left.location.register);
//          rg.ungetregisterint(exprasmlist,R_EDI);
          end
        ELSE
          begin
            { Optimizations when right.location is a constant value }
            IF(op=A_CMP)AND(nodetype IN [equaln,unequaln])AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=0)
            THEN
              begin
//                emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
              end
            ELSE IF(op=A_ADD)AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=1)AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              with ExprAsmList,left.location do
                begin
                  concat(TAiCpu.op_reg_const_reg(A_ADD,register,1,register));
                end
            ELSE IF(op=A_SUB)AND(right.location.loc=LOC_CONSTANT)AND(right.location.value=1)AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              begin
                emit_reg(A_DEC,opsize,left.location.register);
              end
            ELSE IF(op=A_SMUL)AND(right.location.loc=LOC_CONSTANT)AND(ispowerof2(right.location.value,power))AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              begin
                emit_const_reg(A_SLL,opsize,power,left.location.register);
              end
            ELSE
              begin
                IF extra_not
                THEN
                  begin
//                  rg.getexplicitregisterint(exprasmlist,R_EDI);
//                  cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
//                  emit_reg(A_NOT,S_L,R_EDI);
//                  emit_reg_reg(A_AND,S_L,R_EDI,left.location.register);
//                  rg.ungetregisterint(exprasmlist,R_EDI);
                  end
                ELSE
                  begin
                    emit_op_right_left(op);
                  end;
              end;
          end;
      end;
    { only in case of overflow operations }
    { produce overflow code }
    { we must put it here directly, because sign of operation }
    { is in unsigned VAR!!                                   }
    IF mboverflow
    THEN
      begin
        IF cs_check_overflow IN aktlocalswitches
        THEN
          begin
      //      getlabel(hl4);
            IF unsigned
            THEN
              emitjmp(C_NB,hl4)
            ELSE
              emitjmp(C_NO,hl4);
            cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
            cg.a_label(exprasmlist,hl4);
          end;
      end;
  end;
procedure TSparcAddNode.emit_op_right_left(op:TAsmOp);
  begin
    {left must be a register}
    with left,location,exprasmlist do
      case Right.Location.Loc of
        LOC_REGISTER,LOC_CREGISTER:
          concat(taicpu.op_reg_reg_reg(op,Register,Right.Location.register,register));
        LOC_REFERENCE,LOC_CREFERENCE :
          begin
            location_force_reg(exprasmlist,Right.Location,OS_32,(nodetype in [ltn,lten,gtn,gten,equaln,unequaln]));
            concat(taicpu.op_reg_reg_reg(op,register,Right.Location.register,register));
          end;
        LOC_CONSTANT:
          concat(taicpu.op_reg_const_reg(op,register,Right.Location.value,register));
        else
          InternalError(200203232);
      end;
  end;
procedure TSparcAddNode.set_result_location(cmpOp,unsigned:Boolean);
  begin
    IF cmpOp
    THEN
      begin
        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=GetResFlags(unsigned);
      end
    ELSE
      location_copy(location,left.location);
  end;
procedure TSparcAddNode.pass_2;
{is also being used for "xor", and "mul", "sub", or and comparative operators}
  var
    popeax,popedx,pushedfpu,mboverflow,cmpop:Boolean;
    op:TAsmOp;
    power:LongInt;
    OpSize:TOpSize;
    unsigned:Boolean;{true, if unsigned types are compared}
    extra_not:Boolean;
    cgop:TOpCg;
  begin
{to make it more readable, string and set (not smallset!) have their own
procedures }
    case left.resulttype.def.deftype of
      orddef:
        if is_boolean(left.resulttype.def)and is_boolean(right.resulttype.def)
        then{handling boolean expressions}
          InternalError(20020726)//second_addboolean;
        else if is_64bitint(left.resulttype.def)
        then{64bit operations}
            InternalError(20020726);//second_add64bit;
      stringdef:
        InternalError(20020726);//second_addstring;
      setdef:
        {normalsets are already handled in pass1}
        if(tsetdef(left.resulttype.def).settype<>smallset)
        then
          internalerror(200109041)
        else
          InternalError(20020726);//second_addsmallset;
      arraydef :
        InternalError(2002110600);
      floatdef :
        InternalError(20020726);//second_addfloat;
    end;
{defaults}
    extra_not:=false;
    mboverflow:=false;
    cmpop:=false;
    unsigned:=not(is_signed(left.resulttype.def))or 
              not(is_signed(right.resulttype.def));
    opsize:=def_opsize(left.resulttype.def);
    pass_left_and_right;
    if(left.resulttype.def.deftype=pointerdef)or
      (right.resulttype.def.deftype=pointerdef)or
      (is_class_or_interface(right.resulttype.def)and is_class_or_interface(left.resulttype.def)) or
      (left.resulttype.def.deftype=classrefdef) or
      (left.resulttype.def.deftype=procvardef) or
      ((left.resulttype.def.deftype=enumdef)and(left.resulttype.def.size=4))or
      ((left.resulttype.def.deftype=orddef)and(torddef(left.resulttype.def).typ in [s32bit,u32bit]))or
      ((right.resulttype.def.deftype=orddef)and(torddef(right.resulttype.def).typ in [s32bit,u32bit]))
    then
      begin
        case NodeType of
          addn:
            begin
              op:=A_ADD;
              mboverflow:=true;
            end;
          muln:
            begin
              IF unsigned
              THEN
                op:=A_UMUL
              ELSE
                op:=A_SMUL;
              mboverflow:=true;
            end;
          subn:
            begin
              op:=A_SUB;
              mboverflow:=true;
            end;
          ltn,lten,
          gtn,gten,
          equaln,unequaln:
            begin
              op:=A_CMP;
              cmpop:=true;
            end;
          xorn:
            op:=A_XOR;
          orn:
            op:=A_OR;
          andn:
            op:=A_AND;
        else
          CGMessage(type_e_mismatch);
        end;
   { Convert flags to register first }
        if(left.location.loc=LOC_FLAGS)
        then
          location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
        if (right.location.loc=LOC_FLAGS)
        then
          location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],false);
        left_must_be_reg(OpSize,false);
        emit_generic_code(op,opsize,unsigned,extra_not,mboverflow);
        location_freetemp(exprasmlist,right.location);
        location_release(exprasmlist,right.location);
        if cmpop and(left.location.loc<>LOC_CREGISTER)
        then
          begin
            location_freetemp(exprasmlist,left.location);
            location_release(exprasmlist,left.location);
          end;
        set_result_location(cmpop,unsigned);
      end;
   end;
procedure TSparcAddNode.pass_left_and_right;
  var
    pushedregs:tmaybesave;
    tmpreg:tregister;
    pushedfpu:boolean;
  begin
    { calculate the operator which is more difficult }
    firstcomplex(self);
    { in case of constant put it to the left }
    if (left.nodetype=ordconstn)
    then
      swapleftright;
    secondpass(left);
    { are too few registers free? }
    maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
    if location.loc=LOC_FPUREGISTER
    then
      pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
    else
      pushedfpu:=false;
    secondpass(right);
    maybe_restore(exprasmlist,left.location,pushedregs);
    if pushedfpu
    then
      begin
        tmpreg := rg.getregisterfpu(exprasmlist);
        cg.a_loadfpu_loc_reg(exprasmlist,left.location,tmpreg);
        location_reset(left.location,LOC_FPUREGISTER,left.location.size);
        left.location.register := tmpreg;
      end;
  end;
begin
  cAddNode:=TSparcAddNode;
end.
{
    $Log$
    Revision 1.9  2002-11-10 19:07:46  mazen
    * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

    Revision 1.8  2002/11/06 15:34:00  mazen
    *** empty log message ***

    Revision 1.7  2002/11/06 11:31:24  mazen
    * op_reg_reg_reg don't need any more a TOpSize parameter

    Revision 1.6  2002/11/05 16:15:00  mazen
    *** empty log message ***

    Revision 1.5  2002/10/22 13:43:01  mazen
    - cga.pas redueced to an empty unit

    Revision 1.4  2002/10/10 20:23:57  mazen
    * tabs replaces by spaces

}
