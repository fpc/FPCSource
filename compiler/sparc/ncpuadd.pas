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
unit ncpuadd;
{$INCLUDE fpcdefs.inc}
interface
uses
  node,nadd,cpubase,cginfo;
type
  TSparcAddNode=class(TAddNode)
    procedure pass_2;override;
  private
    procedure second_addboolean;
    function GetResFlags(unsigned:Boolean):TResFlags;
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
  symconst,symdef,SymType,paramgr,
  aasmbase,aasmtai,aasmcpu,defutil,htypechk,
  cgbase,pass_2,regvars,
  cpupara,
  ncon,nset,
  ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;
const
  opsize_2_cgSize:array[S_B..S_L]of TCgSize=(OS_8,OS_16,OS_32);
procedure TSparcAddNode.second_addboolean;
  var
    cgop:TOpCg;
    cgsize:TCgSize;
    cmpop,isjump:boolean;
    otl,ofl:tasmlabel;
    pushedregs:TMaybeSave;
  begin
    { calculate the operator which is more difficult }
    firstcomplex(self);
    cmpop:=false;
    if (torddef(left.resulttype.def).typ=bool8bit) or
       (torddef(right.resulttype.def).typ=bool8bit)
    then
      cgsize:=OS_8
    else if (torddef(left.resulttype.def).typ=bool16bit) or
            (torddef(right.resulttype.def).typ=bool16bit)
    then
      cgsize:=OS_16
    else
      cgsize:=OS_32;
    if (cs_full_boolean_eval in aktlocalswitches) or
       (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn])
    then
      begin
        if left.nodetype in [ordconstn,realconstn]
        then
          swapleftright;
        isjump:=(left.location.loc=LOC_JUMP);
        if isjump
        then
          begin
            otl:=truelabel;
            objectlibrary.getlabel(truelabel);
            ofl:=falselabel;
            objectlibrary.getlabel(falselabel);
          end;
        secondpass(left);
        if left.location.loc in [LOC_FLAGS,LOC_JUMP]
        then
          location_force_reg(exprasmlist,left.location,cgsize,false);
        if isjump
        then
          begin
            truelabel:=otl;
            falselabel:=ofl;
          end;
        maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
        isjump:=(right.location.loc=LOC_JUMP);
        if isjump
        then
          begin
            otl:=truelabel;
            objectlibrary.getlabel(truelabel);
            ofl:=falselabel;
            objectlibrary.getlabel(falselabel);
          end;
        secondpass(right);
        maybe_restore(exprasmlist,left.location,pushedregs);
        if right.location.loc in [LOC_FLAGS,LOC_JUMP]
        then
          location_force_reg(exprasmlist,right.location,cgsize,false);
        if isjump
        then
          begin
            truelabel:=otl;
            falselabel:=ofl;
          end;
        cmpop := nodetype in [ltn,lten,gtn,gten,equaln,unequaln];
        { set result location }
        if not cmpop
        then
          location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
        else
          location_reset(location,LOC_FLAGS,OS_NO);
        //load_left_right(cmpop,false);
        if (left.location.loc = LOC_CONSTANT)
        then
          swapleftright;
        { compare the }
        case nodetype of
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
              if (right.location.loc <> LOC_CONSTANT)
              then
                exprasmlist.concat(taicpu.op_reg_reg(A_JMPL,left.location.register,right.location.register))
              else
                exprasmlist.concat(taicpu.op_reg_const(A_JMPL,left.location.register,longint(right.location.value)));
              location.resflags := GetResFlags(true);
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
              if right.location.loc <> LOC_CONSTANT
              then
                cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,left.location.register,right.location.register,location.register)
              else
                cg.a_op_const_reg_reg(exprasmlist,cgop,OS_INT,right.location.value,left.location.register,location.register);
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
      end;
//    clear_left_right(CmpOp);
  end;
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
              exprasmList.concat(Taicpu.Op_reg(A_NOT,left.location.register));
            exprasmList.concat(Taicpu.Op_reg_reg_reg(Op,right.location.register,left.location.register,right.location.register));
            { newly swapped also set swapped flag }
            location_swap(left.location,right.location);
            toggleflag(nf_swaped);
          end
        else
          begin
            if extra_not
            then
              exprasmList.concat(Taicpu.Op_reg(A_NOT,right.location.register));
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
              exprasmList.concat(Taicpu.Op_reg(A_NOT,left.location.register));
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
                exprasmList.concat(Taicpu.Op_reg(A_DEC,left.location.register));
              end
            ELSE IF(op=A_SMUL)AND(right.location.loc=LOC_CONSTANT)AND(ispowerof2(right.location.value,power))AND NOT(cs_check_overflow in aktlocalswitches)
            THEN
              begin
                exprasmList.concat(Taicpu.Op_const_reg(A_SLL,power,left.location.register));
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
              exprasmList.concat(Taicpu.Op_sym(A_JMPL,S_NO,hl4))
            ELSE
              exprasmList.concat(Taicpu.Op_sym(A_JMPL,S_NO,hl4));
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
function def_opsize(p1:tdef):topsize;
  begin
    case p1.size of
      1:def_opsize:=S_B;
      2:def_opsize:=S_W;
      4:def_opsize:=S_L;
      8:def_opsize:=S_L;
      else
        InternalError(130820001);
    end;
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
          second_addboolean
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
    cmpop:=nodetype in [ltn,lten,gtn,gten,equaln,unequaln];
    unsigned:=not(is_signed(left.resulttype.def))or
              not(is_signed(right.resulttype.def));
    opsize:=def_opsize(left.resulttype.def);
    pass_left_and_right;
    { set result location }
    if not cmpop
    then
      location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
    else
      location_reset(location,LOC_FLAGS,OS_NO);
    //load_left_right(cmpop, (cs_check_overflow in aktlocalswitches) and
    //(nodetype in [addn,subn,muln]));
    if(location.register.enum = R_NO)and not(cmpop)
    then
      location.register := rg.getregisterint(exprasmlist);
    if not(cs_check_overflow in aktlocalswitches)or cmpop or (nodetype in [orn,andn,xorn])
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
      end;
      //clear_left_right(cmpop);
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
    Revision 1.6  2003-01-08 18:43:58  daniel
     * Tregister changed into a record

    Revision 1.5  2003/01/07 22:03:40  mazen
    * adding unequaln node support to sparc compiler

    Revision 1.4  2002/12/30 21:17:22  mazen
    - unit cga no more used in sparc compiler.

    Revision 1.3  2002/12/25 20:59:49  mazen
    - many emitXXX removed from cga.pas in order to remove that file.

    Revision 1.2  2002/12/22 19:26:32  mazen
    * many internal errors related to unimplemented nodes are fixed

    Revision 1.1  2002/12/21 23:21:47  mazen
    + added support for the shift nodes
    + added debug output on screen with -an command line option

    Revision 1.10  2002/11/25 17:43:28  peter
      * splitted defbase in defutil,symutil,defcmp
      * merged isconvertable and is_equal into compare_defs(_ext)
      * made operator search faster by walking the list only once

    Revision 1.9  2002/11/10 19:07:46  mazen
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
