{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for math nodes

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
unit ncgmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat;

    type
      tm68kmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tm68kshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      tm68knotnode = class(tnotnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defbase,
      cginfo,cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      tgobj,ncgutil,cgobj,rgobj,rgcpu;

{*****************************************************************************
                             TI386MODDIVNODE
*****************************************************************************}

    procedure tm68kmoddivnode.pass_2;
      var
         hreg1 : tregister;
         hreg2 : tregister;
         hdenom : tregister;
         shrdiv,popeax,popedx : boolean;
         power : longint;
         hl : tasmlabel;
         pushedregs : tmaybesave;
      begin
         shrdiv := false;
         secondpass(left);
         if codegenerror then
          exit;
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,pushedregs);
         if codegenerror then
          exit;
         location_copy(location,left.location);

         if is_64bitint(resulttype.def) then
           begin
             { should be handled in pass_1 (JM) }
             internalerror(200109052);
           end
         else
           begin
              { put numerator in register }
              location_force_reg(exprasmlist,left.location,OS_INT,false);
              hreg1:=left.location.register;

              if (nodetype=divn) and
                 (right.nodetype=ordconstn) and
                 ispowerof2(tordconstnode(right).value,power) then
                Begin
                  shrdiv := true;
                  { for signed numbers, the numerator must be adjusted before the
                    shift instruction, but not wih unsigned numbers! Otherwise,
                    "Cardinal($ffffffff) div 16" overflows! (JM) }
                  If is_signed(left.resulttype.def) Then
                    Begin
                      objectlibrary.getlabel(hl);
                      cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_GT,0,hreg,hl);
                      if power=1 then
                          cg.a_op_const_reg(exprasmlist,OP_ADD,OS_32,1,hreg1)
                      else
                          cg.a_op_const_reg(exprasmlist,OP_ADD,OS_32,
                             tordconstnode(right).value-1,hreg1);
                      cg.a_label(exprasmlist,hl);    
                      cg.a_op_const_reg(exprasmlist,OP_SAR,OS_INT,power,hreg1);
                      End
                    Else { not signed }
                     Begin
                      cg.a_op_const_reg(exprasmlist,OP_SHR,OS_INT,power,hreg1);
                     end;
                End
              else
                begin
                  { bring denominator to D1 }
                  { D1 is always free, it's }
                  { only used for temporary }
                  { purposes                }
                  hdenom := rg.getregisterint(exprasmlist);
                  if right.location.loc<>LOC_CREGISTER then
                   location_release(exprasmlist,right.location);
                  cg.a_load_loc_reg(exprasmlist,right.location,hdenom);
                  
                  { verify if the divisor is zero, if so return an error
                    immediately
                  }
                  objectlibrary.getlabel(hl1);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_NE,0,hdenom,hl1);
                  cg.a_param_reg(exprasmlist,OS_S32,paramanager.getintparaloc(1));
                  cg.a_call_name('FPC_HANDLERROR');
                  cg.a_label(exprasmlist,hl1);
{ This should be moved to emit_moddiv_reg_reg }                  
                  if is_signed(left.resulttype.def) then
                     cg.a_op_reg_reg(exprasmlist,OS_INT,OP_IDIV,hdenom,hreg1)
                  else
                     cg.a_op_reg_reg(exprasmlist,OS_INT,OP_DIV,hdenom,hreg1);
                  if nodetype = modn then
                    begin
{$warning modnode should be tested}                    
                       { multiply by denominator to get modulo }
                       cg.a_op_reg_reg(exprasmlist,OS_INT,OP_IMUL,hdenom,hreg1)
                    end;
                end;
              location_reset(location,LOC_REGISTER,OS_INT);
              location.register:=hreg1;
           end;
      end;


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}

    procedure tm68kshlshrnode.pass_2;
      var
         hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         popecx : boolean;
         op : tasmop;
         l1,l2,l3 : tasmlabel;
         pushedregs : tmaybesave;
      begin
         popecx:=false;

         secondpass(left);
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,pushedregs);

         { determine operator }
         case nodetype of
           shln: op:=A_SHL;
           shrn: op:=A_SHR;
         end;
(*
         if is_64bitint(left.resulttype.def) then
           begin
              location_reset(location,LOC_REGISTER,OS_64);

              { load left operator in a register }
              location_force_reg(exprasmlist,left.location,OS_64,false);
              hregisterhigh:=left.location.registerhigh;
              hregisterlow:=left.location.registerlow;

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                begin
                   { shrd/shl works only for values <=31 !! }
                   if tordconstnode(right).value>31 then
                     begin
                        if nodetype=shln then
                          begin
                             emit_reg_reg(A_XOR,S_L,hregisterhigh,
                               hregisterhigh);
                             if ((tordconstnode(right).value and 31) <> 0) then
                               emit_const_reg(A_SHL,S_L,tordconstnode(right).value and 31,
                                 hregisterlow);
                          end
                        else
                          begin
                             emit_reg_reg(A_XOR,S_L,hregisterlow,
                               hregisterlow);
                             if ((tordconstnode(right).value and 31) <> 0) then
                               emit_const_reg(A_SHR,S_L,tordconstnode(right).value and 31,
                                 hregisterhigh);
                          end;
                        location.registerhigh:=hregisterlow;
                        location.registerlow:=hregisterhigh;
                     end
                   else
                     begin
                        if nodetype=shln then
                          begin
                             emit_const_reg_reg(A_SHLD,S_L,tordconstnode(right).value and 31,
                               hregisterlow,hregisterhigh);
                             emit_const_reg(A_SHL,S_L,tordconstnode(right).value and 31,
                               hregisterlow);
                          end
                        else
                          begin
                             emit_const_reg_reg(A_SHRD,S_L,tordconstnode(right).value and 31,
                               hregisterhigh,hregisterlow);
                             emit_const_reg(A_SHR,S_L,tordconstnode(right).value and 31,
                               hregisterhigh);
                          end;
                        location.registerlow:=hregisterlow;
                        location.registerhigh:=hregisterhigh;
                     end;
                end
              else
                begin
                   { load right operators in a register }
                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                       hregister2:=rg.getexplicitregisterint(exprasmlist,R_ECX);
                       cg.a_load_loc_reg(exprasmlist,right.location,hregister2);
                     end
                   else
                     hregister2:=right.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (hregisterlow=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterlow,hregister2);
                        hregister3:=hregisterlow;
                        hregisterlow:=hregister2;
                        hregister2:=hregister3;
                     end
                   else if (hregisterhigh=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterhigh,hregister2);
                        hregister3:=hregisterhigh;
                        hregisterhigh:=hregister2;
                        hregister2:=hregister3;
                     end

                   { if second operator not in ECX ? }
                   else if (hregister2<>R_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (R_ECX in rg.unusedregsint) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,R_ECX);
                         end
                        else
                          rg.getexplicitregisterint(exprasmlist,R_ECX);
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;

                   if hregister2 <> R_ECX then
                     rg.ungetregisterint(exprasmlist,hregister2);

                   { the damned shift instructions work only til a count of 32 }
                   { so we've to do some tricks here                           }
                   if nodetype=shln then
                     begin
                        objectlibrary.getlabel(l1);
                        objectlibrary.getlabel(l2);
                        objectlibrary.getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l2);
                        emit_reg_reg_reg(A_SHLD,S_L,R_CL,
                          hregisterlow,hregisterhigh);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        cg.a_label(exprasmlist,l3);
                     end
                   else
                     begin
                        objectlibrary.getlabel(l1);
                        objectlibrary.getlabel(l2);
                        objectlibrary.getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l2);
                        emit_reg_reg_reg(A_SHRD,S_L,R_CL,
                          hregisterhigh,hregisterlow);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        cg.a_label(exprasmlist,l3);

                     end;

                   { maybe put ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,R_ECX)
                   else
                     rg.ungetregisterint(exprasmlist,R_ECX);

                   location.registerlow:=hregisterlow;
                   location.registerhigh:=hregisterhigh;
                end;
           end
         else
           begin
              { load left operators in a register }
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_INT,false);

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                begin
                   { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)
                   if right.value<=31 then
                   }
                   emit_const_reg(op,S_L,tordconstnode(right).value and 31,
                     location.register);
                   {
                   else
                     emit_reg_reg(A_XOR,S_L,hregister1,
                       hregister1);
                   }
                end
              else
                begin
                   { load right operators in a register }
                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                       hregister2:=rg.getexplicitregisterint(exprasmlist,R_ECX);
                       cg.a_load_loc_reg(exprasmlist,right.location,hregister2);
                     end
                   else
                     hregister2:=right.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (location.register=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,location.register,hregister2);
                        hregister3:=location.register;
                        location.register:=hregister2;
                        hregister2:=hregister3;
                     end
                   { if second operator not in ECX ? }
                   else if (hregister2<>R_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (R_ECX in rg.unusedregsint) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,R_ECX);
                         end
                        else
                          rg.getexplicitregisterint(exprasmlist,R_ECX);
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;
                   rg.ungetregisterint(exprasmlist,hregister2);
                   { right operand is in ECX }
                   emit_reg_reg(op,S_L,R_CL,location.register);
                   { maybe ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,R_ECX)
                   else
                     rg.ungetregisterint(exprasmlist,R_ECX);
                end;
           end;
*)           
      end;



{*****************************************************************************
                               TI386NOTNODE
*****************************************************************************}

    procedure tm68knotnode.pass_2;
      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_BE,F_B,F_AE,F_A);
      var
         hl : tasmlabel;
         opsize : topsize;
      begin
         if is_boolean(resulttype.def) then
          begin
            opsize:=def_opsize(resulttype.def);
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.location.loc<>LOC_JUMP then
             secondpass(left);

            case left.location.loc of
              LOC_JUMP :
                begin
                  location_reset(location,LOC_JUMP,OS_NO);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(left);
                  maketojumpbool(exprasmlist,left,lr_load_regvars);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=flagsinvers[left.location.resflags];
                end;
              LOC_CONSTANT,
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  location_force_reg(exprasmlist,left.location,def_cgsize(resulttype.def),true);
                  list.concat(taicpu.op_reg(A_TST,opsize,left.location.register));
                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
             else
                internalerror(200203224);
            end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_64,false);
              cg.a_op64_op_loc_reg(exprasmlist,A_NOT,OS_64,
                 location,joinreg64(l.registerlow,l.registerhigh));
           end
         else
          begin
            secondpass(left);
            location_copy(location,left.location);
            location_force_reg(exprasmlist,location,def_cgsize(resulttype.def),false);

            opsize:=def_cgsize(resulttype.def);
            cg.a_op_reg_reg(exprasmlist,OP_NOT,location.register,location.register);
          end;
      end;

begin
   cmoddivnode:=tm68kmoddivnode;
   cshlshrnode:=tm68kshlshrnode;
   cnotnode:=tm68knotnode;
end.
{
  $Log$
  Revision 1.1  2002-08-14 19:16:34  carl
    + m68k type conversion nodes
    + started some mathematical nodes
    * out of bound references should now be handled correctly

}
