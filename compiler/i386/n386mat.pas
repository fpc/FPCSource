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
unit n386mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,nx86mat;

    type
      ti386moddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      ti386shlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      ti386unaryminusnode = class(tx86unaryminusnode)
      end;

      ti386notnode = class(tx86notnode)
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,defutil,
      cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,ncgutil,cgobj;

{*****************************************************************************
                             TI386MODDIVNODE
*****************************************************************************}

    procedure ti386moddivnode.pass_2;

    var  hreg1,hreg2:Tregister;
         power:longint;
         hl:Tasmlabel;
         op:Tasmop;

    begin
      secondpass(left);
      if codegenerror then
        exit;
      secondpass(right);
      if codegenerror then
        exit;

      if is_64bitint(resulttype.def) then
        { should be handled in pass_1 (JM) }
        internalerror(200109052);
      { put numerator in register }
      location_reset(location,LOC_REGISTER,OS_INT);
      location_force_reg(exprasmlist,left.location,OS_INT,false);
      hreg1:=left.location.register;

      if (nodetype=divn) and (right.nodetype=ordconstn) and
         ispowerof2(tordconstnode(right).value,power) then
        begin
          { for signed numbers, the numerator must be adjusted before the
            shift instruction, but not wih unsigned numbers! Otherwise,
            "Cardinal($ffffffff) div 16" overflows! (JM) }
          if is_signed(left.resulttype.def) Then
            begin
              if (aktOptProcessor <> class386) and
                 not(cs_littlesize in aktglobalswitches) then
                { use a sequence without jumps, saw this in
                  comp.compilers (JM) }
                begin
                  { no jumps, but more operations }
                  hreg2:=cg.getintregister(exprasmlist,OS_INT);
                  emit_reg_reg(A_MOV,S_L,hreg1,hreg2);
                  {If the left value is signed, hreg2=$ffffffff, otherwise 0.}
                  emit_const_reg(A_SAR,S_L,31,hreg2);
                  {If signed, hreg2=right value-1, otherwise 0.}
                  emit_const_reg(A_AND,S_L,tordconstnode(right).value-1,hreg2);
                  { add to the left value }
                  emit_reg_reg(A_ADD,S_L,hreg2,hreg1);
                  { release EDX if we used it }
                  cg.ungetregister(exprasmlist,hreg2);
                  { do the shift }
                  emit_const_reg(A_SAR,S_L,power,hreg1);
                end
              else
                begin
                  { a jump, but less operations }
                  emit_reg_reg(A_TEST,S_L,hreg1,hreg1);
                  objectlibrary.getlabel(hl);
                  cg.a_jmp_flags(exprasmlist,F_NS,hl);
                  if power=1 then
                    emit_reg(A_INC,S_L,hreg1)
                  else
                    emit_const_reg(A_ADD,S_L,tordconstnode(right).value-1,hreg1);
                  cg.a_label(exprasmlist,hl);
                  emit_const_reg(A_SAR,S_L,power,hreg1);
                end
            end
          else
            emit_const_reg(A_SHR,S_L,power,hreg1);
          location.register:=hreg1;
        end
      else
        begin
          {Bring denominator to a register.}
          cg.ungetregister(exprasmlist,hreg1);
          cg.getexplicitregister(exprasmlist,NR_EAX);
          emit_reg_reg(A_MOV,S_L,hreg1,NR_EAX);
          cg.getexplicitregister(exprasmlist,NR_EDX);
          {Sign extension depends on the left type.}
          if torddef(left.resulttype.def).typ=u32bit then
            emit_reg_reg(A_XOR,S_L,NR_EDX,NR_EDX)
          else
            emit_none(A_CDQ,S_NO);

          {Division depends on the right type.}
          if Torddef(right.resulttype.def).typ=u32bit then
            op:=A_DIV
          else
            op:=A_IDIV;

          if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
            emit_ref(op,S_L,right.location.reference)
          else if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
            emit_reg(op,S_L,right.location.register)
          else
            begin
              hreg1:=cg.getintregister(exprasmlist,right.location.size);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,hreg1);
              cg.ungetregister(exprasmlist,hreg1);
              emit_reg(op,S_L,hreg1);
            end;
          location_release(exprasmlist,right.location);

          {Copy the result into a new register. Release EAX & EDX.}
          if nodetype=divn then
            begin
              cg.ungetregister(exprasmlist,NR_EDX);
              cg.ungetregister(exprasmlist,NR_EAX);
              location.register:=cg.getintregister(exprasmlist,OS_INT);
              emit_reg_reg(A_MOV,S_L,NR_EAX,location.register);
            end
          else
            begin
              cg.ungetregister(exprasmlist,NR_EAX);
              cg.ungetregister(exprasmlist,NR_EDX);
              location.register:=cg.getintregister(exprasmlist,OS_INT);
              emit_reg_reg(A_MOV,S_L,NR_EDX,location.register);
            end;
        end;
    end;


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}


    function ti386shlshrnode.first_shlshr64bitint: tnode;

    begin
      result := nil;
    end;

    procedure ti386shlshrnode.pass_2;

    var hregisterhigh,hregisterlow:Tregister;
        op:Tasmop;
        v : TConstExprInt;
        l1,l2,l3:Tasmlabel;

    begin
      secondpass(left);
      secondpass(right);

      { determine operator }
      if nodetype=shln then
        op:=A_SHL
      else
        op:=A_SHR;

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
              v:=Tordconstnode(right).value and 63;
              if v>31 then
                begin
                  if nodetype=shln then
                    begin
                      emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                      if ((v and 31) <> 0) then
                        emit_const_reg(A_SHL,S_L,v and 31,hregisterlow);
                    end
                  else
                    begin
                      emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                      if ((v and 31) <> 0) then
                        emit_const_reg(A_SHR,S_L,v and 31,hregisterhigh);
                    end;
                  location.registerhigh:=hregisterlow;
                  location.registerlow:=hregisterhigh;
                end
              else
                begin
                  if nodetype=shln then
                    begin
                      emit_const_reg_reg(A_SHLD,S_L,v and 31,hregisterlow,hregisterhigh);
                      emit_const_reg(A_SHL,S_L,v and 31,hregisterlow);
                    end
                  else
                    begin
                      emit_const_reg_reg(A_SHRD,S_L,v and 31,hregisterhigh,hregisterlow);
                      emit_const_reg(A_SHR,S_L,v and 31,hregisterhigh);
                    end;
                  location.registerlow:=hregisterlow;
                  location.registerhigh:=hregisterhigh;
                end;
            end
          else
            begin
              { load right operators in a register }
              cg.getexplicitregister(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,NR_ECX);
              if right.location.loc<>LOC_CREGISTER then
                location_release(exprasmlist,right.location);

              { left operator is already in a register }
              { hence are both in a register }
              { is it in the case ECX ? }

              { the damned shift instructions work only til a count of 32 }
              { so we've to do some tricks here                           }
              objectlibrary.getlabel(l1);
              objectlibrary.getlabel(l2);
              objectlibrary.getlabel(l3);
              emit_const_reg(A_CMP,S_L,64,NR_ECX);
              cg.a_jmp_flags(exprasmlist,F_L,l1);
              emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
              emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
              cg.a_jmp_always(exprasmlist,l3);
              cg.a_label(exprasmlist,l1);
              emit_const_reg(A_CMP,S_L,32,NR_ECX);
              cg.a_jmp_flags(exprasmlist,F_L,l2);
              emit_const_reg(A_SUB,S_L,32,NR_ECX);
              if nodetype=shln then
                begin
                  emit_reg_reg(A_SHL,S_L,NR_CL,hregisterlow);
                  emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                  emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHLD,S_L,NR_CL,hregisterlow,hregisterhigh);
                  emit_reg_reg(A_SHL,S_L,NR_CL,hregisterlow);
                end
              else
                begin
                  emit_reg_reg(A_SHR,S_L,NR_CL,hregisterhigh);
                  emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                  emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHRD,S_L,NR_CL,hregisterhigh,hregisterlow);
                  emit_reg_reg(A_SHR,S_L,NR_CL,hregisterhigh);
                end;
              cg.a_label(exprasmlist,l3);

              cg.ungetregister(exprasmlist,NR_ECX);
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
            { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)}
            emit_const_reg(op,S_L,tordconstnode(right).value and 31,location.register)
          else
            begin
              { load right operators in a ECX }
              if right.location.loc<>LOC_CREGISTER then
                location_release(exprasmlist,right.location);
              cg.getexplicitregister(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,NR_ECX);

              { right operand is in ECX }
              cg.ungetregister(exprasmlist,NR_ECX);
              emit_reg_reg(op,S_L,NR_CL,location.register);
            end;
        end;
    end;


begin
   cunaryminusnode:=ti386unaryminusnode;
   cmoddivnode:=ti386moddivnode;
   cshlshrnode:=ti386shlshrnode;
   cnotnode:=ti386notnode;
end.
{
  $Log$
  Revision 1.71  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.70  2004/05/23 14:10:17  peter
    * fix shl/shr with value > 63

  Revision 1.69  2004/01/20 12:59:37  florian
    * common addnode code for x86-64 and i386

}
