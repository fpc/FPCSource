{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      node,nmat;

    type
      ti386moddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      ti386shlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      ti386unaryminusnode = class(tunaryminusnode)
         function pass_1 : tnode;override;
         procedure pass_2;override;
      end;

      ti386notnode = class(tnotnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasm,types,
      cginfo,cgbase,pass_1,pass_2,
      ncon,
      cpubase,
      cga,tgobj,n386util,ncgutil,cgobj,cg64f32,rgobj,rgcpu;

{*****************************************************************************
                             TI386MODDIVNODE
*****************************************************************************}

    procedure ti386moddivnode.pass_2;
      var
         hreg1 : tregister;
         hreg2 : tregister;
         shrdiv, pushed,popeax,popedx : boolean;
         power : longint;
         hl : tasmlabel;
      begin
         shrdiv := false;
         secondpass(left);
         if codegenerror then
          exit;
         pushed:=maybe_push(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if pushed then
           restore(left,is_64bitint(left.resulttype.def));
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
              location_force_reg(left.location,OS_INT,false);
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
                      If (aktOptProcessor <> class386) and
                         not(CS_LittleSize in aktglobalswitches) then
                         { use a sequence without jumps, saw this in
                           comp.compilers (JM) }
                        begin
                          { no jumps, but more operations }
                          if (hreg1 = R_EAX) and
                             (R_EDX in rg.unusedregsint) then
                            begin
                              hreg2 := rg.getexplicitregisterint(exprasmlist,R_EDX);
                              emit_none(A_CDQ,S_NO);
                            end
                          else
                            begin
                              rg.getexplicitregisterint(exprasmlist,R_EDI);
                              hreg2 := R_EDI;
                              emit_reg_reg(A_MOV,S_L,hreg1,R_EDI);
                              { if the left value is signed, R_EDI := $ffffffff,
                                otherwise 0 }
                              emit_const_reg(A_SAR,S_L,31,R_EDI);
                              { if signed, R_EDI := right value-1, otherwise 0 }
                            end;
                          emit_const_reg(A_AND,S_L,tordconstnode(right).value-1,hreg2);
                          { add to the left value }
                          emit_reg_reg(A_ADD,S_L,hreg2,hreg1);
                          { release EDX if we used it }
                          { also releas EDI }
                          rg.ungetregisterint(exprasmlist,hreg2);
                          { do the shift }
                          emit_const_reg(A_SAR,S_L,power,hreg1);
                        end
                      else
                        begin
                          { a jump, but less operations }
                          emit_reg_reg(A_TEST,S_L,hreg1,hreg1);
                          getlabel(hl);
                          emitjmp(C_NS,hl);
                          if power=1 then
                            emit_reg(A_INC,S_L,hreg1)
                          else
                            emit_const_reg(A_ADD,S_L,tordconstnode(right).value-1,hreg1);
                          emitlab(hl);
                          emit_const_reg(A_SAR,S_L,power,hreg1);
                        end
                    End
                  Else
                    emit_const_reg(A_SHR,S_L,power,hreg1);
                End
              else
                begin
                  { bring denominator to EDI }
                  { EDI is always free, it's }
                  { only used for temporary  }
                  { purposes              }
                  rg.getexplicitregisterint(exprasmlist,R_EDI);
                  if right.location.loc<>LOC_CREGISTER then
                   location_release(exprasmlist,right.location);
                  cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,R_EDI);
                  popedx:=false;
                  popeax:=false;
                  if hreg1=R_EDX then
                    begin
                      if not(R_EAX in rg.unusedregsint) then
                         begin
                            emit_reg(A_PUSH,S_L,R_EAX);
                            popeax:=true;
                         end
                       else
                         rg.getexplicitregisterint(exprasmlist,R_EAX);
                      emit_reg_reg(A_MOV,S_L,R_EDX,R_EAX);
                    end
                  else
                    begin
                       if not(R_EDX in rg.unusedregsint) then
                         begin
                            emit_reg(A_PUSH,S_L,R_EDX);
                            popedx:=true;
                         end
                       else
                         rg.getexplicitregisterint(exprasmlist,R_EDX);
                       if hreg1<>R_EAX then
                         begin
                            if not(R_EAX in rg.unusedregsint) then
                              begin
                                 emit_reg(A_PUSH,S_L,R_EAX);
                                 popeax:=true;
                              end
                            else
                              rg.getexplicitregisterint(exprasmlist,R_EAX);
                            emit_reg_reg(A_MOV,S_L,hreg1,R_EAX);
                         end;
                    end;
                  { sign extension depends on the left type }
                  if torddef(left.resulttype.def).typ=u32bit then
                     emit_reg_reg(A_XOR,S_L,R_EDX,R_EDX)
                  else
                     emit_none(A_CDQ,S_NO);

                  { division depends on the right type }
                  if torddef(right.resulttype.def).typ=u32bit then
                    emit_reg(A_DIV,S_L,R_EDI)
                  else
                    emit_reg(A_IDIV,S_L,R_EDI);
                  rg.ungetregisterint(exprasmlist,R_EDI);
                  if nodetype=divn then
                    begin
                       if not popedx and (hreg1 <> R_EDX) then
                         rg.ungetregister(exprasmlist,R_EDX);
                       { if result register is busy then copy }
                       if popeax then
                         begin
                            if hreg1=R_EAX then
                              internalerror(112);
                            emit_reg_reg(A_MOV,S_L,R_EAX,hreg1)
                         end
                       else
                         if hreg1<>R_EAX then
                           Begin
                             rg.ungetregisterint(exprasmlist,hreg1);
                             { no need to allocate eax, that's already done before }
                             { the div (JM)                                        }
                             hreg1 := R_EAX;
                           end;
                    end
                  else
                    begin
                      if not popeax and (hreg1 <> R_EAX)then
                        rg.ungetregister(exprasmlist,R_EAX);
                      if popedx then
                       {the mod was done by an (i)div (so the result is now in
                        edx), but edx was occupied prior to the division, so
                        move the result into a safe place (JM)}
                        emit_reg_reg(A_MOV,S_L,R_EDX,hreg1)
                      else
                        Begin
                          if hreg1 <> R_EDX then
                            rg.ungetregisterint(exprasmlist,hreg1);
                          hreg1 := R_EDX
                        End;
                    end;
                  if popeax then
                    emit_reg(A_POP,S_L,R_EAX);
                  if popedx then
                    emit_reg(A_POP,S_L,R_EDX);
                end;
              If not(shrdiv) then
               { shrdiv only use hreg1 (which is already in usedinproc,   }
               { since it was acquired with getregister), the others also }
               { use both EAX and EDX (JM)                                }
                Begin
                  include(rg.usedinproc,R_EAX);
                  include(rg.usedinproc,R_EDX);
                End;
              location_reset(location,LOC_REGISTER,OS_32);
              location.register:=hreg1;
           end;
      end;


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}

    procedure ti386shlshrnode.pass_2;
      var
         hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         pushed,popecx : boolean;
         op : tasmop;
         l1,l2,l3 : tasmlabel;

      begin
         popecx:=false;

         secondpass(left);
         pushed:=maybe_push(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if pushed then
           restore(left,is_64bitint(left.resulttype.def));

         { determine operator }
         case nodetype of
           shln: op:=A_SHL;
           shrn: op:=A_SHR;
         end;

         if is_64bitint(left.resulttype.def) then
           begin
              location_reset(location,LOC_REGISTER,OS_64);

              { load left operator in a register }
              location_force_reg(left.location,OS_64,false);
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
                       cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,hregister2);
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
                        getlabel(l1);
                        getlabel(l2);
                        getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emitjmp(C_None,l3);
                        emitlab(l2);
                        emit_reg_reg_reg(A_SHLD,S_L,R_CL,
                          hregisterlow,hregisterhigh);
                        emit_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow);
                        emitlab(l3);
                     end
                   else
                     begin
                        getlabel(l1);
                        getlabel(l2);
                        getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,R_ECX);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l1);
                        emit_const_reg(A_CMP,S_L,32,R_ECX);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,R_ECX);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        emitjmp(C_None,l3);
                        emitlab(l2);
                        emit_reg_reg_reg(A_SHRD,S_L,R_CL,
                          hregisterhigh,hregisterlow);
                        emit_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh);
                        emitlab(l3);

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
              location_force_reg(location,OS_INT,false);

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
                       cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,hregister2);
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
      end;


{*****************************************************************************
                          TI386UNARYMINUSNODE
*****************************************************************************}

    function ti386unaryminusnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         if (left.resulttype.def.deftype=floatdef) then
           begin
             if (registersfpu < 1) then
               registersfpu := 1;
             location.loc:=LOC_FPUREGISTER;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype.def) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<2) then
                registers32:=2;
              location.loc:=LOC_REGISTER;
           end
         else if (left.resulttype.def.deftype=orddef) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end;
      end;


    procedure ti386unaryminusnode.pass_2;

{$ifdef SUPPORT_MMX}
      procedure do_mmx_neg;
        var
           op : tasmop;
        begin
           location_reset(location,LOC_MMXREGISTER,OS_NO);
           if cs_mmx_saturation in aktlocalswitches then
             case mmx_type(resulttype.def) of
                mmxs8bit:
                  op:=A_PSUBSB;
                mmxu8bit:
                  op:=A_PSUBUSB;
                mmxs16bit,mmxfixed16:
                  op:=A_PSUBSW;
                mmxu16bit:
                  op:=A_PSUBUSW;
             end
           else
             case mmx_type(resulttype.def) of
                mmxs8bit,mmxu8bit:
                  op:=A_PSUBB;
                mmxs16bit,mmxu16bit,mmxfixed16:
                  op:=A_PSUBW;
                mmxs32bit,mmxu32bit:
                  op:=A_PSUBD;
             end;
           emit_reg_reg(op,S_NO,location.register,R_MM7);
           emit_reg_reg(A_MOVQ,S_NO,R_MM7,location.register);
        end;
{$endif}

      begin
         if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);

              { load left operator in a register }
              location_copy(location,left.location);
              location_force_reg(location,OS_64,false);

              emit_reg(A_NOT,S_L,location.registerhigh);
              emit_reg(A_NEG,S_L,location.registerlow);
              emit_const_reg(A_SBB,S_L,-1,location.registerhigh);
           end
         else
           begin
              secondpass(left);
              location_reset(location,LOC_REGISTER,OS_INT);
              case left.location.loc of
                 LOC_REGISTER:
                   begin
                      location.register:=left.location.register;
                      emit_reg(A_NEG,S_L,location.register);
                   end;
                 LOC_CREGISTER:
                   begin
                      location.register:=rg.getregisterint(exprasmlist);
                      emit_reg_reg(A_MOV,S_L,location.register,
                        location.register);
                      emit_reg(A_NEG,S_L,location.register);
                   end;
{$ifdef SUPPORT_MMX}
                 LOC_MMXREGISTER:
                   begin
                      location_copy(location,left.location);
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      do_mmx_neg;
                   end;
                 LOC_CMMXREGISTER:
                   begin
                      location.register:=rg.getregistermm(exprasmlist);
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      emit_reg_reg(A_MOVQ,S_NO,left.location.register,
                        location.register);
                      do_mmx_neg;
                   end;
{$endif SUPPORT_MMX}
                 LOC_REFERENCE,
                 LOC_CREFERENCE:
                   begin
                      reference_release(exprasmlist,left.location.reference);
                      if (left.resulttype.def.deftype=floatdef) then
                        begin
                           location_reset(location,LOC_FPUREGISTER,OS_NO);
                           location.register := R_ST;
                           cg.a_loadfpu_ref_reg(exprasmlist,
                              def_cgsize(left.resulttype.def),
                              left.location.reference,R_ST);
                           emit_none(A_FCHS,S_NO);
                        end
{$ifdef SUPPORT_MMX}
                      else if (cs_mmx in aktlocalswitches) and is_mmx_able_array(left.resulttype.def) then
                        begin
                           location.register:=rg.getregistermm(exprasmlist);
                           emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                           emit_ref_reg(A_MOVQ,S_NO,left.location.reference,location.register);
                           do_mmx_neg;
                        end
{$endif SUPPORT_MMX}
                      else
                        begin
                           location.register:=rg.getregisterint(exprasmlist);
                           emit_ref_reg(A_MOV,S_L,left.location.reference,location.register);
                           emit_reg(A_NEG,S_L,location.register);
                        end;
                   end;
                 LOC_FPUREGISTER,LOC_CFPUREGISTER:
                   begin
                      { "load st,st" is ignored by the code generator }
                      cg.a_loadfpu_reg_reg(exprasmlist,left.location.register,R_ST);
                      location_reset(location,LOC_FPUREGISTER,OS_NO);
                      location.register:=R_ST;
                      emit_none(A_FCHS,S_NO);
                   end;
                 else
                    internalerror(200203225);
              end;
           end;
         { Here was a problem...     }
         { Operand to be negated always     }
         { seems to be converted to signed  }
         { 32-bit before doing neg!!     }
         { So this is useless...     }
         { that's not true: -2^31 gives an overflow error if it is negaded (FK) }
         { emitoverflowcheck(p);}
      end;


{*****************************************************************************
                               TI386NOTNODE
*****************************************************************************}

    procedure ti386notnode.pass_2;
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
                  maketojumpbool(left,lr_load_regvars);
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
                  location_force_reg(left.location,def_cgsize(resulttype.def),true);
                  location_release(exprasmlist,left.location);
                  emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
             else
                internalerror(200203224);
            end;
          end
{$ifdef SUPPORT_MMX}
         else
          if (cs_mmx in aktlocalswitches) and is_mmx_able_array(left.resulttype.def) then
           begin
             secondpass(left);
             location_reset(location,LOC_MMXREGISTER,OS_NO);
             { prepare EDI }
             rg.getexplicitregisterint(exprasmlist,R_EDI);
             emit_const_reg(A_MOV,S_L,longint($ffffffff),R_EDI);
             { load operand }
             case left.location.loc of
               LOC_MMXREGISTER:
                 location_copy(location,left.location);
               LOC_CMMXREGISTER:
                 begin
                   location.register:=rg.getregistermm(exprasmlist);
                   emit_reg_reg(A_MOVQ,S_NO,left.location.register,location.register);
                 end;
               LOC_REFERENCE,
               LOC_CREFERENCE:
                 begin
                   location_release(exprasmlist,left.location);
                   location.register:=rg.getregistermm(exprasmlist);
                   emit_ref_reg(A_MOVQ,S_NO,left.location.reference,location.register);
                 end;
             end;
             { load mask }
             emit_reg_reg(A_MOVD,S_NO,R_EDI,R_MM7);
             rg.ungetregisterint(exprasmlist,R_EDI);
             { lower 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,location.register);
             { shift mask }
             emit_const_reg(A_PSLLQ,S_NO,32,R_MM7);
             { higher 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,location.register);
           end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);
              location_copy(location,left.location);
              location_force_reg(location,OS_64,false);

              emit_reg(A_NOT,S_L,location.registerlow);
              emit_reg(A_NOT,S_L,location.registerhigh);
           end
         else
          begin
            secondpass(left);
            location_copy(location,left.location);
            location_force_reg(location,def_cgsize(resulttype.def),false);

            opsize:=def_opsize(resulttype.def);
            emit_reg(A_NOT,opsize,location.register);
          end;
      end;


begin
   cmoddivnode:=ti386moddivnode;
   cshlshrnode:=ti386shlshrnode;
   cunaryminusnode:=ti386unaryminusnode;
   cnotnode:=ti386notnode;
end.
{
  $Log$
  Revision 1.25  2002-04-02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.24  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.23  2002/03/04 19:10:14  peter
    * removed compiler warnings

  Revision 1.22  2001/12/30 17:24:47  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.21  2001/12/29 15:27:24  jonas
    * made 'mod powerof2' -> 'and' optimization processor independent

  Revision 1.20  2001/12/27 15:33:58  jonas
    * fixed fpuregister counting errors ("merged")

  Revision 1.19  2001/12/07 13:03:49  jonas
    * fixed web bug 1716

  Revision 1.18  2001/12/04 15:57:28  jonas
    * never generate any "shll/shrl $0,%reg" anymore

  Revision 1.17  2001/12/02 16:19:17  jonas
    * less unnecessary regvar loading with if-statements

  Revision 1.16  2001/09/05 15:22:10  jonas
    * made multiplying, dividing and mod'ing of int64 and qword processor
      independent with compilerprocs (+ small optimizations by using shift/and
      where possible)

  Revision 1.15  2001/08/29 12:03:23  jonas
    * fixed wrong regalloc info around FPC_MUL/DIV/MOD_INT64/QWORD calls
    * fixed partial result overwriting with the above calls too

  Revision 1.14  2001/08/26 13:37:00  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.13  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.12  2001/04/04 22:37:06  peter
    * fix for not with no 32bit values

  Revision 1.11  2001/04/02 21:20:38  peter
    * resulttype rewrite

  Revision 1.10  2001/02/03 12:52:34  jonas
    * fixed web bug 1383

  Revision 1.9  2000/12/07 17:19:46  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.8  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.7  2000/11/29 00:30:48  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.6  2000/11/20 14:05:50  jonas
    * fixed bug in my changes to fix the regalloc info for div/mod ("merged")

  Revision 1.5  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.4  2000/10/19 16:26:52  jonas
    * fixed wrong regalloc info for secondmoddiv ("merged", also small
      correction made afterwards in fixes branch)

  Revision 1.3  2000/10/17 15:41:48  jonas
    * fixed stupid error in previous commit :/

  Revision 1.1  2000/10/15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.4  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.3  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.2  2000/09/24 15:06:18  peter
    * use defines.inc

  Revision 1.1  2000/09/22 22:24:37  florian
    * initial revision

}
