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
      node,nmat,ncgmat;

    type
      ti386moddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      ti386shlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      ti386unaryminusnode = class(tcgunaryminusnode)
{$ifdef SUPPORT_MMX}
         procedure second_mmx;override;
{$endif SUPPORT_MMX}
         procedure second_float;override;
         function pass_1:tnode;override;
      end;

      ti386notnode = class(tcgnotnode)
         procedure second_boolean;override;
{$ifdef SUPPORT_MMX}
         procedure second_mmx;override;
{$endif SUPPORT_MMX}
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,defutil,
      cginfo,cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,tgobj,ncgutil,cgobj,rgobj;

{*****************************************************************************
                             TI386MODDIVNODE
*****************************************************************************}

{$ifdef newra}
    procedure ti386moddivnode.pass_2;

    var  r,r2,hreg1,hreg2:Tregister;
         power:longint;
         hl:Tasmlabel;
         op:Tasmop;
         pushedregs:Tmaybesave;

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
                  hreg2:=rg.getregisterint(exprasmlist,OS_INT);
                  emit_reg_reg(A_MOV,S_L,hreg1,hreg2);
                  {If the left value is signed, hreg2=$ffffffff, otherwise 0.}
                  emit_const_reg(A_SAR,S_L,31,hreg2);
                  {If signed, hreg2=right value-1, otherwise 0.}
                  emit_const_reg(A_AND,S_L,tordconstnode(right).value-1,hreg2);
                  { add to the left value }
                  emit_reg_reg(A_ADD,S_L,hreg2,hreg1);
                  { release EDX if we used it }
                  rg.ungetregisterint(exprasmlist,hreg2);
                  { do the shift }
                  emit_const_reg(A_SAR,S_L,power,hreg1);
                end
              else
                begin
                  { a jump, but less operations }
                  emit_reg_reg(A_TEST,S_L,hreg1,hreg1);
                  objectlibrary.getlabel(hl);
                  emitjmp(C_NS,hl);
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
          rg.ungetregisterint(exprasmlist,hreg1);
          rg.getexplicitregisterint(exprasmlist,NR_EAX);
          r.enum:=R_INTREGISTER;
          r.number:=NR_EAX;
          r2.enum:=R_INTREGISTER;
          r2.number:=NR_EDX;
          emit_reg_reg(A_MOV,S_L,hreg1,r);
          rg.getexplicitregisterint(exprasmlist,NR_EDX);
          {Sign extension depends on the left type.}
          if torddef(left.resulttype.def).typ=u32bit then
            emit_reg_reg(A_XOR,S_L,r2,r2)
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
              hreg1:=rg.getregisterint(exprasmlist,right.location.size);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,hreg1);
              rg.ungetregisterint(exprasmlist,hreg1);
              emit_reg(op,S_L,hreg1);
            end;
          location_release(exprasmlist,right.location);

          {Copy the result into a new register. Release EAX & EDX.}
          if nodetype=divn then
            begin
              rg.ungetregisterint(exprasmlist,r2);
              rg.ungetregisterint(exprasmlist,r);
              location.register:=rg.getregisterint(exprasmlist,OS_INT);
              emit_reg_reg(A_MOV,S_L,r,location.register);
            end
          else
            begin
              rg.ungetregisterint(exprasmlist,r);
              rg.ungetregisterint(exprasmlist,r2);
              location.register:=rg.getregisterint(exprasmlist,OS_INT);
              emit_reg_reg(A_MOV,S_L,r2,location.register);
            end;
        end;
    end;
{$else}
    procedure ti386moddivnode.pass_2;
      var
         hreg1 : tregister;
         hreg2 : tregister;
         r,r2  : Tregister;
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
              if hreg1.enum<>R_INTREGISTER then
                internalerror(200302042);

              if (nodetype=divn) and (right.nodetype=ordconstn) and
                 ispowerof2(tordconstnode(right).value,power) then
                begin
                  shrdiv := true;
                  { for signed numbers, the numerator must be adjusted before the
                    shift instruction, but not wih unsigned numbers! Otherwise,
                    "Cardinal($ffffffff) div 16" overflows! (JM) }
                  if is_signed(left.resulttype.def) Then
                    begin
                      if (aktOptProcessor <> class386) and
                         not(CS_LittleSize in aktglobalswitches) then
                         { use a sequence without jumps, saw this in
                           comp.compilers (JM) }
                        begin
                          { no jumps, but more operations }
                          if (hreg1.number = NR_EAX) and
                             (RS_EDX in rg.unusedregsint) then
                            begin
                              hreg2 := rg.getexplicitregisterint(exprasmlist,NR_EDX);
                              emit_none(A_CDQ,S_NO);
                            end
                          else
                            begin
                              rg.getexplicitregisterint(exprasmlist,NR_EDI);
                              hreg2.enum := R_INTREGISTER;
                              hreg2.number := NR_EDI;
                              emit_reg_reg(A_MOV,S_L,hreg1,hreg2);
                              { if the left value is signed, R_EDI := $ffffffff,
                                otherwise 0 }
                              emit_const_reg(A_SAR,S_L,31,hreg2);
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
                          objectlibrary.getlabel(hl);
                          emitjmp(C_NS,hl);
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
                end
              else
                begin
                  { bring denominator to EDI }
                  { EDI is always free, it's }
                  { only used for temporary  }
                  { purposes              }
                  rg.getexplicitregisterint(exprasmlist,NR_EDI);
                  if right.location.loc<>LOC_CREGISTER then
                   location_release(exprasmlist,right.location);
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_EDI;
                  cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,r);
                  popedx:=false;
                  popeax:=false;
                  r.number:=NR_EAX;
                  r2.enum:=R_INTREGISTER;
                  r2.number:=NR_EDX;
                  if hreg1.number=NR_EDX then
                    begin
                      if not(RS_EAX in rg.unusedregsint) then
                         begin
                            emit_reg(A_PUSH,S_L,r);
                            popeax:=true;
                         end
                       else
                         rg.getexplicitregisterint(exprasmlist,NR_EAX);
                      emit_reg_reg(A_MOV,S_L,r2,r);
                    end
                  else
                    begin
                       if not(RS_EDX in rg.unusedregsint) then
                         begin
                            emit_reg(A_PUSH,S_L,r2);
                            popedx:=true;
                         end
                       else
                         rg.getexplicitregisterint(exprasmlist,NR_EDX);
                       if hreg1.number<>NR_EAX then
                         begin
                            if not(RS_EAX in rg.unusedregsint) then
                              begin
                                 emit_reg(A_PUSH,S_L,r);
                                 popeax:=true;
                              end
                            else
                              rg.getexplicitregisterint(exprasmlist,NR_EAX);
                            emit_reg_reg(A_MOV,S_L,hreg1,r);
                         end;
                    end;
                  { sign extension depends on the left type }
                  if torddef(left.resulttype.def).typ=u32bit then
                     emit_reg_reg(A_XOR,S_L,r2,r2)
                  else
                     emit_none(A_CDQ,S_NO);

                  { division depends on the right type }
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_EDI;
                  if torddef(right.resulttype.def).typ=u32bit then
                    emit_reg(A_DIV,S_L,r)
                  else
                    emit_reg(A_IDIV,S_L,r);
                  rg.ungetregisterint(exprasmlist,r);
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_EAX;
                  if nodetype=divn then
                    begin
                       if not popedx and (hreg1.number <> NR_EDX) then
                        begin
                           r2.enum:=R_INTREGISTER;
                           r2.number:=NR_EDX;
                           rg.ungetregisterint(exprasmlist,r2);
                        end;
                       { if result register is busy then copy }
                       if popeax then
                         begin
                            if hreg1.number=NR_EAX then
                              internalerror(112);
                            emit_reg_reg(A_MOV,S_L,r,hreg1)
                         end
                       else
                         if hreg1.number<>NR_EAX then
                           begin
                             rg.ungetregisterint(exprasmlist,hreg1);
                             { no need to allocate eax, that's already done before }
                             { the div (JM)                                        }
                             hreg1.number:=NR_EAX;
                           end;
                    end
                  else
                    begin
                      if not popeax and (hreg1.number <> NR_EAX)then
                        begin
                          r.number:=NR_EAX;
                          rg.ungetregisterint(exprasmlist,r);
                        end;
                      if popedx then
                       {the mod was done by an (i)div (so the result is now in
                        edx), but edx was occupied prior to the division, so
                        move the result into a safe place (JM)}
                        emit_reg_reg(A_MOV,S_L,r2,hreg1)
                      else
                        begin
                          if hreg1.number <> NR_EDX then
                            rg.ungetregisterint(exprasmlist,hreg1);
                          hreg1.number:=NR_EDX
                        end;
                    end;
                  if popeax then
                    emit_reg(A_POP,S_L,r);
                  if popedx then
                    emit_reg(A_POP,S_L,r2);
                end;
              If not(shrdiv) then
               { shrdiv only use hreg1 (which is already in usedinproc,   }
               { since it was acquired with getregister), the others also }
               { use both EAX and EDX (JM)                                }
                begin
                  include(rg.used_in_proc_int,RS_EAX);
                  include(rg.used_in_proc_int,RS_EDX);
                end;
              location_reset(location,LOC_REGISTER,OS_INT);
              location.register:=hreg1;
           end;
      end;
{$endif}


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}


    function ti386shlshrnode.first_shlshr64bitint: tnode;

    begin
      result := nil;
    end;

{$ifdef newra}
    procedure ti386shlshrnode.pass_2;

    var hregister2,hregisterhigh,hregisterlow:Tregister;
        r,r2:Tregister;
        op:Tasmop;
        l1,l2,l3:Tasmlabel;
        pushedregs:Tmaybesave;

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
              { shrd/shl works only for values <=31 !! }
              if Tordconstnode(right).value>31 then
                begin
                  if nodetype=shln then
                    begin
                      emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                      if ((tordconstnode(right).value and 31) <> 0) then
                        emit_const_reg(A_SHL,S_L,tordconstnode(right).value and 31,
                                       hregisterlow);
                    end
                  else
                    begin
                      emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
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
              hregister2:=rg.getexplicitregisterint(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,hregister2);
              if right.location.loc<>LOC_CREGISTER then
                location_release(exprasmlist,right.location);

              { left operator is already in a register }
              { hence are both in a register }
              { is it in the case ECX ? }
              r.enum:=R_INTREGISTER;
              r.number:=NR_ECX;
              r2.enum:=R_INTREGISTER;
              r2.number:=NR_CL;

              { the damned shift instructions work only til a count of 32 }
              { so we've to do some tricks here                           }
              objectlibrary.getlabel(l1);
              objectlibrary.getlabel(l2);
              objectlibrary.getlabel(l3);
              emit_const_reg(A_CMP,S_L,64,hregister2);
              emitjmp(C_L,l1);
              emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
              emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
              cg.a_jmp_always(exprasmlist,l3);
              cg.a_label(exprasmlist,l1);
              emit_const_reg(A_CMP,S_L,32,hregister2);
              emitjmp(C_L,l2);
              emit_const_reg(A_SUB,S_L,32,hregister2);
              if nodetype=shln then
                begin
                  emit_reg_reg(A_SHL,S_L,r2,hregisterlow);
                  emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                  emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHLD,S_L,r2,hregisterlow,hregisterhigh);
                  emit_reg_reg(A_SHL,S_L,r2,hregisterlow);
                end
              else
                begin
                  emit_reg_reg(A_SHR,S_L,r2,hregisterhigh);
                  emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                  emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHRD,S_L,r2,hregisterhigh,hregisterlow);
                  emit_reg_reg(A_SHR,S_L,r2,hregisterhigh);
                end;
              cg.a_label(exprasmlist,l3);

              rg.ungetregisterint(exprasmlist,hregister2);
              location.registerlow:=hregisterlow;
              location.registerhigh:=hregisterhigh;
            end;
        end
      else
        begin
          { load left operators in a register }
          location_copy(location,left.location);
          location_force_reg(exprasmlist,location,OS_INT,false);

          r2.enum:=R_INTREGISTER;
          r2.number:=NR_CL;

          { shifting by a constant directly coded: }
          if (right.nodetype=ordconstn) then
            { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)}
            emit_const_reg(op,S_L,tordconstnode(right).value and 31,location.register)
          else
            begin
              { load right operators in a ECX }
              if right.location.loc<>LOC_CREGISTER then
                location_release(exprasmlist,right.location);
              hregister2:=rg.getexplicitregisterint(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,hregister2);

              { right operand is in ECX }
              emit_reg_reg(op,S_L,r2,location.register);
              rg.ungetregisterint(exprasmlist,hregister2);
            end;
        end;
    end;
{$else}
    procedure ti386shlshrnode.pass_2;
      var
         hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         popecx : boolean;
         op : tasmop;
         l1,l2,l3 : tasmlabel;
         pushedregs : tmaybesave;
         r,r2:Tregister;
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

         if is_64bitint(left.resulttype.def) then
           begin
              location_reset(location,LOC_REGISTER,OS_64);

              { load left operator in a register }
              location_force_reg(exprasmlist,left.location,OS_64,false);
              hregisterhigh:=left.location.registerhigh;
              hregisterlow:=left.location.registerlow;
              if hregisterhigh.enum<>R_INTREGISTER then
                internalerror(200302056);
              if hregisterlow.enum<>R_INTREGISTER then
                internalerror(200302056);

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
                       if (RS_ECX in rg.unusedregsint) then
                        hregister2:=rg.getexplicitregisterint(exprasmlist,NR_ECX)
                       else
                        hregister2:=rg.getregisterint(exprasmlist,OS_INT);
                       cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,hregister2);
                     end
                   else
                     hregister2:=right.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   r.enum:=R_INTREGISTER;
                   r.number:=NR_ECX;
                   r2.enum:=R_INTREGISTER;
                   r2.number:=NR_CL;
                   if (hregisterlow.number=NR_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterlow,hregister2);
                        hregister3:=hregisterlow;
                        hregisterlow:=hregister2;
                        hregister2:=hregister3;
                     end
                   else if (hregisterhigh.number=NR_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregisterhigh,hregister2);
                        hregister3:=hregisterhigh;
                        hregisterhigh:=hregister2;
                        hregister2:=hregister3;
                     end

                   { if second operator not in ECX ? }
                   else if (hregister2.number<>NR_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (RS_ECX in rg.unusedregsint) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,r);
                         end
                        else
                          rg.getexplicitregisterint(exprasmlist,NR_ECX);
                        emit_reg_reg(A_MOV,S_L,hregister2,r);
                     end;

                   if hregister2.number <> NR_ECX then
                     rg.ungetregisterint(exprasmlist,hregister2);

                   { the damned shift instructions work only til a count of 32 }
                   { so we've to do some tricks here                           }
                   if nodetype=shln then
                     begin
                        objectlibrary.getlabel(l1);
                        objectlibrary.getlabel(l2);
                        objectlibrary.getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,r);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l1);
                        emit_const_reg(A_CMP,S_L,32,r);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,r);
                        emit_reg_reg(A_SHL,S_L,r2,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,hregisterlow,hregisterhigh);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l2);
                        emit_reg_reg_reg(A_SHLD,S_L,r2,
                          hregisterlow,hregisterhigh);
                        emit_reg_reg(A_SHL,S_L,r2,
                          hregisterlow);
                        cg.a_label(exprasmlist,l3);
                     end
                   else
                     begin
                        objectlibrary.getlabel(l1);
                        objectlibrary.getlabel(l2);
                        objectlibrary.getlabel(l3);
                        emit_const_reg(A_CMP,S_L,64,r);
                        emitjmp(C_L,l1);
                        emit_reg_reg(A_XOR,S_L,hregisterlow,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l1);
                        emit_const_reg(A_CMP,S_L,32,r);
                        emitjmp(C_L,l2);
                        emit_const_reg(A_SUB,S_L,32,r);
                        emit_reg_reg(A_SHR,S_L,r2,
                          hregisterhigh);
                        emit_reg_reg(A_MOV,S_L,hregisterhigh,hregisterlow);
                        emit_reg_reg(A_XOR,S_L,hregisterhigh,hregisterhigh);
                        cg.a_jmp_always(exprasmlist,l3);
                        cg.a_label(exprasmlist,l2);
                        emit_reg_reg_reg(A_SHRD,S_L,r2,
                          hregisterhigh,hregisterlow);
                        emit_reg_reg(A_SHR,S_L,r2,
                          hregisterhigh);
                        cg.a_label(exprasmlist,l3);

                     end;

                   { maybe put ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,r)
                   else
                     rg.ungetregisterint(exprasmlist,r);

                   location.registerlow:=hregisterlow;
                   location.registerhigh:=hregisterhigh;
                end;
           end
         else
           begin
              { load left operators in a register }
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_INT,false);

              r.enum:=R_INTREGISTER;
              r.number:=NR_ECX;
              r2.enum:=R_INTREGISTER;
              r2.number:=NR_CL;

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
                       hregister2:=rg.getexplicitregisterint(exprasmlist,NR_ECX);
                       cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,hregister2);
                     end
                   else
                     hregister2:=right.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (location.register.number=NR_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,location.register,hregister2);
                        hregister3:=location.register;
                        location.register:=hregister2;
                        hregister2:=hregister3;
                     end
                   { if second operator not in ECX ? }
                   else if (hregister2.number<>NR_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (RS_ECX in rg.unusedregsint) then
                         begin
                           popecx:=true;
                           emit_reg(A_PUSH,S_L,r);
                         end
                        else
                          rg.getexplicitregisterint(exprasmlist,NR_ECX);
                        emit_reg_reg(A_MOV,S_L,hregister2,r);
                     end;
                   rg.ungetregisterint(exprasmlist,hregister2);
                   { right operand is in ECX }
                   emit_reg_reg(op,S_L,r2,location.register);
                   { maybe ECX back }
                   if popecx then
                     emit_reg(A_POP,S_L,r)
                   else
                     rg.ungetregisterint(exprasmlist,r);
                end;
           end;
      end;
{$endif}


{*****************************************************************************
                          TI386UNARYMINUSNODE
*****************************************************************************}

    function ti386unaryminusnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         if (left.resulttype.def.deftype=floatdef) then
           begin
             if (registersfpu < 1) then
               registersfpu := 1;
             expectloc:=LOC_FPUREGISTER;
           end
{$ifdef SUPPORT_MMX}
         else
           if (cs_mmx in aktlocalswitches) and
              is_mmx_able_array(left.resulttype.def) then
             begin
               registers32:=left.registers32;
               registersfpu:=left.registersfpu;
               registersmmx:=left.registersmmx;
               if (left.location.loc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
         else
           inherited pass_1;
      end;


{$ifdef SUPPORT_MMX}
    procedure ti386unaryminusnode.second_mmx;
      var
        r : Tregister;
        op : tasmop;
      begin
        secondpass(left);
        location_reset(location,LOC_MMXREGISTER,OS_NO);
        case left.location.loc of
          LOC_MMXREGISTER:
            begin
               location.register:=left.location.register;
               r.enum:=R_MM7;
               emit_reg_reg(A_PXOR,S_NO,r,r);
            end;
          LOC_CMMXREGISTER:
            begin
               location.register:=rg.getregistermm(exprasmlist);
               r.enum:=R_MM7;
               emit_reg_reg(A_PXOR,S_NO,r,r);
               emit_reg_reg(A_MOVQ,S_NO,left.location.register,location.register);
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE:
            begin
               reference_release(exprasmlist,left.location.reference);
               r.enum:=R_MM7;
               location.register:=rg.getregistermm(exprasmlist);
               emit_reg_reg(A_PXOR,S_NO,r,r);
               emit_ref_reg(A_MOVQ,S_NO,left.location.reference,location.register);
            end;
          else
            internalerror(200203225);
        end;
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
        r.enum:=R_MM7;
        emit_reg_reg(op,S_NO,location.register,r);
        emit_reg_reg(A_MOVQ,S_NO,r,location.register);
      end;
{$endif SUPPORT_MMX}


    procedure ti386unaryminusnode.second_float;
      var
        r : tregister;
      begin
        secondpass(left);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
        case left.location.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE:
            begin
              reference_release(exprasmlist,left.location.reference);
              location.register.enum:=R_ST;
              cg.a_loadfpu_ref_reg(exprasmlist,
                 def_cgsize(left.resulttype.def),
                 left.location.reference,location.register);
              emit_none(A_FCHS,S_NO);
            end;
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER:
            begin
               { "load st,st" is ignored by the code generator }
               r.enum:=R_ST;
               cg.a_loadfpu_reg_reg(exprasmlist,left.location.register,r);
               location.register.enum:=R_ST;
               emit_none(A_FCHS,S_NO);
            end;
        end;
      end;


{*****************************************************************************
                               TI386NOTNODE
*****************************************************************************}

    procedure ti386notnode.second_boolean;
      var
         hl : tasmlabel;
         opsize : topsize;
      begin
        opsize:=def_opsize(resulttype.def);

        if left.expectloc=LOC_JUMP then
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
         end
        else
         begin
           { the second pass could change the location of left }
           { if it is a register variable, so we've to do      }
           { this before the case statement                    }
           secondpass(left);
           case left.expectloc of
             LOC_FLAGS :
               begin
                 location_release(exprasmlist,left.location);
                 location_reset(location,LOC_FLAGS,OS_NO);
                 location.resflags:=left.location.resflags;
                 inverse_flags(location.resflags);
               end;
             LOC_CONSTANT,
             LOC_REGISTER,
             LOC_CREGISTER,
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 location_force_reg(exprasmlist,left.location,def_cgsize(resulttype.def),true);
                 emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
                 location_release(exprasmlist,left.location);
                 location_reset(location,LOC_FLAGS,OS_NO);
                 location.resflags:=F_E;
               end;
            else
               internalerror(200203224);
           end;
         end;
      end;


{$ifdef SUPPORT_MMX}
    procedure ti386notnode.second_mmx;
      var
         r,r2 : tregister;
      begin
        secondpass(left);
        location_reset(location,LOC_MMXREGISTER,OS_NO);
        { prepare EDI }
        r.enum:=R_INTREGISTER;
        r.number:=NR_EDI;
        r2.enum:=R_MM7;
        rg.getexplicitregisterint(exprasmlist,NR_EDI);
        emit_const_reg(A_MOV,S_L,longint($ffffffff),r);
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
        emit_reg_reg(A_MOVD,S_NO,r,r2);
        rg.ungetregisterint(exprasmlist,r);
        { lower 32 bit }
        emit_reg_reg(A_PXOR,S_D,r2,location.register);
        { shift mask }
        emit_const_reg(A_PSLLQ,S_NO,32,r2);
        { higher 32 bit }
        emit_reg_reg(A_PXOR,S_D,r2,location.register);
      end;
{$endif SUPPORT_MMX}

begin
   cmoddivnode:=ti386moddivnode;
   cshlshrnode:=ti386shlshrnode;
   cunaryminusnode:=ti386unaryminusnode;
   cnotnode:=ti386notnode;
end.
{
  $Log$
  Revision 1.58  2003-06-13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.57  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.56  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.55  2003/05/31 15:04:31  peter
    * load_loc_reg update

  Revision 1.54  2003/05/22 21:32:29  peter
    * removed some unit dependencies

  Revision 1.53  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.52  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.51  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.50  2003/04/21 19:15:26  peter
    * when ecx is not available allocated another register

  Revision 1.49  2003/04/17 10:02:48  daniel
    * Tweaked register allocate/deallocate positition to less interferences
      are generated.

  Revision 1.48  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.47  2003/03/08 20:36:41  daniel
    + Added newra version of Ti386shlshrnode
    + Added interference graph construction code

  Revision 1.46  2003/03/08 13:59:17  daniel
    * Work to handle new register notation in ag386nsm
    + Added newra version of Ti386moddivnode

  Revision 1.45  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.44  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.43  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.42  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.41  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.40  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.39  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.38  2002/08/14 19:18:16  carl
    * bugfix of unaryminus node with left LOC_CREGISTER

  Revision 1.37  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.36  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.35  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.34  2002/08/02 07:44:31  jonas
    * made assigned() handling generic
    * add nodes now can also evaluate constant expressions at compile time
      that contain nil nodes

  Revision 1.33  2002/07/20 11:58:02  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.32  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.31  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.30  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.28  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.27  2002/05/12 16:53:17  peter
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

  Revision 1.26  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.25  2002/04/02 17:11:36  peter
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

}
