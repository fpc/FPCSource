{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
unit cg386mat;
interface

    uses
      tree;

    procedure secondmoddiv(var p : ptree);
    procedure secondshlshr(var p : ptree);
    procedure secondumminus(var p : ptree);
    procedure secondnot(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
{$ifndef OLDASM}
      i386base,i386asm,
{$else}
      i386,
{$endif}
      cgai386,tgeni386;

{*****************************************************************************
                             SecondModDiv
*****************************************************************************}

    procedure secondmoddiv(var p : ptree);
      var
         hreg1 : tregister;
         pushed,popeax,popedx : boolean;
         power : longint;
         hl : plabel;

      begin
         secondpass(p^.left);
         set_location(p^.location,p^.left^.location);
         pushed:=maybe_push(p^.right^.registers32,p);
         secondpass(p^.right);
         if pushed then restore(p);

         { put numerator in register }
         if p^.left^.location.loc<>LOC_REGISTER then
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                  hreg1:=getregister32;
                  emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hreg1);
                end
              else
                begin
                  del_reference(p^.left^.location.reference);
                  hreg1:=getregister32;
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                    hreg1)));
                end;
              clear_location(p^.left^.location);
              p^.left^.location.loc:=LOC_REGISTER;
              p^.left^.location.register:=hreg1;
           end
         else hreg1:=p^.left^.location.register;

           if (p^.treetype=divn) and (p^.right^.treetype=ordconstn) and
               ispowerof2(p^.right^.value,power) then
             begin
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hreg1,hreg1)));
                 getlabel(hl);
                 emitjmp(C_NS,hl);
                 if power=1 then
                   exprasmlist^.concat(new(pai386,op_reg(A_INC,S_L,hreg1)))
                 else
                   exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,p^.right^.value-1,hreg1)));
                 emitlab(hl);
                 exprasmlist^.concat(new(pai386,op_const_reg(A_SAR,S_L,power,hreg1)));
             end
           else
             begin
                 { bring denominator to EDI }
                 { EDI is always free, it's }
                 { only used for temporary  }
                 { purposes                 }
                 if (p^.right^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_CREGISTER) then
                    begin
                       del_reference(p^.right^.location.reference);
                       p^.left^.location.loc:=LOC_REGISTER;
                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),R_EDI)));
                end
              else
                begin
                   emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
                   ungetregister32(p^.right^.location.register);
                end;
              popedx:=false;
              popeax:=false;
              if hreg1=R_EDX then
                begin
                       if not(R_EAX in unused) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
                        popeax:=true;
                     end;
                   emit_reg_reg(A_MOV,S_L,R_EDX,R_EAX);
                end
                 else
                begin
                   if not(R_EDX in unused) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));
                        popedx:=true;
                     end;
                   if hreg1<>R_EAX then
                     begin
                        if not(R_EAX in unused) then
                          begin
                             exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
                             popeax:=true;
                          end;
                        emit_reg_reg(A_MOV,S_L,hreg1,R_EAX);
                     end;
                end;
              { sign extension depends on the left type }
              if porddef(p^.left^.resulttype)^.typ=u32bit then
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EDX,R_EDX)))
              else
                 exprasmlist^.concat(new(pai386,op_none(A_CDQ,S_NO)));

              { division depends on the right type }
              if porddef(p^.right^.resulttype)^.typ=u32bit then
                exprasmlist^.concat(new(pai386,op_reg(A_DIV,S_L,R_EDI)))
              else
                exprasmlist^.concat(new(pai386,op_reg(A_IDIV,S_L,R_EDI)));
              if p^.treetype=divn then
                begin
                   { if result register is busy then copy }
                   if popeax then
                     begin
                        if hreg1=R_EAX then
                          internalerror(112);
                        emit_reg_reg(A_MOV,S_L,R_EAX,hreg1)
                     end
                   else
                     if hreg1<>R_EAX then
                       emit_reg_reg(A_MOV,S_L,R_EAX,hreg1);
                end
              else
                emit_reg_reg(A_MOV,S_L,R_EDX,hreg1);
              if popeax then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));
              if popedx then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
             end;
           { this registers are always used when div/mod are present }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));
         usedinproc:=usedinproc or ($80 shr byte(R_EDX));
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hreg1;
      end;


{*****************************************************************************
                             SecondShlShr
*****************************************************************************}

    procedure secondshlshr(var p : ptree);
      var
         hregister1,hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         pushed,popecx : boolean;
         op : tasmop;
         hr : preference;

      begin
         popecx:=false;

         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p);
         secondpass(p^.right);
         if pushed then
           restore(p);

         if is_64bitint(p^.left^.resulttype) then
           begin
              { load left operators in a register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregisterlow:=getregister32;
                        hregisterhigh:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                          hregisterlow);
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,
                          hregisterlow);
                     end
                   else
                     begin
                        del_reference(p^.left^.location.reference);
                        hregisterlow:=getregister32;
                        hregisterhigh:=getregister32;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                          hregisterlow)));
                        hr:=newreference(p^.left^.location.reference);
                        inc(hr^.offset,4);
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hr,
                          hregisterhigh)));
                     end;
                end
              else
                begin
                   hregisterlow:=p^.left^.location.registerlow;
                   hregisterhigh:=p^.left^.location.registerhigh;
                end;

              { shifting by a constant directly coded: }
              if (p^.right^.treetype=ordconstn) then
                begin
                   if p^.treetype=shln then
                     begin
                        exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_L,p^.right^.location.reference.offset and 31,
                          hregisterlow,hregisterhigh)));
                        exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,p^.right^.location.reference.offset and 31,
                          hregisterlow)));
                     end
                   else
                     begin
                        exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHRD,S_L,p^.right^.location.reference.offset and 31,
                          hregisterhigh,hregisterlow)));
                        exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_L,p^.right^.location.reference.offset and 31,
                          hregisterhigh)));
                     end;
                   p^.location.loc:=LOC_REGISTER;
                   p^.location.registerlow:=hregisterlow;
                   p^.location.registerhigh:=hregisterhigh;
                end
              else
                begin
                   { load right operators in a register }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                               hregister2);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             hregister2:=getexplicitregister32(R_ECX);
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                               hregister2)));
                          end;
                     end
                   else
                     hregister2:=p^.right^.location.register;

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
                        if not (R_ECX in unused) then
                         begin
                           popecx:=true;
                           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
                         end;
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;

                   ungetregister32(hregister2);

                   if p^.treetype=shln then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg_reg_reg(A_SHLD,S_L,R_CL,
                          hregisterlow,hregisterhigh)));
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_SHL,S_L,R_CL,
                          hregisterlow)));
                     end
                   else
                     begin
                        exprasmlist^.concat(new(pai386,op_reg_reg_reg(A_SHRD,S_L,R_CL,
                          hregisterhigh,hregisterlow)));
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_SHR,S_L,R_CL,
                          hregisterhigh)));
                     end;

                   { maybe put ECX back }
                   if popecx then
                     exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));

                   p^.location.registerlow:=hregisterlow;
                   p^.location.registerhigh:=hregisterhigh;
                end;
           end
         else
           begin
              { load left operators in a register }
              if p^.left^.location.loc<>LOC_REGISTER then
                begin
                   if p^.left^.location.loc=LOC_CREGISTER then
                     begin
                        hregister1:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                          hregister1);
                     end
                   else
                     begin
                        del_reference(p^.left^.location.reference);
                        hregister1:=getregister32;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                          hregister1)));
                     end;
                end
              else
                hregister1:=p^.left^.location.register;

              { determine operator }
              if p^.treetype=shln then
                op:=A_SHL
              else
                op:=A_SHR;

              { shifting by a constant directly coded: }
              if (p^.right^.treetype=ordconstn) then
                begin
                   exprasmlist^.concat(new(pai386,op_const_reg(op,S_L,p^.right^.location.reference.offset and 31,
                     hregister1)));
                   p^.location.loc:=LOC_REGISTER;
                   p^.location.register:=hregister1;
                end
              else
                begin
                   { load right operators in a register }
                   if p^.right^.location.loc<>LOC_REGISTER then
                     begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                          begin
                             hregister2:=getexplicitregister32(R_ECX);
                             emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                               hregister2);
                          end
                        else
                          begin
                             del_reference(p^.right^.location.reference);
                             hregister2:=getexplicitregister32(R_ECX);
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                               hregister2)));
                          end;
                     end
                   else
                     hregister2:=p^.right^.location.register;

                   { left operator is already in a register }
                   { hence are both in a register }
                   { is it in the case ECX ? }
                   if (hregister1=R_ECX) then
                     begin
                        { then only swap }
                        emit_reg_reg(A_XCHG,S_L,hregister1,hregister2);
                        hregister3:=hregister1;
                        hregister1:=hregister2;
                        hregister2:=hregister3;
                     end
                   { if second operator not in ECX ? }
                   else if (hregister2<>R_ECX) then
                     begin
                        { ECX occupied then push it }
                        if not (R_ECX in unused) then
                         begin
                           popecx:=true;
                           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
                         end;
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                     end;
                   ungetregister32(hregister2);
                   { right operand is in ECX }
                   emit_reg_reg(op,S_L,R_CL,hregister1);
                   { maybe ECX back }
                   if popecx then
                     exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
                   p^.location.register:=hregister1;
                end;
           end;
      end;


{*****************************************************************************
                             SecondUmMinus
*****************************************************************************}

    procedure secondumminus(var p : ptree);

{$ifdef SUPPORT_MMX}
      procedure do_mmx_neg;
        var
           op : tasmop;
        begin
           p^.location.loc:=LOC_MMXREGISTER;
           if cs_mmx_saturation in aktlocalswitches then
             case mmx_type(p^.resulttype) of
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
             case mmx_type(p^.resulttype) of
                mmxs8bit,mmxu8bit:
                  op:=A_PSUBB;
                mmxs16bit,mmxu16bit,mmxfixed16:
                  op:=A_PSUBW;
                mmxs32bit,mmxu32bit:
                  op:=A_PSUBD;
             end;
           emit_reg_reg(op,S_NO,p^.location.register,R_MM7);
           emit_reg_reg(A_MOVQ,S_NO,R_MM7,p^.location.register);
        end;
{$endif}
      var
         hr : preference;

      begin
         if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
              case p^.left^.location.loc of
                LOC_REGISTER :
                  begin
                     p^.location.registerlow:=p^.left^.location.registerlow;
                     p^.location.registerhigh:=p^.left^.location.registerhigh;
                  end;
                LOC_CREGISTER :
                  begin
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
                  end;
                LOC_REFERENCE,LOC_MEM :
                  begin
                     del_reference(p^.left^.location.reference);
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       newreference(p^.left^.location.reference),p^.location.registerlow)));
                     hr:=newreference(p^.left^.location.reference);
                     inc(hr^.offset,4);
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       hr,p^.location.registerhigh)));
                  end;
              end;
            exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.registerlow)));
            exprasmlist^.concat(new(pai386,op_const_reg(A_ADC,S_L,0,p^.location.registerhigh)));
            exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.registerhigh)));
           end
         else
           begin
              secondpass(p^.left);
              p^.location.loc:=LOC_REGISTER;
              case p^.left^.location.loc of
                 LOC_REGISTER:
                   begin
                      p^.location.register:=p^.left^.location.register;
                      exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
                   end;
                 LOC_CREGISTER:
                   begin
                      p^.location.register:=getregister32;
                      emit_reg_reg(A_MOV,S_L,p^.location.register,
                        p^.location.register);
                      exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
                   end;
{$ifdef SUPPORT_MMX}
                 LOC_MMXREGISTER:
                   begin
                      set_location(p^.location,p^.left^.location);
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      do_mmx_neg;
                   end;
                 LOC_CMMXREGISTER:
                   begin
                      p^.location.register:=getregistermmx;
                      emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                      emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                        p^.location.register);
                      do_mmx_neg;
                   end;
{$endif SUPPORT_MMX}
                 LOC_REFERENCE,LOC_MEM:
                                begin
                                   del_reference(p^.left^.location.reference);
                                   if (p^.left^.resulttype^.deftype=floatdef) and
                                      (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                     begin
                                        p^.location.loc:=LOC_FPU;
                                        floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                          p^.left^.location.reference);
                                        exprasmlist^.concat(new(pai386,op_none(A_FCHS,S_NO)));
                                     end
{$ifdef SUPPORT_MMX}
                                   else if (cs_mmx in aktlocalswitches) and is_mmx_able_array(p^.left^.resulttype) then
                                     begin
                                        p^.location.register:=getregistermmx;
                                        emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                                          newreference(p^.left^.location.reference),
                                          p^.location.register)));
                                        do_mmx_neg;
                                     end
{$endif SUPPORT_MMX}
                                   else
                                     begin
                                        p^.location.register:=getregister32;
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                          newreference(p^.left^.location.reference),
                                          p^.location.register)));
                                        exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
                                     end;
                                end;
                 LOC_FPU:
                   begin
                      p^.location.loc:=LOC_FPU;
                      exprasmlist^.concat(new(pai386,op_none(A_FCHS,S_NO)));
                   end;
              end;
           end;
{ Here was a problem...            }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!        }
{ So this is useless...            }
{         emitoverflowcheck(p);}
      end;


{*****************************************************************************
                               SecondNot
*****************************************************************************}

    procedure secondnot(var p : ptree);
      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_A,F_AE,F_B,F_BE);
      var
         hl : plabel;
         opsize : topsize;
         hr : preference;

      begin
         if is_boolean(p^.resulttype) then
          begin
            opsize:=def_opsize(p^.resulttype);
            case p^.left^.location.loc of
              LOC_JUMP :
                begin
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(p^.left);
                  maketojumpbool(p^.left);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  secondpass(p^.left);
                  p^.location.resflags:=flagsinvers[p^.left^.location.resflags];
                end;
              LOC_REGISTER :
                begin
                  secondpass(p^.left);
                  {p^.location.register:=p^.left^.location.register;
                  exprasmlist^.concat(new(pai386,op_const_reg(A_XOR,opsize,1,p^.location.register)));}
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_TEST,opsize,
                    p^.left^.location.register,p^.left^.location.register)));
                  ungetregister(p^.left^.location.register);
                end;
              LOC_CREGISTER :
                begin
                  secondpass(p^.left);
                  clear_location(p^.location);
                  p^.location.loc:=LOC_REGISTER;
                  p^.location.register:=def_getreg(p^.resulttype);
                  emit_reg_reg(A_MOV,opsize,p^.left^.location.register,p^.location.register);
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_TEST,opsize,p^.location.register,p^.location.register)));
                  ungetregister(p^.location.register);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                end;
              LOC_REFERENCE,
              LOC_MEM :
                begin
                  secondpass(p^.left);
                  clear_location(p^.location);
                  p^.location.loc:=LOC_REGISTER;
                  del_reference(p^.left^.location.reference);
                  { this was placed before del_ref => internaalerror(10) }
                  p^.location.register:=def_getreg(p^.resulttype);
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                    newreference(p^.left^.location.reference),p^.location.register)));
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_TEST,opsize,p^.location.register,p^.location.register)));
                  ungetregister(p^.location.register);
                  p^.location.loc:=LOC_FLAGS;
                  p^.location.resflags:=F_E;
                end;
            end;
          end
{$ifdef SUPPORT_MMX}
         else
          if (cs_mmx in aktlocalswitches) and is_mmx_able_array(p^.left^.resulttype) then
           begin
             secondpass(p^.left);
             p^.location.loc:=LOC_MMXREGISTER;
             { prepare EDI }
             exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,$ffffffff,R_EDI)));
             { load operand }
             case p^.left^.location.loc of
               LOC_MMXREGISTER:
                 set_location(p^.location,p^.left^.location);
               LOC_CMMXREGISTER:
                 begin
                   p^.location.register:=getregistermmx;
                   emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,p^.location.register);
                 end;
               LOC_REFERENCE,LOC_MEM:
                 begin
                   del_reference(p^.left^.location.reference);
                   p^.location.register:=getregistermmx;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                     newreference(p^.left^.location.reference),p^.location.register)));
                 end;
             end;
             { load mask }
             emit_reg_reg(A_MOV,S_D,R_EDI,R_MM7);
             { lower 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
             { shift mask }
             exprasmlist^.concat(new(pai386,op_const_reg(A_PSLLQ,S_NO,32,R_MM7)));
             { higher 32 bit }
             emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
           end
{$endif SUPPORT_MMX}
         else if is_64bitint(p^.left^.resulttype) then
           begin
              secondpass(p^.left);
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
              case p^.left^.location.loc of
                LOC_REGISTER :
                  begin
                     p^.location.registerlow:=p^.left^.location.registerlow;
                     p^.location.registerhigh:=p^.left^.location.registerhigh;
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerlow)));
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerhigh)));
                  end;
                LOC_CREGISTER :
                  begin
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,p^.location.registerlow);
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,p^.location.registerhigh);
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerlow)));
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerhigh)));
                  end;
                LOC_REFERENCE,LOC_MEM :
                  begin
                     del_reference(p^.left^.location.reference);
                     p^.location.registerlow:=getregister32;
                     p^.location.registerhigh:=getregister32;
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       newreference(p^.left^.location.reference),p^.location.registerlow)));
                     hr:=newreference(p^.left^.location.reference);
                     inc(hr^.offset,4);
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       hr,p^.location.registerhigh)));
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerlow)));
                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.registerhigh)));
                  end;
              end;
           end
         else
          begin
            secondpass(p^.left);
            clear_location(p^.location);
            p^.location.loc:=LOC_REGISTER;
            case p^.left^.location.loc of
              LOC_REGISTER :
                begin
                  p^.location.register:=p^.left^.location.register;
                  exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                end;
              LOC_CREGISTER :
                begin
                  p^.location.register:=getregister32;
                  emit_reg_reg(A_MOV,S_L,p^.left^.location.register,p^.location.register);
                  exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                end;
              LOC_REFERENCE,LOC_MEM :
                begin
                  del_reference(p^.left^.location.reference);
                  p^.location.register:=getregister32;
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                    newreference(p^.left^.location.reference),p^.location.register)));
                  exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                end;
            end;
          end;
      end;



end.
{
  $Log$
  Revision 1.22  1999-05-01 13:24:11  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.21  1999/04/16 13:42:27  jonas
    * more regalloc fixes (still not complete)

  Revision 1.20  1999/02/22 02:15:13  peter
    * updates for ag386bin

  Revision 1.19  1999/02/04 10:49:40  florian
    + range checking for ansi- and widestrings
    * made it compilable with TP

  Revision 1.18  1999/02/03 10:11:12  pierre
   * fix for bug0211 for i386

  Revision 1.17  1999/01/21 22:10:41  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.16  1999/01/19 10:51:32  pierre
   * fix to bug0183 in secondnot

  Revision 1.15  1998/12/11 16:50:22  florian
    + typed const int64 and qword
    + unary minus-operator  q1:=-q2;
    + not-operator

  Revision 1.14  1998/12/11 16:10:07  florian
    + shifting for 64 bit ints added
    * bug in getexplicitregister32 fixed: usableregs wasn't decremented !!

  Revision 1.13  1998/12/11 00:02:52  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.12  1998/11/26 21:45:29  jonas
    - removed A_CLTD opcode (use A_CDQ instead)
    * changed cbw, cwde and cwd to cbtw, cwtl and cwtd in att_op2str array
    * in daopt386: adapted AsmInstr array to reflect changes + fixed line too long

  Revision 1.11  1998/11/05 14:26:02  peter
    * fixed shlshr which would push ecx when not needed

  Revision 1.10  1998/10/20 13:12:38  peter
    * fixed 'not not boolean', the location was not set to register

  Revision 1.9  1998/10/20 08:06:42  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.8  1998/10/09 08:56:24  pierre
    * several memory leaks fixed

  Revision 1.7  1998/09/17 09:42:17  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.6  1998/09/09 14:37:37  florian
    * mod/div for cardinal type fixed

  Revision 1.5  1998/08/23 16:07:20  florian
    * internalerror with mod/div fixed

  Revision 1.4  1998/08/18 09:24:38  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.3  1998/06/05 17:44:12  peter
    * splitted cgi386

  Revision 1.2  1998/06/02 17:02:59  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.1  1998/06/01 16:50:18  peter
    + boolean -> ord conversion
    * fixed ord -> boolean conversion

}
