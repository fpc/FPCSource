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

   uses tree;

    procedure secondmoddiv(var p : ptree);
    procedure secondshlshr(var p : ptree);
    procedure secondumminus(var p : ptree);

implementation

   uses
     cobjects,verbose,globals,
     symtable,aasm,i386,
     types,cgi386,cgai386,tgeni386,hcodegen;

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
              p^.left^.location.loc:=LOC_REGISTER;
              p^.left^.location.register:=hreg1;
           end
         else hreg1:=p^.left^.location.register;

           if (p^.treetype=divn) and (p^.right^.treetype=ordconstn) and
               ispowerof2(p^.right^.value,power) then
             begin
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hreg1,hreg1)));
                 getlabel(hl);
                 emitl(A_JNS,hl);
                 if power=1 then
                    exprasmlist^.concat(new(pai386,op_reg(A_INC,S_L,hreg1)))
                 else exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,p^.right^.value-1,hreg1)));

                 emitl(A_LABEL,hl);
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
                   ungetregister32(p^.right^.location.register);
                   emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
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
              exprasmlist^.concat(new(pai386,op_none(A_CLTD,S_NO)));
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
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hreg1;
      end;

    procedure secondshlshr(var p : ptree);

      var
         hregister1,hregister2,hregister3 : tregister;
         pushed,popecx : boolean;
         op : tasmop;

      begin
         popecx:=false;

         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p);
         secondpass(p^.right);
         if pushed then restore(p);

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
           else hregister1:=p^.left^.location.register;

         { determine operator }
         if p^.treetype=shln then
           op:=A_SHL
         else
           op:=A_SHR;

         { shifting by a constant directly decode: }
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
                              hregister2:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                          hregister2);
                     end
                   else
                     begin
                        del_reference(p^.right^.location.reference);
                        hregister2:=getregister32;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                          hregister2)));
                     end;
                end
              else hregister2:=p^.right^.location.register;

                 { left operator is already in a register }
              { hence are both in a register }
              { is it in the case ECX ? }
              if (hregister1=R_ECX) then
                begin
                   { then only swap }
                   emit_reg_reg(A_XCHG,S_L,hregister1,
                     hregister2);

                   hregister3:=hregister1;
                   hregister1:=hregister2;
                   hregister2:=hregister3;
                end
              { if second operator not in ECX ? }
              else if (hregister2<>R_ECX) then
                begin
                   { ECX not occupied then swap with right register }
                   if R_ECX in unused then
                     begin
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                        ungetregister32(hregister2);
                          end
                       else
                     begin
                        { else save ECX and then copy it }
                        popecx:=true;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                        ungetregister32(hregister2);
                     end;
                end;
              { right operand is in ECX }
              emit_reg_reg(op,S_L,R_CL,hregister1);
              { maybe ECX back }
              if popecx then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
              p^.location.register:=hregister1;
             end;
         { this register is always used when shl/shr are present }
         usedinproc:=usedinproc or ($80 shr byte(R_ECX));
      end;

    procedure secondumminus(var p : ptree);

{$ifdef SUPPORT_MMX}
      procedure do_mmx_neg;

        var
           op : tasmop;

        begin
           p^.location.loc:=LOC_MMXREGISTER;
           if cs_mmx_saturation in aktswitches then
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
                 p^.location:=p^.left^.location;
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
                              else if (cs_mmx in aktswitches) and is_mmx_able_array(p^.left^.resulttype) then
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
{ Here was a problem...            }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!        }
{ So this is useless...            }
{         emitoverflowcheck(p);}
      end;

end.
{
  $Log$
  Revision 1.2  1998-06-02 17:02:59  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.1  1998/06/01 16:50:18  peter
    + boolean -> ord conversion
    * fixed ord -> boolean conversion

}
