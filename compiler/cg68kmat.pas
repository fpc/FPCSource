{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for math nodes

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
unit cg68kmat;
interface

    uses
      tree;

    procedure secondmoddiv(var p : ptree);
    procedure secondshlshr(var p : ptree);
    procedure secondunaryminus(var p : ptree);
    procedure secondnot(var p : ptree);


implementation

    uses
      globtype,systems,symconst,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cga68k,tgen68k;

{*****************************************************************************
                             SecondModDiv
*****************************************************************************}

    { D0 and D1 used as temp (ok)   }
    procedure secondmoddiv(var p : ptree);

      var
         hreg1 : tregister;
         power : longint;
         hl : pasmlabel;
         reg: tregister;
         pushed: boolean;
         hl1: pasmlabel;
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
                  emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hreg1);
                end
              else
                begin
                  del_reference(p^.left^.location.reference);
                  hreg1:=getregister32;
                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
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
              exprasmlist^.concat(new(paicpu, op_reg(A_TST, S_L, hreg1)));
              getlabel(hl);
              emitl(A_BPL,hl);
              if (power = 1) then
                 exprasmlist^.concat(new(paicpu, op_const_reg(A_ADDQ, S_L,1, hreg1)))
              else
               Begin
                 { optimize using ADDQ if possible!   }
                 if (p^.right^.value-1) < 9 then
                   exprasmlist^.concat(new(paicpu, op_const_reg(A_ADDQ, S_L,p^.right^.value-1, hreg1)))
                 else
                   exprasmlist^.concat(new(paicpu, op_const_reg(A_ADD, S_L,p^.right^.value-1, hreg1)));
               end;
              emitl(A_LABEL, hl);
              if (power > 0) and (power < 9) then
                 exprasmlist^.concat(new(paicpu, op_const_reg(A_ASR, S_L,power, hreg1)))
              else
               begin
                  exprasmlist^.concat(new(paicpu, op_const_reg(A_MOVE,S_L,power, R_D0)));
                  exprasmlist^.concat(new(paicpu, op_reg_reg(A_ASR,S_L,R_D0, hreg1)));
               end;
           end
         else
           begin
              { bring denominator to D1 }
              { D1 is always free, it's }
              { only used for temporary  }
              { purposes                 }
              if (p^.right^.location.loc<>LOC_REGISTER) and
                 (p^.right^.location.loc<>LOC_CREGISTER) then
                 begin
                   del_reference(p^.right^.location.reference);
                   p^.left^.location.loc:=LOC_REGISTER;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),R_D1)));
                end
             else
              begin
                   ungetregister32(p^.right^.location.register);
                   emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D1);
              end;

              { on entering this section D1 should contain the divisor }

              if (aktoptprocessor = MC68020) then
              begin
                 { Check if divisor is ZERO - if so call HALT_ERROR }
                 { with d0 = 200 (Division by zero!)                }
                 getlabel(hl1);
                 exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_L,R_D1)));
                 { if not zero then simply continue on }
                 emitl(A_BNE,hl1);
                 exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,200,R_D0)));
                 emitcall('FPC_HALT_ERROR',true);
                 emitl(A_LABEL,hl1);
                 if (p^.treetype = modn) then
                 Begin
                   reg := getregister32;
                   exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_L,reg)));
                   getlabel(hl);
                   { here what we do is prepare the high register with the     }
                   { correct sign. i.e we clear it, check if the low dword reg }
                   { which will participate in the division is signed, if so we}
                   { we extend the sign to the high doword register by inverting }
                   { all the bits.                                             }
                   exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_L,hreg1)));
                   emitl(A_BPL,hl);
                   exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,reg)));
                   emitl(A_LABEL,hl);
                   { reg:hreg1 / d1 }
                   exprasmlist^.concat(new(paicpu,op_reg_reg_reg(A_DIVSL,S_L,R_D1,reg,hreg1)));
                   { hreg1 already contains quotient }
                   { looking for remainder }
                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,reg,hreg1)));
                   ungetregister32(reg);
                 end
                 else
                 { simple division... }
                 Begin
                   { reg:hreg1 / d1 }
                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_DIVS,S_L,R_D1,hreg1)));
                 end;
              end
              else { MC68000 operations }
                 begin
                     { put numerator in d0 }
                     emit_reg_reg(A_MOVE,S_L,hreg1,R_D0);
                     { operation to perform on entry to both }
                     { routines...  d0/d1                    }
                     { return result in d0                   }
                     if p^.treetype = divn then
                       emitcall('FPC_LONGDIV',true)
                     else
                       emitcall('FPC_LONGMOD',true);
                     emit_reg_reg(A_MOVE,S_L,R_D0,hreg1);
              end; { endif }
         end;
         { this registers are always used when div/mod are present }
         usedinproc:=usedinproc or ($800 shr word(R_D1));
         usedinproc:=usedinproc or ($800 shr word(R_D0));
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hreg1;
      end;


{*****************************************************************************
                             SecondShlShr
*****************************************************************************}

    { D6 used as scratch (ok) }
    procedure secondshlshr(var p : ptree);

      var
         hregister1,hregister2,hregister3 : tregister;
         op : tasmop;
         pushed : boolean;
      begin

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
                   emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                     hregister1);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   hregister1:=getregister32;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                     hregister1)));
                end;
           end
         else hregister1:=p^.left^.location.register;

         { determine operator }
         if p^.treetype=shln then
           op:=A_LSL
         else
           op:=A_LSR;

         { shifting by a constant directly decode: }
         if (p^.right^.treetype=ordconstn) then
           begin
             if (p^.right^.location.reference.offset and 31 > 0) and (p^.right^.location.reference.offset and 31 < 9) then
                 exprasmlist^.concat(new(paicpu,op_const_reg(op,S_L,p^.right^.location.reference.offset and 31,
                   hregister1)))
             else
               begin
                 exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,p^.right^.location.reference.offset and 31,
                   R_D6)));
                 exprasmlist^.concat(new(paicpu,op_reg_reg(op,S_L,R_D6,hregister1)));
               end;
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
                        emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,
                          hregister2);
                     end
                   else
                     begin
                        del_reference(p^.right^.location.reference);
                        hregister2:=getregister32;
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                          hregister2)));
                     end;
                end
              else hregister2:=p^.right^.location.register;


              emit_reg_reg(op,S_L,hregister2,hregister1);
              p^.location.register:=hregister1;
           end;
         { this register is always used when shl/shr are present }
         usedinproc:=usedinproc or ($800 shr byte(R_D6));
      end;

{*****************************************************************************
                             Secondunaryminus
*****************************************************************************}

    procedure secondunaryminus(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         case p^.left^.location.loc of
            LOC_REGISTER : begin
                              p^.location.register:=p^.left^.location.register;
                              exprasmlist^.concat(new(paicpu,op_reg(A_NEG,S_L,p^.location.register)));
                           end;
            LOC_CREGISTER : begin
                               p^.location.register:=getregister32;
                               emit_reg_reg(A_MOVE,S_L,p^.location.register,
                                 p^.location.register);
                               exprasmlist^.concat(new(paicpu,op_reg(A_NEG,S_L,p^.location.register)));
                            end;
            LOC_REFERENCE,LOC_MEM :
                           begin
                              del_reference(p^.left^.location.reference);
                              { change sign of a floating point  }
                              { in the case of emulation, get    }
                              { a free register, and change sign }
                              { manually.                        }
                              { otherwise simply load into an FPU}
                              { register.                        }
                              if (p^.left^.resulttype^.deftype=floatdef) and
                                 (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                begin
                                   { move to FPU }
                                   floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                     p^.left^.location.reference,p^.location);
                                   if (cs_fp_emulation) in aktmoduleswitches then
                                       { if in emulation mode change sign manually }
                                       exprasmlist^.concat(new(paicpu,op_const_reg(A_BCHG,S_L,31,
                                          p^.location.fpureg)))
                                   else
                                       exprasmlist^.concat(new(paicpu,op_reg(A_FNEG,S_FX,
                                          p^.location.fpureg)));
                                end
                              else
                                begin
                                   p^.location.register:=getregister32;
                                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                     newreference(p^.left^.location.reference),
                                     p^.location.register)));
                                   exprasmlist^.concat(new(paicpu,op_reg(A_NEG,S_L,p^.location.register)));
                                end;
                           end;
            LOC_FPU : begin
                              p^.location.loc:=LOC_FPU;
                              p^.location.fpureg := p^.left^.location.fpureg;
                              if (cs_fp_emulation) in aktmoduleswitches then
                                  exprasmlist^.concat(new(paicpu,op_const_reg(A_BCHG,S_L,31,p^.location.fpureg)))
                              else
                                 exprasmlist^.concat(new(paicpu,op_reg(A_FNEG,S_FX,p^.location.fpureg)));
                           end;
         end;
{         emitoverflowcheck;}
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
         hl : pasmlabel;

      begin
         if (p^.resulttype^.deftype=orddef) and
            (porddef(p^.resulttype)^.typ=bool8bit) then
              begin
                 case p^.location.loc of
                    LOC_JUMP : begin
                                  hl:=truelabel;
                                  truelabel:=falselabel;
                                  falselabel:=hl;
                                  secondpass(p^.left);
                                  maketojumpbool(p^.left);
                                  hl:=truelabel;
                                  truelabel:=falselabel;
                                  falselabel:=hl;
                               end;
                    LOC_FLAGS : begin
                                   secondpass(p^.left);
                                   p^.location.resflags:=flagsinvers[p^.left^.location.resflags];
                                end;
                    LOC_REGISTER : begin
                                      secondpass(p^.left);
                                      p^.location.register:=p^.left^.location.register;
                                      exprasmlist^.concat(new(paicpu,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                   end;
                    LOC_CREGISTER : begin
                                       secondpass(p^.left);
                                       p^.location.loc:=LOC_REGISTER;
                                       p^.location.register:=getregister32;
                                       emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,
                                         p^.location.register);
                                       exprasmlist^.concat(new(paicpu,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                    end;
                    LOC_REFERENCE,LOC_MEM : begin
                                              secondpass(p^.left);
                                              del_reference(p^.left^.location.reference);
                                              p^.location.loc:=LOC_REGISTER;
                                              p^.location.register:=getregister32;
                                              if p^.left^.location.loc=LOC_CREGISTER then
                                                emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,
                                                   p^.location.register)
                                              else
                                                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,
                                              newreference(p^.left^.location.reference),
                                                p^.location.register)));
                                              exprasmlist^.concat(new(paicpu,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                           end;
                 end;
              end
            else
              begin
                secondpass(p^.left);
                p^.location.loc:=LOC_REGISTER;

                case p^.left^.location.loc of
                   LOC_REGISTER : begin
                                     p^.location.register:=p^.left^.location.register;
                                     exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                   LOC_CREGISTER : begin
                                     p^.location.register:=getregister32;
                                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                                       p^.location.register);
                                     exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.location.register)));
                                   end;
                   LOC_REFERENCE,LOC_MEM :
                                  begin
                                     del_reference(p^.left^.location.reference);
                                     p^.location.register:=getregister32;
                                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                       newreference(p^.left^.location.reference),
                                       p^.location.register)));
                                     exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                end;
                {if  p^.left^.location.loc=loc_register then
                  p^.location.register:=p^.left^.location.register
                else
                  begin
                     del_locref(p^.left^.location);
                     p^.location.register:=getregister32;
                     exprasmlist^.concat(new(paicpu,op_loc_reg(A_MOV,S_L,
                       p^.left^.location,
                       p^.location.register)));
                  end;
                exprasmlist^.concat(new(paicpu,op_reg(A_NOT,S_L,p^.location.register)));}

             end;
      end;

end.
{
  $Log$
  Revision 1.7  2000-01-07 01:14:22  peter
    * updated copyright to 2000

  Revision 1.6  1999/11/18 15:34:44  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.5  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.4  1998/12/11 00:03:05  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.3  1998/10/13 16:50:10  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.2  1998/09/14 10:44:01  peter
    * all internal RTL functions start with FPC_

  Revision 1.1  1998/09/01 09:07:09  peter
    * m68k fixes, splitted cg68k like cgi386

}
