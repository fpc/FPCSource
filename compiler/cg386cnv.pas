{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for type converting nodes

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
{$ifdef TP}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
unit cg386cnv;
interface

    uses
      tree;

    procedure secondtypeconv(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);


implementation

   uses
     cobjects,verbose,globals,
     symtable,aasm,i386,types,
     cgi386,cgai386,temp_gen,tgeni386,hcodegen;

{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

     procedure maybe_rangechecking(p : ptree;p2,p1 : pdef);
     {
       produces if necessary rangecheckcode
     }
       var
          hp : preference;
          hregister : tregister;
          neglabel,poslabel : plabel;
          is_register : boolean;
      begin
         { convert from p2 to p1 }
         { range check from enums is not made yet !!}
         { and its probably not easy }
         if (p1^.deftype<>orddef) or (p2^.deftype<>orddef) then
           exit;
         { range checking is different for u32bit }
         { lets try to generate it allways }
         if (cs_check_range in aktlocalswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(p^.explizit) or not(cs_tp_compatible in aktmoduleswitches)) and
           ((porddef(p1)^.low>porddef(p2)^.low) or
           (porddef(p1)^.high<porddef(p2)^.high) or
           (porddef(p1)^.typ=u32bit) or
           (porddef(p2)^.typ=u32bit)) then
           begin
              porddef(p1)^.genrangecheck;
              is_register:=(p^.left^.location.loc=LOC_REGISTER) or
                (p^.left^.location.loc=LOC_CREGISTER);
              if porddef(p2)^.typ=u8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              { rangechecking for u32bit ?? !!!!!!}
              { lets try }
              else if (porddef(p2)^.typ=s32bit) or (porddef(p2)^.typ=u32bit)  then
                begin
                   if is_register then
                     hregister:=p^.location.register
                   else
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),R_EDI)));
                        hregister:=R_EDI;
                     end;
                end
              else if porddef(p2)^.typ=u16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else internalerror(6);
              hp:=new_reference(R_NO,0);
              hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr));
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   getlabel(neglabel);
                   getlabel(poslabel);
                   exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hregister,hregister)));
                   emitl(A_JL,neglabel);
                end;
              exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hp)));
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   hp:=new_reference(R_NO,0);
                   hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr+1));
                   emitl(A_JMP,poslabel);
                   emitl(A_LABEL,neglabel);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hp)));
                   emitl(A_LABEL,poslabel);
                end;

           end;
      end;

     type
        tsecondconvproc = procedure(p,hp : ptree;convtyp : tconverttype);

    procedure second_only_rangecheck(p,hp : ptree;convtyp : tconverttype);

      begin
         maybe_rangechecking(p,hp^.resulttype,p^.resulttype);
      end;


    procedure second_smaller(p,hp : ptree;convtyp : tconverttype);

      var
         hregister,destregister : tregister;
         ref : boolean;
         hpp : preference;

      begin
         ref:=false;
         { problems with enums !! }
         if (cs_check_range in aktlocalswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(p^.explizit) or not(cs_tp_compatible in aktmoduleswitches)) and
           (p^.resulttype^.deftype=orddef) and
           (hp^.resulttype^.deftype=orddef) then
           begin
              if porddef(hp^.resulttype)^.typ=u32bit then
                begin
                   { when doing range checking for u32bit, we have some trouble }
                   { because BOUND assumes signed values                        }
                   { first, we check if the values is greater than 2^31:        }
                   { the u32bit rangenr contains the appropriate rangenr        }
                   porddef(hp^.resulttype)^.genrangecheck;
                   hregister:=R_EDI;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       newreference(p^.location.reference),R_EDI)));
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(hp^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));

                   { then we do a normal range check }
                   porddef(p^.resulttype)^.genrangecheck;
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                end
              else
                if ((porddef(p^.resulttype)^.low>porddef(hp^.resulttype)^.low) or
                (porddef(p^.resulttype)^.high<porddef(hp^.resulttype)^.high)) then
                begin
                   porddef(p^.resulttype)^.genrangecheck;
                   { per default the var is copied to EDI }
                   hregister:=R_EDI;
                   if porddef(hp^.resulttype)^.typ=s32bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          hregister:=p^.location.register
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),R_EDI)));
                     end
                   else if porddef(hp^.resulttype)^.typ=u16bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.location.reference),R_EDI)));
                     end
                   else if porddef(hp^.resulttype)^.typ=s16bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.location.reference),R_EDI)));
                     end
                   else internalerror(6);
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                   (*
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                        destregister:=p^.left^.location.register;
                        case convtyp of
                           tc_s32bit_2_s8bit,
                           tc_s32bit_2_u8bit:
                             destregister:=reg32toreg8(destregister);
                           tc_s32bit_2_s16bit,
                           tc_s32bit_2_u16bit:
                             destregister:=reg32toreg16(destregister);
                           { this was false because destregister is allways a 32bitreg }
                           tc_s16bit_2_s8bit,
                           tc_s16bit_2_u8bit,
                           tc_u16bit_2_s8bit,
                           tc_u16bit_2_u8bit:
                             destregister:=reg32toreg8(destregister);
                        end;
                   p^.location.register:=destregister;
                   exit;
                   *)
                end;
           end;
         { p^.location.loc is already set! }
         if (p^.location.loc=LOC_REGISTER) or
           (p^.location.loc=LOC_CREGISTER) then
           begin
              destregister:=p^.left^.location.register;
              case convtyp of
                 tc_s32bit_2_s8bit,
                 tc_s32bit_2_u8bit:
                   destregister:=reg32toreg8(destregister);
                 tc_s32bit_2_s16bit,
                 tc_s32bit_2_u16bit:
                   destregister:=reg32toreg16(destregister);
                 tc_s16bit_2_s8bit,
                 tc_s16bit_2_u8bit,
                 tc_u16bit_2_s8bit,
                 tc_u16bit_2_u8bit:
                   destregister:=reg16toreg8(destregister);
              end;
              p^.location.register:=destregister;
           end;
      end;

    procedure second_bigger(p,hp : ptree;convtyp : tconverttype);

      var
         hregister : tregister;
         opsize : topsize;
         op : tasmop;
         is_register : boolean;

      begin
           is_register:=p^.left^.location.loc=LOC_REGISTER;
           if not(is_register) and (p^.left^.location.loc<>LOC_CREGISTER) then
             begin
                del_reference(p^.left^.location.reference);
                { we can do this here as we need no temp inside second_bigger }
                ungetiftemp(p^.left^.location.reference);
             end;
         { this is wrong !!!
         gives me movl (%eax),%eax
         for the length(string !!!
         use only for constant values }
           {Constant cannot be loaded into registers using MOVZX!}
           if (p^.left^.location.loc<>LOC_MEM) or (not p^.left^.location.reference.isintvalue) then
                case convtyp of
                    tc_u8bit_2_s32bit,tc_u8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_BL;
                      end;
                    { here what do we do for negative values ? }
                    tc_s8bit_2_s32bit,tc_s8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_BL;
                      end;
                    tc_u16bit_2_s32bit,tc_u16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_WL;
                      end;
                    tc_s16bit_2_s32bit,tc_s16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_WL;
                      end;
                    tc_s8bit_2_u16bit,
                    tc_u8bit_2_s16bit,
                    tc_u8bit_2_u16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(p^.left^.location.register)
                          else hregister:=reg32toreg16(getregister32);
                          op:=A_MOVZX;
                          opsize:=S_BW;
                      end;
                    tc_s8bit_2_s16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(p^.left^.location.register)
                          else hregister:=reg32toreg16(getregister32);
                          op:=A_MOVSX;
                          opsize:=S_BW;
                      end;
                end
           else
                case convtyp of
                    tc_u8bit_2_s32bit,
                    tc_s8bit_2_s32bit,
                    tc_u16bit_2_s32bit,
                    tc_s16bit_2_s32bit,
                    tc_u8bit_2_u32bit,
                    tc_s8bit_2_u32bit,
                    tc_u16bit_2_u32bit,
                    tc_s16bit_2_u32bit:
                      begin
                         hregister:=getregister32;
                         op:=A_MOV;
                         opsize:=S_L;
                      end;
                    tc_s8bit_2_u16bit,
                    tc_s8bit_2_s16bit,
                    tc_u8bit_2_s16bit,
                    tc_u8bit_2_u16bit:
                      begin
                         hregister:=reg32toreg16(getregister32);
                         op:=A_MOV;
                         opsize:=S_W;
                     end;
                end;
           if is_register then
             begin
                 emit_reg_reg(op,opsize,p^.left^.location.register,hregister);
             end
           else
             begin
                 if p^.left^.location.loc=LOC_CREGISTER then
                    emit_reg_reg(op,opsize,p^.left^.location.register,hregister)
                 else exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                    newreference(p^.left^.location.reference),hregister)));
             end;
           p^.location.loc:=LOC_REGISTER;
           p^.location.register:=hregister;
           maybe_rangechecking(p,p^.left^.resulttype,p^.resulttype);
       end;

    procedure second_string_string(p,hp : ptree;convtyp : tconverttype);

      var
         pushed : tpushed;

      begin
{$ifdef UseAnsiString}
         { does anybody know a better solution than this big case statement ? }
         { ok, a proc table would do the job                                  }
         case pstringdef(p)^.string_typ of

            st_shortstring:
              case pstringdef(p^.left)^.string_typ of
                 st_shortstring:
                   begin
                      stringdispose(p^.location.reference.symbol);
                      gettempofsizereference(p^.resulttype^.size,p^.location.reference);
                      del_reference(p^.left^.location.reference);
                      copystring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
                      ungetiftemp(p^.left^.location.reference);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_longstring:
              case pstringdef(p^.left)^.string_typ of
                 st_shortstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_ansistring:
              case pstringdef(p^.left)^.string_typ of
                 st_shortstring:
                   begin
                      pushusedregisters(pushed,$ff);
                      push_int(p^.resulttype^.size-1);
                      gettempofsizereference(p^.resulttype^.size,p^.location.reference);
                      emitpushreferenceaddr(exprasmlist,p^.location.reference);
                      case p^.right^.location.loc of
                         LOC_REGISTER,LOC_CREGISTER:
                           begin
                              exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.right^.location.register)));
                              ungetregister32(p^.left^.location.register);
                           end;
                         LOC_REFERENCE,LOC_MEM:
                           begin
                              emit_push_mem(p^.left^.location.reference);
                              del_reference(p^.left^.location.reference);
                           end;
                      end;
                      emitcall('FPC_ANSI_TO_SHORTSTRING',true);
                      maybe_loadesi;
                      popusedregisters(pushed);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_widestring:
              case pstringdef(p^.left)^.string_typ of
                 st_shortstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;
         end;
{$ifdef dummy}
         if is_ansistring(p^.resulttype) and not is_ansistring(p^.left^.resulttype) then
           begin
              { call shortstring to ansistring conversion }
              { result is in register }
              del_reference(p^.left^.location.reference);
              {!!!!
              copyshortstringtoansistring(p^.location,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              }
              ungetiftemp(p^.left^.location.reference);
           end
         else if not is_ansistring(p^.resulttype) and is_ansistring(p^.left^.resulttype) then
           begin
              { call ansistring to shortstring conversion }
              { result is in mem }
              stringdispose(p^.location.reference.symbol);
              gettempofsizereference(p^.resulttype^.size,p^.location.reference);
              if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                del_reference(p^.left^.location.reference);
              copyansistringtoshortstring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              ungetiftemp(p^.left^.location.reference);
           end
         else
{$endif dummy}
{$else UseAnsiString}
           begin
              stringdispose(p^.location.reference.symbol);
              gettempofsizereference(p^.resulttype^.size,p^.location.reference);
              del_reference(p^.left^.location.reference);
              copystring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              ungetiftemp(p^.left^.location.reference);
           end;
{$endif UseAnsiString}
      end;

    procedure second_cstring_charpointer(p,hp : ptree;convtyp : tconverttype);

      begin
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         inc(p^.left^.location.reference.offset);
           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
             p^.location.register)));
      end;

    procedure second_string_chararray(p,hp : ptree;convtyp : tconverttype);

      begin
         inc(p^.location.reference.offset);
      end;

    procedure second_array_to_pointer(p,hp : ptree;convtyp : tconverttype);

      begin
         del_reference(p^.left^.location.reference);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           p^.location.register)));
      end;

    procedure second_pointer_to_array(p,hp : ptree;convtyp : tconverttype);

      begin
         p^.location.loc:=LOC_REFERENCE;
         clear_reference(p^.location.reference);
         if p^.left^.location.loc=LOC_REGISTER then
           p^.location.reference.base:=p^.left^.location.register
         else
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                   p^.location.reference.base:=getregister32;
                   emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                     p^.location.reference.base);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getregister32;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                     p^.location.reference.base)));
                end;
           end;
      end;

    { generates the code for the type conversion from an array of char }
    { to a string                                                        }
    procedure second_chararray_to_string(p,hp : ptree;convtyp : tconverttype);

      var
         l : longint;

      begin
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                             }
         p^.location.loc:=LOC_MEM;

         { first get the memory for the string }
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);

         { calc the length of the array }
         l:=parraydef(p^.left^.resulttype)^.highrange-
           parraydef(p^.left^.resulttype)^.lowrange+1;

         if l>255 then
           Message(sym_e_type_mismatch);

         { write the length }
             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,l,
               newreference(p^.location.reference))));

         { copy to first char of string }
         inc(p^.location.reference.offset);

         { generates the copy code      }
         { and we need the source never }
         concatcopy(p^.left^.location.reference,p^.location.reference,l,true);

         { correct the string location }
         dec(p^.location.reference.offset);
      end;

    procedure second_char_to_string(p,hp : ptree;convtyp : tconverttype);

      begin
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);
      { call loadstring with correct left and right }
         p^.right:=p^.left;
         p^.left:=p;
         loadstring(p);
         p^.left:=nil; { reset left tree, which is empty }
      end;

    procedure second_int_real(p,hp : ptree;convtyp : tconverttype);

      var
         r : preference;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              case porddef(p^.left^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,p^.left^.location.register,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,p^.left^.location.register,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.left^.location.register,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.left^.location.register,R_EDI)));
                 u32bit,s32bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EDI)));
                 {!!!! u32bit }
              end;
              ungetregister(p^.left^.location.register);
           end
         else
           begin
              r:=newreference(p^.left^.location.reference);
              case porddef(p^.left^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,r,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,r,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,r,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,r,R_EDI)));
                 u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                 {!!!! u32bit }
              end;
              del_reference(p^.left^.location.reference);
              ungetiftemp(p^.left^.location.reference);
         end;
          if porddef(p^.left^.resulttype)^.typ=u32bit then
            push_int(0);
          exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
          r:=new_reference(R_ESP,0);
         { for u32bit a solution is to push $0 and to load a
         comp }
          if porddef(p^.left^.resulttype)^.typ=u32bit then
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IQ,r)))
          else
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IL,r)));

         { better than an add on all processors }
         if porddef(p^.left^.resulttype)^.typ=u32bit then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,8,R_ESP)))
         else
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         p^.location.loc:=LOC_FPU;
      end;

    procedure second_real_fix(p,hp : ptree;convtyp : tconverttype);

      var
         {hs : string;}
         rreg : tregister;
         ref : treference;

      begin
         { real must be on fpu stack }
         if (p^.left^.location.loc<>LOC_FPU) then
           exprasmlist^.concat(new(pai386,op_ref(A_FLD,S_FL,newreference(p^.left^.location.reference))));
         push_int($1f3f);
         push_int(65536);
         reset_reference(ref);
         ref.base:=R_ESP;

         exprasmlist^.concat(new(pai386,op_ref(A_FIMUL,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FSTCW,S_L,newreference(ref))));

         ref.offset:=6;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_L,newreference(ref))));

         ref.offset:=0;
         exprasmlist^.concat(new(pai386,op_ref(A_FISTP,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_L,newreference(ref))));

         rreg:=getregister32;
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,rreg)));
         { better than an add on all processors }
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=rreg;
      end;

    procedure second_float_float(p,hp : ptree;convtyp : tconverttype);

      begin
         case p^.left^.location.loc of
            LOC_FPU : ;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 floatload(pfloatdef(p^.left^.resulttype)^.typ,
                   p^.left^.location.reference);
                 { we have to free the reference }
                 del_reference(p^.left^.location.reference);
              end;
         end;
         p^.location.loc:=LOC_FPU;
      end;

    procedure second_fix_real(p,hp : ptree;convtyp : tconverttype);

    var popeax,popebx,popecx,popedx : boolean;
        startreg : tregister;
        hl : plabel;
        r : treference;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=p^.left^.location.register;
              ungetregister(startreg);
              popeax:=(startreg<>R_EAX) and not (R_EAX in unused);
              if popeax then
                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
              { mov eax,eax is removed by emit_reg_reg }
              emit_reg_reg(A_MOV,S_L,startreg,R_EAX);
           end
         else
           begin
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(
                p^.left^.location.reference),R_EAX)));
              del_reference(p^.left^.location.reference);
              startreg:=R_NO;
           end;

         popebx:=(startreg<>R_EBX) and not (R_EBX in unused);
         if popebx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));

         popecx:=(startreg<>R_ECX) and not (R_ECX in unused);
         if popecx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));

         popedx:=(startreg<>R_EDX) and not (R_EDX in unused);
         if popedx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));

         exprasmlist^.concat(new(pai386,op_none(A_CDQ,S_NO)));
         emit_reg_reg(A_XOR,S_L,R_EDX,R_EAX);
         emit_reg_reg(A_MOV,S_L,R_EAX,R_EBX);
         emit_reg_reg(A_SUB,S_L,R_EDX,R_EAX);
         getlabel(hl);
         emitl(A_JZ,hl);
         exprasmlist^.concat(new(pai386,op_const_reg(A_RCL,S_L,1,R_EBX)));
         emit_reg_reg(A_BSR,S_L,R_EAX,R_EDX);
         exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,32,R_CL)));
         emit_reg_reg(A_SUB,S_B,R_DL,R_CL);
         emit_reg_reg(A_SHL,S_L,R_CL,R_EAX);
         exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_W,1007,R_DX)));
         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_W,5,R_DX)));
         exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_W,11,R_DX,R_BX)));
         exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_W,20,R_EAX,R_EBX)));

         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,20,R_EAX)));
         emitl(A_LABEL,hl);
         { better than an add on all processors }
         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));

         reset_reference(r);
         r.base:=R_ESP;
         exprasmlist^.concat(new(pai386,op_ref(A_FLD,S_FL,newreference(r))));
         exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,8,R_ESP)));
         if popedx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
         if popecx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
         if popebx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
         if popeax then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));

         p^.location.loc:=LOC_FPU;
      end;

    procedure second_int_fix(p,hp : ptree;convtyp : tconverttype);

      var
         {hs : string;}
         hregister : tregister;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) then
           hregister:=p^.left^.location.register
         else if (p^.left^.location.loc=LOC_CREGISTER) then
           hregister:=getregister32
         else
           begin
              del_reference(p^.left^.location.reference);
              hregister:=getregister32;
              case porddef(p^.left^.resulttype)^.typ of
                s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(p^.left^.location.reference),
                  hregister)));
                u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.left^.location.reference),
                  hregister)));
                s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.left^.location.reference),
                  hregister)));
                u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.left^.location.reference),
                  hregister)));
                u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                  hregister)));
                {!!!! u32bit }
              end;
           end;
         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,16,hregister)));

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
      end;


     procedure second_proc_to_procvar(p,hp : ptree;convtyp : tconverttype);

     begin
          p^.location.loc:=LOC_REGISTER;
          del_reference(hp^.location.reference);
          p^.location.register:=getregister32;
          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
           newreference(hp^.location.reference),p^.location.register)));
     end;

     procedure second_bool_to_int(p,hp : ptree;convtyp : tconverttype);

      var
         oldtruelabel,oldfalselabel,hlabel : plabel;
         hregister : tregister;
         newsize,
         opsize : topsize;
         op     : tasmop;
     begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(hp);
         p^.location.loc:=LOC_REGISTER;
         del_reference(hp^.location.reference);
         hregister:=getregister32;
         case porddef(hp^.resulttype)^.typ of
          bool8bit : begin
                       case porddef(p^.resulttype)^.typ of
                     u8bit,s8bit,
                        bool8bit : opsize:=S_B;
                   u16bit,s16bit,
                       bool16bit : opsize:=S_BW;
                   u32bit,s32bit,
                       bool32bit : opsize:=S_BL;
                       end;
                     end;
         bool16bit : begin
                       case porddef(p^.resulttype)^.typ of
                     u8bit,s8bit,
                        bool8bit : opsize:=S_B;
                   u16bit,s16bit,
                       bool16bit : opsize:=S_W;
                   u32bit,s32bit,
                       bool32bit : opsize:=S_WL;
                       end;
                     end;
         bool32bit : begin
                       case porddef(p^.resulttype)^.typ of
                     u8bit,s8bit,
                        bool8bit : opsize:=S_B;
                   u16bit,s16bit,
                       bool16bit : opsize:=S_W;
                   u32bit,s32bit,
                       bool32bit : opsize:=S_L;
                       end;
                     end;
         end;
         if opsize in [S_B,S_W,S_L] then
          op:=A_MOV
         else
          if (porddef(p^.resulttype)^.typ in [s8bit,s16bit,s32bit]) then
           op:=A_MOVSX
          else
           op:=A_MOVZX;
         case porddef(p^.resulttype)^.typ of
          bool8bit,u8bit,s8bit : begin
                                   p^.location.register:=reg32toreg8(hregister);
                                   newsize:=S_B;
                                 end;
       bool16bit,u16bit,s16bit : begin
                                   p^.location.register:=reg32toreg16(hregister);
                                   newsize:=S_W;
                                 end;
       bool32bit,u32bit,s32bit : begin
                                   p^.location.register:=hregister;
                                   newsize:=S_L;
                                 end;
         else
          internalerror(10060);
         end;

         case hp^.location.loc of
            LOC_MEM,
      LOC_REFERENCE : exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                        newreference(hp^.location.reference),p^.location.register)));
       LOC_REGISTER,
      LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,
                        hp^.location.register,p^.location.register)));
          LOC_FLAGS : begin
                        hregister:=reg32toreg8(hregister);
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[hp^.location.resflags],S_B,hregister)));
                        case porddef(p^.resulttype)^.typ of
                  bool16bit,
              u16bit,s16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,hregister,p^.location.register)));
                  bool32bit,
              u32bit,s32bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,p^.location.register)));
                        end;
                      end;
           LOC_JUMP : begin
                        getlabel(hlabel);
                        emitl(A_LABEL,truelabel);
                        exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,newsize,1,hregister)));
                        emitl(A_JMP,hlabel);
                        emitl(A_LABEL,falselabel);
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,newsize,hregister,hregister)));
                        emitl(A_LABEL,hlabel);
                      end;
         else
           internalerror(10061);
         end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
     end;


     procedure second_int_to_bool(p,hp : ptree;convtyp : tconverttype);
     var
        hregister : tregister;
     begin
         p^.location.loc:=LOC_REGISTER;
         del_reference(hp^.location.reference);
         case hp^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                hregister:=getregister32;
                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                  newreference(hp^.location.reference),hregister)));
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=hp^.location.register;
              end;
          else
            internalerror(10062);
          end;
         exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hregister,hregister)));
         hregister:=reg32toreg8(hregister);
         exprasmlist^.concat(new(pai386,op_reg(flag_2_set[hp^.location.resflags],S_B,hregister)));
         case porddef(p^.resulttype)^.typ of
           bool8bit : p^.location.register:=hregister;
          bool16bit : begin
                        p^.location.register:=reg8toreg16(hregister);
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,hregister,p^.location.register)));
                      end;
          bool32bit : begin
                        p^.location.register:=reg16toreg32(hregister);
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,p^.location.register)));
                      end;
         else
          internalerror(10064);
         end;
     end;

    procedure second_nothing(p,hp : ptree;convtyp : tconverttype);
      begin
      end;


{****************************************************************************
                             SecondTypeConv
****************************************************************************}

    procedure secondtypeconv(var p : ptree);

      const
         secondconvert : array[tc_u8bit_2_s32bit..tc_cchar_charpointer] of
           tsecondconvproc = (second_bigger,second_only_rangecheck,
           second_bigger,second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_string_string,
           second_cstring_charpointer,second_string_chararray,
           second_array_to_pointer,second_pointer_to_array,
           second_char_to_string,second_bigger,
           second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_smaller,
           second_bigger,second_smaller,
           second_only_rangecheck,second_bigger,
           second_bigger,second_bigger,
           second_bigger,second_only_rangecheck,
           second_smaller,second_smaller,
           second_smaller,second_smaller,
           second_bool_to_int,second_int_to_bool,
           second_int_real,second_real_fix,
           second_fix_real,second_int_fix,second_float_float,
           second_chararray_to_string,
           second_proc_to_procvar,
           { is constant char to pchar, is done by firstpass }
           second_nothing);

      begin
         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                        }

         { this is necessary, because second_bool_byte, have to change   }
         { true- and false label before calling secondpass               }
         if p^.convtyp<>tc_bool_2_int then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
           end;
         if (p^.convtyp<>tc_equal) and (p^.convtyp<>tc_not_possible) then
           {the second argument only is for maybe_range_checking !}
           secondconvert[p^.convtyp](p,p^.left,p^.convtyp)
      end;


{*****************************************************************************
                             SecondIs
*****************************************************************************}

    procedure secondis(var p : ptree);
      var
         pushed : tpushed;

      begin
         { save all used registers }
         pushusedregisters(pushed,$ff);
         secondpass(p^.left);
         p^.location.loc:=LOC_FLAGS;
         p^.location.resflags:=F_NE;

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,
                   S_L,p^.left^.location.register)));
                 ungetregister32(p^.left^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,
                   S_L,newreference(p^.left^.location.reference))));
                 del_reference(p^.left^.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,
                   S_L,p^.right^.location.register)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,
                   S_L,newreference(p^.right^.location.reference))));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('DO_IS',true);
         exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_B,R_AL,R_AL)));
         popusedregisters(pushed);
      end;


{*****************************************************************************
                             SecondAs
*****************************************************************************}

    procedure secondas(var p : ptree);
      var
         pushed : tpushed;
      begin
         secondpass(p^.left);
         { save all used registers }
         pushusedregisters(pushed,$ff);

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              exprasmlist^.concat(new(pai386,op_reg(A_PUSH,
                S_L,p^.left^.location.register)));
            LOC_MEM,LOC_REFERENCE:
              exprasmlist^.concat(new(pai386,op_ref(A_PUSH,
                S_L,newreference(p^.left^.location.reference))));
            else internalerror(100);
         end;

         { we doesn't modifiy the left side, we check only the type }
         set_location(p^.location,p^.left^.location);

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,
                   S_L,p^.right^.location.register)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,
                   S_L,newreference(p^.right^.location.reference))));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('DO_AS',true);
         { restore register, this restores automatically the }
         { result                                            }
         popusedregisters(pushed);
      end;


end.
{
  $Log$
  Revision 1.10  1998-08-10 14:49:47  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.9  1998/08/05 16:00:09  florian
    * some fixes for ansi strings
    * $log$ to $Log$
    * $log$ to Revision 1.10  1998-08-10 14:49:47  peter
    * $log$ to   + localswitches, moduleswitches, globalswitches splitting
    * $log$ to changed

  Revision 1.8  1998/07/18 22:54:24  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.7  1998/06/12 13:10:34  peter
    * small internalerror nr change

  Revision 1.6  1998/06/12 10:43:12  michael
  Fixed ansistrings : is_ansistring not found

  Revision 1.5  1998/06/08 13:13:30  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.4  1998/06/05 17:44:10  peter
    * splitted cgi386

  Revision 1.3  1998/06/03 22:48:50  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.2  1998/06/02 10:52:10  peter
    * fixed second_bool_to_int with bool8bit return

  Revision 1.1  1998/06/01 16:50:18  peter
    + boolean -> ord conversion
    * fixed ord -> boolean conversion

}
