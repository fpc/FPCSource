{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for type converting nodes

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
  {$E+,F+,N+}
{$endif}
unit cg68kcnv;
interface

    uses
      tree;

    procedure secondtypeconv(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);


implementation

   uses
     globtype,systems,symconst,
     cobjects,verbose,globals,
     symtable,aasm,types,
     hcodegen,temp_gen,pass_2,
     cpubase,cga68k,tgen68k;

{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure maybe_rangechecking(p : ptree;p2,p1 : pdef);

      var
         hp : preference;
       hregister : tregister;
       neglabel,poslabel : pasmlabel;

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
             (not(p^.explizit) {or not(cs_tp_compatible in aktmoduleswitches)}) and
             ((porddef(p1)^.low>porddef(p2)^.low) or
             (porddef(p1)^.high<porddef(p2)^.high) or
             (porddef(p1)^.typ=u32bit) or
             (porddef(p2)^.typ=u32bit)) then
           begin
              porddef(p1)^.genrangecheck;
              if porddef(p2)^.typ=u8bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_B,p^.location.register,R_D6)));
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$FF,R_D6)));
                     end
                   else
                     begin
                         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),R_D6)));
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$FF,R_D6)));
                     end;
                   hregister:=R_D6;
                end
              else if porddef(p2)^.typ=s8bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_B,p^.location.register,R_D6)));
                         { byte to long }
                         if aktoptprocessor = MC68020 then
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXTB,S_L,R_D6)))
                         else
                           begin
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_W,R_D6)));
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,R_D6)));
                           end;
                     end
                   else
                     begin
                         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),R_D6)));
                         { byte to long }
                         if aktoptprocessor = MC68020 then
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXTB,S_L,R_D6)))
                         else
                           begin
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_W,R_D6)));
                             exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,R_D6)));
                           end;
                     end; { end outermost else }
                   hregister:=R_D6;
                end
               { rangechecking for u32bit ?? !!!!!!}
               { lets try }
               else if (porddef(p2)^.typ=s32bit) or (porddef(p2)^.typ=u32bit)  then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     begin
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),R_D6)));
                        hregister:=R_D6;
                     end;
                end
              { rangechecking for u32bit ?? !!!!!!}
              else if porddef(p2)^.typ=u16bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,p^.location.register,R_D6)))
                   else
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),R_D6)));
                   { unisgned extend }
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$FFFF,R_D6)));
                   hregister:=R_D6;
                end
              else if porddef(p2)^.typ=s16bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,p^.location.register,R_D6)))
                   else
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),R_D6)));
                   { sign extend }
                   exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,R_D6)));
                   hregister:=R_D6;
                end
              else internalerror(6);
              new(hp);
              reset_reference(hp^);
              hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr));
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   getlabel(neglabel);
                   getlabel(poslabel);
                   exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_L,hregister)));
                   emitl(A_BLT,neglabel);
                end;
              emit_bounds_check(hp^,hregister);
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr+1));
                   emitl(A_JMP,poslabel);
                   emitl(A_LABEL,neglabel);
                   emit_bounds_check(hp^,hregister);
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
         {opsize : topsize;}
         ref : boolean;
         hpp : preference;

      begin
         { !!!!!!!! Rangechecking }
         ref:=false;
         { problems with enums !! }
           { with $R+ explicit type conversations in TP aren't range checked! }
         if (p^.resulttype^.deftype=orddef) and
           (hp^.resulttype^.deftype=orddef) and
           ((porddef(p^.resulttype)^.low>porddef(hp^.resulttype)^.low) or
           (porddef(p^.resulttype)^.high<porddef(hp^.resulttype)^.high)) then
           begin
              if (cs_check_range in aktlocalswitches) and
                 (not(p^.explizit) {or not(cs_tp_compatible in aktmoduleswitches)}) then
              porddef(p^.resulttype)^.genrangecheck;
              if porddef(hp^.resulttype)^.typ=s32bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     begin
                        hregister:=getregister32;
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),hregister)));
                     end;
                end
              { rangechecking for u32bit ?? !!!!!!}
              else if porddef(hp^.resulttype)^.typ=u16bit then
                begin
                   hregister:=getregister32;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                   begin
                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,p^.location.register,hregister)));
                   end
                   else
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),hregister)));
                   { clear unused bits  i.e unsigned extend}
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L, $FFFF, hregister)));
                end
              else if porddef(hp^.resulttype)^.typ=s16bit then
                begin
                   hregister:=getregister32;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,p^.location.register,hregister)))
                   else
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),hregister)));
                   { sign extend }
                   exprasmlist^.concat(new(paicpu,op_reg(A_EXT, S_L, hregister)));
                end
              else internalerror(6);

              if (cs_check_range in aktlocalswitches) and
                 (not(p^.explizit) {or not(cs_tp_compatible in aktmoduleswitches)}) then
              Begin
              new(hpp);
              reset_reference(hpp^);
              hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));


              emit_bounds_check(hpp^, hregister);
              end;
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hregister;
              exit;
           end
         { -------------- endian problems once again --------------------}
         { If RIGHT   enumdef (32-bit) and we do a typecase to a smaller }
         { type we must absolutely load it into a register first.        }
         { --------------------------------------------------------------}
         { ------------ supposing enumdef is always 32-bit --------------}
         { --------------------------------------------------------------}
         else
         if (hp^.resulttype^.deftype = enumdef) and (p^.resulttype^.deftype = orddef) then
           begin
              if (hp^.location.loc=LOC_REGISTER) or (hp^.location.loc=LOC_CREGISTER) then
                 hregister:=hp^.location.register
              else
                 begin
                     hregister:=getregister32;
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(hp^.location.reference),hregister)));
                 end;
              clear_location(p^.location);
              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hregister;
              exit;
           end;
         if (p^.left^.location.loc=LOC_REGISTER) or
           (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              { handled by secondpas by called routine ??? }
              p^.location.register:=p^.left^.location.register;
           end;
      end;


    procedure second_bigger(p,hp : ptree;convtyp : tconverttype);

      var
         hregister : tregister;
         opsize : topsize;
         op : tasmop;
         is_register : boolean;

      begin
{$ifdef dummy}
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
         {Constanst cannot be loaded into registers using MOVZX!}
         if (p^.left^.location.loc<>LOC_MEM) or (not p^.left^.location.reference.isintvalue) then
             case convtyp of
                     tc_int_2_int:
                                 begin
                                    if is_register then
                                      hregister := p^.left^.location.register
                                    else
                                      hregister := getregister32;
                                    if is_register then
                                      emit_reg_reg(A_MOVE,S_B,p^.left^.location.register, hregister)
                                    else
                                    begin
                                      if p^.left^.location.loc = LOC_CREGISTER then
                                        emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,hregister)
                                      else
                                        exprasmlist^.concat(new(paicpu, op_ref_reg(A_MOVE,S_B,
                                         newreference(P^.left^.location.reference), hregister)));
                                    end;
                                    case convtyp of
                                      tc_u8bit_2_s32bit,
                                      tc_u8bit_2_u32bit:
                                                   exprasmlist^.concat(new(paicpu, op_const_reg(
                                                   A_AND,S_L,$FF,hregister)));
                                      tc_s8bit_2_u32bit,
                                      tc_s8bit_2_s32bit:
                                                  begin
                                                    if aktoptprocessor = MC68020 then
                                                      exprasmlist^.concat(new(paicpu,op_reg
                                                        (A_EXTB,S_L,hregister)))
                                                    else { else if aktoptprocessor }
                                                    begin
                                                    { byte to word }
                                                      exprasmlist^.concat(new(paicpu,op_reg
                                                        (A_EXT,S_W,hregister)));
                                                    { word to long }
                                                      exprasmlist^.concat(new(paicpu,op_reg
                                                        (A_EXT,S_L,hregister)));
                                                    end;
                                                  end;
                                      tc_s8bit_2_u16bit,
                                      tc_u8bit_2_s16bit,
                                      tc_u8bit_2_u16bit:
                                                  exprasmlist^.concat(new(paicpu, op_const_reg(
                                                                A_AND,S_W,$FF,hregister)));

                                      tc_s8bit_2_s16bit:
                                                  exprasmlist^.concat(new(paicpu, op_reg(
                                                                A_EXT, S_W, hregister)));

                                    end; { inner case }
                                   end;
                tc_u16bit_2_u32bit,
                tc_u16bit_2_s32bit,
                tc_s16bit_2_u32bit,
                tc_s16bit_2_s32bit: begin
                                     if is_register then
                                       hregister := p^.left^.location.register
                                     else
                                       hregister := getregister32;
                                     if is_register then
                                       emit_reg_reg(A_MOVE,S_W,p^.left^.location.register, hregister)
                                     else
                                     begin
                                       if p^.left^.location.loc = LOC_CREGISTER then
                                         emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,hregister)
                                       else
                                         exprasmlist^.concat(new(paicpu, op_ref_reg(A_MOVE,S_W,
                                           newreference(P^.left^.location.reference), hregister)));
                                     end;
                                     if (convtyp = tc_u16bit_2_s32bit) or
                                        (convtyp = tc_u16bit_2_u32bit) then
                                         exprasmlist^.concat(new(paicpu, op_const_reg(
                                           A_AND, S_L, $ffff, hregister)))
                                     else { tc_s16bit_2_s32bit }
                                          { tc_s16bit_2_u32bit }
                                         exprasmlist^.concat(new(paicpu, op_reg(A_EXT,S_L,
                                           hregister)));
                                    end;
             end { end case }
         else
         begin
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
                        op:=A_MOVE;
                        opsize:=S_L;
                    end;
                tc_s8bit_2_u16bit,
                tc_s8bit_2_s16bit,
                tc_u8bit_2_s16bit,
                tc_u8bit_2_u16bit:
                    begin
                        hregister:=getregister32;
                        op:=A_MOVE;
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
                 else exprasmlist^.concat(new(paicpu,op_ref_reg(op,opsize,
                     newreference(p^.left^.location.reference),hregister)));
              end;
         end; { end elseif }

         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
         maybe_rangechecking(p,p^.left^.resulttype,p^.resulttype);
{$endif dummy}
      end;


    procedure second_string_string(p,hp : ptree;convtyp : tconverttype);

      var
         pushed : tpushed;

      begin
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
                              { !!!!! exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.right^.location.register))); }
                              ungetregister32(p^.left^.location.register);
                           end;
                         LOC_REFERENCE,LOC_MEM:
                           begin
                              emit_push_mem(p^.left^.location.reference);
                              del_reference(p^.left^.location.reference);
                           end;
                      end;
                      emitcall('FPC_ANSI_TO_SHORTSTRING',true);
                      maybe_loada5;
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
      end;

    procedure second_cstring_charpointer(p,hp : ptree;convtyp : tconverttype);

      begin
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         inc(p^.left^.location.reference.offset);
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           R_A0)));
         emit_reg_reg(A_MOVE, S_L, R_A0, p^.location.register);
      end;

    procedure second_string_chararray(p,hp : ptree;convtyp : tconverttype);

      begin
         inc(p^.location.reference.offset);
      end;

    procedure second_array_to_pointer(p,hp : ptree;convtyp : tconverttype);

      begin
         del_reference(p^.left^.location.reference);
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           R_A0)));
         emit_reg_reg(A_MOVE,S_L,R_A0, P^.location.register);
      end;

    procedure second_pointer_to_array(p,hp : ptree;convtyp : tconverttype);
      var
       reg: tregister;
      begin
         clear_location(p^.location);
         p^.location.loc:=LOC_REFERENCE;
         clear_reference(p^.location.reference);
         { here, after doing some arithmetic on the pointer }
         { we put it back in an address register            }
         if p^.left^.location.loc=LOC_REGISTER then
         begin
           reg := getaddressreg;
           { move the pointer in a data register back into }
           { an address register.                          }
           emit_reg_reg(A_MOVE, S_L, p^.left^.location.register,reg);

           p^.location.reference.base:=reg;
           ungetregister32(p^.left^.location.register);
         end
         else
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                   p^.location.reference.base:=getaddressreg;
                   emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                     p^.location.reference.base);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getaddressreg;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
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
         clear_location(p^.location);
         p^.location.loc:=LOC_MEM;

         { first get the memory for the string }
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);

         { calc the length of the array }
         l:=parraydef(p^.left^.resulttype)^.highrange-
           parraydef(p^.left^.resulttype)^.lowrange+1;

         if l>255 then
           CGMessage(type_e_mismatch);

         { write the length }
           exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,S_B,l,
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
         { p^.right is not disposed for typeconv !! PM }
         disposetree(p^.right);
         p^.right:=nil;
      end;

    procedure second_int_real(p,hp : ptree;convtyp : tconverttype);

      var
         r : preference;

      begin
        emitloadord2reg(p^.left^.location, porddef(p^.left^.resulttype), R_D6, true);
        ungetiftemp(p^.left^.location.reference);
        if porddef(p^.left^.resulttype)^.typ=u32bit then
           push_int(0);

        emit_reg_reg(A_MOVE, S_L, R_D6, R_SPPUSH);
        new(r);
        reset_reference(r^);
        r^.base := R_SP;
        { no emulation }
{           for u32bit a solution would be to push $0 and to load a
+          comp
+           if porddef(p^.left^.resulttype)^.typ=u32bit then
+             exprasmlist^.concat(new(paicpu,op_ref(A_FILD,S_IQ,r)))
+           else}
          clear_location(p^.location);
          p^.location.loc := LOC_FPU;
          { get floating point register. }
          if (cs_fp_emulation in aktmoduleswitches) then
          begin
            p^.location.fpureg := getregister32;
            exprasmlist^.concat(new(paicpu, op_ref_reg(A_MOVE, S_L, r, R_D0)));
            emitcall('FPC_LONG2SINGLE',true);
            emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.fpureg);
          end
          else
          begin
            p^.location.fpureg := getfloatreg;
            exprasmlist^.concat(new(paicpu, op_ref_reg(A_FMOVE, S_L, r, p^.location.fpureg)))
          end;
        if porddef(p^.left^.resulttype)^.typ=u32bit then
           exprasmlist^.concat(new(paicpu,op_const_reg(A_ADD,S_L,8,R_SP)))
        else
        { restore the stack to the previous address }
           exprasmlist^.concat(new(paicpu, op_const_reg(A_ADDQ, S_L, 4, R_SP)));
      end;

    procedure second_real_fix(p,hp : ptree;convtyp : tconverttype);
      var
         rreg : tregister;
         ref : treference;
      begin
         rreg:=getregister32;
         { Are we in a LOC_FPU, if not then use scratch registers }
         { instead of allocating reserved registers.              }
         if (p^.left^.location.loc<>LOC_FPU) then
         begin
           if (cs_fp_emulation in aktmoduleswitches) then
           begin
             exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),R_D0)));
             exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,65536,R_D1)));
             emitcall('FPC_LONGMUL',true);
             emit_reg_reg(A_MOVE,S_L,R_D0,rreg);
           end
           else
           begin
             exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,S_L,newreference(p^.left^.location.reference),R_FP0)));
             exprasmlist^.concat(new(paicpu,op_const_reg(A_FMUL,S_L,65536,R_FP0)));
             exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,S_L,R_FP0,rreg)));
           end;
         end
         else
         begin
           if (cs_fp_emulation in aktmoduleswitches) then
           begin
             exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)));
             exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,65536,R_D1)));
             emitcall('FPC_LONGMUL',true);
             emit_reg_reg(A_MOVE,S_L,R_D0,rreg);
           end
           else
           begin
             exprasmlist^.concat(new(paicpu,op_const_reg(A_FMUL,S_L,65536,p^.left^.location.fpureg)));
             exprasmlist^.concat(new(paicpu,op_reg_reg(A_FMOVE,S_L,p^.left^.location.fpureg,rreg)));
           end;
         end;
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=rreg;
      end;


    procedure second_float_float(p,hp : ptree;convtyp : tconverttype);

      begin
         case p^.left^.location.loc of
            LOC_FPU :  begin
                         { reload }
                         clear_location(p^.location);
                         p^.location.loc := LOC_FPU;
                         p^.location.fpureg := p^.left^.location.fpureg;
                       end;
            LOC_MEM,
            LOC_REFERENCE : floatload(pfloatdef(p^.left^.resulttype)^.typ,
                              p^.left^.location.reference,p^.location);
         end;
{ ALREADY HANDLED BY FLOATLOAD      }
{         p^.location.loc:=LOC_FPU; }
      end;

    procedure second_fix_real(p,hp : ptree;convtyp : tconverttype);
    var
        startreg : tregister;
        hl : pasmlabel;
        r : treference;
        reg1: tregister;
        hl1,hl2,hl3,hl4,hl5,hl6,hl7,hl8,hl9: pasmlabel;
      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=p^.left^.location.register;
              ungetregister(startreg);
              { move d0,d0 is removed by emit_reg_reg }
              emit_reg_reg(A_MOVE,S_L,startreg,R_D0);
           end
         else
           begin
              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(
                p^.left^.location.reference),R_D0)));
              del_reference(p^.left^.location.reference);
              startreg:=R_NO;
           end;

         reg1 := getregister32;

         { Motorola 68000 equivalent of CDQ     }
         { we choose d1:d0 pair for quad word   }
         exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_L,R_D0)));
         getlabel(hl1);
         emitl(A_BPL,hl1);
         { we copy all bits (-ve number) }
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,$ffffffff,R_D1)));
         getlabel(hl2);
         emitl(A_BRA,hl2);
         emitl(A_LABEL,hl1);
         exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_L,R_D0)));
         emitl(A_LABEL,hl2);
         { end CDQ }

         exprasmlist^.concat(new(paicpu,op_reg_reg(A_EOR,S_L,R_D1,R_D0)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,reg1)));
         getlabel(hl3);
         emitl(A_BEQ,hl3);

         { Motorola 68000 equivalent of RCL    }
         getlabel(hl4);
         emitl(A_BCC,hl4);
         exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,1,reg1)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_OR,S_L,1,reg1)));
         getlabel(hl5);
         emitl(A_BRA,hl5);
         emitl(A_LABEL,hl4);
         exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,1,reg1)));
         emitl(A_LABEL,hl5);
         { end RCL }

         { Motorola 68000 equivalent of BSR }
         { save register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,R_D6)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_B,31,R_D0)));
         getlabel(hl6);
         emitl(A_LABEL,hl6);
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_BTST,S_L,R_D0,R_D1)));
         getlabel(hl7);
         emitl(A_BNE,hl7);
         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,S_B,1,R_D0)));
         emitl(A_BPL,hl6);
         { restore register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D6,R_D0)));
         emitl(A_LABEL,hl7);
         { end BSR }

         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_B,32,R_D6)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_SUB,S_B,R_D1,R_D6)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_LSL,S_L,R_D6,R_D0)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ADD,S_W,1007,R_D1)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,5,R_D1)));

         { Motorola 68000 equivalent of SHLD }
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_W,11,R_D6)));
         { save register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D1,R_A0)));
         getlabel(hl8);
         emitl(A_LABEL,hl8);
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ROXL,S_W,1,R_D1)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ROXL,S_W,1,reg1)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,S_B,1,R_D6)));
         emitl(A_BNE,hl8);
         { restore register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A0,R_D1)));
         { end Motorola equivalent of SHLD }

         { Motorola 68000 equivalent of SHLD }
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_W,20,R_D6)));
         { save register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,R_A0)));
         getlabel(hl9);
         emitl(A_LABEL,hl9);
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ROXL,S_W,1,R_D0)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ROXL,S_W,1,reg1)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_SUBQ,S_B,1,R_D6)));
         emitl(A_BNE,hl9);
         { restore register }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A0,R_D0)));
         { end Motorola equivalent of SHLD }

         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_B,20,R_D6)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_SUB,S_L,R_D6,R_D0)));
         emitl(A_LABEL, hl3);

         { create temp values and put on stack }
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,reg1,R_SPPUSH)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH)));


         reset_reference(r);
         r.base:=R_SP;

         if (cs_fp_emulation in aktmoduleswitches) then
         begin
           clear_location(p^.location);
           p^.location.loc:=LOC_FPU;
           p^.location.fpureg := getregister32;
           exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(r),
             p^.left^.location.fpureg)))
         end
         else
         begin
           clear_location(p^.location);
           p^.location.loc:=LOC_FPU;
           p^.location.fpureg := getfloatreg;
           exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,S_L,newreference(r),
               p^.left^.location.fpureg)))
         end;
         { clear temporary space }
         exprasmlist^.concat(new(paicpu,op_const_reg(A_ADDQ,S_L,8,R_SP)));
         ungetregister32(reg1);
{ Alreadu handled above...          }
{         p^.location.loc:=LOC_FPU; }
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
                s8bit : begin
                           exprasmlist^.concat(new(paicpu, op_ref_reg(A_MOVE,S_B,
                              newreference(p^.left^.location.reference),hregister)));
                           if aktoptprocessor = MC68020 then
                              exprasmlist^.concat(new(paicpu, op_reg(A_EXTB,S_L,hregister)))
                           else
                            begin
                              exprasmlist^.concat(new(paicpu, op_reg(A_EXT,S_W,hregister)));
                              exprasmlist^.concat(new(paicpu, op_reg(A_EXT,S_L,hregister)));
                            end;
                        end;
                u8bit : begin
                          exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,newreference(p^.left^.location.reference),
                            hregister)));
                          exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ff,hregister)));
                        end;
                s16bit :begin
                         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.left^.location.reference),
                           hregister)));
                          exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,hregister)));
                        end;
                u16bit : begin
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(p^.left^.location.reference),
                               hregister)));
                            exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ffff,hregister)));
                         end;
                s32bit,u32bit : exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                  hregister)));
                {!!!! u32bit }
              end;
           end;
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVEQ,S_L,16,R_D1)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_LSL,S_L,R_D1,hregister)));

         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
      end;


     procedure second_proc_to_procvar(p,hp : ptree;convtyp : tconverttype);

     begin
        { secondpass(hp); already done in secondtypeconv PM }
        clear_location(p^.location);
        p^.location.loc:=LOC_REGISTER;
        del_reference(hp^.location.reference);
        p^.location.register:=getregister32;
        exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,
         newreference(hp^.location.reference),R_A0)));

        emit_reg_reg(A_MOVE, S_L, R_A0, P^.location.register);
     end;

      procedure second_bool_to_int(p,hp : ptree;convtyp : tconverttype);

      var
         oldtruelabel,oldfalselabel,hlabel : pasmlabel;
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
         clear_location(p^.location);
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
         op:=A_MOVE;
{         if opsize in [S_B,S_W,S_L] then
          op:=A_MOVE
         else
          if (porddef(p^.resulttype)^.typ in [s8bit,s16bit,s32bit]) then
           op:=A_MOVSX
          else
           op:=A_MOVZX; }
         case porddef(p^.resulttype)^.typ of
          bool8bit,u8bit,s8bit : begin
                                   p^.location.register:=hregister;
                                   newsize:=S_B;
                                 end;
       bool16bit,u16bit,s16bit : begin
                                   p^.location.register:=hregister;
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
      LOC_REFERENCE : exprasmlist^.concat(new(paicpu,op_ref_reg(op,opsize,
                        newreference(hp^.location.reference),p^.location.register)));
       LOC_REGISTER,
      LOC_CREGISTER : exprasmlist^.concat(new(paicpu,op_reg_reg(op,opsize,
                        hp^.location.register,p^.location.register)));
          LOC_FLAGS : begin
{                       hregister:=reg32toreg8(hregister); }
                        exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[hp^.location.resflags],S_B,hregister)));
{ !!!!!!!!
                        case porddef(p^.resulttype)^.typ of
                  bool16bit,
              u16bit,s16bit : exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVZX,S_BW,hregister,p^.location.register)));
                  bool32bit,
              u32bit,s32bit : exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVZX,S_BL,hregister,p^.location.register)));
                        end; }
                      end;
           LOC_JUMP : begin
                        getlabel(hlabel);
                        emitl(A_LABEL,truelabel);
                        exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,newsize,1,hregister)));
                        emitl(A_JMP,hlabel);
                        emitl(A_LABEL,falselabel);
                        exprasmlist^.concat(new(paicpu,op_reg(A_CLR,newsize,hregister)));
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
         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         del_reference(hp^.location.reference);
         case hp^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                hregister:=getregister32;
                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                  newreference(hp^.location.reference),hregister)));
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=hp^.location.register;
              end;
          else
            internalerror(10062);
          end;
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_OR,S_L,hregister,hregister)));
{        hregister:=reg32toreg8(hregister); }
         exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[hp^.location.resflags],S_B,hregister)));
         case porddef(p^.resulttype)^.typ of
           bool8bit : p^.location.register:=hregister;
{ !!!!!!!!!!!

          bool16bit : begin
                        p^.location.register:=reg8toreg16(hregister);
                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVZX,S_BW,hregister,p^.location.register)));
                      end;
          bool32bit : begin
                        p^.location.register:=reg16toreg32(hregister);
                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVZX,S_BL,hregister,p^.location.register)));
                      end; }
         else
          internalerror(10064);
         end;
     end;

    procedure second_load_smallset(p,hp : ptree;convtyp : tconverttype);
      var
        href : treference;
        pushedregs : tpushed;
      begin
        href.symbol:=nil;
        pushusedregisters(pushedregs,$ff);
        gettempofsizereference(32,href);
        emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
        emitpushreferenceaddr(exprasmlist,href);
        emitcall('FPC_SET_LOAD_SMALL',true);
        maybe_loada5;
        popusedregisters(pushedregs);
        clear_location(p^.location);
        p^.location.loc:=LOC_MEM;
        stringdispose(p^.location.reference.symbol);
        p^.location.reference:=href;
      end;

    procedure second_ansistring_to_pchar(p,hp : ptree;convtyp : tconverttype);

      var
         l1,l2 : pasmlabel;
         hr : preference;

      begin
        InternalError(342132);
{!!!!!!!!!!!

         clear_location(p^.location);
         p^.location.loc:=LOC_REGISTER;
         getlabel(l1);
         getlabel(l2);
         case hp^.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              exprasmlist^.concat(new(paicpu,op_const_reg(A_CMP,S_L,0,
                hp^.location.register)));
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(paicpu,op_const_ref(A_CMP,S_L,0,
                   newreference(hp^.location.reference))));
                  del_reference(hp^.location.reference);
                  p^.location.register:=getregister32;
               end;
         end;
         emitl(A_JZ,l1);
         if hp^.location.loc in [LOC_MEM,LOC_REFERENCE] then
           exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,newreference(
             hp^.location.reference),
             p^.location.register)));
         emitl(A_JMP,l2);
         emitl(A_LABEL,l1);
         new(hr);
         reset_reference(hr^);
         hr^.symbol:=stringdup('FPC_EMPTYCHAR');
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,hr,
           p^.location.register)));
         emitl(A_LABEL,l2); }
      end;

    procedure second_pchar_to_string(p,hp : ptree;convtyp : tconverttype);
      begin
         internalerror(12121);
      end;

    procedure second_nothing(p,hp : ptree;convtyp : tconverttype);
      begin
      end;

{****************************************************************************
                             SecondTypeConv
****************************************************************************}

    procedure secondtypeconv(var p : ptree);
      const
         secondconvert : array[tconverttype] of
           tsecondconvproc = (second_nothing,second_nothing,
           second_bigger,second_only_rangecheck,
           second_bigger,second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_string_string,
           second_cstring_charpointer,second_string_chararray,
           second_array_to_pointer,second_pointer_to_array,
           second_char_to_string,second_bigger,
           second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_smaller,
           second_bigger);

{$ifdef dummy}
           ,second_smaller,
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
           second_nothing,
           second_load_smallset,
           second_ansistring_to_pchar,
           second_pchar_to_string,
           second_nothing);
{$endif dummy}

      begin
         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                        }

         { this is necessary, because second_bool_byte, have to change   }
         { true- and false label before calling secondpass               }
         if p^.convtyp<>tc_bool_2_int then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
              if codegenerror then
               exit;
           end;

         if not(p^.convtyp in [tc_equal,tc_not_possible]) then
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
         pushusedregisters(pushed,$ffff);
         secondpass(p^.left);
         clear_location(p^.location);
         p^.location.loc:=LOC_FLAGS;
         p^.location.resflags:=F_NE;

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,
                   S_L,p^.left^.location.register,R_SPPUSH)));
                 ungetregister32(p^.left^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,
                   S_L,newreference(p^.left^.location.reference),R_SPPUSH)));
                 del_reference(p^.left^.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,
                   S_L,p^.right^.location.register,R_SPPUSH)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,
                   S_L,newreference(p^.right^.location.reference),R_SPPUSH)));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_IS',true);
         exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_B,R_D0)));
         popusedregisters(pushed);
      end;


{*****************************************************************************
                             SecondAs
*****************************************************************************}

    procedure secondas(var p : ptree);

      var
         pushed : tpushed;

      begin
         set_location(p^.location,p^.left^.location);
         { save all used registers }
         pushusedregisters(pushed,$ffff);
         { push the vmt of the class }
         exprasmlist^.concat(new(paicpu,op_csymbol_reg(A_MOVE,
           S_L,newcsymbol(pobjectdef(p^.right^.resulttype)^.vmt_mangledname,0),R_SPPUSH)));
         emitpushreferenceaddr(exprasmlist,p^.location.reference);
          emitcall('FPC_DO_AS',true);
         popusedregisters(pushed);
      end;


end.
{
  $Log$
  Revision 1.16  2000-01-07 01:14:21  peter
    * updated copyright to 2000

  Revision 1.15  1999/12/22 01:01:47  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.14  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.13  1999/08/25 11:59:48  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.12  1998/12/11 00:02:59  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.11  1998/11/05 12:02:36  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.10  1998/10/15 12:41:17  pierre
    * last memory leaks found when compiler
      a native atari compiler fixed

  Revision 1.9  1998/10/14 11:28:17  florian
    * emitpushreferenceaddress gets now the asmlist as parameter
    * m68k version compiles with -duseansistrings

  Revision 1.8  1998/10/14 10:45:05  pierre
    * ppu problems for m68k fixed (at least in cross compiling)
    * one last memory leak for sysamiga fixed
    * the amiga RTL compiles now completely !!

  Revision 1.7  1998/10/13 16:50:06  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.6  1998/10/06 20:48:56  peter
    * m68k compiler compiles again

  Revision 1.5  1998/09/17 09:42:23  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.4  1998/09/14 10:43:56  peter
    * all internal RTL functions start with FPC_

  Revision 1.3  1998/09/11 12:29:43  pierre
    * removed explicit range_checking as it is buggy

  Revision 1.2.2.1  1998/09/11 12:08:57  pierre
    * removed explicit range_check was buggy

  Revision 1.2  1998/09/04 08:41:45  peter
    * updated some error messages

  Revision 1.1  1998/09/01 09:07:09  peter
    * m68k fixes, splitted cg68k like cgi386

}
