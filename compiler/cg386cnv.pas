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

    procedure loadshortstring(p:ptree);

    procedure secondtypeconv(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);


implementation

   uses
      cobjects,verbose,globals,systems,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,pass_1,
      i386,cgai386,tgeni386;



    procedure push_shortstring_length(p:ptree);
      var
        r : preference;
        hightree : ptree;

      begin
        if is_open_string(p^.resulttype) then
         begin
           getsymonlyin(p^.symtable,'high'+pvarsym(p^.symtableentry)^.name);
           hightree:=genloadnode(pvarsym(srsym),p^.symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,0);
           disposetree(hightree);
{           r:=new_reference(highframepointer,highoffset+4);
           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,r,R_EDI)));
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI))); }
         end
        else
         begin
           push_int(pstringdef(p^.resulttype)^.len);
         end;
      end;


    procedure loadshortstring(p:ptree);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      begin
         case p^.right^.resulttype^.deftype of
            stringdef:
              begin
                 if (p^.right^.treetype=stringconstn) and
                   (str_length(p^.right)=0) then
                   exprasmlist^.concat(new(pai386,op_const_ref(
                      A_MOV,S_B,0,newreference(p^.left^.location.reference))))
                 else
                   begin
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     emitpushreferenceaddr(exprasmlist,p^.right^.location.reference);
                     push_shortstring_length(p^.left);
                     emitcall('FPC_SHORTSTR_COPY',true);
                     maybe_loadesi;
                   end;
              end;
            orddef:
              begin
                 if p^.right^.treetype=ordconstn then
                   exprasmlist^.concat(new(pai386,op_const_ref(
                      A_MOV,S_W,p^.right^.value*256+1,newreference(p^.left^.location.reference))))
                 else
                   begin
                      { not so elegant (goes better with extra register }
                      if (p^.right^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                        begin
                           exprasmlist^.concat(new(pai386,op_reg_reg(
                              A_MOV,S_L,makereg32(p^.right^.location.register),R_EDI)));
                           ungetregister(p^.right^.location.register);
                        end
                      else
                        begin
                           exprasmlist^.concat(new(pai386,op_ref_reg(
                              A_MOV,S_L,newreference(p^.right^.location.reference),R_EDI)));
                           del_reference(p^.right^.location.reference);
                        end;
                      exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,8,R_EDI)));
                      exprasmlist^.concat(new(pai386,op_const_reg(A_OR,S_L,1,R_EDI)));
                      exprasmlist^.concat(new(pai386,op_reg_ref(
                         A_MOV,S_W,R_DI,newreference(p^.left^.location.reference))));
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;




{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    type
      tsecondconvproc = procedure(pto,pfrom : ptree;convtyp : tconverttype);

{$ifndef OLDCNV}

    procedure second_int_to_int(pto,pfrom : ptree;convtyp : tconverttype);
      var
        op        : tasmop;
        opsize    : topsize;
        hregister : tregister;
      begin
        { insert range check if not explicit conversion }
        if not(pto^.explizit) then
          emitrangecheck(pfrom,pto^.resulttype);

        { is the result size smaller ? }
        if pto^.resulttype^.size<pfrom^.resulttype^.size then
          begin
            { only need to set the new size of a register }
            if (pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             begin
               case pto^.resulttype^.size of
                1 : pto^.location.register:=makereg8(pfrom^.location.register);
                2 : pto^.location.register:=makereg16(pfrom^.location.register);
                4 : pto^.location.register:=makereg32(pfrom^.location.register);
               end;
             end;
          end

        { is the result size bigger ? }
        else if pto^.resulttype^.size>pfrom^.resulttype^.size then
          begin
            { remove reference }
            if not(pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
              begin
                del_reference(pfrom^.location.reference);
                { we can do this here as we need no temp inside }
                ungetiftemp(pfrom^.location.reference);
              end;

            { get op and opsize, handle separate for constants, becuase
              movz doesn't support constant values }
            if (pfrom^.location.loc=LOC_MEM) and (pfrom^.location.reference.isintvalue) then
             begin
               opsize:=def_opsize(pto^.resulttype);
               op:=A_MOV;
             end
            else
             begin
               opsize:=def2def_opsize(pfrom^.resulttype,pto^.resulttype);
               if opsize in [S_B,S_W,S_L] then
                op:=A_MOV
               else
                if is_signed(pfrom^.resulttype) then
                 op:=A_MOVSX
                else
                 op:=A_MOVZX;
             end;
            { load the register we need }
            if pfrom^.location.loc<>LOC_REGISTER then
              hregister:=getregister32
            else
              hregister:=pfrom^.location.register;
            { set the correct register size and location }
            clear_location(pto^.location);
            pto^.location.loc:=LOC_REGISTER;
            case pto^.resulttype^.size of
             1 : pto^.location.register:=makereg8(hregister);
             2 : pto^.location.register:=makereg16(hregister);
             4 : pto^.location.register:=makereg32(hregister);
            end;
            { insert the assembler code }
            if pfrom^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
              emit_reg_reg(op,opsize,pfrom^.location.register,pto^.location.register)
            else
              exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                newreference(pfrom^.location.reference),pto^.location.register)));
          end;
      end;

{$else}

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
           (not(p^.explizit) {or not(cs_tp_compatible in aktmoduleswitches)}) and
           ((porddef(p1)^.low>porddef(p2)^.low) or
           (porddef(p1)^.high<porddef(p2)^.high) or
           (porddef(p1)^.typ=u32bit) or
           (porddef(p2)^.typ=u32bit)) then
           begin
              porddef(p1)^.genrangecheck;
              is_register:=(p^.location.loc=LOC_REGISTER) or
                (p^.location.loc=LOC_CREGISTER);
              if porddef(p2)^.typ=u8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,p^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,p^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(p^.location.reference),R_EDI)));
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
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),R_EDI)));
                        hregister:=R_EDI;
                     end;
                end
              else if porddef(p2)^.typ=u16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else internalerror(6);
              hp:=new_reference(R_NO,0);
              hp^.symbol:=stringdup(porddef(p1)^.getrangecheckstring);
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
                   hp^.symbol:=stringdup(porddef(p1)^.getrangecheckstring);
                   { second part here !! }
                   hp^.offset:=8;
                   emitl(A_JMP,poslabel);
                   emitl(A_LABEL,neglabel);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hp)));
                   emitl(A_LABEL,poslabel);
                end;
           end;
      end;


    procedure second_only_rangecheck(pto,pfrom : ptree;convtyp : tconverttype);

      begin
         maybe_rangechecking(pto,pfrom^.resulttype,pto^.resulttype);
      end;


    procedure second_smaller(pto,pfrom : ptree;convtyp : tconverttype);

      var
         hregister,destregister : tregister;
         ref : boolean;
         hpp : preference;

      begin
         ref:=false;
         { problems with enums !! }
         if (cs_check_range in aktlocalswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(pto^.explizit) {or not(cs_tp_compatible in aktmoduleswitches)}) and
           (pto^.resulttype^.deftype=orddef) and
           (pfrom^.resulttype^.deftype=orddef) then
           begin
              if porddef(pfrom^.resulttype)^.typ=u32bit then
                begin
                   { when doing range checking for u32bit, we have some trouble }
                   { because BOUND assumes signed values                        }
                   { first, we check if the values is greater than 2^31:        }
                   { the u32bit rangenr contains the appropriate rangenr        }
                   porddef(pfrom^.resulttype)^.genrangecheck;
                   hregister:=R_EDI;
                   if (pto^.location.loc=LOC_REGISTER) or
                      (pto^.location.loc=LOC_CREGISTER) then
                     hregister:=pto^.location.register
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       newreference(pto^.location.reference),R_EDI)));
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup(porddef(pfrom^.resulttype)^.getrangecheckstring);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));

                   { then we do a normal range check }
                   porddef(pto^.resulttype)^.genrangecheck;
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup(porddef(pto^.resulttype)^.getrangecheckstring);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                end
              else
                if ((porddef(pto^.resulttype)^.low>porddef(pfrom^.resulttype)^.low) or
                (porddef(pto^.resulttype)^.high<porddef(pfrom^.resulttype)^.high)) then
                begin
                   porddef(pto^.resulttype)^.genrangecheck;
                   { per default the var is copied to EDI }
                   hregister:=R_EDI;
                   if porddef(pfrom^.resulttype)^.typ=s32bit then
                     begin
                        if (pto^.location.loc=LOC_REGISTER) or
                           (pto^.location.loc=LOC_CREGISTER) then
                          hregister:=pto^.location.register
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(pto^.location.reference),R_EDI)));
                     end
                   else if porddef(pfrom^.resulttype)^.typ=u16bit then
                     begin
                        if (pto^.location.loc=LOC_REGISTER) or
                           (pto^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,pto^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,
                            newreference(pto^.location.reference),R_EDI)));
                     end
                   else if porddef(pfrom^.resulttype)^.typ=s16bit then
                     begin
                        if (pto^.location.loc=LOC_REGISTER) or
                           (pto^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,pto^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,
                            newreference(pto^.location.reference),R_EDI)));
                     end
                   else internalerror(6);
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup(porddef(pto^.resulttype)^.getrangecheckstring);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                   (*
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                        destregister:=pfrom^.location.register;
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
         if (pto^.location.loc=LOC_REGISTER) or
           (pto^.location.loc=LOC_CREGISTER) then
           begin
              destregister:=pfrom^.location.register;
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
              pto^.location.register:=destregister;
           end;
      end;

    procedure second_bigger(pto,pfrom : ptree;convtyp : tconverttype);

      var
         hregister : tregister;
         opsize : topsize;
         op : tasmop;
         is_register : boolean;

      begin
           is_register:=pfrom^.location.loc=LOC_REGISTER;
           if not(is_register) and (pfrom^.location.loc<>LOC_CREGISTER) then
             begin
                del_reference(pfrom^.location.reference);
                { we can do this here as we need no temp inside second_bigger }
                ungetiftemp(pfrom^.location.reference);
             end;
         { this is wrong !!!
         gives me movl (%eax),%eax
         for the length(string !!!
         use only for constant values }
           {Constant cannot be loaded into registers using MOVZX!}
           if (pfrom^.location.loc<>LOC_MEM) or (not pfrom^.location.reference.isintvalue) then
                case convtyp of
                    tc_u8bit_2_s32bit,tc_u8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(pfrom^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_BL;
                      end;
                    { here what do we do for negative values ? }
                    tc_s8bit_2_s32bit,tc_s8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(pfrom^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_BL;
                      end;
                    tc_u16bit_2_s32bit,tc_u16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(pfrom^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_WL;
                      end;
                    tc_s16bit_2_s32bit,tc_s16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(pfrom^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_WL;
                      end;
                    tc_s8bit_2_u16bit,
                    tc_u8bit_2_s16bit,
                    tc_u8bit_2_u16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(pfrom^.location.register)
                          else hregister:=reg32toreg16(getregister32);
                          op:=A_MOVZX;
                          opsize:=S_BW;
                      end;
                    tc_s8bit_2_s16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(pfrom^.location.register)
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
                 emit_reg_reg(op,opsize,pfrom^.location.register,hregister);
             end
           else
             begin
                 if pfrom^.location.loc=LOC_CREGISTER then
                    emit_reg_reg(op,opsize,pfrom^.location.register,hregister)
                 else exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                    newreference(pfrom^.location.reference),hregister)));
             end;
           clear_location(pto^.location);
           pto^.location.loc:=LOC_REGISTER;
           pto^.location.register:=hregister;
           maybe_rangechecking(pfrom,pfrom^.resulttype,pto^.resulttype);
       end;

{$endif}

    var
       ltemptoremove : plinkedlist;

    procedure second_string_to_string(pto,pfrom : ptree;convtyp : tconverttype);

      var
         pushed : tpushed;

      begin
         { does anybody know a better solution than this big case statement ? }
         { ok, a proc table would do the job                                  }
         case pstringdef(pto^.resulttype)^.string_typ of

            st_shortstring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      stringdispose(pto^.location.reference.symbol);
                      gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                      del_reference(pfrom^.location.reference);
                      copyshortstring(pto^.location.reference,pfrom^.location.reference,
                        pstringdef(pto^.resulttype)^.len,false);
                      ungetiftemp(pfrom^.location.reference);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                      loadansi2short(pfrom,pto);
                      { this is done in secondtypeconv (FK)
                      removetemps(exprasmlist,temptoremove);
                      destroys:=true;
                      }
                   end;
                 st_widestring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
              end;

            st_longstring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
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
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      clear_location(pto^.location);
                      pto^.location.loc:=LOC_REFERENCE;
                      gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                      ltemptoremove^.concat(new(ptemptodestroy,init(pto^.location.reference,pto^.resulttype)));
                      exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,newreference(pto^.location.reference))));
                      pushusedregisters(pushed,$ff);
                      emit_push_lea_loc(pfrom^.location);
                      emit_push_lea_loc(pto^.location);
                      emitcall('FPC_SHORTSTR_TO_ANSISTR',true);
                      maybe_loadesi;
                      popusedregisters(pushed);

                      ungetiftemp(pfrom^.location.reference);
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
              case pstringdef(pfrom^.resulttype)^.string_typ of
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


    procedure second_cstring_to_pchar(pto,pfrom : ptree;convtyp : tconverttype);
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=getregister32;
         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(pfrom^.location.reference.offset);
               exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),
                 pto^.location.register)));
             end;
           st_ansistring :
             begin
               exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                 pto^.location.register)));
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
      end;


    procedure second_string_to_chararray(pto,pfrom : ptree;convtyp : tconverttype);
      begin
         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(pto^.location.reference.offset);
             end;
           st_ansistring :
             begin
               {!!!!!!!}
               internalerror(8888);
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
      end;


    procedure second_array_to_pointer(pto,pfrom : ptree;convtyp : tconverttype);
      begin
         del_reference(pfrom^.location.reference);
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=getregister32;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),
           pto^.location.register)));
      end;


    procedure second_pointer_to_array(pto,pfrom : ptree;convtyp : tconverttype);
      begin
        clear_location(pto^.location);
        pto^.location.loc:=LOC_REFERENCE;
        clear_reference(pto^.location.reference);
        case pfrom^.location.loc of
          LOC_REGISTER :
            pto^.location.reference.base:=pfrom^.location.register;
          LOC_CREGISTER :
            begin
              pto^.location.reference.base:=getregister32;
              emit_reg_reg(A_MOV,S_L,pfrom^.location.register,pto^.location.reference.base);
            end
         else
            begin
              del_reference(pfrom^.location.reference);
              pto^.location.reference.base:=getregister32;
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                pto^.location.reference.base)));
            end;
        end;
      end;


    { generates the code for the type conversion from an array of char }
    { to a string                                                        }
    procedure second_chararray_to_string(pto,pfrom : ptree;convtyp : tconverttype);
      var
         l : longint;
      begin
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                             }
         clear_location(pto^.location);
         pto^.location.loc:=LOC_MEM;
         { first get the memory for the string }
         gettempofsizereference(256,pto^.location.reference);

         { calc the length of the array }
         l:=parraydef(pfrom^.resulttype)^.highrange-
           parraydef(pfrom^.resulttype)^.lowrange+1;

         if l>255 then
           CGMessage(type_e_mismatch);

         { write the length }
             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,l,
               newreference(pto^.location.reference))));

         { copy to first char of string }
         inc(pto^.location.reference.offset);

         { generates the copy code      }
         { and we need the source never }
         concatcopy(pfrom^.location.reference,pto^.location.reference,l,true,false);

         { correct the string location }
         dec(pto^.location.reference.offset);
      end;


    procedure second_char_to_string(pto,pfrom : ptree;convtyp : tconverttype);
      var
        pushed : tpushed;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_MEM;
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring :
             begin
               gettempofsizereference(256,pto^.location.reference);
               { call loadstring with correct left and right }
               pto^.right:=pfrom;
               pto^.left:=pto;
               loadshortstring(pto);
               pto^.left:=nil; { reset left tree, which is empty }
               { pto^.right is not disposed for typeconv !! PM }
               disposetree(pto^.right);
               pto^.right:=nil;
             end;
           st_ansistring :
             begin
               gettempofsizereference(4,pto^.location.reference);
               ltemptoremove^.concat(new(ptemptodestroy,init(pto^.location.reference,pto^.resulttype)));
               exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,newreference(pto^.location.reference))));
               pushusedregisters(pushed,$ff);
               emit_pushw_loc(pfrom^.location);
               emitpushreferenceaddr(exprasmlist,pto^.location.reference);
               emitcall('FPC_CHAR_TO_ANSISTR',true);
               popusedregisters(pushed);
               maybe_loadesi;
             end;
           else
            internalerror(4179);
        end;
      end;


    procedure second_int_to_real(pto,pfrom : ptree;convtyp : tconverttype);
      var
         r : preference;
         hregister : tregister;
      begin
         { for u32bit a solution is to push $0 and to load a comp }
         { does this first, it destroys maybe EDI }
         hregister:=R_EDI;
         if porddef(pfrom^.resulttype)^.typ=u32bit then
            push_int(0);
         if (pfrom^.location.loc=LOC_REGISTER) or
            (pfrom^.location.loc=LOC_CREGISTER) then
           begin
              case porddef(pfrom^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,pfrom^.location.register,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,pfrom^.location.register,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,pfrom^.location.register,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,pfrom^.location.register,R_EDI)));
                 u32bit,s32bit:
                   hregister:=pfrom^.location.register
              end;
              ungetregister(pfrom^.location.register);
           end
         else
           begin
              r:=newreference(pfrom^.location.reference);
              case porddef(pfrom^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,r,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,r,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,r,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,r,R_EDI)));
                 u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
              end;
              del_reference(pfrom^.location.reference);
              ungetiftemp(pfrom^.location.reference);
         end;
          exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hregister)));
          r:=new_reference(R_ESP,0);
          if porddef(pfrom^.resulttype)^.typ=u32bit then
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IQ,r)))
          else
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IL,r)));

         { better than an add on all processors }
         if porddef(pfrom^.resulttype)^.typ=u32bit then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,8,R_ESP)))
         else
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    procedure second_real_to_fix(pto,pfrom : ptree;convtyp : tconverttype);
      var
         rreg : tregister;
         ref : treference;
      begin
         { real must be on fpu stack }
         if (pfrom^.location.loc<>LOC_FPU) then
           exprasmlist^.concat(new(pai386,op_ref(A_FLD,S_FL,newreference(pfrom^.location.reference))));
         push_int($1f3f);
         push_int(65536);
         reset_reference(ref);
         ref.base:=R_ESP;

         exprasmlist^.concat(new(pai386,op_ref(A_FIMUL,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FSTCW,S_NO,newreference(ref))));

         ref.offset:=6;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_NO,newreference(ref))));

         ref.offset:=0;
         exprasmlist^.concat(new(pai386,op_ref(A_FISTP,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_NO,newreference(ref))));

         rreg:=getregister32;
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,rreg)));
         { better than an add on all processors }
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=rreg;
      end;


    procedure second_real_to_real(pto,pfrom : ptree;convtyp : tconverttype);
      begin
         case pfrom^.location.loc of
            LOC_FPU : ;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 floatload(pfloatdef(pfrom^.resulttype)^.typ,
                   pfrom^.location.reference);
                 { we have to free the reference }
                 del_reference(pfrom^.location.reference);
              end;
         end;
         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    procedure second_fix_to_real(pto,pfrom : ptree;convtyp : tconverttype);
      var
        popeax,popebx,popecx,popedx : boolean;
        startreg : tregister;
        hl : plabel;
        r : treference;
      begin
         if (pfrom^.location.loc=LOC_REGISTER) or
            (pfrom^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=pfrom^.location.register;
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
                pfrom^.location.reference),R_EAX)));
              del_reference(pfrom^.location.reference);
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
         exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_L,20,R_EAX,R_EBX)));

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

         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    procedure second_int_to_fix(pto,pfrom : ptree;convtyp : tconverttype);
      var
         hregister : tregister;
      begin
         if (pfrom^.location.loc=LOC_REGISTER) then
           hregister:=pfrom^.location.register
         else if (pfrom^.location.loc=LOC_CREGISTER) then
           hregister:=getregister32
         else
           begin
              del_reference(pfrom^.location.reference);
              hregister:=getregister32;
              case porddef(pfrom^.resulttype)^.typ of
                s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(pfrom^.location.reference),
                  hregister)));
                u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(pfrom^.location.reference),
                  hregister)));
                s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(pfrom^.location.reference),
                  hregister)));
                u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(pfrom^.location.reference),
                  hregister)));
                u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                  hregister)));
                {!!!! u32bit }
              end;
           end;
         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,16,hregister)));

         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=hregister;
      end;


    procedure second_proc_to_procvar(pto,pfrom : ptree;convtyp : tconverttype);
      begin
        { method pointer ? }
        if assigned(pfrom^.left) then
          begin
             set_location(pto^.location,pfrom^.location);
          end
        else
          begin
             clear_location(pto^.location);
             pto^.location.loc:=LOC_REGISTER;
             pto^.location.register:=getregister32;
             del_reference(pfrom^.location.reference);
             exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
               newreference(pfrom^.location.reference),pto^.location.register)));
          end;
      end;


    procedure second_bool_to_int(pto,pfrom : ptree;convtyp : tconverttype);
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
         secondpass(pfrom);
{$ifndef OLDBOOL}
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.location.loc in [LOC_REGISTER,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              exit;
           end;
{$endif ndef OLDBOOL}
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         del_reference(pfrom^.location.reference);
         case pfrom^.resulttype^.size of
          1 : begin
                case pto^.resulttype^.size of
                 1 : opsize:=S_B;
                 2 : opsize:=S_BW;
                 4 : opsize:=S_BL;
                end;
              end;
          2 : begin
                case pto^.resulttype^.size of
                 1 : begin
                       if pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        pfrom^.location.register:=reg16toreg8(pfrom^.location.register);
                       opsize:=S_B;
                     end;
                 2 : opsize:=S_W;
                 4 : opsize:=S_WL;
                end;
              end;
          4 : begin
                case pto^.resulttype^.size of
                 1 : begin
                       if pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        pfrom^.location.register:=reg32toreg8(pfrom^.location.register);
                       opsize:=S_B;
                     end;
                 2 : begin
                       if pfrom^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        pfrom^.location.register:=reg32toreg16(pfrom^.location.register);
                       opsize:=S_W;
                     end;
                 4 : opsize:=S_L;
                end;
              end;
         end;
         if opsize in [S_B,S_W,S_L] then
          op:=A_MOV
         else
          if is_signed(pto^.resulttype) then
           op:=A_MOVSX
          else
           op:=A_MOVZX;
         hregister:=getregister32;
         case pto^.resulttype^.size of
          1 : begin
                pto^.location.register:=reg32toreg8(hregister);
                newsize:=S_B;
              end;
          2 : begin
                pto^.location.register:=reg32toreg16(hregister);
                newsize:=S_W;
              end;
          4 : begin
                pto^.location.register:=hregister;
                newsize:=S_L;
              end;
         else
          internalerror(10060);
         end;

         case pfrom^.location.loc of
            LOC_MEM,
      LOC_REFERENCE : exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                        newreference(pfrom^.location.reference),pto^.location.register)));
       LOC_REGISTER,
      LOC_CREGISTER : begin
                      { remove things like movb %al,%al }
                        if pfrom^.location.register<>pto^.location.register then
                          exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,
                            pfrom^.location.register,pto^.location.register)));
                      end;
          LOC_FLAGS : begin
                        hregister:=reg32toreg8(hregister);
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[pfrom^.location.resflags],S_B,hregister)));
                        case pto^.resulttype^.size of
                         2 : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,hregister,pto^.location.register)));
                         4 : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,pto^.location.register)));
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
         freelabel(truelabel);
         freelabel(falselabel);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure second_int_to_bool(pto,pfrom : ptree;convtyp : tconverttype);
      var
        hregister : tregister;
      begin
         clear_location(pto^.location);
{$ifndef OLDBOOL}
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.location.loc in [LOC_REGISTER,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              exit;
           end;
{$endif ndef OLDBOOL}
         pto^.location.loc:=LOC_REGISTER;
         del_reference(pfrom^.location.reference);
         case pfrom^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                hregister:=getregister32;
                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                  newreference(pfrom^.location.reference),hregister)));
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=pfrom^.location.register;
              end;
          else
            internalerror(10062);
          end;
         exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hregister,hregister)));
         hregister:=reg32toreg8(hregister);
         exprasmlist^.concat(new(pai386,op_reg(flag_2_set[pfrom^.location.resflags],S_B,hregister)));
         case pto^.resulttype^.size of
          1 : pto^.location.register:=hregister;
          2 : begin
                pto^.location.register:=reg8toreg16(hregister);
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,hregister,pto^.location.register)));
              end;
          4 : begin
                pto^.location.register:=reg8toreg32(hregister);
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,pto^.location.register)));
              end;
         else
          internalerror(10064);
         end;
      end;


    procedure second_load_smallset(pto,pfrom : ptree;convtyp : tconverttype);
      var
        href : treference;
        pushedregs : tpushed;
      begin
        href.symbol:=nil;
        pushusedregisters(pushedregs,$ff);
        gettempofsizereference(32,href);
        emitpushreferenceaddr(exprasmlist,pfrom^.location.reference);
        emitpushreferenceaddr(exprasmlist,href);
        emitcall('FPC_SET_LOAD_SMALL',true);
        maybe_loadesi;
        popusedregisters(pushedregs);
        clear_location(pto^.location);
        pto^.location.loc:=LOC_MEM;
        pto^.location.reference:=href;
      end;


    procedure second_ansistring_to_pchar(pto,pfrom : ptree;convtyp : tconverttype);
      var
         l1,l2 : plabel;
         hr : preference;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         getlabel(l1);
         getlabel(l2);
         case pfrom^.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,S_L,0,
                pfrom^.location.register)));
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_L,0,
                   newreference(pfrom^.location.reference))));
                  del_reference(pfrom^.location.reference);
                  pto^.location.register:=getregister32;
               end;
         end;
         emitl(A_JZ,l1);
         if pfrom^.location.loc in [LOC_MEM,LOC_REFERENCE] then
           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(
             pfrom^.location.reference),
             pto^.location.register)));
         emitl(A_JMP,l2);
         emitl(A_LABEL,l1);
         new(hr);
         reset_reference(hr^);
         hr^.symbol:=stringdup('FPC_EMPTYCHAR');
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,hr,
           pto^.location.register)));
         emitl(A_LABEL,l2);
      end;


    procedure second_pchar_to_string(pto,pfrom : ptree;convtyp : tconverttype);
      var
        pushed : tpushed;
      begin
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring:
             begin
                pushusedregisters(pushed,$ff);
                stringdispose(pto^.location.reference.symbol);
                gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                case pfrom^.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,pfrom^.location.register)));
                        ungetregister32(pfrom^.location.register);
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                        emit_push_mem(pfrom^.location.reference);
                        del_reference(pfrom^.location.reference);
                     end;
                end;
                emitpushreferenceaddr(exprasmlist,pto^.location.reference);
                emitcall('FPC_PCHAR_TO_SHORTSTR',true);
                maybe_loadesi;
                popusedregisters(pushed);
             end;
           st_ansistring:
             begin
                stringdispose(pto^.location.reference.symbol);
                gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                ltemptoremove^.concat(new(ptemptodestroy,init(pto^.location.reference,pto^.resulttype)));
                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,newreference(pto^.location.reference))));
                case pfrom^.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                        ungetregister32(pfrom^.location.register);
                        pushusedregisters(pushed,$ff);
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,pfrom^.location.register)));
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                        del_reference(pfrom^.location.reference);
                        pushusedregisters(pushed,$ff);
                        emit_push_mem(pfrom^.location.reference);
                     end;
                end;
                emitpushreferenceaddr(exprasmlist,pto^.location.reference);
                emitcall('FPC_PCHAR_TO_ANSISTR',true);
                maybe_loadesi;
                popusedregisters(pushed);
             end;
         else
          begin
            clear_location(pto^.location);
            pto^.location.loc:=LOC_REGISTER;
            internalerror(12121);
          end;
         end;
      end;


    procedure second_nothing(pto,pfrom : ptree;convtyp : tconverttype);
      begin
      end;


{****************************************************************************
                             SecondTypeConv
****************************************************************************}

    procedure secondtypeconv(var p : ptree);
      const
         secondconvert : array[tconverttype] of tsecondconvproc = (
{$ifndef OLDCNV}
           second_nothing, {equal}
           second_nothing, {not_possible}
           second_string_to_string,
           second_char_to_string,
           second_pchar_to_string,
           second_nothing, {cchar_to_pchar}
           second_cstring_to_pchar,
           second_ansistring_to_pchar,
           second_string_to_chararray,
           second_chararray_to_string,
           second_array_to_pointer,
           second_pointer_to_array,
           second_int_to_int,
           second_bool_to_int,
           second_int_to_bool,
           second_real_to_real,
           second_int_to_real,
           second_int_to_fix,
           second_real_to_fix,
           second_fix_to_real,
           second_proc_to_procvar,
           second_nothing, {arrayconstructor_to_set}
           second_load_smallset
         );
{$else}
           second_nothing,second_nothing,
           second_bigger,second_only_rangecheck,
           second_bigger,second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_string_to_string,
           second_cstring_to_pchar,second_string_to_chararray,
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
           second_int_to_real,second_real_to_fix,
           second_fix_to_real,second_int_to_fix,second_real_to_real,
           second_chararray_to_string,
           second_proc_to_procvar,
           { is constant char to pchar, is done by firstpass }
           second_nothing,
           second_load_smallset,
           second_ansistring_to_pchar,
           second_pchar_to_string,
           second_nothing);
{$endif}
      var
         oldrl : plinkedlist;

      begin
         { the ansi string disposing is a little bit hairy: }
         oldrl:=temptoremove;
         temptoremove:=new(plinkedlist,init);

         { the helper routines need access to the release list }
         ltemptoremove:=oldrl;

         if not(assigned(ltemptoremove)) then
           internalerror(18011);

         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                        }

         { this is necessary, because second_bool_2_int, have to change   }
         { true- and false label before calling secondpass               }
         if p^.convtyp<>tc_bool_2_int then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
              if codegenerror then
               exit;
           end;
         { the second argument only is for maybe_range_checking !}
         secondconvert[p^.convtyp](p,p^.left,p^.convtyp);

         { clean up all temp. objects (ansi/widestrings) }
         removetemps(exprasmlist,temptoremove);
         dispose(temptoremove,done);
         temptoremove:=oldrl;
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
         clear_location(p^.location);
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
         emitcall('FPC_DO_IS',true);
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
         emitcall('FPC_DO_AS',true);
         { restore register, this restores automatically the }
         { result                                            }
         popusedregisters(pushed);
      end;


end.
{
  $Log$
  Revision 1.47  1999-01-27 12:59:32  pierre
  tccnv.pas

  Revision 1.46  1999/01/27 00:13:53  florian
    * "procedure of object"-stuff fixed

  Revision 1.45  1999/01/21 22:10:36  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.44  1999/01/19 10:18:59  florian
    * bug with mul. of dwords fixed, reported by Alexander Stohr
    * some changes to compile with TP
    + small enhancements for the new code generator

  Revision 1.43  1998/12/22 13:10:59  florian
    * memory leaks for ansistring type casts fixed

  Revision 1.42  1998/12/19 00:23:42  florian
    * ansistring memory leaks fixed

  Revision 1.41  1998/11/30 19:48:54  peter
    * some more rangecheck fixes

  Revision 1.40  1998/11/30 09:43:02  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.39  1998/11/29 22:37:30  peter
    * fixed constant ansistring -> pchar

  Revision 1.38  1998/11/29 12:40:19  peter
    * newcnv -> not oldcnv

  Revision 1.37  1998/11/26 21:33:06  peter
    * rangecheck updates

  Revision 1.36  1998/11/26 14:39:11  peter
    * ansistring -> pchar fixed
    * ansistring constants fixed
    * ansistring constants are now written once

  Revision 1.35  1998/11/26 13:10:39  peter
    * new int - int conversion -dNEWCNV
    * some function renamings

  Revision 1.34  1998/11/18 15:44:08  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.33  1998/11/17 00:36:39  peter
    * more ansistring fixes

  Revision 1.32  1998/11/16 15:35:38  peter
    * rename laod/copystring -> load/copyshortstring
    * fixed int-bool cnv bug
    + char-ansistring conversion

  Revision 1.31  1998/11/05 12:02:30  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.30  1998/10/27 11:12:45  peter
    * fixed char_to_string which did not set the .loc

  Revision 1.29  1998/10/26 15:18:41  peter
    * fixed fldcw,fstcw for as 2.9.1

  Revision 1.28  1998/10/08 17:17:11  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.27  1998/10/06 17:16:40  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.26  1998/10/02 07:20:35  florian
    * range checking in units doesn't work if the units are smartlinked, fixed

  Revision 1.25  1998/09/30 12:14:24  peter
    * fixed boolean(longbool) conversion

  Revision 1.24  1998/09/27 10:16:22  florian
    * type casts pchar<->ansistring fixed
    * ansistring[..] calls does now an unique call

  Revision 1.23  1998/09/23 12:03:51  peter
    * overloading fix for array of const

  Revision 1.22  1998/09/22 15:34:09  peter
    + pchar -> string conversion

  Revision 1.21  1998/09/20 17:46:47  florian
    * some things regarding ansistrings fixed

  Revision 1.20  1998/09/17 09:42:12  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.19  1998/09/14 10:43:46  peter
    * all internal RTL functions start with FPC_

  Revision 1.18  1998/09/11 12:29:40  pierre
    * removed explicit range_checking as it is buggy

  Revision 1.17.2.1  1998/09/11 12:08:54  pierre
    * removed explicit range_check was buggy

  Revision 1.17  1998/09/04 08:41:38  peter
    * updated some error CGMessages

  Revision 1.16  1998/09/03 17:39:03  florian
    + better code for type conversation longint/dword to real type

  Revision 1.15  1998/09/03 16:24:50  florian
    * bug of type conversation from dword to real fixed
    * bug fix of Jonas applied

  Revision 1.14  1998/08/28 12:51:39  florian
    + ansistring to pchar type cast fixed

  Revision 1.13  1998/08/28 10:56:56  peter
    * removed warnings

  Revision 1.12  1998/08/14 18:18:38  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.11  1998/08/10 23:59:59  peter
    * fixed dup log

  Revision 1.10  1998/08/10 14:49:47  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.9  1998/08/05 16:00:09  florian
    * some fixes for ansi strings

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
