{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
    procedure loadlongstring(p:ptree);
    procedure loadansi2short(source,dest : ptree);

    procedure secondtypeconv(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);


implementation

   uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,pass_1,
      cpubase,cpuasm,
      cgai386,tgeni386;



    procedure push_shortstring_length(p:ptree);
      var
        hightree : ptree;
      begin
        if is_open_string(p^.resulttype) then
         begin
           getsymonlyin(p^.symtable,'high'+pvarsym(p^.symtableentry)^.name);
           hightree:=genloadnode(pvarsym(srsym),p^.symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,0,4);
           disposetree(hightree);
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
                   emit_const_ref(
                      A_MOV,S_B,0,newreference(p^.left^.location.reference))
                 else
                   begin
                     emitpushreferenceaddr(p^.left^.location.reference);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     push_shortstring_length(p^.left);
                     emitcall('FPC_SHORTSTR_COPY');
                     maybe_loadesi;
                   end;
              end;
            orddef:
              begin
                 if p^.right^.treetype=ordconstn then
                   emit_const_ref(
                      A_MOV,S_W,p^.right^.value*256+1,newreference(p^.left^.location.reference))
                 else
                   begin
                      { not so elegant (goes better with extra register }
                      if (p^.right^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                        begin
                           emit_reg_reg(A_MOV,S_L,makereg32(p^.right^.location.register),R_EDI);
                           ungetregister(p^.right^.location.register);
                        end
                      else
                        begin
                           emit_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),R_EDI);
                           del_reference(p^.right^.location.reference);
                        end;
                      emit_const_reg(A_SHL,S_L,8,R_EDI);
                      emit_const_reg(A_OR,S_L,1,R_EDI);
                      emit_reg_ref(A_MOV,S_W,R_DI,newreference(p^.left^.location.reference));
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;

    procedure loadlongstring(p:ptree);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
         r : preference;

      begin
         case p^.right^.resulttype^.deftype of
            stringdef:
              begin
                 if (p^.right^.treetype=stringconstn) and
                   (str_length(p^.right)=0) then
                   emit_const_ref(A_MOV,S_L,0,newreference(p^.left^.location.reference))
                 else
                   begin
                     emitpushreferenceaddr(p^.left^.location.reference);
                     emitpushreferenceaddr(p^.right^.location.reference);
                     push_shortstring_length(p^.left);
                     emitcall('FPC_LONGSTR_COPY');
                     maybe_loadesi;
                   end;
              end;
            orddef:
              begin
                 emit_const_ref(A_MOV,S_L,1,newreference(p^.left^.location.reference));

                 r:=newreference(p^.left^.location.reference);
                 inc(r^.offset,4);

                 if p^.right^.treetype=ordconstn then
                   emit_const_ref(A_MOV,S_B,p^.right^.value,r)
                 else
                   begin
                      case p^.right^.location.loc of
                         LOC_REGISTER,LOC_CREGISTER:
                           begin
                              emit_reg_ref(A_MOV,S_B,p^.right^.location.register,r);
                              ungetregister(p^.right^.location.register);
                           end;
                         LOC_MEM,LOC_REFERENCE:
                           begin
                              if not(R_EAX in unused) then
                                emit_reg(A_PUSH,S_L,R_EAX);
                              emit_ref_reg(A_MOV,S_B,newreference(p^.right^.location.reference),R_AL);
                              emit_reg_ref(A_MOV,S_B,R_AL,r);

                              if not(R_EAX in unused) then
                                emit_reg(A_POP,S_L,R_EAX);
                              del_reference(p^.right^.location.reference);
                           end
                         else
                           internalerror(20799);
                        end;
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;


    procedure loadansi2short(source,dest : ptree);
      var
         pushed : tpushed;
      begin
         del_reference(dest^.location.reference);
         case source^.location.loc of
           LOC_REFERENCE,LOC_MEM:
             begin
                ungetiftemp(source^.location.reference);
{$IfNDef regallocfix}
                del_reference(source^.location.reference);
                pushusedregisters(pushed,$ff);
                emit_push_mem(source^.location.reference);
{$Else regallocfix}
                 pushusedregisters(pushed,$ff
                   xor ($80 shr byte(source^.location.reference.base))
                   xor ($80 shr byte(source^.location.reference.index)));
                 emit_push_mem(source^.location.reference);
                 del_reference(source^.location.reference);
{$EndIf regallocfix}
             end;
           LOC_REGISTER,LOC_CREGISTER:
             begin
{$IfNDef regallocfix}
                ungetregister32(source^.location.register);
                pushusedregisters(pushed,$ff);
                emit_reg(A_PUSH,S_L,source^.location.register);
{$Else regallocfix}
                 pushusedregisters(pushed, $ff xor ($80 shr byte(source^.location.register)));
                 emit_reg(A_PUSH,S_L,source^.location.register);
                 ungetregister32(source^.location.register);
{$EndIf regallocfix}
             end;
         end;
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest^.location.reference);
         emitcall('FPC_ANSISTR_TO_SHORTSTR');
         popusedregisters(pushed);
         maybe_loadesi;
      end;



{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    type
      tsecondconvproc = procedure(var pto,pfrom : ptree;convtyp : tconverttype);

    procedure second_int_to_int(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        op      : tasmop;
        opsize    : topsize;
        hregister,
        hregister2 : tregister;
        l : pasmlabel;

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
               { we can release the upper register }
               if is_64bitint(pfrom^.resulttype) then
                 ungetregister32(pfrom^.location.registerhigh);
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

            { get op and opsize, handle separate for constants, because
              movz doesn't support constant values }
            if (pfrom^.location.loc=LOC_MEM) and (pfrom^.location.reference.is_immediate) then
             begin
               if is_64bitint(pto^.resulttype) then
                 opsize:=S_L
               else
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

            { do we need a second register for a 64 bit type ? }
            if is_64bitint(pto^.resulttype) then
              begin
                 hregister2:=getregister32;
                 pto^.location.registerhigh:=hregister2;
              end;
            case pto^.resulttype^.size of
             1:
               pto^.location.register:=makereg8(hregister);
             2:
               pto^.location.register:=makereg16(hregister);
             4,8:
               pto^.location.register:=makereg32(hregister);
            end;
            { insert the assembler code }
            if pfrom^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
              emit_reg_reg(op,opsize,pfrom^.location.register,pto^.location.register)
            else
              emit_ref_reg(op,opsize,
                newreference(pfrom^.location.reference),pto^.location.register);

            { do we need a sign extension for int64? }
            if is_64bitint(pto^.resulttype) then
              begin
                 emit_reg_reg(A_XOR,S_L,
                   hregister2,hregister2);
                 if (porddef(pto^.resulttype)^.typ=s64bit) and
                   is_signed(pfrom^.resulttype) then
                   begin
                      getlabel(l);
                      emit_const_reg(A_TEST,S_L,$80000000,makereg32(hregister));
                      emitjmp(C_Z,l);
                      emit_reg(A_NOT,S_L,
                        hregister2);
                      emitlab(l);
                   end;
              end;
          end;
      end;

    procedure second_string_to_string(var pto,pfrom : ptree;convtyp : tconverttype);

      var
         pushed : tpushed;

      begin
         { does anybody know a better solution than this big case statement ? }
         { ok, a proc table would do the job                              }
         case pstringdef(pto^.resulttype)^.string_typ of

            st_shortstring:
              case pstringdef(pfrom^.resulttype)^.string_typ of
                 st_shortstring:
                   begin
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
                      gettempansistringreference(pto^.location.reference);
                      decrstringref(cansistringdef,pto^.location.reference);
                      pushusedregisters(pushed,$ff);
                      emit_push_lea_loc(pfrom^.location,true);
                      emit_push_lea_loc(pto^.location,false);
                      emitcall('FPC_SHORTSTR_TO_ANSISTR');
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


    procedure second_cstring_to_pchar(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        hr : preference;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=getregister32;
         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(pfrom^.location.reference.offset);
               emit_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),
                 pto^.location.register);
             end;
           st_ansistring :
             begin
               if (pfrom^.treetype=stringconstn) and
                  (str_length(pfrom)=0) then
                begin
                  new(hr);
                  reset_reference(hr^);
                  hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  emit_ref_reg(A_LEA,S_L,hr,pto^.location.register);
                end
               else
                emit_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                  pto^.location.register);
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


    procedure second_string_to_chararray(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         l1 : pasmlabel;
         hr : preference;
      begin
         case pstringdef(pfrom^.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(pto^.location.reference.offset);
             end;
           st_ansistring :
             begin
               clear_location(pto^.location);
               pto^.location.loc:=LOC_REFERENCE;
               reset_reference(pto^.location.reference);
               getlabel(l1);
               case pfrom^.location.loc of
                  LOC_CREGISTER,LOC_REGISTER:
                    pto^.location.reference.base:=pfrom^.location.register;
                  LOC_MEM,LOC_REFERENCE:
                    begin
                      pto^.location.reference.base:=getregister32;
                      emit_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                        pto^.location.reference.base);
                      del_reference(pfrom^.location.reference);
                    end;
               end;
               emit_const_reg(A_CMP,S_L,0,pto^.location.reference.base);
               emitjmp(C_NZ,l1);
               new(hr);
               reset_reference(hr^);
               hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
               emit_ref_reg(A_LEA,S_L,hr,pto^.location.reference.base);
               emitlab(l1);
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


    procedure second_array_to_pointer(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
         del_reference(pfrom^.location.reference);
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=getregister32;
         emit_ref_reg(A_LEA,S_L,newreference(pfrom^.location.reference),
           pto^.location.register);
      end;


    procedure second_pointer_to_array(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
        clear_location(pto^.location);
        pto^.location.loc:=LOC_REFERENCE;
        reset_reference(pto^.location.reference);
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
              emit_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                pto^.location.reference.base);
            end;
        end;
      end;


    { generates the code for the type conversion from an array of char }
    { to a string                                                       }
    procedure second_chararray_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         pushed : tpushed;
         l : longint;
      begin
         { calc the length of the array }
         l:=parraydef(pfrom^.resulttype)^.highrange-parraydef(pfrom^.resulttype)^.lowrange+1;
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                        }
         clear_location(pto^.location);
         pto^.location.loc:=LOC_MEM;
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring :
             begin
               if l>255 then
                begin
                  CGMessage(type_e_mismatch);
                  l:=255;
                end;
               gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
               pushusedregisters(pushed,$ff);
               if l>=pto^.resulttype^.size then
                 push_int(pto^.resulttype^.size-1)
               else
                 push_int(l);
               emit_push_lea_loc(pfrom^.location,true);
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHARARRAY_TO_SHORTSTR');
               maybe_loadesi;
               popusedregisters(pushed);
             end;
           st_ansistring :
             begin
               gettempansistringreference(pto^.location.reference);
               decrstringref(cansistringdef,pto^.location.reference);
               release_loc(pfrom^.location);
               pushusedregisters(pushed,$ff);
               push_int(l);
               emitpushreferenceaddr(pfrom^.location.reference);
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHARARRAY_TO_ANSISTR');
               popusedregisters(pushed);
               maybe_loadesi;
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


    procedure second_char_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
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
               gettempansistringreference(pto^.location.reference);
               decrstringref(cansistringdef,pto^.location.reference);
               release_loc(pfrom^.location);
               pushusedregisters(pushed,$ff);
               emit_pushw_loc(pfrom^.location);
               emitpushreferenceaddr(pto^.location.reference);
               emitcall('FPC_CHAR_TO_ANSISTR');
               popusedregisters(pushed);
               maybe_loadesi;
             end;
           else
            internalerror(4179);
        end;
      end;


    procedure second_int_to_real(var pto,pfrom : ptree;convtyp : tconverttype);

      var
         r : preference;
         hregister : tregister;
         l1,l2 : pasmlabel;

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
                 s8bit : emit_reg_reg(A_MOVSX,S_BL,pfrom^.location.register,R_EDI);
                 u8bit : emit_reg_reg(A_MOVZX,S_BL,pfrom^.location.register,R_EDI);
                 s16bit : emit_reg_reg(A_MOVSX,S_WL,pfrom^.location.register,R_EDI);
                 u16bit : emit_reg_reg(A_MOVZX,S_WL,pfrom^.location.register,R_EDI);
                 u32bit,s32bit:
                   hregister:=pfrom^.location.register;
                 u64bit,s64bit:
                   begin
                      emit_reg(A_PUSH,S_L,pfrom^.location.registerhigh);
                      hregister:=pfrom^.location.registerlow;
                   end;
              end;
              ungetregister(pfrom^.location.register);
           end
         else
           begin
              r:=newreference(pfrom^.location.reference);
              case porddef(pfrom^.resulttype)^.typ of
                 s8bit:
                   emit_ref_reg(A_MOVSX,S_BL,r,R_EDI);
                 u8bit:
                   emit_ref_reg(A_MOVZX,S_BL,r,R_EDI);
                 s16bit:
                   emit_ref_reg(A_MOVSX,S_WL,r,R_EDI);
                 u16bit:
                   emit_ref_reg(A_MOVZX,S_WL,r,R_EDI);
                 u32bit,s32bit:
                   emit_ref_reg(A_MOV,S_L,r,R_EDI);
                 u64bit,s64bit:
                   begin
                      inc(r^.offset,4);
                      emit_ref_reg(A_MOV,S_L,r,R_EDI);
                      emit_reg(A_PUSH,S_L,R_EDI);
                      r:=newreference(pfrom^.location.reference);
                      emit_ref_reg(A_MOV,S_L,r,R_EDI);
                   end;
              end;
              del_reference(pfrom^.location.reference);
              ungetiftemp(pfrom^.location.reference);
           end;
         { for 64 bit integers, the high dword is already pushed }
         emit_reg(A_PUSH,S_L,hregister);
         r:=new_reference(R_ESP,0);
         case porddef(pfrom^.resulttype)^.typ of
           u32bit:
             begin
                emit_ref(A_FILD,S_IQ,r);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end;
           s64bit:
             begin
                emit_ref(A_FILD,S_IQ,r);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end;
           u64bit:
             begin
                { unsigned 64 bit ints are harder to handle: }
                { we load bits 0..62 and then check bit 63:  }
                { if it is 1 then we add $80000000 000000000 }
                { as double                                  }
                inc(r^.offset,4);
                emit_ref_reg(A_MOV,S_L,r,R_EDI);
                r:=new_reference(R_ESP,4);
                emit_const_ref(A_AND,S_L,$7fffffff,r);
                emit_const_reg(A_AND,S_L,$80000000,R_EDI);
                r:=new_reference(R_ESP,0);
                emit_ref(A_FILD,S_IQ,r);
                getdatalabel(l1);
                getlabel(l2);
                emitjmp(C_Z,l2);
                consts^.concat(new(pai_label,init(l1)));
                { I got this constant from a test progtram (FK) }
                consts^.concat(new(pai_const,init_32bit(0)));
                consts^.concat(new(pai_const,init_32bit(1138753536)));
                r:=new_reference(R_NO,0);
                r^.symbol:=l1;
                emit_ref(A_FADD,S_FL,r);
                emitlab(l2);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end
           else
             begin
                emit_ref(A_FILD,S_IL,r);
                emit_reg(A_POP,S_L,R_EDI);
             end;
         end;
         inc(fpuvaroffset);
         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    procedure second_real_to_fix(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         rreg : tregister;
         ref : treference;
      begin
         { real must be on fpu stack }
         if (pfrom^.location.loc<>LOC_FPU) then
           emit_ref(A_FLD,S_FL,newreference(pfrom^.location.reference));
         push_int($1f3f);
         push_int(65536);
         reset_reference(ref);
         ref.base:=R_ESP;

         emit_ref(A_FIMUL,S_IL,newreference(ref));

         ref.offset:=4;
         emit_ref(A_FSTCW,S_NO,newreference(ref));

         ref.offset:=6;
         emit_ref(A_FLDCW,S_NO,newreference(ref));

         ref.offset:=0;
         emit_ref(A_FISTP,S_IL,newreference(ref));

         ref.offset:=4;
         emit_ref(A_FLDCW,S_NO,newreference(ref));

         rreg:=getregister32;
         emit_reg(A_POP,S_L,rreg);
         { better than an add on all processors }
         emit_reg(A_POP,S_L,R_EDI);

         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=rreg;
         inc(fpuvaroffset);
      end;


    procedure second_real_to_real(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
         case pfrom^.location.loc of
            LOC_FPU : ;
            LOC_CFPUREGISTER:
              begin
                 pto^.location:=pfrom^.location;
                 exit;
              end;
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


    procedure second_fix_to_real(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        popeax,popebx,popecx,popedx : boolean;
        startreg : tregister;
        hl : pasmlabel;
        r : treference;
      begin
         if (pfrom^.location.loc=LOC_REGISTER) or
            (pfrom^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=pfrom^.location.register;
              ungetregister(startreg);
              popeax:=(startreg<>R_EAX) and not (R_EAX in unused);
              if popeax then
                emit_reg(A_PUSH,S_L,R_EAX);
              { mov eax,eax is removed by emit_reg_reg }
              emit_reg_reg(A_MOV,S_L,startreg,R_EAX);
           end
         else
           begin
              emit_ref_reg(A_MOV,S_L,newreference(
                pfrom^.location.reference),R_EAX);
              del_reference(pfrom^.location.reference);
              startreg:=R_NO;
           end;

         popebx:=(startreg<>R_EBX) and not (R_EBX in unused);
         if popebx then
           emit_reg(A_PUSH,S_L,R_EBX);

         popecx:=(startreg<>R_ECX) and not (R_ECX in unused);
         if popecx then
           emit_reg(A_PUSH,S_L,R_ECX);

         popedx:=(startreg<>R_EDX) and not (R_EDX in unused);
         if popedx then
           emit_reg(A_PUSH,S_L,R_EDX);

         emit_none(A_CDQ,S_NO);
         emit_reg_reg(A_XOR,S_L,R_EDX,R_EAX);
         emit_reg_reg(A_MOV,S_L,R_EAX,R_EBX);
         emit_reg_reg(A_SUB,S_L,R_EDX,R_EAX);
         getlabel(hl);
         emitjmp(C_Z,hl);
         emit_const_reg(A_RCL,S_L,1,R_EBX);
         emit_reg_reg(A_BSR,S_L,R_EAX,R_EDX);
         emit_const_reg(A_MOV,S_B,32,R_CL);
         emit_reg_reg(A_SUB,S_B,R_DL,R_CL);
         emit_reg_reg(A_SHL,S_L,R_CL,R_EAX);
         emit_const_reg(A_ADD,S_W,1007,R_DX);
         emit_const_reg(A_SHL,S_W,5,R_DX);
         emit_const_reg_reg(A_SHLD,S_W,11,R_DX,R_BX);
         emit_const_reg_reg(A_SHLD,S_L,20,R_EAX,R_EBX);

         emit_const_reg(A_SHL,S_L,20,R_EAX);
         emitlab(hl);
         { better than an add on all processors }
         emit_reg(A_PUSH,S_L,R_EBX);
         emit_reg(A_PUSH,S_L,R_EAX);

         reset_reference(r);
         r.base:=R_ESP;
         emit_ref(A_FLD,S_FL,newreference(r));
         emit_const_reg(A_ADD,S_L,8,R_ESP);
         if popedx then
           emit_reg(A_POP,S_L,R_EDX);
         if popecx then
           emit_reg(A_POP,S_L,R_ECX);
         if popebx then
           emit_reg(A_POP,S_L,R_EBX);
         if popeax then
           emit_reg(A_POP,S_L,R_EAX);

         clear_location(pto^.location);
         pto^.location.loc:=LOC_FPU;
      end;


    procedure second_cord_to_pointer(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
        { this can't happend, because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;


    procedure second_int_to_fix(var pto,pfrom : ptree;convtyp : tconverttype);
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
                s8bit : emit_ref_reg(A_MOVSX,S_BL,newreference(pfrom^.location.reference),
                  hregister);
                u8bit : emit_ref_reg(A_MOVZX,S_BL,newreference(pfrom^.location.reference),
                  hregister);
                s16bit : emit_ref_reg(A_MOVSX,S_WL,newreference(pfrom^.location.reference),
                  hregister);
                u16bit : emit_ref_reg(A_MOVZX,S_WL,newreference(pfrom^.location.reference),
                  hregister);
                u32bit,s32bit : emit_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                  hregister);
                {!!!! u32bit }
              end;
           end;
         emit_const_reg(A_SHL,S_L,16,hregister);

         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         pto^.location.register:=hregister;
      end;


    procedure second_proc_to_procvar(var pto,pfrom : ptree;convtyp : tconverttype);
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
             emit_ref_reg(A_LEA,S_L,
               newreference(pfrom^.location.reference),pto^.location.register);
          end;
      end;


    procedure second_bool_to_int(var pto,pfrom : ptree;convtyp : tconverttype);
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
         secondpass(pfrom);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;
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
      LOC_REFERENCE : emit_ref_reg(op,opsize,
                        newreference(pfrom^.location.reference),pto^.location.register);
       LOC_REGISTER,
      LOC_CREGISTER : begin
                      { remove things like movb %al,%al }
                        if pfrom^.location.register<>pto^.location.register then
                          emit_reg_reg(op,opsize,
                            pfrom^.location.register,pto^.location.register);
                      end;
          LOC_FLAGS : begin
                        emit_flag2reg(pfrom^.location.resflags,pto^.location.register);
                      end;
           LOC_JUMP : begin
                        getlabel(hlabel);
                        emitlab(truelabel);
                        emit_const_reg(A_MOV,newsize,1,pto^.location.register);
                        emitjmp(C_None,hlabel);
                        emitlab(falselabel);
                        emit_reg_reg(A_XOR,newsize,pto^.location.register,
                          pto^.location.register);
                        emitlab(hlabel);
                      end;
         else
           internalerror(10061);
         end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure second_int_to_bool(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        hregister : tregister;
        flags     : tresflags;
        opsize    : topsize;
      begin
         clear_location(pto^.location);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (pto^.explizit) and
            (pfrom^.resulttype^.size=pto^.resulttype^.size) and
            (pfrom^.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(pto^.location,pfrom^.location);
              exit;
           end;
         pto^.location.loc:=LOC_REGISTER;
         del_reference(pfrom^.location.reference);
         opsize:=def_opsize(pfrom^.resulttype);
         case pfrom^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                hregister:=def_getreg(pfrom^.resulttype);
                emit_ref_reg(A_MOV,opsize,
                  newreference(pfrom^.location.reference),hregister);
                emit_reg_reg(A_OR,opsize,hregister,hregister);
                flags:=F_NE;
              end;
            LOC_FLAGS :
              begin
                hregister:=getregister32;
                flags:=pfrom^.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=pfrom^.location.register;
                emit_reg_reg(A_OR,opsize,hregister,hregister);
                flags:=F_NE;
              end;
            else
              internalerror(10062);
         end;
         case pto^.resulttype^.size of
          1 : pto^.location.register:=makereg8(hregister);
          2 : pto^.location.register:=makereg16(hregister);
          4 : pto^.location.register:=makereg32(hregister);
         else
          internalerror(10064);
         end;
         emit_flag2reg(flags,pto^.location.register);
      end;


    procedure second_load_smallset(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        href : treference;
        pushedregs : tpushed;
      begin
        href.symbol:=nil;
        pushusedregisters(pushedregs,$ff);
        gettempofsizereference(32,href);
        emitpushreferenceaddr(pfrom^.location.reference);
        emitpushreferenceaddr(href);
        emitcall('FPC_SET_LOAD_SMALL');
        maybe_loadesi;
        popusedregisters(pushedregs);
        clear_location(pto^.location);
        pto^.location.loc:=LOC_MEM;
        pto^.location.reference:=href;
      end;


    procedure second_ansistring_to_pchar(var pto,pfrom : ptree;convtyp : tconverttype);
      var
         l1 : pasmlabel;
         hr : preference;
      begin
         clear_location(pto^.location);
         pto^.location.loc:=LOC_REGISTER;
         getlabel(l1);
         case pfrom^.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              pto^.location.register:=pfrom^.location.register;
            LOC_MEM,LOC_REFERENCE:
              begin
                pto^.location.register:=getregister32;
                emit_ref_reg(A_MOV,S_L,newreference(pfrom^.location.reference),
                  pto^.location.register);
                del_reference(pfrom^.location.reference);
              end;
         end;
         emit_const_reg(A_CMP,S_L,0,pto^.location.register);
         emitjmp(C_NZ,l1);
         new(hr);
         reset_reference(hr^);
         hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
         emit_ref_reg(A_LEA,S_L,hr,pto^.location.register);
         emitlab(l1);
      end;


    procedure second_pchar_to_string(var pto,pfrom : ptree;convtyp : tconverttype);
      var
        pushed : tpushed;
      begin
         case pstringdef(pto^.resulttype)^.string_typ of
           st_shortstring:
             begin
                pto^.location.loc:=LOC_REFERENCE;
                gettempofsizereference(pto^.resulttype^.size,pto^.location.reference);
                pushusedregisters(pushed,$ff);
                case pfrom^.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                        emit_reg(A_PUSH,S_L,pfrom^.location.register);
                        ungetregister32(pfrom^.location.register);
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                        emit_push_mem(pfrom^.location.reference);
                        del_reference(pfrom^.location.reference);
                     end;
                end;
                emitpushreferenceaddr(pto^.location.reference);
                emitcall('FPC_PCHAR_TO_SHORTSTR');
                maybe_loadesi;
                popusedregisters(pushed);
             end;
           st_ansistring:
             begin
                pto^.location.loc:=LOC_REFERENCE;
                gettempansistringreference(pto^.location.reference);
                decrstringref(cansistringdef,pto^.location.reference);
                case pfrom^.location.loc of
                  LOC_REFERENCE,LOC_MEM:
                    begin
{$IfNDef regallocfix}
                      del_reference(pfrom^.location.reference);
                      pushusedregisters(pushed,$ff);
                      emit_push_mem(pfrom^.location.reference);
{$Else regallocfix}
                      pushusedregisters(pushed,$ff
                        xor ($80 shr byte(pfrom^.location.reference.base))
                        xor ($80 shr byte(pfrom^.location.reference.index)));
                      emit_push_mem(pfrom^.location.reference);
                      del_reference(pfrom^.location.reference);
{$EndIf regallocfix}
                    end;
                  LOC_REGISTER,LOC_CREGISTER:
                    begin
{$IfNDef regallocfix}
                      ungetregister32(pfrom^.location.register);
                      pushusedregisters(pushed,$ff);
                      emit_reg(A_PUSH,S_L,pfrom^.location.register);
{$Else regallocfix}
                      pushusedregisters(pushed, $ff xor ($80 shr byte(pfrom^.location.register)));
                      emit_reg(A_PUSH,S_L,pfrom^.location.register);
                      ungetregister32(pfrom^.location.register);
{$EndIf regallocfix}
                   end;
                end;
                emitpushreferenceaddr(pto^.location.reference);
                emitcall('FPC_PCHAR_TO_ANSISTR');
                maybe_loadesi;
                popusedregisters(pushed);
             end;
         else
          begin
            internalerror(12121);
          end;
         end;
      end;


    procedure second_nothing(var pto,pfrom : ptree;convtyp : tconverttype);
      begin
      end;


{****************************************************************************
                             SecondTypeConv
****************************************************************************}

    procedure secondtypeconv(var p : ptree);
      const
         secondconvert : array[tconverttype] of tsecondconvproc = (
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
           second_int_to_bool,
           second_bool_to_int, { bool_to_bool }
           second_bool_to_int,
           second_real_to_real,
           second_int_to_real,
           second_int_to_fix,
           second_real_to_fix,
           second_fix_to_real,
           second_proc_to_procvar,
           second_nothing, {arrayconstructor_to_set}
           second_load_smallset,
           second_cord_to_pointer
         );
{$ifdef TESTOBJEXT2}
      var
         r : preference;
         nillabel : plabel;
{$endif TESTOBJEXT2}
      begin

         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                 }

         if not(p^.convtyp in [tc_bool_2_int,tc_bool_2_bool]) then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
              if codegenerror then
               exit;
           end;
         { the second argument only is for maybe_range_checking !}
         secondconvert[p^.convtyp](p,p^.left,p^.convtyp);

{$ifdef TESTOBJEXT2}
                  { Check explicit conversions to objects pointers !! }
                     if p^.explizit and
                        (p^.resulttype^.deftype=pointerdef) and
                        (ppointerdef(p^.resulttype)^.definition^.deftype=objectdef) and not
                        (pobjectdef(ppointerdef(p^.resulttype)^.definition)^.isclass) and
                        ((pobjectdef(ppointerdef(p^.resulttype)^.definition)^.options and oo_hasvmt)<>0) and
                        (cs_check_range in aktlocalswitches) then
                       begin
                          new(r);
                          reset_reference(r^);
                          if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                           r^.base:=p^.location.register
                          else
                            begin
                               emit_mov_loc_reg(p^.location,R_EDI);
                               r^.base:=R_EDI;
                            end;
                          { NIL must be accepted !! }
                          emit_reg_reg(A_OR,S_L,r^.base,r^.base);
                          getlabel(nillabel);
                          emitjmp(C_E,nillabel);
                          { this is one point where we need vmt_offset (PM) }
                          r^.offset:= pobjectdef(ppointerdef(p^.resulttype)^.definition)^.vmt_offset;
                          emit_ref_reg(A_MOV,S_L,r,R_EDI);
                          emit_sym(A_PUSH,S_L,
                            newasmsymbol(pobjectdef(ppointerdef(p^.resulttype)^.definition)^.vmt_mangledname));
                          emit_reg(A_PUSH,S_L,R_EDI);
                          emitcall('FPC_CHECK_OBJECT_EXT');
                          emitlab(nillabel);
                       end;
{$endif TESTOBJEXT2}
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
                 emit_reg(A_PUSH,
                   S_L,p^.left^.location.register);
                 ungetregister32(p^.left^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(p^.left^.location.reference));
                 del_reference(p^.left^.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg(A_PUSH,
                   S_L,p^.right^.location.register);
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(p^.right^.location.reference));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_IS');
         emit_reg_reg(A_OR,S_B,R_AL,R_AL);
         popusedregisters(pushed);
         maybe_loadesi;
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
              emit_reg(A_PUSH,
                S_L,p^.left^.location.register);
            LOC_MEM,LOC_REFERENCE:
              emit_ref(A_PUSH,
                S_L,newreference(p^.left^.location.reference));
            else internalerror(100);
         end;

         { we doesn't modifiy the left side, we check only the type }
         set_location(p^.location,p^.left^.location);

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg(A_PUSH,
                   S_L,p^.right^.location.register);
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(p^.right^.location.reference));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_AS');
         { restore register, this restores automatically the }
         { result                                           }
         popusedregisters(pushed);
         maybe_loadesi;
      end;


end.
{
  $Log$
  Revision 1.98  2000-01-07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.97  1999/12/22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.96  1999/12/21 11:49:51  pierre
   * array of char to short string bug fixed

  Revision 1.95  1999/12/01 12:42:31  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.94  1999/11/29 22:15:25  pierre
   * fix for internalerror(12) on ord(booleanarray[1])

  Revision 1.93  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.92  1999/10/25 10:32:43  peter
    * ansistring 2 chararray support
    * optimized ansitring 2 pchar

  Revision 1.91  1999/10/22 14:36:04  peter
    * fixed esi reload with as

  Revision 1.90  1999/10/06 08:32:00  peter
    * fixed empty const ansistring 2 pchar

  Revision 1.89  1999/09/26 21:30:15  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.88  1999/09/26 13:26:04  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.87  1999/09/23 21:20:37  peter
    * fixed temp allocation for short->ansi

  Revision 1.86  1999/09/01 09:42:13  peter
    * update for new push_lea_loc

  Revision 1.85  1999/08/19 13:08:46  pierre
   * emit_??? used

  Revision 1.84  1999/08/05 14:58:03  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.83  1999/08/04 13:45:19  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.82  1999/08/04 00:22:43  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.81  1999/08/03 22:02:36  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.80  1999/08/01 23:36:38  florian
    * some changes to compile the new code generator

  Revision 1.79  1999/07/22 09:37:34  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.78  1999/07/05 20:13:07  peter
    * removed temp defines

  Revision 1.77  1999/07/04 16:37:08  florian
    + qword/int64 -> floating point type cast

  Revision 1.76  1999/06/28 22:29:10  florian
    * qword division fixed
    + code for qword/int64 type casting added:
      range checking isn't implemented yet

  Revision 1.75  1999/05/31 20:35:46  peter
    * ansistring fixes, decr_ansistr called after all temp ansi reuses

  Revision 1.74  1999/05/27 19:44:09  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.73  1999/05/18 21:58:26  florian
    * fixed some bugs related to temp. ansistrings and functions results
      which return records/objects/arrays which need init/final.

}
