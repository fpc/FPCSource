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
unit n386cnv;

{$i defines.inc}

interface

    uses
      node,ncnv,types;

    type
       ti386typeconvnode = class(ttypeconvnode)
          procedure second_int_to_int;virtual;
          procedure second_string_to_string;virtual;
          procedure second_cstring_to_pchar;virtual;
          procedure second_string_to_chararray;virtual;
          procedure second_array_to_pointer;virtual;
          procedure second_pointer_to_array;virtual;
          procedure second_chararray_to_string;virtual;
          procedure second_char_to_string;virtual;
          procedure second_int_to_real;virtual;
          procedure second_real_to_fix;virtual;
          procedure second_real_to_real;virtual;
          procedure second_fix_to_real;virtual;
          procedure second_cord_to_pointer;virtual;
          procedure second_int_to_fix;virtual;
          procedure second_proc_to_procvar;virtual;
          procedure second_bool_to_int;virtual;
          procedure second_int_to_bool;virtual;
          procedure second_load_smallset;virtual;
          procedure second_ansistring_to_pchar;virtual;
          procedure second_pchar_to_string;virtual;
          procedure second_nothing;virtual;
          procedure pass_2;override;
          procedure second_call_helper(c : tconverttype);
       end;

       ti386asnode = class(tasnode)
          procedure pass_2;override;
       end;

       ti386isnode = class(tisnode)
          procedure pass_2;override;
       end;

implementation

   uses
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,
      hcodegen,temp_gen,pass_2,pass_1,
      ncon,ncal,
      cpubase,cpuasm,
      cgai386,tgeni386,n386util;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure ti386typeconvnode.second_int_to_int;
      var
        op      : tasmop;
        opsize    : topsize;
        hregister,
        hregister2 : tregister;
        l : pasmlabel;

      begin
        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          emitrangecheck(left,resulttype);

        { is the result size smaller ? }
        if resulttype^.size<left.resulttype^.size then
          begin
            { only need to set the new size of a register }
            if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             begin
               case resulttype^.size of
                1 : location.register:=makereg8(left.location.register);
                2 : location.register:=makereg16(left.location.register);
                4 : location.register:=makereg32(left.location.register);
               end;
               { we can release the upper register }
               if is_64bitint(left.resulttype) then
                 ungetregister32(left.location.registerhigh);
             end;
          end

        { is the result size bigger ? }
        else if resulttype^.size>left.resulttype^.size then
          begin
            { remove reference }
            if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
              begin
                del_reference(left.location.reference);
                { we can do this here as we need no temp inside }
                ungetiftemp(left.location.reference);
              end;

            { get op and opsize, handle separate for constants, because
              movz doesn't support constant values }
            if (left.location.loc=LOC_MEM) and (left.location.reference.is_immediate) then
             begin
               if is_64bitint(resulttype) then
                 opsize:=S_L
               else
                 opsize:=def_opsize(resulttype);
               op:=A_MOV;
             end
            else
             begin
               opsize:=def2def_opsize(left.resulttype,resulttype);
               if opsize in [S_B,S_W,S_L] then
                op:=A_MOV
               else
                if is_signed(left.resulttype) then
                 op:=A_MOVSX
                else
                 op:=A_MOVZX;
             end;
            { load the register we need }
            if left.location.loc<>LOC_REGISTER then
              hregister:=getregister32
            else
              hregister:=left.location.register;

            { set the correct register size and location }
            clear_location(location);
            location.loc:=LOC_REGISTER;

            { do we need a second register for a 64 bit type ? }
            if is_64bitint(resulttype) then
              begin
                 hregister2:=getregister32;
                 location.registerhigh:=hregister2;
              end;
            case resulttype^.size of
             1:
               location.register:=makereg8(hregister);
             2:
               location.register:=makereg16(hregister);
             4,8:
               location.register:=makereg32(hregister);
            end;
            { insert the assembler code }
            if left.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
              emit_reg_reg(op,opsize,left.location.register,location.register)
            else
              emit_ref_reg(op,opsize,
                newreference(left.location.reference),location.register);

            { do we need a sign extension for int64? }
            if is_64bitint(resulttype) then
              begin
                 emit_reg_reg(A_XOR,S_L,
                   hregister2,hregister2);
                 if (porddef(resulttype)^.typ=s64bit) and
                   is_signed(left.resulttype) then
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

    procedure ti386typeconvnode.second_string_to_string;

      var
         pushed : tpushed;
         regs_to_push: byte;

      begin
         { does anybody know a better solution than this big case statement ? }
         { ok, a proc table would do the job                              }
         case pstringdef(resulttype)^.string_typ of

            st_shortstring:
              case pstringdef(left.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      gettempofsizereference(resulttype^.size,location.reference);
                      copyshortstring(location.reference,left.location.reference,
                        pstringdef(resulttype)^.len,false,true);
{                      done by copyshortstring now (JM)          }
{                      del_reference(left.location.reference); }
                      ungetiftemp(left.location.reference);
                   end;
                 st_longstring:
                   begin
                      {!!!!!!!}
                      internalerror(8888);
                   end;
                 st_ansistring:
                   begin
                      gettempofsizereference(resulttype^.size,location.reference);
                      loadansi2short(left,self);
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
              case pstringdef(left.resulttype)^.string_typ of
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
              case pstringdef(left.resulttype)^.string_typ of
                 st_shortstring:
                   begin
                      clear_location(location);
                      location.loc:=LOC_REFERENCE;
                      gettempansistringreference(location.reference);
                      decrstringref(cansistringdef,location.reference);
                      { We don't need the source regs anymore (JM) }
                      regs_to_push := $ff;
                      remove_non_regvars_from_loc(left.location,regs_to_push);
                      pushusedregisters(pushed,regs_to_push);
                      release_loc(left.location);
                      emit_push_lea_loc(left.location,true);
                      emit_push_lea_loc(location,false);
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
              case pstringdef(left.resulttype)^.string_typ of
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


    procedure ti386typeconvnode.second_cstring_to_pchar;
      var
        hr : preference;
      begin
         clear_location(location);
         location.loc:=LOC_REGISTER;
         location.register:=getregister32;
         case pstringdef(left.resulttype)^.string_typ of
           st_shortstring :
             begin
               inc(left.location.reference.offset);
               emit_ref_reg(A_LEA,S_L,newreference(left.location.reference),
                 location.register);
             end;
           st_ansistring :
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  new(hr);
                  reset_reference(hr^);
                  hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  emit_ref_reg(A_LEA,S_L,hr,location.register);
                end
               else
                emit_ref_reg(A_MOV,S_L,newreference(left.location.reference),
                  location.register);
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


    procedure ti386typeconvnode.second_string_to_chararray;
      var
         pushedregs: tpushed;
         //l1 : pasmlabel;
         //hr : preference;
         arrsize, strtype: longint;
         regstopush: byte;
      begin
         with parraydef(resulttype)^ do
           arrsize := highrange-lowrange+1;

         if (left.nodetype = stringconstn) and
            { left.length+1 since there's always a terminating #0 character (JM) }
            (tstringconstnode(left).len+1 >= arrsize) and
            (pstringdef(left.resulttype)^.string_typ=st_shortstring) then
           begin
             inc(location.reference.offset);
             exit;
           end;
         clear_location(location);
         location.loc := LOC_REFERENCE;
         gettempofsizereference(arrsize,location.reference);

         regstopush := $ff;
         remove_non_regvars_from_loc(left.location,regstopush);
         pushusedregisters(pushedregs,regstopush);

         emit_push_lea_loc(location,false);

         case pstringdef(left.resulttype)^.string_typ of
           st_shortstring :
             begin
               { 0 means shortstring }
               strtype := 0;
               del_reference(left.location.reference);
               emit_push_lea_loc(left.location,true);
               ungetiftemp(left.location.reference);
             end;
           st_ansistring :
             begin
               { 1 means ansistring }
               strtype := 1;
               case left.location.loc of
                  LOC_CREGISTER,LOC_REGISTER:
                    begin
                      ungetregister(left.location.register);
                      emit_push_loc(left.location);
                    end;
                  LOC_MEM,LOC_REFERENCE:
                    begin
                      del_reference(left.location.reference);
                      emit_push_loc(left.location);
                      ungetiftemp(left.location.reference);
                    end;
               end;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               { 2 means longstring, but still needs support in FPC_STR_TO_CHARARRAY,
                 which is in i386.inc and/or generic.inc (JM) }
               strtype := 2;

               internalerror(8888);
             end;
           st_widestring:
             begin
               {!!!!!!!}
               { 3 means widestring, but still needs support in FPC_STR_TO_CHARARRAY,
                 which is in i386.inc and/or generic.inc (JM) }
               strtype := 3;
               internalerror(8888);
             end;
         end;
         push_int(arrsize);
         push_int(strtype);
         emitcall('FPC_STR_TO_CHARARRAY');
         popusedregisters(pushedregs);
      end;


    procedure ti386typeconvnode.second_array_to_pointer;
      begin
         del_reference(left.location.reference);
         clear_location(location);
         location.loc:=LOC_REGISTER;
         location.register:=getregister32;
         emit_ref_reg(A_LEA,S_L,newreference(left.location.reference),
           location.register);
      end;


    procedure ti386typeconvnode.second_pointer_to_array;
      begin
        clear_location(location);
        location.loc:=LOC_REFERENCE;
        reset_reference(location.reference);
        case left.location.loc of
          LOC_REGISTER :
            location.reference.base:=left.location.register;
          LOC_CREGISTER :
            begin
              location.reference.base:=getregister32;
              emit_reg_reg(A_MOV,S_L,left.location.register,location.reference.base);
            end
         else
            begin
              del_reference(left.location.reference);
              location.reference.base:=getregister32;
              emit_ref_reg(A_MOV,S_L,newreference(left.location.reference),
                location.reference.base);
            end;
        end;
      end;


    { generates the code for the type conversion from an array of char }
    { to a string                                                       }
    procedure ti386typeconvnode.second_chararray_to_string;
      var
         pushed : tpushed;
         regstopush: byte;
         l : longint;
      begin
         { calc the length of the array }
         l:=parraydef(left.resulttype)^.highrange-parraydef(left.resulttype)^.lowrange+1;
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                        }
         clear_location(location);
         location.loc:=LOC_MEM;
         case pstringdef(resulttype)^.string_typ of
           st_shortstring :
             begin
               if l>255 then
                begin
                  CGMessage(type_e_mismatch);
                  l:=255;
                end;
               gettempofsizereference(resulttype^.size,location.reference);
               { we've also to release the registers ... }
               { Yes, but before pushusedregisters since that one resets unused! }
               { This caused web bug 1073 (JM)                                   }
               regstopush := $ff;
               remove_non_regvars_from_loc(left.location,regstopush);
               pushusedregisters(pushed,regstopush);
               if l>=resulttype^.size then
                 push_int(resulttype^.size-1)
               else
                 push_int(l);
               { ... here only the temp. location is released }
               emit_push_lea_loc(left.location,true);
               del_reference(left.location.reference);
               emitpushreferenceaddr(location.reference);
               emitcall('FPC_CHARARRAY_TO_SHORTSTR');
               maybe_loadesi;
               popusedregisters(pushed);
             end;
           st_ansistring :
             begin
               gettempansistringreference(location.reference);
               decrstringref(cansistringdef,location.reference);
               regstopush := $ff;
               remove_non_regvars_from_loc(left.location,regstopush);
               pushusedregisters(pushed,regstopush);
               push_int(l);
               emitpushreferenceaddr(left.location.reference);
               release_loc(left.location);
               emitpushreferenceaddr(location.reference);
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


    procedure ti386typeconvnode.second_char_to_string;
      var
        pushed : tpushed;

      begin
         clear_location(location);
         location.loc:=LOC_MEM;
         case pstringdef(resulttype)^.string_typ of
           st_shortstring :
             begin
               gettempofsizereference(256,location.reference);
               loadshortstring(left,self);
             end;
           st_ansistring :
             begin
               gettempansistringreference(location.reference);
               decrstringref(cansistringdef,location.reference);
               release_loc(left.location);
               pushusedregisters(pushed,$ff);
               emit_pushw_loc(left.location);
               emitpushreferenceaddr(location.reference);
               emitcall('FPC_CHAR_TO_ANSISTR');
               popusedregisters(pushed);
               maybe_loadesi;
             end;
           else
            internalerror(4179);
        end;
      end;


    procedure ti386typeconvnode.second_int_to_real;

      var
         r : preference;
         hregister : tregister;
         l1,l2 : pasmlabel;

      begin
         { for u32bit a solution is to push $0 and to load a comp }
         { does this first, it destroys maybe EDI }
         hregister:=R_EDI;
         if porddef(left.resulttype)^.typ=u32bit then
            push_int(0);
         if (left.location.loc=LOC_REGISTER) or
            (left.location.loc=LOC_CREGISTER) then
           begin
{$ifndef noAllocEdi}
              if not (porddef(left.resulttype)^.typ in [u32bit,s32bit,u64bit,s64bit]) then
                getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              case porddef(left.resulttype)^.typ of
                 s8bit : emit_reg_reg(A_MOVSX,S_BL,left.location.register,R_EDI);
                 u8bit : emit_reg_reg(A_MOVZX,S_BL,left.location.register,R_EDI);
                 s16bit : emit_reg_reg(A_MOVSX,S_WL,left.location.register,R_EDI);
                 u16bit : emit_reg_reg(A_MOVZX,S_WL,left.location.register,R_EDI);
                 u32bit,s32bit:
                   hregister:=left.location.register;
                 u64bit,s64bit:
                   begin
                      emit_reg(A_PUSH,S_L,left.location.registerhigh);
                      hregister:=left.location.registerlow;
                   end;
              end;
              ungetregister(left.location.register);
           end
         else
           begin
              r:=newreference(left.location.reference);
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              case porddef(left.resulttype)^.typ of
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
                      r:=newreference(left.location.reference);
                      emit_ref_reg(A_MOV,S_L,r,R_EDI);
                   end;
              end;
              del_reference(left.location.reference);
              ungetiftemp(left.location.reference);
           end;
         { for 64 bit integers, the high dword is already pushed }
         emit_reg(A_PUSH,S_L,hregister);
{$ifndef noAllocEdi}
         if hregister = R_EDI then
           ungetregister32(R_EDI);
{$endif noAllocEdi}
         r:=new_reference(R_ESP,0);
         case porddef(left.resulttype)^.typ of
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
{$ifndef noAllocEdi}
                getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                emit_ref_reg(A_MOV,S_L,r,R_EDI);
                r:=new_reference(R_ESP,4);
                emit_const_ref(A_AND,S_L,$7fffffff,r);
                emit_const_reg(A_TEST,S_L,$80000000,R_EDI);
{$ifndef noAllocEdi}
                ungetregister32(R_EDI);
{$endif noAllocEdi}
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
{$ifndef noAllocEdi}
                getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                emit_reg(A_POP,S_L,R_EDI);
{$ifndef noAllocEdi}
                ungetregister32(R_EDI);
{$endif noAllocEdi}
             end;
         end;
         inc(fpuvaroffset);
         clear_location(location);
         location.loc:=LOC_FPU;
      end;


    procedure ti386typeconvnode.second_real_to_fix;
      var
         rreg : tregister;
         ref : treference;
      begin
         { real must be on fpu stack }
         if (left.location.loc<>LOC_FPU) then
           emit_ref(A_FLD,S_FL,newreference(left.location.reference));
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
{$ifndef noAllocEdi}
         getexplicitregister32(R_EDI);
{$endif noAllocEdi}
         emit_reg(A_POP,S_L,R_EDI);
{$ifndef noAllocEdi}
         ungetregister32(R_EDI);
{$endif noAllocEdi}

         clear_location(location);
         location.loc:=LOC_REGISTER;
         location.register:=rreg;
         inc(fpuvaroffset);
      end;


    procedure ti386typeconvnode.second_real_to_real;
      begin
         case left.location.loc of
            LOC_FPU : ;
            LOC_CFPUREGISTER:
              begin
                 location:=left.location;
                 exit;
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 floatload(pfloatdef(left.resulttype)^.typ,
                   left.location.reference);
                 { we have to free the reference }
                 del_reference(left.location.reference);
              end;
         end;
         clear_location(location);
         location.loc:=LOC_FPU;
      end;


    procedure ti386typeconvnode.second_fix_to_real;
      var
        popeax,popebx,popecx,popedx : boolean;
        startreg : tregister;
        hl : pasmlabel;
        r : treference;
      begin
         if (left.location.loc=LOC_REGISTER) or
            (left.location.loc=LOC_CREGISTER) then
           begin
              startreg:=left.location.register;
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
                left.location.reference),R_EAX);
              del_reference(left.location.reference);
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

         clear_location(location);
         location.loc:=LOC_FPU;
      end;


    procedure ti386typeconvnode.second_cord_to_pointer;
      begin
        { this can't happend, because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;


    procedure ti386typeconvnode.second_int_to_fix;
      var
         hregister : tregister;
      begin
         if (left.location.loc=LOC_REGISTER) then
           hregister:=left.location.register
         else if (left.location.loc=LOC_CREGISTER) then
           hregister:=getregister32
         else
           begin
              del_reference(left.location.reference);
              hregister:=getregister32;
              case porddef(left.resulttype)^.typ of
                s8bit : emit_ref_reg(A_MOVSX,S_BL,newreference(left.location.reference),
                  hregister);
                u8bit : emit_ref_reg(A_MOVZX,S_BL,newreference(left.location.reference),
                  hregister);
                s16bit : emit_ref_reg(A_MOVSX,S_WL,newreference(left.location.reference),
                  hregister);
                u16bit : emit_ref_reg(A_MOVZX,S_WL,newreference(left.location.reference),
                  hregister);
                u32bit,s32bit : emit_ref_reg(A_MOV,S_L,newreference(left.location.reference),
                  hregister);
                {!!!! u32bit }
              end;
           end;
         emit_const_reg(A_SHL,S_L,16,hregister);

         clear_location(location);
         location.loc:=LOC_REGISTER;
         location.register:=hregister;
      end;


    procedure ti386typeconvnode.second_proc_to_procvar;
      begin
        { method pointer ? }
        if assigned(tcallnode(left).left) then
          begin
             set_location(location,left.location);
          end
        else
          begin
             clear_location(location);
             location.loc:=LOC_REGISTER;
             location.register:=getregister32;
             del_reference(left.location.reference);
             emit_ref_reg(A_LEA,S_L,
               newreference(left.location.reference),location.register);
          end;
      end;


    procedure ti386typeconvnode.second_bool_to_int;
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
         secondpass(left);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explizit in flags) and
            (left.resulttype^.size=resulttype^.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(location,left.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;
         clear_location(location);
         location.loc:=LOC_REGISTER;
         del_reference(left.location.reference);
         case left.resulttype^.size of
          1 : begin
                case resulttype^.size of
                 1 : opsize:=S_B;
                 2 : opsize:=S_BW;
                 4 : opsize:=S_BL;
                end;
              end;
          2 : begin
                case resulttype^.size of
                 1 : begin
                       if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        left.location.register:=reg16toreg8(left.location.register);
                       opsize:=S_B;
                     end;
                 2 : opsize:=S_W;
                 4 : opsize:=S_WL;
                end;
              end;
          4 : begin
                case resulttype^.size of
                 1 : begin
                       if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        left.location.register:=reg32toreg8(left.location.register);
                       opsize:=S_B;
                     end;
                 2 : begin
                       if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        left.location.register:=reg32toreg16(left.location.register);
                       opsize:=S_W;
                     end;
                 4 : opsize:=S_L;
                end;
              end;
         end;
         if opsize in [S_B,S_W,S_L] then
          op:=A_MOV
         else
          if is_signed(resulttype) then
           op:=A_MOVSX
          else
           op:=A_MOVZX;
         hregister:=getregister32;
         case resulttype^.size of
          1 : begin
                location.register:=reg32toreg8(hregister);
                newsize:=S_B;
              end;
          2 : begin
                location.register:=reg32toreg16(hregister);
                newsize:=S_W;
              end;
          4 : begin
                location.register:=hregister;
                newsize:=S_L;
              end;
         else
          internalerror(10060);
         end;

         case left.location.loc of
            LOC_MEM,
      LOC_REFERENCE : emit_ref_reg(op,opsize,
                        newreference(left.location.reference),location.register);
       LOC_REGISTER,
      LOC_CREGISTER : begin
                      { remove things like movb %al,%al }
                        if left.location.register<>location.register then
                          emit_reg_reg(op,opsize,
                            left.location.register,location.register);
                      end;
          LOC_FLAGS : begin
                        emit_flag2reg(left.location.resflags,location.register);
                      end;
           LOC_JUMP : begin
                        getlabel(hlabel);
                        emitlab(truelabel);
                        emit_const_reg(A_MOV,newsize,1,location.register);
                        emitjmp(C_None,hlabel);
                        emitlab(falselabel);
                        emit_reg_reg(A_XOR,newsize,location.register,
                          location.register);
                        emitlab(hlabel);
                      end;
         else
           internalerror(10061);
         end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure ti386typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        resflags  : tresflags;
        opsize    : topsize;
      begin
         clear_location(location);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explizit in flags) and
            (left.resulttype^.size=resulttype^.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(location,left.location);
              exit;
           end;
         location.loc:=LOC_REGISTER;
         del_reference(left.location.reference);
         opsize:=def_opsize(left.resulttype);
         case left.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                hregister:=def_getreg(left.resulttype);
                emit_ref_reg(A_MOV,opsize,
                  newreference(left.location.reference),hregister);
                emit_reg_reg(A_OR,opsize,hregister,hregister);
                resflags:=F_NE;
              end;
            LOC_FLAGS :
              begin
                hregister:=getregister32;
                resflags:=left.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=left.location.register;
                emit_reg_reg(A_OR,opsize,hregister,hregister);
                resflags:=F_NE;
              end;
            else
              internalerror(10062);
         end;
         case resulttype^.size of
          1 : location.register:=makereg8(hregister);
          2 : location.register:=makereg16(hregister);
          4 : location.register:=makereg32(hregister);
         else
          internalerror(10064);
         end;
         emit_flag2reg(resflags,location.register);
      end;


    procedure ti386typeconvnode.second_load_smallset;
      var
        href : treference;
        pushedregs : tpushed;
      begin
        href.symbol:=nil;
        pushusedregisters(pushedregs,$ff);
        gettempofsizereference(32,href);
        emitpushreferenceaddr(left.location.reference);
        emitpushreferenceaddr(href);
        emitcall('FPC_SET_LOAD_SMALL');
        maybe_loadesi;
        popusedregisters(pushedregs);
        clear_location(location);
        location.loc:=LOC_MEM;
        location.reference:=href;
      end;


    procedure ti386typeconvnode.second_ansistring_to_pchar;
      var
         l1 : pasmlabel;
         hr : preference;
      begin
         clear_location(location);
         location.loc:=LOC_REGISTER;
         getlabel(l1);
         case left.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              location.register:=left.location.register;
            LOC_MEM,LOC_REFERENCE:
              begin
                location.register:=getregister32;
                emit_ref_reg(A_MOV,S_L,newreference(left.location.reference),
                  location.register);
                del_reference(left.location.reference);
              end;
         end;
         emit_const_reg(A_CMP,S_L,0,location.register);
         emitjmp(C_NZ,l1);
         new(hr);
         reset_reference(hr^);
         hr^.symbol:=newasmsymbol('FPC_EMPTYCHAR');
         emit_ref_reg(A_LEA,S_L,hr,location.register);
         emitlab(l1);
      end;


    procedure ti386typeconvnode.second_pchar_to_string;
      var
        pushed : tpushed;
        regs_to_push: byte;
      begin
         case pstringdef(resulttype)^.string_typ of
           st_shortstring:
             begin
                location.loc:=LOC_REFERENCE;
                gettempofsizereference(resulttype^.size,location.reference);
                pushusedregisters(pushed,$ff);
                case left.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                        emit_reg(A_PUSH,S_L,left.location.register);
                        ungetregister32(left.location.register);
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                       { Now release the registers (see cgai386.pas:     }
                       { loadansistring for more info on the order) (JM) }
                        del_reference(left.location.reference);
                        emit_push_mem(left.location.reference);
                     end;
                end;
                emitpushreferenceaddr(location.reference);
                emitcall('FPC_PCHAR_TO_SHORTSTR');
                maybe_loadesi;
                popusedregisters(pushed);
             end;
           st_ansistring:
             begin
                location.loc:=LOC_REFERENCE;
                gettempansistringreference(location.reference);
                decrstringref(cansistringdef,location.reference);
                { Find out which regs have to be pushed (JM) }
                regs_to_push := $ff;
                remove_non_regvars_from_loc(left.location,regs_to_push);
                pushusedregisters(pushed,regs_to_push);
                case left.location.loc of
                  LOC_REFERENCE,LOC_MEM:
                    begin
                      { Now release the registers (see cgai386.pas:     }
                      { loadansistring for more info on the order) (JM) }
                      del_reference(left.location.reference);
                      emit_push_mem(left.location.reference);
                    end;
                  LOC_REGISTER,LOC_CREGISTER:
                    begin
                       { Now release the registers (see cgai386.pas:     }
                       { loadansistring for more info on the order) (JM) }
                      emit_reg(A_PUSH,S_L,left.location.register);
                      ungetregister32(left.location.register);
                   end;
                end;
                emitpushreferenceaddr(location.reference);
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


    procedure ti386typeconvnode.second_nothing;
      begin
      end;


{****************************************************************************
                           TI386TYPECONVNODE
****************************************************************************}

    procedure ti386typeconvnode.second_call_helper(c : tconverttype);

      const
         secondconvert : array[tconverttype] of pointer = (
           @ti386typeconvnode.second_nothing, {equal}
           @ti386typeconvnode.second_nothing, {not_possible}
           @ti386typeconvnode.second_string_to_string,
           @ti386typeconvnode.second_char_to_string,
           @ti386typeconvnode.second_pchar_to_string,
           @ti386typeconvnode.second_nothing, {cchar_to_pchar}
           @ti386typeconvnode.second_cstring_to_pchar,
           @ti386typeconvnode.second_ansistring_to_pchar,
           @ti386typeconvnode.second_string_to_chararray,
           @ti386typeconvnode.second_chararray_to_string,
           @ti386typeconvnode.second_array_to_pointer,
           @ti386typeconvnode.second_pointer_to_array,
           @ti386typeconvnode.second_int_to_int,
           @ti386typeconvnode.second_int_to_bool,
           @ti386typeconvnode.second_bool_to_int, { bool_to_bool }
           @ti386typeconvnode.second_bool_to_int,
           @ti386typeconvnode.second_real_to_real,
           @ti386typeconvnode.second_int_to_real,
           @ti386typeconvnode.second_int_to_fix,
           @ti386typeconvnode.second_real_to_fix,
           @ti386typeconvnode.second_fix_to_real,
           @ti386typeconvnode.second_proc_to_procvar,
           @ti386typeconvnode.second_nothing, {arrayconstructor_to_set}
           @ti386typeconvnode.second_load_smallset,
           @ti386typeconvnode.second_cord_to_pointer
         );
      type
         tprocedureofobject = procedure of object;

      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;

      begin
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=secondconvert[c];
         r.obj:=self;
         tprocedureofobject(r){$ifdef FPC}();{$endif FPC}
      end;

    procedure ti386typeconvnode.pass_2;
{$ifdef TESTOBJEXT2}
      var
         r : preference;
         nillabel : plabel;
{$endif TESTOBJEXT2}
      begin

         { this isn't good coding, I think tc_bool_2_int, shouldn't be }
         { type conversion (FK)                                 }

         if not(convtype in [tc_bool_2_int,tc_bool_2_bool]) then
           begin
              secondpass(left);
              set_location(location,left.location);
              if codegenerror then
               exit;
           end;
         second_call_helper(convtype);

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
{$ifndef noAllocEdi}
                               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_mov_loc_reg(p^.location,R_EDI);
                               r^.base:=R_EDI;
                            end;
                          { NIL must be accepted !! }
                          emit_reg_reg(A_OR,S_L,r^.base,r^.base);
{$ifndef noAllocEdi}
                          ungetregister32(R_EDI);
{$endif noAllocEdi}
                          getlabel(nillabel);
                          emitjmp(C_E,nillabel);
                          { this is one point where we need vmt_offset (PM) }
                          r^.offset:= pobjectdef(ppointerdef(p^.resulttype)^.definition)^.vmt_offset;
{$ifndef noAllocEdi}
                          getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                          emit_ref_reg(A_MOV,S_L,r,R_EDI);
                          emit_sym(A_PUSH,S_L,
                            newasmsymbol(pobjectdef(ppointerdef(p^.resulttype)^.definition)^.vmt_mangledname));
                          emit_reg(A_PUSH,S_L,R_EDI);
{$ifndef noAllocEdi}
                          ungetregister32(R_EDI);
{$endif noAllocEdi}
                          emitcall('FPC_CHECK_OBJECT_EXT');
                          emitlab(nillabel);
                       end;
{$endif TESTOBJEXT2}
      end;


{*****************************************************************************
                             TI386ISNODE
*****************************************************************************}

    procedure ti386isnode.pass_2;
      var
         pushed : tpushed;

      begin
         { save all used registers }
         pushusedregisters(pushed,$ff);
         secondpass(left);
         clear_location(location);
         location.loc:=LOC_FLAGS;
         location.resflags:=F_NE;

         { push instance to check: }
         case left.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg(A_PUSH,
                   S_L,left.location.register);
                 ungetregister32(left.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(left.location.reference));
                 del_reference(left.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(right);
         case right.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg(A_PUSH,
                   S_L,right.location.register);
                 ungetregister32(right.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(right.location.reference));
                 del_reference(right.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_IS');
         emit_reg_reg(A_OR,S_B,R_AL,R_AL);
         popusedregisters(pushed);
         maybe_loadesi;
      end;


{*****************************************************************************
                             TI386ASNODE
*****************************************************************************}

    procedure ti386asnode.pass_2;
      var
         pushed : tpushed;
      begin
         secondpass(left);
         { save all used registers }
         pushusedregisters(pushed,$ff);

         { push instance to check: }
         case left.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              emit_reg(A_PUSH,
                S_L,left.location.register);
            LOC_MEM,LOC_REFERENCE:
              emit_ref(A_PUSH,
                S_L,newreference(left.location.reference));
            else internalerror(100);
         end;

         { we doesn't modifiy the left side, we check only the type }
         set_location(location,left.location);

         { generate type checking }
         secondpass(right);
         case right.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 emit_reg(A_PUSH,
                   S_L,right.location.register);
                 ungetregister32(right.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 emit_ref(A_PUSH,
                   S_L,newreference(right.location.reference));
                 del_reference(right.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('FPC_DO_AS');
         { restore register, this restores automatically the }
         { result                                           }
         popusedregisters(pushed);
         maybe_loadesi;
      end;

begin
   ctypeconvnode:=ti386typeconvnode;
   cisnode:=ti386isnode;
   casnode:=ti386asnode;
end.
{
  $Log$
  Revision 1.1  2000-10-15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.1  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

}
