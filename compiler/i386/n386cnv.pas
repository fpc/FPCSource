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
      node,ncnv,ncgcnv,types;

    type
       ti386typeconvnode = class(tcgtypeconvnode)
         protected
          procedure second_int_to_int;override;
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
          procedure pass_2;override;
          procedure second_call_helper(c : tconverttype);
       end;

implementation

   uses
      verbose,systems,
      symconst,symdef,aasm,
      cgbase,temp_gen,pass_2,
      ncon,ncal,
      cpubase,
      cga,tgcpu,n386util;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure ti386typeconvnode.second_int_to_int;
      var
        op      : tasmop;
        opsize    : topsize;
        hregister,
        hregister2 : tregister;

      begin
        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          emitrangecheck(left,resulttype.def);

        { is the result size smaller ? }
        if resulttype.def.size<left.resulttype.def.size then
          begin
            { only need to set the new size of a register }
            if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             begin
               case resulttype.def.size of
                1 : location.register:=makereg8(left.location.register);
                2 : location.register:=makereg16(left.location.register);
                4 : location.register:=makereg32(left.location.register);
               end;
               { we can release the upper register }
               if is_64bitint(left.resulttype.def) then
                 ungetregister32(left.location.registerhigh);
             end;
          end

        { is the result size bigger ? }
        else if resulttype.def.size>left.resulttype.def.size then
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
               if is_64bitint(resulttype.def) then
                 opsize:=S_L
               else
                 opsize:=def_opsize(resulttype.def);
               op:=A_MOV;
             end
            else
             begin
               opsize:=def2def_opsize(left.resulttype.def,resulttype.def);
               if opsize in [S_B,S_W,S_L] then
                op:=A_MOV
               else
                if is_signed(left.resulttype.def) then
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
            if is_64bitint(resulttype.def) then
              begin
                 hregister2:=getregister32;
                 location.registerhigh:=hregister2;
              end;
            case resulttype.def.size of
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
            if is_64bitint(resulttype.def) then
              { special case for constants (JM) }
              if is_constintnode(left) then
                begin
                  if tordconstnode(left).value >= 0 then
                    emit_reg_reg(A_XOR,S_L,
                      hregister2,hregister2)
                  else
                    emit_const_reg(A_MOV,S_L,longint($ffffffff),hregister2);
                end
              else
                begin
                  if (torddef(resulttype.def).typ=s64bit) and
                     is_signed(left.resulttype.def) then
                    begin
                      emit_reg_reg(A_MOV,S_L,location.register,hregister2);
                      emit_const_reg(A_SAR,S_L,31,hregister2);
                    end
                   else
                     emit_reg_reg(A_XOR,S_L,hregister2,hregister2);
                end;
          end;
      end;


    procedure ti386typeconvnode.second_int_to_real;

      var
         r : preference;
         hregister : tregister;
         l1,l2 : tasmlabel;

      begin
         { for u32bit a solution is to push $0 and to load a comp }
         { does this first, it destroys maybe EDI }
         hregister:=R_EDI;
         if torddef(left.resulttype.def).typ=u32bit then
            push_int(0);
         if (left.location.loc=LOC_REGISTER) or
            (left.location.loc=LOC_CREGISTER) then
           begin
              if not (torddef(left.resulttype.def).typ in [u32bit,s32bit,u64bit,s64bit]) then
                getexplicitregister32(R_EDI);
              case torddef(left.resulttype.def).typ of
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
              getexplicitregister32(R_EDI);
              case torddef(left.resulttype.def).typ of
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
         if hregister = R_EDI then
           ungetregister32(R_EDI);
         r:=new_reference(R_ESP,0);
         case torddef(left.resulttype.def).typ of
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
                getexplicitregister32(R_EDI);
                emit_ref_reg(A_MOV,S_L,r,R_EDI);
                r:=new_reference(R_ESP,4);
                emit_const_ref(A_AND,S_L,$7fffffff,r);
                emit_const_reg(A_TEST,S_L,longint($80000000),R_EDI);
                ungetregister32(R_EDI);
                r:=new_reference(R_ESP,0);
                emit_ref(A_FILD,S_IQ,r);
                getdatalabel(l1);
                getlabel(l2);
                emitjmp(C_Z,l2);
                Consts.concat(Tai_label.Create(l1));
                { I got this constant from a test progtram (FK) }
                Consts.concat(Tai_const.Create_32bit(0));
                Consts.concat(Tai_const.Create_32bit(1138753536));
                r:=new_reference(R_NO,0);
                r^.symbol:=l1;
                emit_ref(A_FADD,S_FL,r);
                emitlab(l2);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end
           else
             begin
                emit_ref(A_FILD,S_IL,r);
                getexplicitregister32(R_EDI);
                emit_reg(A_POP,S_L,R_EDI);
                ungetregister32(R_EDI);
             end;
         end;
         inc(fpuvaroffset);
         clear_location(location);
         location.loc:=LOC_FPU;
      end;


    procedure ti386typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        resflags  : tresflags;
        opsize    : topsize;
        pref      : preference;
      begin
         clear_location(location);
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(location,left.location);
              exit;
           end;
         location.loc:=LOC_REGISTER;
         del_location(left.location);
         opsize:=def_opsize(left.resulttype.def);
         case left.location.loc of
            LOC_MEM,LOC_REFERENCE :
              begin
                if is_64bitint(left.resulttype.def) then
                 begin
                   hregister:=getregister32;
                   emit_ref_reg(A_MOV,opsize,
                     newreference(left.location.reference),hregister);
                   pref:=newreference(left.location.reference);
                   inc(pref^.offset,4);
                   emit_reg_ref(A_OR,opsize,
                     hregister,pref);
                 end
                else
                 begin
                   hregister:=def_getreg(left.resulttype.def);
                   emit_ref_reg(A_MOV,opsize,
                     newreference(left.location.reference),hregister);
                   emit_reg_reg(A_OR,opsize,hregister,hregister);
                 end;
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
         case resulttype.def.size of
          1 : location.register:=makereg8(hregister);
          2 : location.register:=makereg16(hregister);
          4 : location.register:=makereg32(hregister);
         else
          internalerror(10064);
         end;
         emit_flag2reg(resflags,location.register);
      end;


{****************************************************************************
                           TI386TYPECONVNODE
****************************************************************************}

    procedure ti386typeconvnode.second_call_helper(c : tconverttype);

      const
         secondconvert : array[tconverttype] of pointer = (
           @second_nothing, {equal}
           @second_nothing, {not_possible}
           @second_nothing, {second_string_to_string, handled in resulttype pass }
           @second_char_to_string,
           @second_nothing, {char_to_charray}
           @second_nothing, { pchar_to_string, handled in resulttype pass }
           @second_nothing, {cchar_to_pchar}
           @second_cstring_to_pchar,
           @second_ansistring_to_pchar,
           @second_string_to_chararray,
           @second_nothing, { chararray_to_string, handled in resulttype pass }
           @second_array_to_pointer,
           @second_pointer_to_array,
           @second_int_to_int,
           @second_int_to_bool,
           @second_bool_to_int, { bool_to_bool }
           @second_bool_to_int,
           @second_real_to_real,
           @second_int_to_real,
           @second_proc_to_procvar,
           @second_nothing, { arrayconstructor_to_set }
           @second_nothing, { second_load_smallset, handled in first pass }
           @second_cord_to_pointer,
           @second_nothing, { interface 2 string }
           @second_nothing, { interface 2 guid   }
           @second_class_to_intf,
           @second_char_to_char,
           @second_nothing,  { normal_2_smallset }
           @second_nothing   { dynarray_2_openarray }
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
                        (p^.resulttype.def.deftype=pointerdef) and
                        (tpointerdef(p^.resulttype.def).definition.deftype=objectdef) and not
                        (tobjectdef(tpointerdef(p^.resulttype.def).definition).isclass) and
                        ((tobjectdef(tpointerdef(p^.resulttype.def).definition).options and oo_hasvmt)<>0) and
                        (cs_check_range in aktlocalswitches) then
                       begin
                          new(r);
                          reset_reference(r^);
                          if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                           r^.base:=p^.location.register
                          else
                            begin
                               getexplicitregister32(R_EDI);
                               emit_mov_loc_reg(p^.location,R_EDI);
                               r^.base:=R_EDI;
                            end;
                          { NIL must be accepted !! }
                          emit_reg_reg(A_OR,S_L,r^.base,r^.base);
                          ungetregister32(R_EDI);
                          getlabel(nillabel);
                          emitjmp(C_E,nillabel);
                          { this is one point where we need vmt_offset (PM) }
                          r^.offset:= tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_offset;
                          getexplicitregister32(R_EDI);
                          emit_ref_reg(A_MOV,S_L,r,R_EDI);
                          emit_sym(A_PUSH,S_L,
                            newasmsymbol(tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_mangledname));
                          emit_reg(A_PUSH,S_L,R_EDI);
                          ungetregister32(R_EDI);
                          emitcall('FPC_CHECK_OBJECT_EXT');
                          emitlab(nillabel);
                       end;
{$endif TESTOBJEXT2}
      end;


begin
   ctypeconvnode:=ti386typeconvnode;
end.
{
  $Log$
  Revision 1.28  2001-12-11 08:14:17  jonas
    * part of my fix for dynarray -> open array conversion, forgot to
      commit yesterday :(

  Revision 1.27  2001/11/02 23:24:12  jonas
    * fixed web bug 1665 (allow char to chararray type conversion) ("merged")

  Revision 1.26  2001/09/30 21:28:34  peter
    * int64->boolean fixed

  Revision 1.25  2001/09/30 16:12:47  jonas
    - removed unnecessary i386 pass_2 of as- and isnode and added dummy generic ones

  Revision 1.24  2001/09/29 21:32:47  jonas
    * almost all second pass typeconvnode helpers are now processor independent
    * fixed converting boolean to int64/qword
    * fixed register allocation bugs which could cause internalerror 10
    * isnode and asnode are completely processor indepent now as well
    * fpc_do_as now returns its class argument (necessary to be able to use it
      properly with compilerproc)

  Revision 1.23  2001/09/03 13:27:42  jonas
    * compilerproc implementation of set addition/substraction/...
    * changed the declaration of some set helpers somewhat to accomodate the
      above change
    * i386 still uses the old code for comparisons of sets, because its
      helpers return the results in the flags
    * dummy tc_normal_2_small_set type conversion because I need the original
      resulttype of the set add nodes
    NOTE: you have to start a cycle with 1.0.5!

  Revision 1.22  2001/08/29 19:49:03  jonas
    * some fixes in compilerprocs for chararray to string conversions
    * conversion from string to chararray is now also done via compilerprocs

  Revision 1.21  2001/08/28 13:24:47  jonas
    + compilerproc implementation of most string-related type conversions
    - removed all code from the compiler which has been replaced by
      compilerproc implementations (using {$ifdef hascompilerproc} is not
      necessary in the compiler)

  Revision 1.20  2001/08/26 13:36:57  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.19  2001/08/01 21:44:59  peter
    * fixed empty pwidechar register allocation

  Revision 1.18  2001/07/30 20:59:29  peter
    * m68k updates from v10 merged

  Revision 1.17  2001/07/16 13:19:08  jonas
    * fixed allocation of register before release in second_cstring_to_pchar

  Revision 1.16  2001/07/08 21:00:17  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.15  2001/05/08 21:06:33  florian
    * some more support for widechars commited especially
      regarding type casting and constants

  Revision 1.14  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/02 21:20:36  peter
    * resulttype rewrite

  Revision 1.12  2001/01/08 21:45:11  peter
    * internalerror for string to chararray

  Revision 1.11  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/12/07 17:19:46  jonas
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

  Revision 1.9  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.8  2000/11/29 00:30:46  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.7  2000/11/16 15:27:48  jonas
    * fixed web bug 1242

  Revision 1.6  2000/11/13 11:30:56  florian
    * some bugs with interfaces and NIL fixed

  Revision 1.5  2000/11/12 23:24:14  florian
    * interfaces are basically running

  Revision 1.4  2000/11/11 16:00:10  jonas
    * optimize converting of 8/16/32 bit constants to 64bit ones

  Revision 1.3  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.1  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

}
