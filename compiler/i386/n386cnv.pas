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
      cginfo,cgbase,pass_2,
      ncon,ncal,
      cpubase,
      cgobj,cga,tgobj,rgobj,rgcpu,n386util;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure ti386typeconvnode.second_int_to_int;
      var
        newsize,
        oldsize    : tcgsize;
      begin
        newsize:=def_cgsize(resulttype.def);
        oldsize:=def_cgsize(left.resulttype.def);

        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          cg.g_rangecheck(exprasmlist,left,resulttype.def);

        { is the result size smaller ? }
        if resulttype.def.size<left.resulttype.def.size then
          begin
            { reuse the left location by default }
            location_copy(location,left.location);
            location.size:=newsize;

            { update the register to use }
            if (location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
             begin
               if oldsize in [OS_64,OS_S64] then
                begin
                  { we can release the upper register }
                  rg.ungetregisterint(exprasmlist,location.registerhigh);
                  location.registerhigh:=R_NO;
                end;
               case newsize of
                 OS_8,OS_S8 :
                   location.register:=makereg8(location.register);
                 OS_16,OS_S16 :
                   location.register:=makereg16(location.register);
                 OS_32,OS_S32 :
                   location.register:=makereg32(location.register);
               end;
             end;
          end

        { is the result size bigger ? }
        else if resulttype.def.size>left.resulttype.def.size then
          begin
            { we need to load the value in a register }
            location_copy(location,left.location);
            location_force_reg(location,newsize,false);
          end
        else
          begin
            { no special loading is required, reuse current location }
            location_copy(location,left.location);
            location.size:=newsize;
          end;
      end;


    procedure ti386typeconvnode.second_int_to_real;

      var
         r : treference;
         hregister : tregister;
         l1,l2 : tasmlabel;

      begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
         { for u32bit a solution is to push $0 and to load a comp }
         { does this first, it destroys maybe EDI }
         hregister:=R_EDI;
         if torddef(left.resulttype.def).typ=u32bit then
            push_int(0);

         case left.location.loc of
           LOC_REGISTER,
           LOC_CREGISTER :
             begin
                if not (torddef(left.resulttype.def).typ in [u32bit,s32bit,u64bit,s64bit]) then
                  rg.getexplicitregisterint(exprasmlist,R_EDI);
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
              end;
            LOC_REFERENCE,
            LOC_CREFERENCE :
              begin
                r:=left.location.reference;
                rg.getexplicitregisterint(exprasmlist,R_EDI);
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
                        inc(r.offset,4);
                        emit_ref_reg(A_MOV,S_L,r,R_EDI);
                        emit_reg(A_PUSH,S_L,R_EDI);
                        r:=left.location.reference;
                        emit_ref_reg(A_MOV,S_L,r,R_EDI);
                     end;
                end;
             end;
           else
             internalerror(2002032218);
         end;
         location_release(exprasmlist,left.location);
         location_freetemp(exprasmlist,left.location);

         { for 64 bit integers, the high dword is already pushed }
         emit_reg(A_PUSH,S_L,hregister);
         if hregister = R_EDI then
           rg.ungetregisterint(exprasmlist,R_EDI);
         reference_reset_base(r,R_ESP,0);
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
                inc(r.offset,4);
                rg.getexplicitregisterint(exprasmlist,R_EDI);
                emit_ref_reg(A_MOV,S_L,r,R_EDI);
                reference_reset_base(r,R_ESP,4);
                emit_const_ref(A_AND,S_L,$7fffffff,r);
                emit_const_reg(A_TEST,S_L,longint($80000000),R_EDI);
                rg.ungetregisterint(exprasmlist,R_EDI);
                reference_reset_base(r,R_ESP,0);
                emit_ref(A_FILD,S_IQ,r);
                getdatalabel(l1);
                getlabel(l2);
                emitjmp(C_Z,l2);
                Consts.concat(Tai_label.Create(l1));
                { I got this constant from a test progtram (FK) }
                Consts.concat(Tai_const.Create_32bit(0));
                Consts.concat(Tai_const.Create_32bit(1138753536));
                reference_reset_symbol(r,l1,0);
                emit_ref(A_FADD,S_FL,r);
                emitlab(l2);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end
           else
             begin
                emit_ref(A_FILD,S_IL,r);
                rg.getexplicitregisterint(exprasmlist,R_EDI);
                emit_reg(A_POP,S_L,R_EDI);
                rg.ungetregisterint(exprasmlist,R_EDI);
             end;
         end;
         inc(trgcpu(rg).fpuvaroffset);
         location.register:=R_ST;
      end;


    procedure ti386typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        leftopsize,
        opsize    : topsize;
        pref      : treference;
        hlabel,oldtruelabel,oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(left);
         if codegenerror then
          exit;
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;
         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         location_release(exprasmlist,left.location);

         opsize:=def_opsize(resulttype.def);
         leftopsize:=def_opsize(left.resulttype.def);
         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE :
              begin
                if is_64bitint(left.resulttype.def) then
                 begin
                   hregister:=rg.getregisterint(exprasmlist);
                   emit_ref_reg(A_MOV,S_L,left.location.reference,hregister);
                   pref:=left.location.reference;
                   inc(pref.offset,4);
                   emit_ref_reg(A_OR,S_L,pref,hregister);
                 end
                else
                 begin
                   hregister:=def_getreg(left.resulttype.def);
                   emit_ref_reg(A_MOV,leftopsize,left.location.reference,hregister);
                 end;
              end;
            LOC_CONSTANT :
              begin
                if is_64bitint(left.resulttype.def) then
                 begin
                   hregister:=def_getreg(left.resulttype.def);
                   emit_const_reg(A_MOV,S_L,left.location.valuelow,hregister);
                   emit_const_reg(A_OR,S_L,left.location.valuehigh,hregister);
                 end
                else
                 begin
                   hregister:=def_getreg(left.resulttype.def);
                   emit_const_reg(A_MOV,leftopsize,left.location.value,hregister);
                 end;
              end;
            LOC_FLAGS :
              begin
                hregister:=def_getreg(left.resulttype.def);
                emit_flag2reg(left.location.resflags,hregister);
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hregister:=left.location.register;
              end;
            LOC_JUMP :
              begin
                hregister:=def_getreg(left.resulttype.def);
                getlabel(hlabel);
                cg.a_label(exprasmlist,truelabel);
                cg.a_load_const_reg(exprasmlist,def_cgsize(left.resulttype.def),1,hregister);
                cg.a_jmp_cond(exprasmlist,OC_NONE,hlabel);
                cg.a_label(exprasmlist,falselabel);
                cg.a_load_const_reg(exprasmlist,def_cgsize(left.resulttype.def),0,hregister);
                cg.a_label(exprasmlist,hlabel);
              end;
            else
              internalerror(10062);
         end;
         emit_reg(A_NEG,leftopsize,hregister);
         case opsize of
           S_B :
             location.register:=makereg8(hregister);
           S_W :
             location.register:=makereg16(hregister);
           S_L :
             location.register:=makereg32(hregister);
           else
            internalerror(10064);
         end;
         emit_reg_reg(A_SBB,opsize,location.register,location.register);
         emit_reg(A_NEG,opsize,location.register);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
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
           @second_bool_to_bool,
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
        { the boolean routines can be called with LOC_JUMP and
          call secondpass themselves in the helper }
        if not(convtype in [tc_bool_2_int,tc_bool_2_bool,tc_int_2_bool]) then
         begin
           secondpass(left);
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
                               rg.getexplicitregisterint(exprasmlist,R_EDI);
                               emit_mov_loc_reg(p^.location,R_EDI);
                               r^.base:=R_EDI;
                            end;
                          { NIL must be accepted !! }
                          emit_reg_reg(A_OR,S_L,r^.base,r^.base);
                          rg.ungetregisterint(exprasmlist,R_EDI);
                          getlabel(nillabel);
                          emitjmp(C_E,nillabel);
                          { this is one point where we need vmt_offset (PM) }
                          r^.offset:= tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_offset;
                          rg.getexplicitregisterint(exprasmlist,R_EDI);
                          emit_ref_reg(A_MOV,S_L,r,R_EDI);
                          emit_sym(A_PUSH,S_L,
                            newasmsymbol(tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_mangledname));
                          emit_reg(A_PUSH,S_L,R_EDI);
                          rg.ungetregister32(exprasmlist,R_EDI);
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
  Revision 1.33  2002-04-04 19:06:10  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.32  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.31  2002/03/31 20:26:38  jonas
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

  Revision 1.30  2002/03/04 19:10:13  peter
    * removed compiler warnings

  Revision 1.29  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.28  2001/12/11 08:14:17  jonas
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
<<<<<<< n386cnv.pas
      compilerproc implementations (using "$ifdef hascompilerproc" is not
=======
      compilerproc implementations (using $ifdef hascompilerproc is not
>>>>>>> 1.30
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
