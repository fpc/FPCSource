{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for type converting nodes

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
unit nppccnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,defcmp;

    type
       tppctypeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
          function first_int_to_real: tnode; override;
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          procedure second_int_to_real;override;
          procedure second_real_to_real;override;
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
       end;

implementation

   uses
      verbose,globals,systems,
      symconst,symdef,aasmbase,aasmtai,
      defutil,
      cgbase,pass_1,pass_2,
      ncon,ncal,
      ncgutil,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tppctypeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin
        { converting a 64bit integer to a float requires a helper }
        if is_64bitint(left.resulttype.def) or
                is_currency(left.resulttype.def) then
          begin
            { hack to avoid double division by 10000, as it's       }
            { already done by resulttypepass.resulttype_int_to_real }
            if is_currency(left.resulttype.def) then
              left.resulttype := s64inttype;
            if is_signed(left.resulttype.def) then
              fname := 'fpc_int64_to_double'
            else
              fname := 'fpc_qword_to_double';
            result := ccallnode.createintern(fname,ccallparanode.create(
              left,nil));
            left:=nil;
            firstpass(result);
            exit;
          end
        else
          { other integers are supposed to be 32 bit }
          begin
            if is_signed(left.resulttype.def) then
              inserttypeconv(left,s32inttype)
            else
              inserttypeconv(left,u32inttype);
            firstpass(left);
          end;
        result := nil;
        if registersfpu<1 then
          registersfpu:=1;
        expectloc:=LOC_FPUREGISTER;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure tppctypeconvnode.second_int_to_real;

      type
        tdummyarray = packed array[0..7] of byte;

{$ifdef VER1_0}
      var
        dummy1, dummy2: int64;
{$else VER1_0}
      const
         dummy1: int64 = $4330000080000000;
         dummy2: int64 = $4330000000000000;
{$endif VER1_0}

      var
        tempconst: trealconstnode;
        ref: treference;
        valuereg, tempreg, leftreg, tmpfpureg: tregister;
        size: tcgsize;
        signed, valuereg_is_scratch: boolean;
      begin
{$ifdef VER1_0}
        dummy1 := (int64(1) shl 31) or (int64($43300000) shl 32);
        dummy2 := int64($43300000) shl 32;
{$endif VER1_0}

        valuereg_is_scratch := false;
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));

        { the code here comes from the PowerPC Compiler Writer's Guide }

        { * longint to double                               }
        { addis R0,R0,0x4330  # R0 = 0x43300000             }
        { stw R0,disp(R1)     # store upper half            }
        { xoris R3,R3,0x8000  # flip sign bit               }
        { stw R3,disp+4(R1)   # store lower half            }
        { lfd FR1,disp(R1)    # float load double of value  }
        { fsub FR1,FR1,FR2    # subtract 0x4330000080000000 }

        { * cardinal to double                              }
        { addis R0,R0,0x4330  # R0 = 0x43300000             }
        { stw R0,disp(R1)     # store upper half            }
        { stw R3,disp+4(R1)   # store lower half            }
        { lfd FR1,disp(R1)    # float load double of value  }
        { fsub FR1,FR1,FR2    # subtract 0x4330000000000000 }
        tg.Gettemp(exprasmlist,8,tt_normal,ref);

        signed := is_signed(left.resulttype.def);

        { we need a certain constant for the conversion, so create it here }
        if signed then
          tempconst :=
            crealconstnode.create(double(tdummyarray(dummy1)),
            pbestrealtype^)
        else
          tempconst :=
            crealconstnode.create(double(tdummyarray(dummy2)),
            pbestrealtype^);

        resulttypepass(tempconst);
        firstpass(tempconst);
        secondpass(tempconst);
        if (tempconst.location.loc <> LOC_CREFERENCE) or
           { has to be handled by a helper }
           is_64bitint(left.resulttype.def) then
          internalerror(200110011);

        case left.location.loc of
          LOC_REGISTER:
            begin
              leftreg := left.location.register;
              valuereg := leftreg;
            end;
          LOC_CREGISTER:
            begin
              leftreg := left.location.register;
              if signed then
                begin
                  valuereg := cg.getintregister(exprasmlist,OS_INT);
                  valuereg_is_scratch := true;
                end
              else
                valuereg := leftreg;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              leftreg := cg.getintregister(exprasmlist,OS_INT);
              valuereg := leftreg;
              valuereg_is_scratch := true;
              if signed then
                size := OS_S32
              else
                size := OS_32;
              cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                size,left.location.reference,leftreg);
            end
          else
            internalerror(200110012);
         end;
         tempreg := cg.getintregister(exprasmlist,OS_INT);
         exprasmlist.concat(taicpu.op_reg_const(A_LIS,tempreg,$4330));
         cg.a_load_reg_ref(exprasmlist,OS_32,OS_32,tempreg,ref);
         cg.ungetregister(exprasmlist,tempreg);
         if signed then
           exprasmlist.concat(taicpu.op_reg_reg_const(A_XORIS,valuereg,
             { xoris expects a unsigned 16 bit int (FK) }
             leftreg,$8000));
         inc(ref.offset,4);
         cg.a_load_reg_ref(exprasmlist,OS_32,OS_32,valuereg,ref);
         dec(ref.offset,4);
         if (valuereg_is_scratch) then
           cg.ungetregister(exprasmlist,valuereg);

         if (left.location.loc = LOC_REGISTER) or
            ((left.location.loc = LOC_CREGISTER) and
             not signed) then
           cg.ungetregister(exprasmlist,leftreg)
         else
           cg.ungetregister(exprasmlist,valuereg);

         tmpfpureg := cg.getfpuregister(exprasmlist,OS_F64);
         cg.a_loadfpu_ref_reg(exprasmlist,OS_F64,tempconst.location.reference,
           tmpfpureg);
         tempconst.free;

         location.register := cg.getfpuregister(exprasmlist,OS_F64);
         exprasmlist.concat(taicpu.op_reg_ref(A_LFD,location.register,
           ref));

         tg.ungetiftemp(exprasmlist,ref);

         exprasmlist.concat(taicpu.op_reg_reg_reg(A_FSUB,location.register,
           location.register,tmpfpureg));
         cg.ungetregister(exprasmlist,tmpfpureg);

         { work around bug in some PowerPC processors }
         if (tfloatdef(resulttype.def).typ = s32real) then
           exprasmlist.concat(taicpu.op_reg_reg(A_FRSP,location.register,
             location.register));
       end;


     procedure tppctypeconvnode.second_real_to_real;
       begin
          inherited second_real_to_real;
          { work around bug in some powerpc processors where doubles aren't }
          { properly converted to singles                                   }
          if (tfloatdef(left.resulttype.def).typ = s64real) and
             (tfloatdef(resulttype.def).typ = s32real) then
            exprasmlist.concat(taicpu.op_reg_reg(A_FRSP,location.register,
              location.register));
       end;



    procedure tppctypeconvnode.second_int_to_bool;
      var
        hreg1,
        hreg2    : tregister;
        href     : treference;
        resflags : tresflags;
        opsize   : tcgsize;
        hlabel, oldtruelabel, oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         if codegenerror then
          exit;

         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explicit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              exit;
           end;

         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         opsize := def_cgsize(left.resulttype.def);
         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  begin
                    reference_release(exprasmlist,left.location.reference);
                    hreg1:=cg.getintregister(exprasmlist,OS_INT);
                    if left.location.size in [OS_64,OS_S64] then
                      begin
                        cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,
                         left.location.reference,hreg1);
                        hreg2:=cg.getintregister(exprasmlist,OS_INT);
                        href:=left.location.reference;
                        inc(href.offset,4);
                        cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,
                          href,hreg2);
                        cg.ungetregister(exprasmlist,hreg2);
                        cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hreg1,hreg2,hreg1);
                      end
                    else
                      cg.a_load_ref_reg(exprasmlist,opsize,opsize,
                        left.location.reference,hreg1);
                  end
                else
                  begin
                     if left.location.size in [OS_64,OS_S64] then
                       begin
                          location_release(exprasmlist,left.location);
                          hreg1:=cg.getintregister(exprasmlist,OS_32);
                          cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,left.location.registerhigh,left.location.registerlow,hreg1);
                       end
                     else
                       hreg1 := left.location.register;
                  end;
                hreg2 := cg.getintregister(exprasmlist,OS_INT);
                exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBIC,hreg2,
                  hreg1,1));
                cg.ungetregister(exprasmlist,hreg2);
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg2,
                  hreg1));
              end;
            LOC_FLAGS :
              begin
                hreg1:=cg.getintregister(exprasmlist,OS_INT);
                resflags:=left.location.resflags;
                cg.g_flags2reg(exprasmlist,location.size,resflags,hreg1);
              end;
            LOC_JUMP :
              begin
                hreg1:=cg.getintregister(exprasmlist,OS_INT);
                objectlibrary.getlabel(hlabel);
                cg.a_label(exprasmlist,truelabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,1,hreg1);
                cg.a_jmp_always(exprasmlist,hlabel);
                cg.a_label(exprasmlist,falselabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,0,hreg1);
                cg.a_label(exprasmlist,hlabel);
                cg.ungetregister(exprasmlist,hreg1);
              end;
            else
              internalerror(10062);
         end;
         location.register := hreg1;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


begin
   ctypeconvnode:=tppctypeconvnode;
end.
{
  $Log$
  Revision 1.50  2004-02-03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.49  2003/12/31 22:30:39  jonas
    * fixed currency bugs. Int64 currency handling still needs to be
      rewritten so that it doesn't include conversions to real anymore
      though

  Revision 1.48  2003/12/07 11:21:05  jonas
    * finally fixed int->bool conversion properly

  Revision 1.47  2003/12/04 20:37:02  jonas
    * fixed some int<->boolean type conversion issues

  Revision 1.46  2003/11/29 16:27:19  jonas
    * fixed several ppc assembler reader related problems
    * local vars in assembler procedures now start at offset 4
    * fixed second_int_to_bool (apparently an integer can be in  LOC_JUMP??)

  Revision 1.45  2003/11/04 22:30:15  florian
    + type cast variant<->enum
    * cnv. node second pass uses now as well helper wrappers

  Revision 1.44  2003/10/17 01:22:08  florian
    * compilation of the powerpc compiler fixed

  Revision 1.43  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.42  2003/09/03 19:35:24  peter
    * powerpc compiles again

  Revision 1.41  2003/08/09 15:28:29  jonas
    * fixed conversion from signed value to floats if the compiler is
      compiled with a 1.0.x compiler

  Revision 1.40  2003/06/14 22:32:43  jonas
    * ppc compiles with -dnewra, haven't tried to compile anything with it
      yet though

  Revision 1.39  2003/06/12 22:09:54  jonas
    * tcginnode.pass_2 doesn't call a helper anymore in any case
    * fixed ungetregisterfpu compilation problems

  Revision 1.38  2003/06/04 11:58:58  jonas
    * calculate localsize also in g_return_from_proc since it's now called
      before g_stackframe_entry (still have to fix macos)
    * compilation fixes (cycle doesn't work yet though)

  Revision 1.37  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.36  2003/05/11 20:42:08  jonas
    * fixed bug in second_int_to_bool I introduced previous time
      (secondpass was being called twice!)

  Revision 1.35  2003/05/11 13:06:44  jonas
    * fixed second_int_to_bool() (but still problem with typecasts used for
      var parameters, not sure about solution)

  Revision 1.34  2003/05/02 15:13:38  jonas
    * yet another final fix for second_int_to_real() :) (tested this time)

  Revision 1.33  2003/04/24 22:29:58  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.32  2003/04/23 21:10:54  peter
    * fix compile for ppc,sparc,m68k

  Revision 1.31  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.30  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.29  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.28  2002/12/05 14:28:13  florian
    * some variant <-> dyn. array stuff

  Revision 1.27  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.26  2002/10/18 16:38:42  jonas
    + added entry for pwchar_to_string conversion addition

  Revision 1.25  2002/09/17 18:54:06  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.24  2002/08/23 16:14:50  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.23  2002/08/18 10:34:30  florian
    * more ppc assembling fixes

  Revision 1.22  2002/08/14 19:30:42  carl
    + added fixing because first_in_to_real is now completely generic

  Revision 1.21  2002/08/11 06:14:41  florian
    * fixed powerpc compilation problems

  Revision 1.20  2002/08/10 17:15:31  jonas
    * various fixes and optimizations

  Revision 1.19  2002/07/29 21:23:44  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.18  2002/07/29 09:20:20  jonas
    + second_int_to_int implementation which is almost the same as the
      generic implementation, but it avoids some unnecessary type conversions

  Revision 1.17  2002/07/27 19:55:15  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.16  2002/07/24 14:38:00  florian
    * small typo fixed, compiles with 1.0.x again

  Revision 1.15  2002/07/21 16:57:22  jonas
    * hopefully final fix for second_int_to_real()

  Revision 1.14  2002/07/20 11:58:05  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.13  2002/07/13 06:49:39  jonas
    * fixed fpu constants in second_int_to_real (fpu values are also stored
      in big endian)

  Revision 1.12  2002/07/12 22:02:22  florian
    * fixed to compile with 1.1

  Revision 1.11  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.10  2002/07/11 07:42:31  jonas
    * fixed nppccnv and enabled it
    - removed PPC specific second_int_to_int and use the generic one instead

  Revision 1.9  2002/05/20 13:30:42  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.8  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.7  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.5  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
