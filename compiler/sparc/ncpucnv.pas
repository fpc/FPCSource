{    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SPARC assembler for type converting nodes

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

 ****************************************************************************}
unit ncpucnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,defcmp;

    type
       TSparcTypeConvNode = class(TCgTypeConvNode)
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

    function TSparctypeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin
        { converting a 64bit integer to a float requires a helper }
        if is_64bitint(left.resulttype.def) then
          begin
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
        location.loc:=LOC_FPUREGISTER;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure TSparctypeconvnode.second_int_to_real;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
        location_force_mem(exprasmlist,left.location);
        location.register:=cg.getfpuregister(exprasmlist,location.size);
        { Load memory in fpu register }
        cg.a_loadfpu_ref_reg(exprasmlist,location.size,left.location.reference,location.register);
{$warning TODO Handle also double}
        { Convert value in fpu register from integer to float }
        exprasmlist.concat(taicpu.op_reg_reg(A_FiTOs,location.register,location.register));
      end;


    procedure TSparctypeconvnode.second_real_to_real;
      const
        conv_op : array[tfloattype,tfloattype] of tasmop = (
          {    from:   s32     s64     s80     c64     cur    f128 }
          { s32 }  ( A_FMOVS,A_FDTOS,A_NONE, A_NONE, A_NONE, A_NONE ),
          { s64 }  ( A_FSTOD,A_FMOVD,A_NONE, A_NONE, A_NONE, A_NONE ),
          { s80 }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { c64 }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { cur }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { f128 } ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE )
        );
      var
        op : tasmop;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
        location_force_fpureg(exprasmlist,left.location,false);
        { Convert value in fpu register from integer to float }
        op:=conv_op[tfloatdef(resulttype.def).typ,tfloatdef(left.resulttype.def).typ];
        if op=A_NONE then
          internalerror(200401121);
        location_release(exprasmlist,left.location);
        location.register:=cg.getfpuregister(exprasmlist,location.size);
        exprasmlist.concat(taicpu.op_reg_reg(A_FsTOd,left.location.register,location.register));
      end;


    procedure TSparctypeconvnode.second_int_to_bool;
      var
        hreg1,hreg2 : tregister;
        resflags : tresflags;
        opsize   : tcgsize;
        hlabel,oldtruelabel,oldfalselabel : tasmlabel;
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
        if (nf_explicit in flags)and
           (left.resulttype.def.size=resulttype.def.size)and
           (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
          begin
            location_copy(location,left.location);
            truelabel:=oldtruelabel;
            falselabel:=oldfalselabel;
            exit;
          end;
        location_reset(location,LOC_REGISTER,def_cgsize(left.resulttype.def));
        opsize := def_cgsize(left.resulttype.def);
        case left.location.loc of
          LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER:
            begin
              if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                begin
                  reference_release(exprasmlist,left.location.reference);
                  hreg2:=cg.GetIntRegister(exprasmlist,opsize);
                  cg.a_load_ref_reg(exprasmlist,OpSize,OpSize,left.location.reference,hreg2);
                end
              else
                hreg2 := left.location.register;
                hreg1 := cg.GetIntRegister(exprasmlist,opsize);
                exprasmlist.concat(taicpu.op_reg_const_reg(A_SUB,hreg1,1,hreg2));
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,hreg1,hreg1,hreg2));
                cg.UnGetRegister(exprasmlist,hreg2);
            end;
          LOC_FLAGS :
            begin
              hreg1:=cg.GetIntRegister(exprasmlist,location.size);
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
            end;
          else
            internalerror(10062);
        end;
        with Location do
          begin
            location.register := hreg1;
            if Size in [OS_64, OS_S64]
            then
              RegisterHigh:=Tregister(LongInt(hReg1)+1);{Alrady allocated OS_64}
          end;
        truelabel:=oldtruelabel;
        falselabel:=oldfalselabel;
      end;


begin
   ctypeconvnode:=TSparctypeconvnode;
end.
{
  $Log$
  Revision 1.24  2004-03-15 14:37:06  mazen
  + support for LongBool(Int64) type cast

  Revision 1.23  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.22  2004/01/12 22:11:39  peter
    * use localalign info for alignment for locals and temps
    * sparc fpu flags branching added
    * moved powerpc copy_valye_openarray to generic

  Revision 1.21  2003/11/04 22:30:15  florian
    + type cast variant<->enum
    * cnv. node second pass uses now as well helper wrappers

  Revision 1.20  2003/10/24 11:31:43  mazen
  *fixes related to removal of rg

  Revision 1.19  2003/10/01 20:34:50  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.18  2003/09/14 21:36:01  peter
    * remove ppc code

  Revision 1.17  2003/06/04 20:59:37  mazen
  + added size of destination in code gen methods
  + making g_overflowcheck declaration same as
    ancestor's method declaration

  Revision 1.16  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.15  2003/04/23 21:10:54  peter
    * fix compile for ppc,sparc,m68k

  Revision 1.14  2003/04/23 13:35:39  peter
    * fix sparc compile

  Revision 1.13  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.12  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.11  2003/01/22 20:45:15  mazen
  * making math code in RTL compiling.
  *NB : This does NOT mean necessary that it will generate correct code!

  Revision 1.10  2003/01/20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.9  2002/12/05 14:28:03  florian
    * some variant <-> dyn. array stuff

  Revision 1.8  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.7  2002/11/10 19:07:46  mazen
  * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

  Revision 1.6  2002/11/06 11:31:24  mazen
  * op_reg_reg_reg don't need any more a TOpSize parameter

  Revision 1.5  2002/10/22 13:43:01  mazen
  - cga.pas redueced to an empty unit

  Revision 1.4  2002/10/10 19:57:52  mazen
  * Just to update repsitory

  Revision 1.3  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/08/30 06:15:27  mazen
  ncgcall.pas moved to ncpucall.pas (I'd like ncpu* insteade of nsparc* since it
  provides processor independent units naming)

  Revision 1.1  2002/08/29 10:16:20  mazen
  File added support to the new generic parameter handling

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

}
