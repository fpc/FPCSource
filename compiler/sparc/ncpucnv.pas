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
      node,ncnv,ncgcnv,defbase;

    type
       TSparcTypeConvNode = class(TCgTypeConvNode)
         protected
          procedure second_int_to_int;override;
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
          procedure pass_2;override;
          procedure second_call_helper(c : tconverttype); override;
       end;

implementation

   uses
      verbose,globals,systems,
      symconst,symdef,aasmbase,aasmtai,
      cgbase,pass_1,pass_2,
      ncon,ncal,
      ncgutil,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj,cginfo;


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
              inserttypeconv(left,s32bittype)
            else
              inserttypeconv(left,u32bittype);
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

    procedure TSparctypeconvnode.second_int_to_int;
      var
        newsize : tcgsize;
        size, leftsize : cardinal;
      begin
        newsize:=def_cgsize(resulttype.def);

        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          cg.g_rangecheck(exprasmlist,left,resulttype.def);

        { is the result size smaller ? }
        size := resulttype.def.size;
        leftsize := left.resulttype.def.size;
        if (size < leftsize) or
           (((newsize in [OS_64,OS_S64]) or
             (left.location.loc <> LOC_REGISTER)) and
            (size > leftsize)) then
          begin
            { reuse the left location by default }
            location_copy(location,left.location);
            location_force_reg(exprasmlist,location,newsize,false);
          end
        else
          begin
            { no special loading is required, reuse current location }
            location_copy(location,left.location);
            location.size:=newsize;
          end;
      end;


    procedure TSparctypeconvnode.second_int_to_real;

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
        signed, valuereg_is_scratch: boolean;
      begin
{$ifdef VER1_0}
        { the "and" is because 1.0.x will sign-extend the $80000000 to }
        { $ffffffff80000000 when converting it to int64 (JM)           }
        dummy1 := int64($80000000) and (int64($43300000) shl 32);
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
            crealconstnode.create(double(dummy1),
            pbestrealtype^)
        else
          tempconst :=
            crealconstnode.create(double(dummy2),
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
                  valuereg := cg.get_scratch_reg_int(exprasmlist);
                  valuereg_is_scratch := true;
                end
              else
                valuereg := leftreg;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              leftreg := cg.get_scratch_reg_int(exprasmlist);
              valuereg := leftreg;
              valuereg_is_scratch := true;
              cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                left.location.reference,leftreg);
            end
          else
            internalerror(200110012);
         end;
         tempreg := cg.get_scratch_reg_int(exprasmlist);
         {$WARNING FIXME what reallty should be done?}
         exprasmlist.concat(taicpu.op_reg_const_reg(A_OR,S_L,tempreg,$4330,tempreg));
         cg.a_load_reg_ref(exprasmlist,OS_32,tempreg,ref);
         cg.free_scratch_reg(exprasmlist,tempreg);
         if signed then
         {$WARNING FIXME what reallty should be done?}
           exprasmlist.concat(taicpu.op_reg_const_reg(A_XOR,S_L,leftreg,$8000,valuereg));
         inc(ref.offset,4);
         cg.a_load_reg_ref(exprasmlist,OS_32,valuereg,ref);
         dec(ref.offset,4);
         if (valuereg_is_scratch) then
           cg.free_scratch_reg(exprasmlist,valuereg);

         if (left.location.loc = LOC_REGISTER) or
            ((left.location.loc = LOC_CREGISTER) and
             not signed) then
           rg.ungetregister(exprasmlist,leftreg)
         else
           cg.free_scratch_reg(exprasmlist,valuereg);

         tmpfpureg := rg.getregisterfpu(exprasmlist);
         cg.a_loadfpu_ref_reg(exprasmlist,OS_F64,tempconst.location.reference,
           tmpfpureg);
         tempconst.free;

         location.register := rg.getregisterfpu(exprasmlist);
         {$WARNING FIXME what reallty should be done?}
         exprasmlist.concat(taicpu.op_reg_ref(A_LD,S_L,location.register,ref));

         tg.ungetiftemp(exprasmlist,ref);

         exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,S_L,location.register,
           location.register,tmpfpureg));
         rg.ungetregisterfpu(exprasmlist,tmpfpureg);

         { work around bug in some PowerPC processors }
         if (tfloatdef(resulttype.def).typ = s32real) then
         {$WARNING FIXME what reallty should be done?}
           exprasmlist.concat(taicpu.op_reg_reg(A_ADD,S_L,location.register,
             location.register));
       end;


     procedure TSparctypeconvnode.second_real_to_real;
       begin
          inherited second_real_to_real;
          { work around bug in some powerpc processors where doubles aren't }
          { properly converted to singles                                   }
          if (tfloatdef(left.resulttype.def).typ = s64real) and
             (tfloatdef(resulttype.def).typ = s32real) then
         {$WARNING FIXME what reallty should be done?}
            exprasmlist.concat(taicpu.op_reg_reg(A_ADD,S_L,location.register,location.register));
       end;




    procedure TSparctypeconvnode.second_int_to_bool;
      var
        hreg1,
        hreg2    : tregister;
        resflags : tresflags;
        opsize   : tcgsize;
      begin
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              exit;
           end;
         location_reset(location,LOC_REGISTER,def_cgsize(left.resulttype.def));
         opsize := def_cgsize(left.resulttype.def);
         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  begin
                    reference_release(exprasmlist,left.location.reference);
                    hreg2:=rg.getregisterint(exprasmlist);
                    cg.a_load_ref_reg(exprasmlist,opsize,
                      left.location.reference,hreg2);
                  end
                else
                  hreg2 := left.location.register;
                hreg1 := rg.getregisterint(exprasmlist);
                exprasmlist.concat(taicpu.op_reg_const_reg(A_SUB,S_L,hreg1,1,
                  hreg2));
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,S_L,hreg1,hreg1,
                  hreg2));
                rg.ungetregister(exprasmlist,hreg2);
              end;
            LOC_FLAGS :
              begin
                hreg1:=rg.getregisterint(exprasmlist);
                resflags:=left.location.resflags;
                cg.g_flags2reg(exprasmlist,location.size,resflags,hreg1);
              end;
            else
              internalerror(10062);
         end;
         location.register := hreg1;
      end;


    procedure TSparctypeconvnode.second_call_helper(c : tconverttype);

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


    procedure TSparctypeconvnode.pass_2;
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
              location_copy(location,left.location);
              if codegenerror then
               exit;
           end;
         second_call_helper(convtype);
      end;


begin
   ctypeconvnode:=TSparctypeconvnode;
end.
{
  $Log$
  Revision 1.2  2002-08-30 06:15:27  mazen
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

  Revision 1.5  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
