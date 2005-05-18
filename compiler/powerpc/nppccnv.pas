{
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
      verbose,globtype,globals,systems,
      symconst,symdef,aasmbase,aasmtai,
      defutil,
      cgbase,cgutils,pass_1,pass_2,
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
        signed : boolean;
      begin
{$ifdef VER1_0}
        dummy1 := (int64(1) shl 31) or (int64($43300000) shl 32);
        dummy2 := int64($43300000) shl 32;
{$endif VER1_0}

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
                valuereg := cg.getintregister(exprasmlist,OS_INT)
              else
                valuereg := leftreg;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              leftreg := cg.getintregister(exprasmlist,OS_INT);
              valuereg := leftreg;
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
         if signed then
           exprasmlist.concat(taicpu.op_reg_reg_const(A_XORIS,valuereg,
             { xoris expects a unsigned 16 bit int (FK) }
             leftreg,$8000));
         inc(ref.offset,4);
         cg.a_load_reg_ref(exprasmlist,OS_32,OS_32,valuereg,ref);
         dec(ref.offset,4);

         tmpfpureg := cg.getfpuregister(exprasmlist,OS_F64);
         cg.a_loadfpu_ref_reg(exprasmlist,OS_F64,tempconst.location.reference,
           tmpfpureg);
         tempconst.free;

         location.register := cg.getfpuregister(exprasmlist,OS_F64);
         cg.a_loadfpu_ref_reg(exprasmlist,OS_F64,ref,location.register);

         tg.ungetiftemp(exprasmlist,ref);

         exprasmlist.concat(taicpu.op_reg_reg_reg(A_FSUB,location.register,
           location.register,tmpfpureg));

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
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
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
                    hreg1:=cg.getintregister(exprasmlist,OS_INT);
                    if left.location.size in [OS_64,OS_S64] then
                      begin
                        cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,left.location.reference,hreg1);
                        hreg2:=cg.getintregister(exprasmlist,OS_INT);
                        href:=left.location.reference;
                        inc(href.offset,4);
                        cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,href,hreg2);
                        cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hreg1,hreg2,hreg1);
                      end
                    else
                      cg.a_load_ref_reg(exprasmlist,opsize,opsize,left.location.reference,hreg1);
                  end
                else
                  begin
                     if left.location.size in [OS_64,OS_S64] then
                       begin
                          hreg1:=cg.getintregister(exprasmlist,OS_32);
                          cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,left.location.register64.reghi,left.location.register64.reglo,hreg1);
                       end
                     else
                       hreg1 := left.location.register;
                  end;
                hreg2 := cg.getintregister(exprasmlist,OS_INT);
                exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBIC,hreg2,hreg1,1));
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg2,hreg1));
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
