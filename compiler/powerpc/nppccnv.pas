{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit n386cnv;

{$i defines.inc}

interface

    uses
      node,ncnv,ncgcnv,types;

    type
       tppctypeconvnode = class(tcgtypeconvnode)
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
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
          procedure pass_2;override;
          procedure second_call_helper(c : tconverttype);
       end;

implementation

   uses
      verbose,globals,systems,
      symconst,symdef,aasm,
      cgbase,temp_gen,pass_2,
      ncon,ncal,
      cpubase,cpuasm,
      cga,tgcpu;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure tppctypeconvnode.second_int_to_int;
      var
        fromsize,
        tosize    : longint;
        opsize,
        tempsize  : tcgsize;

      begin
        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          emitrangecheck(left,resulttype.def);

        fromsize := left.resulttype.def.size;
        tosize := resulttype.def.size
        { is the result size smaller ? }
        if tosize < fromsize then
          begin
            opsize := def_cgsize(resulttype.def);
            case left.location.loc of
              LOC_REGISTER,LOC_CREGISTER:
                begin
                  if location = LOC_REGISTER then
                    location.register:= left.location.register
                  else
                    location.register := getregister32;
                  case opsize of
                    OS_8:
                      exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                        A_RLWINM,location.register,left.location.register,
                        0,24,31));
                    OS_S8:
                      exprasmlist.concat(taicpu.op_reg_reg(A_EXTSB,
                        location.register,left.location.register));
                    OS_16:
                      exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                        A_RLWINM,location.register,left.location.register,
                        0,16,31));
                    OS_S16:
                      exprasmlist.concat(taicpu.op_reg_reg(A_EXTSH,
                        location.register,left.location.register));
                    else
                      begin
                        if location.register <> left.location.register then
                          exprasmlist.concat(taicpu.op_reg_reg(A_MR,
                            location.register,left.location.register));
                        { we can release the upper register }
                        if opsize in [OS_64,OS_S64] then
                          ungetregister(left.location.registerhigh);
                      end;
                  end;
                end;
              LOC_REFERENCE,LOC_MEM:
                begin
                  set_location(location,left.location);
                  increfofs(location.reference,fromsize-tosize);
                end;
            end;
          end
        { is the result size bigger ? }
        else if resulttype.def.size>left.resulttype.def.size then
          begin
            opsize := def_cgsize(fromsize);
            location.loc := LOC_REGISTER;
            case left.location.loc of
              LOC_REFERENCE,LOC_MEM:
                begin
                  del_reference(left.location.reference);
                  location.register := getregister32;
                  if not (opsize in [OS_64,OS_S64]) then
                    tempsize := pred(opsize)
                  else
                    tempsize := opsize;
                  { this one takes care of the necessary sign extensions }
                  cg.a_load_ref_reg(exprasmlist,tempsize,
                    left.location.reference,location.register);
                  ungetiftemp(left.location.reference);
                end;
              LOC_CREGISTER:
                { since we only have 32bit registers and everything is }
                { always loaded with sign-extending or zeroeing        }
                { instructions as appropriate, the source will contain }
                { the correct value already, so simply copy it         }
                begin
                  location.register := getregister32;
                  exprasmlist.concat(taicpu.op_reg_reg(A_MR,location.register,
                    left.location.register));
                end;
              { see LOC_CREGISTER }
              LOC_REGISTER:;
            end;
            { sign extend even further if necessary }
            if opsize in [OS_64,OS_S64] then
              begin
                location.registerhigh := getregister32;
                if (opsize = OS_64) or
                   not (is_signed(left.resulttype.def)) then
                  cg.a_load_const_reg(exprasmlist,OS_32,0,
                    location.registerhigh)
                else
                  { duplicate the sign bit 32 times in the high reg }
                  exprasmlist.concat(taicpu.op_reg_reg_const(A_SRAWI,
                    location.registerhigh,location.register,31));
              end;
          end
        else
          setlocation(location,left.location);
      end;


    procedure tppctypeconvnode.second_int_to_real;
    
      type
        dummyrec = record
          i: int64;
        end;
      var
        tempconst: trealconstnode;
        ref: treference;
        valuereg, tempreg, leftreg, tmpfpureg: tregister;
      begin
        { the code here comes from the PowerPC Compiler Writer's Guide }
        
        { addis R0,R0,0x4330  # R0 = 0x43300000             }
        { stw R0,disp(R1)     # store upper half            }
        { xoris R3,R3,0x8000  # flip sign bit               }
        { stw R3,disp+4(R1)   # store lower half            }
        { lfd FR1,disp(R1)    # float load double of value  }
        { fsub FR1,FR1,FR2    # subtract 0x4330000080000000 }
        gettempofsizereference(8,ref);
        { we need a certain constant for the conversion, so create it here }
        tempconst :=
          { we need this strange typecast because we want the }
          { double represented by $4330000080000000, not the  }
          { double converted from the integer with that value }
          crealconstnode.create(double(dummyrec($4330000080000000)),
          pbestrealtype^);
        resulttypepass(tempconst);
        firstpass(tempconst);
        secondpass(tempconst);
        if tempconst.location.loc <> LOC_MEM then
          internalerror(200110011);

        case left.location.loc of
          LOC_REGISTER:
            begin
              valuereg := left.location.register;
              leftreg := valuereg;
            end;
          LOC_CREGISTER:
            begin
              valuereg := cg.get_scratch_reg(exprasmlist);
              leftreg := valuereg;
            end;
          LOC_REFERENCE,LOC_MEM:
            begin
              valuereg := cg.get_scratch_reg(exprasmlist);
              leftreg := valuereg;
              cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                left.location.reference,valuereg);
            end
          else
            internalerror(200110012);
         end;
         tempreg := cg.get_scratch_reg(exprasmlist);
         exprasmlist.concat(taicpu.op_reg_const(A_LIS,tempreg,$4330));
         cg.a_load_reg_ref(exprasmlist,OS_32,tempreg,ref);
         exprasmlist.concat(taicpu.op_reg_reg_const(A_XORIS,valuereg,
           leftreg,$8000));
         inc(ref.offset,4);
         cg.a_load_reg_ref(exprasmlist,OS_32,valuereg,ref);
         tmpfpureg := get_scratch_reg_fpu(exprasmlist);
         exprasmlist.concat(taicpu.op_reg_ref(A_LFD,tmpfpureg,tempconst.location.reference));
         tempconst.free;

         location.register := getregisterfpu;
         exprasmlist.concat(taicpu.op_reg_ref(A_LFD,location.register,ref));
         ungetiftemp(ref);
         
         exprasmlist.concat(taicpu.op_reg_reg_reg(A_FSUB,location.register,
           location.register,tmpfpureg));
         ungetregister(tmpfpureg);
       end;


    procedure tppctypeconvnode.second_int_to_bool;
      var
        hreg1,
        hreg2    : tregister;
        resflags : tresflags;
        opsize   : tcgsize;
      begin
         clear_location(location);
         { byte(boolean) or word(wordbool) or longint(longbool) must
         be accepted for var parameters }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_MEM,LOC_CREGISTER]) then
           begin
              set_location(location,left.location);
              exit;
           end;
         location.loc:=LOC_REGISTER;
         opsize := def_cgsize(left.resulttype.def);
         case left.location.loc of
            LOC_MEM,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.loc in [LOC_MEM,LOC_REFERENCE] then
                  begin
                    del_reference(left.location);
                    hreg2:=getregister32;
                    cg.a_load_ref_reg(exprasmlist,opsize,
                      left.location.reference,hreg2);
                  end
                else
                  hreg2 := left.location.register;
                hreg1 := getregister32;
                exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBIC,hreg1,
                  hreg2,1);
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg1,
                  hreg2);
                ungetregister(hreg2);
              end;
            LOC_FLAGS :
              begin
                hreg1:=getregister32;
                resflags:=left.location.resflags;
                emit_flag2reg(resflags,hreg1);
              end;
            else
              internalerror(10062);
         end;
         location.register := hreg1;
      end;

begin
   ctypeconvnode:=tppctypeconvnode;
end.
{
  $Log$
  Revision 1.2  2001-10-01 12:17:26  jonas
    + implemented second_int_to_real
    * fixed small bug in second_int_to_int

  Revision 1.1  2001/09/29 21:33:12  jonas
    + implemented in_to_bool and int_to_int (+ helper in nppcutil)


}
