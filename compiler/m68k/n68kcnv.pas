{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate m68k assembler for type converting nodes

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
unit n68kcnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,defcmp;

    type
       tm68ktypeconvnode = class(tcgtypeconvnode)
         protected
          function first_int_to_real: tnode; override;
          procedure second_int_to_real;override;
          procedure second_int_to_bool;override;
//          procedure pass_2;override;
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
      rgobj,tgobj,cgobj,cgutils,globtype,cgcpu;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tm68ktypeconvnode.first_int_to_real: tnode;
      var
        fname: string[32];
      begin
        { In case we are in emulation mode, we must
          always call the helpers
        }
        if (cs_fp_emulation in aktmoduleswitches) then
          begin
            result := inherited first_int_to_real;
            exit;
          end
        else
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
              { the fpu always considers 32-bit values as signed
                therefore we need to call the helper in case of
                a cardinal value.
              }
              begin
                 fname := 'fpc_longword_to_double';
                 result := ccallnode.createintern(fname,ccallparanode.create(
                    left,nil));
                 left:=nil;
                 firstpass(result);
                 exit;
              end;
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



    procedure tm68ktypeconvnode.second_int_to_real;

      var
        tempconst: trealconstnode;
        ref: treference;
        valuereg, tempreg, leftreg, tmpfpureg: tregister;
        signed : boolean;
        scratch_used : boolean;
        opsize : tcgsize;
      begin
        scratch_used := false;
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
        signed := is_signed(left.resulttype.def);
        opsize := def_cgsize(left.resulttype.def);
        { has to be handled by a helper }
        if is_64bitint(left.resulttype.def) then
          internalerror(200110011);
        { has to be handled by a helper }
        if not signed then
           internalerror(20020814);

        location.register:=cg.getfpuregister(exprasmlist,opsize);
        case left.location.loc of
          LOC_REGISTER, LOC_CREGISTER:
            begin
              leftreg := left.location.register;
              exprasmlist.concat(taicpu.op_reg_reg(A_FMOVE,TCGSize2OpSize[opsize],leftreg,
                  location.register));
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              exprasmlist.concat(taicpu.op_ref_reg(A_FMOVE,TCGSize2OpSize[opsize],
                  left.location.reference,location.register));
            end
          else
            internalerror(200110012);
         end;
       end;


    procedure tm68ktypeconvnode.second_int_to_bool;
      var
        hreg1,
        hreg2    : tregister;
        resflags : tresflags;
        opsize   : tcgsize;
      begin
         secondpass(left);
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explicit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              exit;
           end;
         location_reset(location,LOC_REGISTER,def_cgsize(left.resulttype.def));
         opsize := def_cgsize(left.resulttype.def);
         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE :
              begin
                { can we optimize it, or do we need to fix the ref. ? }
                if isvalidrefoffset(left.location.reference) then
                  begin
                    exprasmlist.concat(taicpu.op_ref(A_TST,TCGSize2OpSize[opsize],
                       left.location.reference));
                  end
                else
                  begin
                     hreg2:=cg.getintregister(exprasmlist,opsize);
                     cg.a_load_ref_reg(exprasmlist,opsize,opsize,
                        left.location.reference,hreg2);
                     exprasmlist.concat(taicpu.op_reg(A_TST,TCGSize2OpSize[opsize],hreg2));
//                     cg.ungetcpuregister(exprasmlist,hreg2);
                  end;
//                reference_release(exprasmlist,left.location.reference);
                resflags:=F_NE;
                hreg1:=cg.getintregister(exprasmlist,opsize);
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                hreg2:=left.location.register;
                exprasmlist.concat(taicpu.op_reg(A_TST,TCGSize2OpSize[opsize],hreg2));
//                cg.ungetcpuregister(exprasmlist,hreg2);
                hreg1:=cg.getintregister(exprasmlist,opsize);
                resflags:=F_NE;
              end;
            LOC_FLAGS :
              begin
                hreg1:=cg.getintregister(exprasmlist,opsize);
                resflags:=left.location.resflags;
              end;
            else
             internalerror(200512182);
         end;
         cg.g_flags2reg(exprasmlist,location.size,resflags,hreg1);
         location.register := hreg1;
      end;

{
    procedure tm68ktypeconvnode.pass_2;
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
}

begin
   ctypeconvnode:=tm68ktypeconvnode;
end.
