{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for math nodes

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
unit nppcmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat;

    type
      tppcmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tppcshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tppcunaryminusnode = class(tunaryminusnode)
         procedure pass_2;override;
      end;

      tppcnotnode = class(tnotnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,
      defutil,
      cgbase,cgobj,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,cginfo,
      ncgutil,cga,cgcpu,cg64f32,rgobj;

{*****************************************************************************
                             TPPCMODDIVNODE
*****************************************************************************}

    procedure tppcmoddivnode.pass_2;
      const
                    { signed   overflow }
        divops: array[boolean, boolean] of tasmop =
          ((A_DIVWU,A_DIVWUO_),(A_DIVW,A_DIVWO_));
      var
         power,
         l1, l2     : longint;
         op         : tasmop;
         numerator,
         divider,
         resultreg  : tregister;
         saved      : tmaybesave;
         size       : Tcgsize;

      begin
         secondpass(left);
         maybe_save(exprasmlist,right.registers32,left.location,saved);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,saved);
         location_copy(location,left.location);

         { put numerator in register }
         size:=def_cgsize(left.resulttype.def);
         location_force_reg(exprasmlist,left.location,
           size,true);
         location_copy(location,left.location);
         numerator := location.register;
         resultreg := location.register;
         if (location.loc = LOC_CREGISTER) then
           begin
             location.loc := LOC_REGISTER;
             location.register := rg.getregisterint(exprasmlist,size);
             resultreg := location.register;
           end;
         if (nodetype = modn) then
           begin
             resultreg := cg.get_scratch_reg_int(exprasmlist,size);
           end;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value,power) then
           begin
             { From "The PowerPC Compiler Writer's Guide":                   }
             { This code uses the fact that, in the PowerPC architecture,    }
             { the shift right algebraic instructions set the Carry bit if   }
             { the source register contains a negative number and one or     }
             { more 1-bits are shifted out. Otherwise, the carry bit is      }
             { cleared. The addze instruction corrects the quotient, if      }
             { necessary, when the dividend is negative. For example, if     }
             { n = -13, (0xFFFF_FFF3), and k = 2, after executing the srawi  }
             { instruction, q = -4 (0xFFFF_FFFC) and CA = 1. After executing }
             { the addze instruction, q = -3, the correct quotient.          }
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_32,aword(power),
               numerator,resultreg);
             exprasmlist.concat(taicpu.op_reg_reg(A_ADDZE,resultreg,resultreg));
           end
         else
           begin
             { load divider in a register if necessary }
             location_force_reg(exprasmlist,right.location,
               def_cgsize(right.resulttype.def),true);
             divider := right.location.register;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { And on PPC, the only way to catch a div-by-0 is by checking  }
             { the overflow flag (JM)                                       }
             op := divops[is_signed(right.resulttype.def),
                          cs_check_overflow in aktlocalswitches];
             exprasmlist.concat(taicpu.op_reg_reg_reg(op,resultreg,numerator,
               divider));

           if (nodetype = modn) then
             begin
               exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULLW,resultreg,
                 divider,resultreg));
               rg.ungetregisterint(exprasmlist,divider);
               exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,location.register,
                 numerator,resultreg));
               cg.free_scratch_reg(exprasmlist,resultreg);
               resultreg := location.register;
             end
           else
             rg.ungetregisterint(exprasmlist,divider);
           end;
       { free used registers }
        if numerator.number <> resultreg.number then
          rg.ungetregisterint(exprasmlist,numerator);
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        cg.g_overflowcheck(exprasmlist,location,resulttype.def);
      end;


{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}

    function tppcshlshrnode.first_shlshr64bitint: tnode;
      begin
        result := nil;
      end;

    procedure tppcshlshrnode.pass_2;

      var
         resultreg, hregister1,hregister2,
         hregisterhigh,hregisterlow : tregister;
         op : topcg;
         asmop1, asmop2: tasmop;
         shiftval: aword;
         saved : tmaybesave;
         r : Tregister;

      begin
         secondpass(left);
         maybe_save(exprasmlist,right.registers32,left.location,saved);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,saved);

         if is_64bitint(left.resulttype.def) then
           begin
             location_force_reg(exprasmlist,left.location,
               def_cgsize(left.resulttype.def),true);
             location_copy(location,left.location);
             hregisterhigh := location.registerhigh;
             hregisterlow := location.registerlow;
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.loc := LOC_REGISTER;
                 location.registerhigh := rg.getregisterint(exprasmlist,OS_32);
                 location.registerlow := rg.getregisterint(exprasmlist,OS_32);
               end;
             if (right.nodetype = ordconstn) then
               begin
                 shiftval := tordconstnode(right).value;
                 if tordconstnode(right).value > 31 then
                   begin
                     if nodetype = shln then
                       begin
                         cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,
                           shiftval and 31,hregisterlow,location.registerhigh);
                         cg.a_load_const_reg(exprasmlist,OS_32,0,location.registerlow);
                       end
                     else
                       begin
                         cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,
                           shiftval and 31,hregisterhigh,location.registerlow);
                         cg.a_load_const_reg(exprasmlist,OS_32,0,location.registerhigh);
                       end;
                   end
                 else
                   begin
                     if nodetype = shln then
                       begin
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.registerhigh,hregisterhigh,shiftval,
                           0,31-shiftval));
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWIMI,location.registerhigh,hregisterlow,shiftval,
                           32-shiftval,31));
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.registerlow,hregisterlow,shiftval,
                           0,31-shiftval));
                       end
                     else
                       begin
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.registerlow,hregisterlow,32-shiftval,
                           shiftval,31));
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWIMI,location.registerlow,hregisterhigh,32-shiftval,
                           0,shiftval-1));
                         exprasmlist.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.registerhigh,hregisterhigh,32-shiftval,
                           shiftval,31));
                       end;
                   end;
               end
             else
               { no constant shiftcount }
               begin
                 location_force_reg(exprasmlist,right.location,OS_S32,true);
                 hregister1 := right.location.register;
                 if nodetype = shln then
                   begin
                     asmop1 := A_SLW;
                     asmop2 := A_SRW;
                   end
                 else
                   begin
                     asmop1 := A_SRW;
                     asmop2 := A_SLW;
                     resultreg := hregisterhigh;
                     hregisterhigh := hregisterlow;
                     hregisterlow := resultreg;
                     resultreg := location.registerhigh;
                     location.registerhigh := location.registerlow;
                     location.registerlow := resultreg;
                   end;

                 rg.getexplicitregisterint(exprasmlist,NR_R0);
                 r.enum:=R_INTREGISTER;
                 r.number:=NR_R0;
                 exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                   r,hregister1,32));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.registerhigh,hregisterhigh,hregister1));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop2,
                   r,hregisterlow,r));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.registerhigh,location.registerhigh,r));
                 exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBI,
                   r,hregister1,32));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   r,hregisterlow,r));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.registerhigh,location.registerhigh,r));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.registerlow,hregisterlow,hregister1));
                 rg.ungetregisterint(exprasmlist,r);

                 if nodetype = shrn then
                   begin
                     resultreg := location.registerhigh;
                     location.registerhigh := location.registerlow;
                     location.registerlow := resultreg;
                   end;

                 if right.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                   cg.free_scratch_reg(exprasmlist,hregister1)
                 else
                   rg.ungetregisterint(exprasmlist,hregister1);
               end
           end
         else
           begin
             { load left operators in a register }
             location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
             location_copy(location,left.location);
             resultreg := location.register;
             hregister1 := location.register;
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.loc := LOC_REGISTER;
                 resultreg := rg.getregisterint(exprasmlist,OS_32);
                 location.register := resultreg;
               end;

              { determine operator }
              if nodetype=shln then
                op:=OP_SHL
              else
                op:=OP_SHR;

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                cg.a_op_const_reg_reg(exprasmlist,op,OS_32,
                  tordconstnode(right).value and 31,hregister1,resultreg)
              else
                begin
                  { load shift count in a register if necessary }
                  location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
                  hregister2 := right.location.register;

                  cg.a_op_reg_reg_reg(exprasmlist,op,OS_32,hregister2,
                    hregister1,resultreg);

                  rg.ungetregisterint(exprasmlist,hregister2);
                end;
           end;
      end;


{*****************************************************************************
                          TPPCUNARYMINUSNODE
*****************************************************************************}

    procedure tppcunaryminusnode.pass_2;

      var
        src1, src2, tmp: tregister;
        op: tasmop;

      begin
         secondpass(left);
         if is_64bitint(left.resulttype.def) then
           begin
             location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
             location_copy(location,left.location);
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.registerlow := rg.getregisterint(exprasmlist,OS_INT);
                 location.registerhigh := rg.getregisterint(exprasmlist,OS_INT);
                 location.loc := LOC_REGISTER;
               end;
             exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
               location.registerlow,left.location.registerlow,0));
             if not(cs_check_overflow in aktlocalswitches) then
               exprasmlist.concat(taicpu.op_reg_reg(A_SUBFZE,
                 location.registerhigh,left.location.registerhigh))
             else
               exprasmlist.concat(taicpu.op_reg_reg(A_SUBFZEO_,
                 location.registerhigh,left.location.registerhigh));
           end
         else
           begin
              location_copy(location,left.location);
              location.loc:=LOC_REGISTER;
              case left.location.loc of
                LOC_FPUREGISTER, LOC_REGISTER:
                  begin
                    src1 := left.location.register;
                    location.register := src1;
                  end;
                LOC_CFPUREGISTER, LOC_CREGISTER:
                  begin
                     src1 := left.location.register;
                     if left.location.loc = LOC_CREGISTER then
                       location.register := rg.getregisterint(exprasmlist,OS_INT)
                     else
                       location.register := rg.getregisterfpu(exprasmlist,location.size);
                  end;
                LOC_REFERENCE,LOC_CREFERENCE:
                  begin
                     if (left.resulttype.def.deftype=floatdef) then
                       begin
                          src1 := rg.getregisterfpu(exprasmlist,def_cgsize(left.resulttype.def));
                          location.register := src1;
                          cg.a_loadfpu_ref_reg(exprasmlist,
                            def_cgsize(left.resulttype.def),
                            left.location.reference,src1);
                       end
                     else
                       begin
                          src1 := rg.getregisterint(exprasmlist,OS_32);
                          location.register:= src1;
                          cg.a_load_ref_reg(exprasmlist,OS_32,OS_32,
                            left.location.reference,src1);
                       end;
                     reference_release(exprasmlist,left.location.reference);
                  end;
              end;
              { choose appropriate operand }
              if left.resulttype.def.deftype <> floatdef then
                begin
                  if not(cs_check_overflow in aktlocalswitches) then
                    op := A_NEG
                  else
                    op := A_NEGO_;
                  location.loc := LOC_REGISTER;
                end
              else
                begin
                  op := A_FNEG;
                  location.loc := LOC_FPUREGISTER;
                end;
              { emit operation }
              exprasmlist.concat(taicpu.op_reg_reg(op,location.register,src1));
           end;
{ Here was a problem...     }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!     }
{ So this is useless...     }
{ that's not true: -2^31 gives an overflow error if it is negated (FK) }
        cg.g_overflowcheck(exprasmlist,location,resulttype.def);
      end;


{*****************************************************************************
                               TPPCNOTNODE
*****************************************************************************}

    procedure tppcnotnode.pass_2;

      var
         hl : tasmlabel;
         regl, regh: tregister;

      begin
         if is_boolean(resulttype.def) then
          begin
            { if the location is LOC_JUMP, we do the secondpass after the
              labels are allocated
            }
            if left.expectloc=LOC_JUMP then
              begin
                hl:=truelabel;
                truelabel:=falselabel;
                falselabel:=hl;
                secondpass(left);
                maketojumpbool(exprasmlist,left,lr_load_regvars);
                hl:=truelabel;
                truelabel:=falselabel;
                falselabel:=hl;
                location.loc:=LOC_JUMP;
              end
            else
              begin
                secondpass(left);
                case left.location.loc of
                  LOC_FLAGS :
                    begin
                      location_copy(location,left.location);
                      inverse_flags(location.resflags);
                    end;
                  LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_CREFERENCE :
                    begin
                      location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
                      exprasmlist.concat(taicpu.op_reg_const(A_CMPWI,left.location.register,0));
                      location_release(exprasmlist,left.location);
                      location_reset(location,LOC_FLAGS,OS_NO);
                      location.resflags.cr:=r_cr0;
                      location.resflags.flag:=F_EQ;
                   end;
                  else
                    internalerror(2003042401);
                end;
              end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
             secondpass(left);
             location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
             location_copy(location,left.location);
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerhigh,
               location.registerhigh));
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerlow,
               location.registerlow));
           end
         else
           begin
             secondpass(left);
             location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
             location_copy(location,left.location);
             if location.loc=LOC_CREGISTER then
              location.register := rg.getregisterint(exprasmlist,OS_INT);
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.register,
               left.location.register));
          end;
      end;

begin
   cmoddivnode:=tppcmoddivnode;
   cshlshrnode:=tppcshlshrnode;
   cunaryminusnode:=tppcunaryminusnode;
   cnotnode:=tppcnotnode;
end.
{
  $Log$
  Revision 1.30  2003-06-08 18:20:02  jonas
    * fixed small bug where a location was set to LOC_CREGISTER instead of
      LOC_REGISTER

  Revision 1.29  2003/06/04 11:58:58  jonas
    * calculate localsize also in g_return_from_proc since it's now called
      before g_stackframe_entry (still have to fix macos)
    * compilation fixes (cycle doesn't work yet though)

  Revision 1.28  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.27  2003/05/24 19:15:29  jonas
    * fixed shr of 64 bit values by non-immediate value

  Revision 1.26  2003/05/11 11:45:08  jonas
    * fixed shifts

  Revision 1.25  2003/04/24 12:57:32  florian
    * fixed not node

  Revision 1.24  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.23  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.22  2003/01/09 20:41:10  florian
    * fixed broken PowerPC compiler

  Revision 1.21  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.20  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.19  2002/09/10 21:21:29  jonas
    * fixed unary minus of 64bit values

  Revision 1.18  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.17  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.16  2002/08/10 17:15:31  jonas
    * various fixes and optimizations

  Revision 1.15  2002/07/26 10:48:34  jonas
    * fixed bug in shl/shr code

  Revision 1.14  2002/07/20 11:58:05  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.13  2002/07/11 07:41:27  jonas
    * fixed tppcmoddivnode
    * fixed 64bit parts of tppcshlshrnode

  Revision 1.12  2002/07/09 19:45:01  jonas
    * unarynminus and shlshr node fixed for 32bit and smaller ordinals
    * small fixes in the assembler writer
    * changed scratch registers, because they were used by the linker (r11
      and r12) and by the abi under linux (r31)

  Revision 1.11  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.10  2002/05/20 13:30:42  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.9  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.8  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.5  2002/05/13 19:52:46  peter
    * a ppcppc can be build again

  Revision 1.4  2002/04/21 15:48:39  carl
  * some small updates according to i386 version

  Revision 1.3  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

  Revision 1.2  2002/01/03 14:57:52  jonas
    * completed (not compilale yet though)

}
