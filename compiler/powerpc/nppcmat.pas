{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      node,nmat;

    type
      tppcmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tppcshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
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
      symconst,symdef,aasm,types,
      cgbase,cgobj,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,cpuasm,cginfo,
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
         saved      : boolean;

      begin
         secondpass(left);
         saved:=maybe_savetotemp(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if saved then
           restorefromtemp(left,is_64bitint(left.resulttype.def));
         set_location(location,left.location);

         resultreg := R_NO;
         { put numerator in register }
         if (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           begin
             reference_release(exprasmlist,left.location.reference);
             numerator := rg.getregisterint(exprasmlist);
             { OS_32 because everything is always converted to longint/ }
             { cardinal in the resulttype pass (JM)                     }
             cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
               numerator);
             resultreg := numerator;
           end
         else
           begin
             numerator := left.location.register;
             if left.location.loc = LOC_CREGISTER then
               resultreg := rg.getregisterint(exprasmlist)
             else
               resultreg := numerator;
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
             case right.location.loc of
               LOC_CREGISTER, LOC_REGISTER:
                 divider := right.location.register;
               LOC_REFERENCE, LOC_CREFERENCE:
                 begin
                   divider := cg.get_scratch_reg(exprasmlist);
                   cg.a_load_ref_reg(exprasmlist,OS_32,
                     right.location.reference,divider);
                   reference_release(exprasmlist,right.location.reference);
                 end;
             end;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { And on PPC, the only way to catch a div-by-0 is by checking  }
             { the overflow flag (JM)                                       }
             op := divops[is_signed(right.resulttype.def),
                          cs_check_overflow in aktlocalswitches];
             exprasmlist.concat(taicpu.op_reg_reg_reg(op,resultreg,numerator,
               divider))
           end;
         { free used registers }
         if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
           cg.free_scratch_reg(exprasmlist,divider)
         else
           rg.ungetregister(exprasmlist,divider);
        if numerator <> resultreg then
          rg.ungetregisterint(exprasmlist,numerator);
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        cg.g_overflowcheck(exprasmlist,self);
      end;


{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}

    procedure tppcshlshrnode.pass_2;

      var
         resultreg, hregister1,hregister2,
         hregisterhigh,hregisterlow : tregister;
         op : topcg;
         asmop1, asmop2: tasmop;
         shiftval: aword;
         saved : boolean;

      begin
         secondpass(left);
         saved:=maybe_savetotemp(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if saved then
           restorefromtemp(left,is_64bitint(left.resulttype.def));

         if is_64bitint(left.resulttype.def) then
           begin
             case left.location.loc of
               LOC_REGISTER, LOC_CREGISTER:
                 begin
                   hregisterhigh := left.location.registerhigh;
                   hregisterlow := left.location.registerlow;
                   if left.location.loc = LOC_REGISTER then
                     begin
                       location.registerhigh := hregisterhigh;
                       location.registerlow := hregisterlow
                     end
                   else
                     begin
                       location.registerhigh := rg.getregisterint(exprasmlist);
                       location.registerlow := rg.getregisterint(exprasmlist);
                     end;
                 end;
               LOC_REFERENCE,LOC_CREFERENCE:
                 begin
                  { !!!!!!!! not good, registers are release too soon this way !!!! (JM) }
                   reference_release(exprasmlist,left.location.reference);
                   hregisterhigh := rg.getregisterint(exprasmlist);
                   location.registerhigh := hregisterhigh;
                   hregisterlow := rg.getregisterint(exprasmlist);
                   location.registerlow := hregisterlow;
                   tcg64f32(cg).a_load64_ref_reg(exprasmlist,
                     left.location.reference,hregisterlow,hregisterhigh);
                 end;
             end;
             if (right.nodetype = ordconstn) then
               begin
                 shiftval := tordconstnode(right).value;
                 if tordconstnode(right).value > 31 then
                   begin
                     if nodetype = shln then
                       begin
                         if (shiftval and 31) <> 0 then
                           cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,
                             shiftval and 31,hregisterlow,location.registerhigh);
                         cg.a_load_const_reg(exprasmlist,OS_32,0,location.registerlow);
                       end
                     else
                       begin
                         if (shiftval and 31) <> 0 then
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
                 case right.location.loc of
                   LOC_REGISTER,LOC_CREGISTER:
                     begin
                       hregister1 := right.location.register;
                     end;
                   LOC_REFERENCE,LOC_CREFERENCE:
                     begin
                       hregister1 := cg.get_scratch_reg(exprasmlist);
                       cg.a_load_ref_reg(exprasmlist,OS_S32,
                         right.location.reference,hregister1);
                     end;
                 end;
                 if nodetype = shln then
                   begin
                     asmop1 := A_SLW;
                     asmop2 := A_SRW;
                   end
                 else
                   begin
                     asmop1 := A_SRW;
                     asmop2 := A_SLW;
                     resultreg := location.registerhigh;
                     location.registerhigh := location.registerlow;
                     location.registerlow := resultreg;
                   end;

                 rg.getexplicitregisterint(exprasmlist,R_0);
                 exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                   R_0,hregister1,32));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.registerhigh,hregisterhigh,hregister1));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop2,
                   R_0,hregisterlow,R_0));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.registerhigh,location.registerhigh,R_0));
                 exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBI,
                   R_0,hregister1,32));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   R_0,hregisterlow,R_0));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.registerhigh,location.registerhigh,R_0));
                 exprasmlist.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.registerlow,hregisterlow,hregister1));
                 rg.ungetregister(exprasmlist,R_0);

                 if right.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                   cg.free_scratch_reg(exprasmlist,hregister1)
                 else
                   rg.ungetregister(exprasmlist,hregister1);
               end
           end
         else
           begin
             { load left operators in a register }
             if (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
               begin
                 reference_release(exprasmlist,left.location.reference);
                 hregister1 := rg.getregisterint(exprasmlist);
                 { OS_32 because everything is always converted to longint/ }
                 { cardinal in the resulttype pass (JM)                     }
                 cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,
                   hregister1);
                 resultreg := hregister1;
               end
             else
               begin
                 hregister1 := left.location.register;
                 if left.location.loc = LOC_CREGISTER then
                   resultreg := rg.getregisterint(exprasmlist)
                 else
                   resultreg := hregister1;
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
                  case right.location.loc of
                    LOC_CREGISTER, LOC_REGISTER:
                      hregister2 := right.location.register;
                    LOC_REFERENCE, LOC_CREFERENCE:
                      begin
                        hregister2 := cg.get_scratch_reg(exprasmlist);
                        cg.a_load_ref_reg(exprasmlist,OS_32,
                          right.location.reference,hregister2);
                        reference_release(exprasmlist,right.location.reference);
                      end;
                  end;

                  tcgppc(cg).a_op_reg_reg_reg(exprasmlist,op,OS_32,hregister1,
                    hregister2,resultreg);

                  if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                    cg.free_scratch_reg(exprasmlist,hregister2)
                  else
                    rg.ungetregister(exprasmlist,hregister2);
                end;
              { set result location }
              location.loc:=LOC_REGISTER;
              location.register:=resultreg;
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
              clear_location(location);
              location.loc:=LOC_REGISTER;
              case left.location.loc of
                LOC_REGISTER, LOC_CREGISTER :
                  begin
                    src1 := left.location.registerlow;
                    src2 := left.location.registerhigh;
                    if left.location.loc = LOC_REGISTER then
                      begin
                        location.registerlow:=src1;
                        location.registerhigh:=src2;
                      end
                    else
                      begin
                        location.registerlow := rg.getregisterint(exprasmlist);
                        location.registerhigh := rg.getregisterint(exprasmlist);
                      end;
                  end;
                LOC_REFERENCE,LOC_CREFERENCE :
                  begin
                    reference_release(exprasmlist,left.location.reference);
                    location.registerlow:=rg.getregisterint(exprasmlist);
                    src1 := location.registerlow;
                    location.registerhigh:=rg.getregisterint(exprasmlist);
                    src2 := location.registerhigh;
                    tcg64f32(cg).a_load64_ref_reg(exprasmlist,left.location.reference,
                      location.registerlow,
                      location.registerhigh);
                  end;
              end;
             exprasmlist.concat(taicpu.op_reg_reg(A_NEG,location.registerlow,
               src1));
             cg.a_op_reg_reg(exprasmlist,OP_NOT,OS_32,src2,location.registerhigh);
             tmp := cg.get_scratch_reg(exprasmlist);
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_32,31,location.registerlow,
               tmp);
             if not(cs_check_overflow in aktlocalswitches) then
               cg.a_op_reg_reg(exprasmlist,OP_ADD,OS_32,location.registerhigh,
                 tmp)
             else
               exprasmlist.concat(taicpu.op_reg_reg_reg(A_ADDO_,tmp,
                 location.registerhigh,tmp));
             cg.free_scratch_reg(exprasmlist,tmp);
           end
         else
           begin
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
                       location.register := rg.getregisterint(exprasmlist)
                     else
                       location.register := rg.getregisterfpu(exprasmlist);
                  end;
                LOC_REFERENCE,LOC_CREFERENCE:
                  begin
                     reference_release(exprasmlist,left.location.reference);
                     if (left.resulttype.def.deftype=floatdef) then
                       begin
                          src1 := rg.getregisterfpu(exprasmlist);
                          location.register := src1;
                          cg.a_loadfpu_ref_reg(exprasmlist,
                            def_cgsize(left.resulttype.def),
                            left.location.reference,src1);
                       end
                     else
                       begin
                          src1 := rg.getregisterint(exprasmlist);
                          location.register:= src1;
                          cg.a_load_ref_reg(exprasmlist,OS_32,
                            left.location.reference,src1);
                       end;
                  end;
              end;
              { choose appropriate operand }
              if left.resulttype.def.deftype <> floatdef then
                if not(cs_check_overflow in aktlocalswitches) then
                  op := A_NEG
                else
                  op := A_NEGO_
              else
                op := A_FNEG;
              { emit operation }
              exprasmlist.concat(taicpu.op_reg_reg(op,location.register,src1));
           end;
{ Here was a problem...     }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!     }
{ So this is useless...     }
{ that's not true: -2^31 gives an overflow error if it is negated (FK) }
        cg.g_overflowcheck(exprasmlist,self);
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
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,
              LOC_FLAGS,LOC_REGISTER,LOC_CREGISTER] then
              secondpass(left);
            case left.location.loc of
              LOC_JUMP :
                begin
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(left);
                  maketojumpbool(left,lr_load_regvars);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  location.resflags:=left.location.resflags;
                  inverse_flags(left.location.resflags);
                end;
              LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_CREFERENCE :
                begin
                  if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                    regl := left.location.register
                  else
                    begin
                      regl := rg.getregisterint(exprasmlist);
                      cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                        left.location.reference,regl);
                    end;
                  location.loc:=LOC_FLAGS;
                  location.resflags.cr:=r_cr0;
                  location.resflags.flag:=F_EQ;
                  exprasmlist.concat(taicpu.op_reg_const(A_CMPWI,regl,0));
                  rg.ungetregister(exprasmlist,regl);
                end;
            end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
             secondpass(left);
             clear_location(location);
             location.loc:=LOC_REGISTER;
             { make sure left is in a register and set the dest register }
             case left.location.loc of
               LOC_REFERENCE, LOC_CREFERENCE, LOC_CREGISTER:
                 begin
                   location.registerlow := rg.getregisterint(exprasmlist);
                   location.registerhigh := rg.getregisterint(exprasmlist);
                   if left.location.loc <> LOC_CREGISTER then
                     begin
                       tcg64f32(cg).a_load64_ref_reg(exprasmlist,
                         left.location.reference,location.registerlow,
                         location.registerhigh);
                       regl := location.registerlow;
                       regh := location.registerhigh;
                     end
                   else
                     begin
                       regl := left.location.registerlow;
                       regh := left.location.registerhigh;
                     end;
                 end;
               LOC_REGISTER:
                 begin
                   regl := left.location.registerlow;
                   location.registerlow := regl;
                   regh := left.location.registerhigh;
                   location.registerhigh := regh;
                 end;
             end;
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerhigh,
               regh));
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerlow,
               regl));
           end
         else
           begin
             secondpass(left);
             clear_location(location);
             location.loc:=LOC_REGISTER;
             { make sure left is in a register and set the dest register }
             case left.location.loc of
               LOC_REFERENCE, LOC_CREFERENCE, LOC_CREGISTER:
                 begin
                   location.register := rg.getregisterint(exprasmlist);
                   if left.location.loc <> LOC_CREGISTER then
                     begin
                       cg.a_load_ref_reg(exprasmlist,
                         def_cgsize(left.resulttype.def),
                         left.location.reference,location.register);
                       regl := location.register;
                     end
                   else
                     regl := left.location.register;
                 end;
               LOC_REGISTER:
                 regl := left.location.register;
             end;
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.register,
               regl));
             { release the source reg if it wasn't reused }
             if regl <> location.register then
               rg.ungetregisterint(exprasmlist,regl);
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
  Revision 1.3  2002-04-06 18:13:02  jonas
    * several powerpc-related additions and fixes

  Revision 1.2  2002/01/03 14:57:52  jonas
    * completed (not compilale yet though)

  Revision 1.1  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)


}
