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
unit ncpumat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tSparcmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tSparcshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tSparcnotnode = class(tcgnotnode)
         procedure second_boolean;override;
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
      ncgutil,cgcpu,cg64f32,rgobj;

{*****************************************************************************
                             TSparcMODDIVNODE
*****************************************************************************}

    procedure tSparcmoddivnode.pass_2;
      const
                    { signed   overflow }
        divops: array[boolean, boolean] of tasmop =
          ((A_SDIV,A_UDIV),(A_SDIVcc,A_UDIVcc));
      var
         power,
         l1, l2     : longint;
         op         : tasmop;
         tmpreg,
         numerator,
         divider,
         resultreg  : tregister;
         saved      : tmaybesave;

      begin
         secondpass(left);
{$ifndef newra}
         maybe_save(exprasmlist,right.registers32,left.location,saved);
{$endif}
         secondpass(right);
{$ifndef newra}
         maybe_restore(exprasmlist,left.location,saved);
{$endif}
         location_copy(location,left.location);

         { put numerator in register }
         location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
         location_copy(location,left.location);
         numerator := location.register;
         resultreg := location.register;
         if (location.loc = LOC_CREGISTER) then
           begin
             location.loc := LOC_REGISTER;
             location.register := rg.getregisterint(exprasmlist,OS_INT);
             resultreg := location.register;
           end;
         if (nodetype = modn) then
           begin
{$ifdef newra}
             resultreg := rg.getregisterint(exprasmlist,OS_INT);
{$else}
             resultreg := cg.get_scratch_reg_int(exprasmlist,OS_INT);
{$endif}
           end;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value,power) then
           begin
{$ifdef newra}
             tmpreg:=rg.getregisterint(exprasmlist,OS_INT);
{$else}
             tmpreg:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
{$endif}
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_INT,31,numerator,tmpreg);
             { if signed, tmpreg=right value-1, otherwise 0 }
             cg.a_op_const_reg(exprasmlist,OP_AND,OS_INT,tordconstnode(right).value-1,tmpreg);
             { add to the left value }
             cg.a_op_reg_reg(exprasmlist,OP_ADD,OS_INT,tmpreg,numerator);
{$ifdef newra}
             rg.ungetregisterint(exprasmlist,tmpreg);
{$else}
             cg.free_scratch_reg(exprasmlist,tmpreg);
{$endif}
             cg.a_op_const_reg_reg(exprasmlist,OP_SAR,OS_INT,aword(power),numerator,resultreg);
           end
         else
           begin
             { load divider in a register if necessary }
             location_force_reg(exprasmlist,right.location,
               def_cgsize(right.resulttype.def),true);
             divider := right.location.register;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { And on Sparc, the only way to catch a div-by-0 is by checking  }
             { the overflow flag (JM)                                       }
             op := divops[is_signed(right.resulttype.def),
                          cs_check_overflow in aktlocalswitches];
             exprasmlist.concat(taicpu.op_reg_reg_reg(op,numerator,divider,resultreg));

             if (nodetype = modn) then
               begin
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_SMUL,resultreg,divider,resultreg));
                 rg.UnGetRegisterInt(exprasmlist,divider);
                 exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,location.register,numerator,resultreg));
  {$ifdef newra}
                 rg.ungetregisterint(exprasmlist,resultreg);
  {$else}
                 cg.free_scratch_reg(exprasmlist,resultreg);
  {$endif}
                 resultreg := location.register;
               end
             else
               rg.UnGetRegisterInt(exprasmlist,divider);
           end;
        { free used registers }
        if numerator.number <> resultreg.number then
          rg.ungetregisterint(exprasmlist,numerator);
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        cg.g_overflowcheck(exprasmlist,Location,ResultType.Def);
      end;


{*****************************************************************************
                             TSparcSHLRSHRNODE
*****************************************************************************}

function TSparcShlShrNode.first_shlshr64bitint:TNode;
  begin
    result := nil;
  end;

procedure tSparcshlshrnode.pass_2;
  var
    resultreg, hregister1,hregister2,
    hregisterhigh,hregisterlow : tregister;
    op : topcg;
    asmop1, asmop2: tasmop;
    shiftval: aword;
    saved : tmaybesave;
    r:Tregister;
  begin
    secondpass(left);
{$ifndef newra}
    maybe_save(exprasmlist,right.registers32,left.location,saved);
{$endif}
    secondpass(right);
{$ifndef newra}
    maybe_restore(exprasmlist,left.location,saved);
{$endif}
    if is_64bitint(left.resulttype.def)
    then
      begin
        location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
        location_copy(location,left.location);
        hregisterhigh := location.registerhigh;
        hregisterlow := location.registerlow;
        if (location.loc = LOC_CREGISTER) then
          begin
            location.loc := LOC_REGISTER;
            location.registerhigh := rg.getregisterint(exprasmlist,OS_INT);
            location.registerlow := rg.getregisterint(exprasmlist,OS_INT);
          end;
        if (right.nodetype = ordconstn) then
          begin
            shiftval := tordconstnode(right).value;
            if tordconstnode(right).value > 31 then
              begin
                if nodetype = shln then
                  begin
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval and 31,hregisterlow,location.registerhigh);
                    cg.a_load_const_reg(exprasmlist,OS_32,0,location.registerlow);
                  end
                else
                  begin
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval and 31,hregisterhigh,location.registerlow);
                    cg.a_load_const_reg(exprasmlist,OS_32,0,location.registerhigh);
                  end;
              end
            else
              begin
{$warning TODO shl 64bit const}
                if nodetype = shln then
                  begin
                    {exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,location.registerhigh,hregisterhigh,shiftval,0,31-shiftval));
                    exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,location.registerhigh,hregisterlow,shiftval,32-shiftval,31));
                    exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,location.registerlow,hregisterlow,shiftval,0,31-shiftval));}
                  end
                else
                  begin
                    {exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,location.registerlow,hregisterlow,32-shiftval,shiftval,31));
                    exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,location.registerlow,hregisterhigh,32-shiftval,0,shiftval-1));
                    exprasmlist.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,location.registerhigh,hregisterhigh,32-shiftval,shiftval,31));}
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
                asmop1 := A_SLL;
                asmop2 := A_SRL;
              end
            else
              begin
                asmop1 := A_SRL;
                asmop2 := A_SLL;
                resultreg := location.registerhigh;
                location.registerhigh := location.registerlow;
                location.registerlow := resultreg;
              end;
{$warning TODO shl 64bit no-const}
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
            resultreg := rg.getregisterint(exprasmlist,OS_INT);
            location.register := resultreg;
          end;
        { determine operator }
        if nodetype=shln then
          op:=OP_SHL
        else
          op:=OP_SHR;
        { shifting by a constant directly coded: }
        if (right.nodetype=ordconstn) then
          cg.a_op_const_reg_reg(exprasmlist,op,OS_32,tordconstnode(right).value and 31,hregister1,resultreg)
        else
          begin
            { load shift count in a register if necessary }
            location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
            hregister2 := right.location.register;
            cg.a_op_reg_reg_reg(exprasmlist,op,OS_32,hregister2,hregister1,resultreg);
            rg.UnGetRegisterInt(exprasmlist,hregister2);
          end;
      end;
  end;


{*****************************************************************************
                               TSPARCNOTNODE
*****************************************************************************}

    procedure tsparcnotnode.second_boolean;
      var
        hl : tasmlabel;
        zeroreg : tregister;
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
                  zeroreg.enum:=R_INTREGISTER;
                  zeroreg.number:=NR_G0;
                  exprasmlist.concat(taicpu.op_reg_const_reg(A_SUBcc,left.location.register,0,zeroreg));
                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
               end;
              else
                internalerror(2003042401);
            end;
          end;
      end;


begin
   cmoddivnode:=tSparcmoddivnode;
   cshlshrnode:=tSparcshlshrnode;
   cnotnode:=tSparcnotnode;
end.
{
  $Log$
  Revision 1.12  2003-07-06 22:09:32  peter
    * shr and div fixed

  Revision 1.11  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.10  2003/06/04 20:59:37  mazen
  + added size of destination in code gen methods
  + making g_overflowcheck declaration same as
    ancestor's method declaration

  Revision 1.9  2003/06/01 21:38:07  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.8  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.7  2003/03/15 22:51:58  mazen
  * remaking sparc rtl compile

  Revision 1.6  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.5  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.4  2003/02/04 21:50:54  mazen
  * fixing internal errors related to notn when compiling RTL

  Revision 1.3  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.2  2002/12/30 21:17:22  mazen
  - unit cga no more used in sparc compiler.

  Revision 1.1  2002/12/21 23:22:59  mazen
  + added shift support

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
    * fixed tSparcmoddivnode
    * fixed 64bit parts of tSparcshlshrnode

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
    * a ppcSparc can be build again

  Revision 1.4  2002/04/21 15:48:39  carl
  * some small updates according to i386 version

  Revision 1.3  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

  Revision 1.2  2002/01/03 14:57:52  jonas
    * completed (not compilale yet though)

}
