{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86 code for math nodes

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
unit nx86mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tx86unaryminusnode = class(tcgunaryminusnode)
{$ifdef SUPPORT_MMX}
         procedure second_mmx;override;
{$endif SUPPORT_MMX}
         procedure second_float;override;
         function pass_1:tnode;override;
      end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,defutil,
      cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,ncgutil,cgobj,cgx86;


{*****************************************************************************
                          TI386UNARYMINUSNODE
*****************************************************************************}

    function tx86unaryminusnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         if (left.resulttype.def.deftype=floatdef) then
           begin
             if use_sse(left.resulttype.def) then
               begin
                 if (registersmm < 1) then
                   registersmm := 1;
                 expectloc:=LOC_MMREGISTER;
               end
             else
               begin
                 if (registersfpu < 1) then
                   registersfpu := 1;
                 expectloc:=LOC_FPUREGISTER;
               end;
           end
{$ifdef SUPPORT_MMX}
         else
           if (cs_mmx in aktlocalswitches) and
              is_mmx_able_array(left.resulttype.def) then
             begin
               registers32:=left.registers32;
               registersfpu:=left.registersfpu;
               registersmmx:=left.registersmmx;
               if (left.location.loc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
         else
           inherited pass_1;
      end;


{$ifdef SUPPORT_MMX}
    procedure tx86unaryminusnode.second_mmx;
      var
        op : tasmop;
        hreg : tregister;
      begin
        secondpass(left);
        location_reset(location,LOC_MMXREGISTER,OS_NO);
        hreg:=cg.getmmxregister(exprasmlist,OS_M64);
        emit_reg_reg(A_PXOR,S_NO,hreg,hreg);
        case left.location.loc of
          LOC_MMXREGISTER:
            begin
               location.register:=left.location.register;
            end;
          LOC_CMMXREGISTER:
            begin
               location.register:=cg.getmmxregister(exprasmlist,OS_M64);
               emit_reg_reg(A_MOVQ,S_NO,left.location.register,location.register);
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE:
            begin
               reference_release(exprasmlist,left.location.reference);
               location.register:=cg.getmmxregister(exprasmlist,OS_M64);
               emit_ref_reg(A_MOVQ,S_NO,left.location.reference,location.register);
            end;
          else
            internalerror(200203225);
        end;
        if cs_mmx_saturation in aktlocalswitches then
          case mmx_type(resulttype.def) of
             mmxs8bit:
               op:=A_PSUBSB;
             mmxu8bit:
               op:=A_PSUBUSB;
             mmxs16bit,mmxfixed16:
               op:=A_PSUBSW;
             mmxu16bit:
               op:=A_PSUBUSW;
          end
        else
          case mmx_type(resulttype.def) of
             mmxs8bit,mmxu8bit:
               op:=A_PSUBB;
             mmxs16bit,mmxu16bit,mmxfixed16:
               op:=A_PSUBW;
             mmxs32bit,mmxu32bit:
               op:=A_PSUBD;
          end;
        emit_reg_reg(op,S_NO,location.register,hreg);
        cg.ungetregister(exprasmlist,hreg);
        emit_reg_reg(A_MOVQ,S_NO,hreg,location.register);
      end;
{$endif SUPPORT_MMX}


    procedure tx86unaryminusnode.second_float;
      var
        reg : tregister;
      begin
        secondpass(left);

        if expectloc=LOC_MMREGISTER then
          begin
            reg:=cg.getmmregister(exprasmlist,OS_M128);
            { zero out the register
              op size doesn't matter }
            cg.a_opmm_reg_reg(exprasmlist,OP_XOR,OS_F32,reg,reg,nil);
            { move to a mm compatible location }
            if left.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER] then
              location_force_mem(exprasmlist,left.location);
            cg.a_opmm_loc_reg(exprasmlist,OP_SUB,left.location.size,left.location,reg,mms_movescalar);
            location_release(exprasmlist,left.location);
            location_reset(location,LOC_MMREGISTER,def_cgsize(resulttype.def));
            location.register:=reg;
          end
        else
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
            case left.location.loc of
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
                  reference_release(exprasmlist,left.location.reference);
                  location.register:=NR_ST;
                  cg.a_loadfpu_ref_reg(exprasmlist,
                     def_cgsize(left.resulttype.def),
                     left.location.reference,location.register);
                  emit_none(A_FCHS,S_NO);
                end;
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER:
                begin
                   { "load st,st" is ignored by the code generator }
                   cg.a_loadfpu_reg_reg(exprasmlist,left.location.size,left.location.register,NR_ST);
                   location.register:=NR_ST;
                   emit_none(A_FCHS,S_NO);
                end;
              else
                internalerror(200312241);
            end;
          end;
      end;


end.
{
  $Log$
  Revision 1.1  2003-12-26 13:47:41  florian
    * rtl and compiler compile with -Cfsse2
}
