{
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

      tx86notnode = class(tcgnotnode)
         procedure second_boolean;override;
{$ifdef SUPPORT_MMX}
         procedure second_mmx;override;
{$endif SUPPORT_MMX}
      end;

  implementation

    uses
      systems,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmtai,defutil,
      cgbase,pass_1,pass_2,
      ncon,
      cpubase,
      cga,ncgutil,cgobj,cgx86,cgutils;


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
               registersint:=left.registersint;
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
        href : treference;
        l1 : tasmlabel;
      begin
        secondpass(left);

        if expectloc=LOC_MMREGISTER then
          begin
            location_force_mmregscalar(exprasmlist,left.location,false);
            location_reset(location,LOC_MMREGISTER,def_cgsize(resulttype.def));

            { make life of register allocator easier }
            location.register:=cg.getmmregister(exprasmlist,def_cgsize(resulttype.def));
            cg.a_loadmm_reg_reg(exprasmlist,def_cgsize(resulttype.def),def_cgsize(resulttype.def),left.location.register,location.register,mms_movescalar);

            reg:=cg.getmmregister(exprasmlist,def_cgsize(resulttype.def));

            objectlibrary.getdatalabel(l1);
            consts.concat(Tai_label.Create(l1));
            case def_cgsize(resulttype.def) of
              OS_F32:
                consts.concat(tai_const.create_32bit(longint(1 shl 31)));
              OS_F64:
                begin
                  consts.concat(tai_const.create_32bit(0));
                  consts.concat(tai_const.create_32bit(-(1 shl 31)));
                end
              else
                internalerror(2004110215);
            end;

            reference_reset_symbol(href,l1,0);
            cg.a_loadmm_ref_reg(exprasmlist,def_cgsize(resulttype.def),def_cgsize(resulttype.def),href,reg,mms_movescalar);

            cg.a_opmm_reg_reg(exprasmlist,OP_XOR,left.location.size,reg,location.register,nil);
          end
        else
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
            case left.location.loc of
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
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


{*****************************************************************************
                               TX86NOTNODE
*****************************************************************************}

    procedure tx86notnode.second_boolean;
      var
         hl : tasmlabel;
         opsize : tcgsize;
      begin
        opsize:=def_cgsize(resulttype.def);

        if left.expectloc=LOC_JUMP then
         begin
           location_reset(location,LOC_JUMP,OS_NO);
           hl:=truelabel;
           truelabel:=falselabel;
           falselabel:=hl;
           secondpass(left);
           maketojumpbool(exprasmlist,left,lr_load_regvars);
           hl:=truelabel;
           truelabel:=falselabel;
           falselabel:=hl;
         end
        else
         begin
           { the second pass could change the location of left }
           { if it is a register variable, so we've to do      }
           { this before the case statement                    }
           secondpass(left);
           case left.expectloc of
             LOC_FLAGS :
               begin
                 location_reset(location,LOC_FLAGS,OS_NO);
                 location.resflags:=left.location.resflags;
                 inverse_flags(location.resflags);
               end;
             LOC_CONSTANT,
             LOC_REGISTER,
             LOC_CREGISTER,
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 location_force_reg(exprasmlist,left.location,opsize,true);
                 emit_reg_reg(A_TEST,TCGSize2Opsize[opsize],left.location.register,left.location.register);
                 location_reset(location,LOC_FLAGS,OS_NO);
                 location.resflags:=F_E;
               end;
            else
               internalerror(200203224);
           end;
         end;
      end;


{$ifdef SUPPORT_MMX}
    procedure tx86notnode.second_mmx;

    var hreg,r:Tregister;

    begin
      secondpass(left);
      location_reset(location,LOC_MMXREGISTER,OS_NO);
      r:=cg.getintregister(exprasmlist,OS_INT);
      emit_const_reg(A_MOV,S_L,longint($ffffffff),r);
      { load operand }
      case left.location.loc of
        LOC_MMXREGISTER:
          location_copy(location,left.location);
        LOC_CMMXREGISTER:
          begin
            location.register:=cg.getmmxregister(exprasmlist,OS_M64);
            emit_reg_reg(A_MOVQ,S_NO,left.location.register,location.register);
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE:
          begin
            location_release(exprasmlist,left.location);
            location.register:=cg.getmmxregister(exprasmlist,OS_M64);
            emit_ref_reg(A_MOVQ,S_NO,left.location.reference,location.register);
          end;
      end;
      { load mask }
      hreg:=cg.getmmxregister(exprasmlist,OS_M64);
      emit_reg_reg(A_MOVD,S_NO,r,hreg);
      cg.ungetregister(exprasmlist,r);
      { lower 32 bit }
      emit_reg_reg(A_PXOR,S_D,hreg,location.register);
      { shift mask }
      emit_const_reg(A_PSLLQ,S_NO,32,hreg);
      { higher 32 bit }
      cg.ungetregister(exprasmlist,hreg);
      emit_reg_reg(A_PXOR,S_D,hreg,location.register);
    end;
{$endif SUPPORT_MMX}
end.
