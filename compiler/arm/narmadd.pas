{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the ARM

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
unit narmadd;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd,cpubase;

    type
       tarmaddnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpordinal;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmcpu,defutil,htypechk,
      cgbase,cpuinfo,pass_1,pass_2,regvars,cgcpu,
      cpupara,
      ncon,nset,nadd,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;

{*****************************************************************************
                               TSparcAddNode
*****************************************************************************}

    function tarmaddnode.GetResFlags(unsigned:Boolean):TResFlags;
      begin
        case NodeType of
          equaln:
            GetResFlags:=F_EQ;
          unequaln:
            GetResFlags:=F_NE;
          else
            if not(unsigned) then
              begin
                if nf_swaped in flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_GT;
                    lten:
                      GetResFlags:=F_GE;
                    gtn:
                      GetResFlags:=F_LT;
                    gten:
                      GetResFlags:=F_LE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_LT;
                    lten:
                      GetResFlags:=F_LE;
                    gtn:
                      GetResFlags:=F_GT;
                    gten:
                      GetResFlags:=F_GE;
                  end;
              end
            else
              begin
                if nf_swaped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_HI;
                    lten:
                      GetResFlags:=F_CS;
                    gtn:
                      GetResFlags:=F_CC;
                    gten:
                      GetResFlags:=F_LS;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_CC;
                    lten:
                      GetResFlags:=F_LS;
                    gtn:
                      GetResFlags:=F_HI;
                    gten:
                      GetResFlags:=F_CS;
                  end;
              end;
        end;
      end;


    procedure tarmaddnode.second_addfloat;
      var
        op : TAsmOp;
        instr : taicpu;
      begin
        case aktfputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              pass_left_right;
              if (nf_swaped in flags) then
                swapleftright;

              case nodetype of
                addn :
                  op:=A_ADF;
                muln :
                  op:=A_MUF;
                subn :
                  op:=A_SUF;
                slashn :
                  op:=A_DVF;
                else
                  internalerror(200308313);
              end;

              { force fpureg as location, left right doesn't matter
                as both will be in a fpureg }
              location_force_fpureg(exprasmlist,left.location,true);
              location_force_fpureg(exprasmlist,right.location,(left.location.loc<>LOC_CFPUREGISTER));

              location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
              if left.location.loc<>LOC_CFPUREGISTER then
                location.register:=left.location.register
              else
                location.register:=right.location.register;

              instr:=taicpu.op_reg_reg_reg(op,
                 location.register,left.location.register,right.location.register);
              instr.oppostfix:=cgsize2fpuoppostfix[def_cgsize(resulttype.def)];
              exprasmlist.concat(instr);

              release_reg_left_right;

              location.loc:=LOC_FPUREGISTER;
            end;
          fpu_soft:
            { this case should be handled already by pass1 }
            internalerror(200308252);
          else
            internalerror(200308251);
        end;
      end;


    procedure tarmaddnode.second_cmpfloat;
      begin
        { we will see what instruction set we'll use on the arm for FP
        pass_left_right;
        if (nf_swaped in flags) then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        location_force_fpureg(exprasmlist,left.location,true);
        location_force_fpureg(exprasmlist,right.location,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);

        exprasmlist.concat(taicpu.op_reg_reg(A_FCMPs,
           left.location.register,right.location.register));
        { Delay slot (can only contain integer operation) }
        exprasmlist.concat(taicpu.op_none(A_NOP));

        release_reg_left_right;
        }
        //!!!!
        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
      end;


    procedure tarmaddnode.second_cmpsmallset;
      var
        tmpreg : tregister;
      begin
        pass_left_right;

        location_reset(location,LOC_FLAGS,OS_NO);

        force_reg_left_right(false,false);

        case nodetype of
          equaln:
            begin
              exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              location.resflags:=F_EQ;
            end;
          unequaln:
            begin
              exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));
              location.resflags:=F_NE;
            end;
          lten,
          gten:
            begin
              if (not(nf_swaped in flags) and
                  (nodetype = lten)) or
                 ((nf_swaped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              tmpreg:=cg.getintregister(exprasmlist,location.size);
              exprasmlist.concat(taicpu.op_reg_reg_reg(A_AND,tmpreg,left.location.register,right.location.register));
              exprasmlist.concat(taicpu.op_reg_reg(A_CMP,tmpreg,right.location.register));
              cg.ungetregister(exprasmlist,tmpreg);
              location.resflags:=F_EQ;
            end;
          else
            internalerror(2004012401);
        end;
        release_reg_left_right;
      end;


    procedure tarmaddnode.second_cmp64bit;
      var
        unsigned : boolean;
        tmpreg : tregister;
      begin
        pass_left_right;
        force_reg_left_right(false,false);

        unsigned:=not(is_signed(left.resulttype.def)) or
                  not(is_signed(right.resulttype.def));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);

        { operation requiring proper N, Z and C flags ? }
        if unsigned or (nodetype in [equaln,unequaln]) then
          begin
            exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register64.reglo,right.location.register64.reglo));
            exprasmlist.concat(setcondition(taicpu.op_reg_reg(A_CMP,left.location.register64.reghi,right.location.register64.reghi),C_EQ));
          end
        { operation requiring proper N, V and C flags ? }
        else if nodetype in [gten,ltn] then
          begin
            tmpreg:=cg.getintregister(exprasmlist,location.size);
            exprasmlist.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SUB,tmpreg,left.location.register64.reglo,right.location.register64.reglo),PF_S));
            exprasmlist.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SBC,tmpreg,left.location.register64.reghi,right.location.register64.reghi),PF_S));
            cg.ungetregister(exprasmlist,tmpreg);
          end
        else
        { operation requiring proper N, Z and V flags ? }
          begin
            { this isn't possible so swap operands and use the "reverse" operation }
            tmpreg:=cg.getintregister(exprasmlist,location.size);
            exprasmlist.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SUB,tmpreg,right.location.register64.reglo,left.location.register64.reglo),PF_S));
            exprasmlist.concat(setoppostfix(taicpu.op_reg_reg_reg(A_SBC,tmpreg,right.location.register64.reghi,left.location.register64.reghi),PF_S));
            cg.ungetregister(exprasmlist,tmpreg);
            if location.resflags=F_GT then
              location.resflags:=F_LT
            else if location.resflags=F_LE then
              location.resflags:=F_GE
            else
              internalerror(200401221);
          end;

        release_reg_left_right;
      end;


    procedure tarmaddnode.second_cmpordinal;
      var
        unsigned : boolean;
        tmpreg : tregister;
        b : byte;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        unsigned:=not(is_signed(left.resulttype.def)) or
                  not(is_signed(right.resulttype.def));

        if right.location.loc = LOC_CONSTANT then
          begin
             if is_shifter_const(right.location.value,b) then
               exprasmlist.concat(taicpu.op_reg_const(A_CMP,left.location.register,right.location.value))
             else
               begin
                 tmpreg:=cg.getintregister(exprasmlist,location.size);
                 cg.a_load_const_reg(exprasmlist,OS_INT,
                   aword(right.location.value),tmpreg);
                 exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register,tmpreg));
                 cg.ungetregister(exprasmlist,tmpreg);
               end;
          end
        else
          exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register,right.location.register));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);

        release_reg_left_right;
      end;

begin
  caddnode:=tarmaddnode;
end.
{
  $Log$
  Revision 1.10  2004-01-24 20:19:46  florian
    * fixed some spilling stuff
    + not(<int64>) implemented
    + small set comparisations implemented

  Revision 1.9  2004/01/24 18:12:40  florian
    * fixed several arm floating point issues

  Revision 1.8  2004/01/23 00:01:48  florian
    * another fix to flag handling

  Revision 1.7  2004/01/22 20:13:18  florian
    * fixed several issues with flags

  Revision 1.6  2004/01/22 01:47:15  florian
    * improved register usage
    + implemented second_cmp64bit

  Revision 1.5  2003/11/02 14:30:03  florian
    * fixed ARM for new reg. allocation scheme

  Revision 1.4  2003/09/01 15:11:16  florian
    * fixed reference handling
    * fixed operand postfix for floating point instructions
    * fixed wrong shifter constant handling

  Revision 1.3  2003/09/01 09:54:57  florian
    *  results of work on arm port last weekend

  Revision 1.2  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.1  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working
}
