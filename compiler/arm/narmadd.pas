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
       node,ncgadd,cpubase,cginfo;

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
                      GetResFlags:=F_CC;
                    lten:
                      GetResFlags:=F_LS;
                    gtn:
                      GetResFlags:=F_HI;
                    gten:
                      GetResFlags:=F_CS;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_HI;
                    lten:
                      GetResFlags:=F_CS;
                    gtn:
                      GetResFlags:=F_CC;
                    gten:
                      GetResFlags:=F_LS;
                  end;
              end;
        end;
      end;


    procedure tarmaddnode.second_addfloat;
      var
        op : TAsmOp;
      begin
        case aktfputype of
           fpu_fpa,
           fpu_fpa10,
           fpu_fpa11:
             begin
               { we will see what instruction set we'll use on the arm for FP
               pass_left_right;
               if (nf_swaped in flags) then
                 swapleftright;

               case nodetype of
                 addn :
                   op:=A_FADDs;
                 muln :
                   op:=A_FMULs;
                 subn :
                   op:=A_FSUBs;
                 slashn :
                   op:=A_FDIVs;
                 else
                   internalerror(200306014);
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

               exprasmlist.concat(taicpu.op_reg_reg_reg(op,
                  left.location.register,right.location.register,location.register));

               release_reg_left_right;
               }
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
        zeroreg : tregister;
      begin
        {!!!!!!!
        pass_left_right;
        force_reg_left_right(true,true);

        zeroreg.enum:=R_INTREGISTER;
        zeroreg.number:=NR_G0;

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(exprasmlist,A_SUBcc,left.location.register,right.location.value,zeroreg)
        else
          exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,zeroreg));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);

        release_reg_left_right;
        }
      end;


    procedure tarmaddnode.second_cmp64bit;
      var
        unsigned : boolean;
      begin
{$warning TODO 64bit compare}
        unsigned:=not(is_signed(left.resulttype.def)) or
                  not(is_signed(right.resulttype.def));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(unsigned);

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
                 tmpreg:=rg.getregisterint(exprasmlist,location.size);
                 cg.a_load_const_reg(exprasmlist,OS_INT,
                   aword(right.location.value),tmpreg);
                 exprasmlist.concat(taicpu.op_reg_reg(A_CMP,left.location.register,tmpreg));
                 rg.ungetregisterint(exprasmlist,tmpreg);
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
  Revision 1.2  2003-08-25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.1  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working
}
