{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the SPARC

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
unit ncpuadd;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd,cpubase,cginfo;

    type
       tsparcaddnode = class(tcgaddnode)
       private
          function  GetResFlags(unsigned:Boolean):TResFlags;
       protected
          procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmpsmallset;override;
          procedure second_cmp64bit;override;
          procedure second_cmpordinal;override;
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

    function TSparcAddNode.GetResFlags(unsigned:Boolean):TResFlags;
      begin
        case NodeType of
          equaln:
            GetResFlags:=F_E;
          unequaln:
            GetResFlags:=F_NE;
          else
            if not(unsigned) then
              begin
                if nf_swaped in flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_G;
                    lten:
                      GetResFlags:=F_GE;
                    gtn:
                      GetResFlags:=F_L;
                    gten:
                      GetResFlags:=F_LE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_L;
                    lten:
                      GetResFlags:=F_LE;
                    gtn:
                      GetResFlags:=F_G;
                    gten:
                      GetResFlags:=F_GE;
                  end;
              end
            else
              begin
                if nf_swaped in Flags then
                  case NodeType of
                    ltn:
                      GetResFlags:=F_A;
                    lten:
                      GetResFlags:=F_AE;
                    gtn:
                      GetResFlags:=F_B;
                    gten:
                      GetResFlags:=F_BE;
                  end
                else
                  case NodeType of
                    ltn:
                      GetResFlags:=F_B;
                    lten:
                      GetResFlags:=F_BE;
                    gtn:
                      GetResFlags:=F_A;
                    gten:
                      GetResFlags:=F_AE;
                  end;
              end;
        end;
      end;


    procedure tsparcaddnode.second_addfloat;
      var
        op : TAsmOp;
      begin
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
           left.location.register,right.location.register,location.register))
      end;


    procedure tsparcaddnode.second_cmpfloat;
      begin
        pass_left_right;
        if (nf_swaped in flags) then
          swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
        location_force_fpureg(exprasmlist,left.location,true);
        location_force_fpureg(exprasmlist,right.location,true);

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags := getresflags(true);

        exprasmlist.concat(taicpu.op_reg_reg(A_FCMPs,
           left.location.register,right.location.register));
        { Delay slot (can only contain integer operation) }
        exprasmlist.concat(taicpu.op_none(A_NOP));
      end;


    procedure tsparcaddnode.second_cmpboolean;
      var
        zeroreg : tregister;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        zeroreg.enum:=R_INTREGISTER;
        zeroreg.number:=NR_G0;

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(exprasmlist,A_SUBcc,left.location.register,right.location.value,zeroreg)
        else
          exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,zeroreg));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags := getresflags(true);
      end;


    procedure tsparcaddnode.second_cmpsmallset;
      var
        zeroreg : tregister;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        zeroreg.enum:=R_INTREGISTER;
        zeroreg.number:=NR_G0;

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(exprasmlist,A_SUBcc,left.location.register,right.location.value,zeroreg)
        else
          exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,zeroreg));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags := getresflags(true);
      end;


    procedure tsparcaddnode.second_cmp64bit;
      begin
        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags := getresflags(true);
      end;


    procedure tsparcaddnode.second_cmpordinal;
      var
        zeroreg : tregister;
      begin
        pass_left_right;
        force_reg_left_right(true,true);

        zeroreg.enum:=R_INTREGISTER;
        zeroreg.number:=NR_G0;

        if right.location.loc = LOC_CONSTANT then
          tcgsparc(cg).handle_reg_const_reg(exprasmlist,A_SUBcc,left.location.register,right.location.value,zeroreg)
        else
          exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBcc,left.location.register,right.location.register,zeroreg));

        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags := getresflags(true);
      end;

begin
  caddnode:=tsparcaddnode;
end.
{
  $Log$
  Revision 1.16  2003-07-06 17:44:12  peter
    * cleanup and first sparc implementation

  Revision 1.15  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

}
