{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generates ARM inline nodes

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
unit narminl;

{$i fpcdefs.inc}

interface

    uses
      node,ninl,ncginl;

    type
      tarminlinenode = class(tcgInlineNode)
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
      private
        procedure load_fpu_location;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,fmodule,
      symconst,symdef,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_1,pass_2,
      cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,
      tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;

{*****************************************************************************
                              tarminlinenode
*****************************************************************************}

    procedure tarminlinenode.load_fpu_location;
      begin
        secondpass(left);
        location_force_fpureg(exprasmlist,left.location,true);
        location_copy(location,left.location);
        if left.location.loc=LOC_CFPUREGISTER then
          begin
           location.register:=rg.getregisterfpu(exprasmlist,location.size);
           location.loc := LOC_FPUREGISTER;
         end;
      end;


    function tarminlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
        first_abs_real := nil;
      end;


    function tarminlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
        first_sqr_real:=nil;
      end;


    function tarminlinenode.first_sqrt_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
        first_sqrt_real := nil;
      end;


    procedure tarminlinenode.second_abs_real;
      begin
        load_fpu_location;
        exprasmlist.concat(taicpu.op_reg(A_ABS,location.register));
      end;


    procedure tarminlinenode.second_sqr_real;
      begin
        load_fpu_location;
        exprasmlist.concat(taicpu.op_reg_reg(A_MUF,location.register,left.location.register));
      end;


    procedure tarminlinenode.second_sqrt_real;
      begin
        load_fpu_location;
        exprasmlist.concat(taicpu.op_reg(A_SQT,location.register));
      end;

begin
  cinlinenode:=tarminlinenode;
end.
{
  $Log$
  Revision 1.1  2003-08-28 00:05:29  florian
    * today's arm patches
}
