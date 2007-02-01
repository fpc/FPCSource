{
    Copyright (c) 1998-2007 by Free Pascal development team

    Generate PowerPC64 inline nodes

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
unit nppcinl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl,ncginl,ngppcinl;

    type
       tppc64inlinenode = class(tgppcinlinenode)
         function first_sqrt_real: tnode; override;
         procedure second_sqrt_real; override;
       end;

implementation

    uses
      cutils,globals,verbose,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      cgbase,pass_2,
      cpubase,ncgutil,
      cgutils,cgobj,rgobj;


{*****************************************************************************
                              tppc64inlinenode
*****************************************************************************}
function tppc64inlinenode.first_sqrt_real : tnode;
begin
  expectloc:=LOC_FPUREGISTER;
  registersint:=left.registersint;
  registersfpu:=max(left.registersfpu,1);
  first_sqrt_real := nil;
end;

procedure tppc64inlinenode.second_sqrt_real;
begin
  location.loc:=LOC_FPUREGISTER;
  load_fpu_location;
  case left.location.size of
  OS_F32:
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRTS,location.register,
      left.location.register));
  OS_F64:
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT,location.register,
      left.location.register));
  else
    inherited;
  end;
end;


begin
   cinlinenode:=tppc64inlinenode;
end.
