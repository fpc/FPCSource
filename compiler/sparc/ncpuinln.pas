{******************************************************************************
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

 *****************************************************************************}
unit nCpuInln;
{Generate SPARC inline nodes}
{$INCLUDE fpcdefs.inc}
interface
uses
  node,ninl,ncginl;
type
  tSparcInlineNode = class(tcgInlineNode)
    {first pass override, so that the code generator will actually generate
    these nodes.}
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
  cga,tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;
{*****************************************************************************
                              TSparcInlineNode
*****************************************************************************}
function tSparcInlineNode.first_abs_real : tnode;
  begin
    location.loc:=LOC_FPUREGISTER;
    registers32:=left.registers32;
    registersfpu:=max(left.registersfpu,1);
    first_abs_real := nil;
  end;
function tSparcInlineNode.first_sqr_real : tnode;
  begin
    location.loc:=LOC_FPUREGISTER;
    registers32:=left.registers32;
    registersfpu:=max(left.registersfpu,1);
    first_sqr_real := nil;
  end;
function tSparcInlineNode.first_sqrt_real : tnode;
  begin
    location.loc:=LOC_FPUREGISTER;
    registers32:=left.registers32;
    registersfpu:=max(left.registersfpu,1);
    first_sqrt_real := nil;
  end;
{ load the FPU into the an fpu register }
procedure tSparcInlineNode.load_fpu_location;
  begin
    location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
    secondpass(left);
    case left.location.loc of
      LOC_FPUREGISTER:
        location.register := left.location.register;
      LOC_CFPUREGISTER:
        begin
          location.register := rg.getregisterfpu(exprasmlist);
        end;
      LOC_REFERENCE,LOC_CREFERENCE:
        begin
          location.register := rg.getregisterfpu(exprasmlist);
          cg.a_loadfpu_ref_reg(exprasmlist,
          def_cgsize(left.resulttype.def),
          left.location.reference,location.register);
          location_release(exprasmlist,left.location);
          location_reset(left.location,LOC_FPUREGISTER,
          left.location.size);
          left.location.register := location.register;
        end
      else
        internalerror(309991);
    end;
  end;
procedure tSparcInlineNode.second_abs_real;
  begin
    load_fpu_location;
    exprasmlist.concat(taicpu.op_reg_reg(A_NONE,location.register,
    left.location.register));
  end;
procedure tSparcInlineNode.second_sqr_real;
  begin
    load_fpu_location;
    exprasmlist.concat(taicpu.op_reg_reg_reg(A_FMULS,location.register,
    left.location.register,left.location.register));
  end;
procedure tSparcInlineNode.second_sqrt_real;
  begin
    load_fpu_location;
    exprasmlist.concat(taicpu.op_reg_reg(A_NONE,location.register,
    left.location.register));
  end;
begin
  cInlineNode:=tSparcInlineNode;
end.
{
  $Log$
  Revision 1.1  2002-11-30 20:03:49  mazen
  + ncpuinln node

  Revision 1.3  2002/09/18 09:19:37  jonas
    * fixed LOC_REFERENCE/LOC_CREFERENCE problems

  Revision 1.2  2002/08/19 17:35:42  jonas
    * fixes

  Revision 1.1  2002/08/10 17:15:00  jonas
    + abs, sqr, sqrt implementations
}
