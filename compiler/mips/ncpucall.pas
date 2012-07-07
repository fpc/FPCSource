{
    Copyright (c) 1998-2009 by Florian Klaempfl and David Zhang

    Generate MIPSEL assembler for in call nodes

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
unit ncpucall;

{$i fpcdefs.inc}

interface

uses
  node, ncgcal;

type
  tMIPSELcallnode = class(tcgcallnode)
    function  pass_1 : tnode; override;
    procedure extra_call_code; override;
    procedure extra_post_call_code; override;
  end;


implementation

uses
  globtype,cpubase,procinfo,
  aasmtai,aasmcpu,aasmdata,
  paramgr,
  ncal;

function TMIPSELcallnode.pass_1 : tnode;
begin
  pass_1 := inherited pass_1;
  if assigned(current_procinfo) and
     assigned(procdefinition) and
	 (procdefinition.proccalloption=pocall_cdecl) then
	include(current_procinfo.flags,pi_needs_got);
end;

procedure tMIPSELcallnode.extra_call_code;
begin
  { MIPS functions should never modify the stack pointer
    after the prologue.
    Enough space must be allocated inside the prologue, not after.  }
  //  if pushedparasize > 0 then
  //  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ADDIU, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, -pushedparasize));
end;

procedure tMIPSELcallnode.extra_post_call_code;
begin
  { MIPS functions should never modify the stack pointer
    after the prologue.
    Enough space must be allocated inside the prologue, not after.  }
   // if pushedparasize > 0 then
   // current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ADDIU, NR_STACK_POINTER_REG, NR_STACK_POINTER_REG, pushedparasize));
end;


begin
  ccallnode := TMIPSELCallNode;
end.
