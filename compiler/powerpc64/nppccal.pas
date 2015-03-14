{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the PowerPC specific part of call nodes

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
unit nppccal;

{$I fpcdefs.inc}

interface

uses
  aasmdata, cgbase,
  symdef, node, ncal, ncgcal;

type

  tppccallparanode = class(tcgcallparanode)
  end;

  tppccallnode = class(tcgcallnode)
   protected
    function get_call_reg(list: TAsmList): tregister; override;
    procedure unget_call_reg(list: TAsmList; reg: tregister); override;
   public
    function pass_1: tnode; override;
    procedure do_syscall; override;
  end;

implementation

uses
  globtype, systems,
  cutils, verbose, globals,
  symconst, symbase, symsym, symtable, defutil, paramgr, parabase,
  pass_2,
  cpuinfo, cpubase, aasmbase, aasmtai, aasmcpu,
  nmem, nld, ncnv,
  ncgutil, cgutils, cgobj, tgobj, regvars, rgobj, rgcpu,
  cgcpu, cpupi, procinfo;


function tppccallnode.get_call_reg(list: TAsmList): tregister;
  begin
    { on the ppc64/ELFv2 abi, all indirect calls must go via R12, so that the
      called function can use R12 as PIC base register }
    cg.getcpuregister(list,NR_R12);
    result:=NR_R12;
  end;


procedure tppccallnode.unget_call_reg(list: TAsmList; reg: tregister);
  begin
    cg.ungetcpuregister(list,NR_R12);
  end;

function tppccallnode.pass_1: tnode;
  begin
    result:=inherited;
    if assigned(result) then
      exit;
    { for ELFv2, we must load the TOC/GOT register in case this routine may
      call an external routine (because the lookup of the target address is
      TOC-based). Maybe needs to be extended to non-ELFv2 too }
    if target_info.abi=abi_powerpc_elfv2 then
      include(current_procinfo.flags,pi_needs_got);
  end;


procedure tppccallnode.do_syscall;
begin
  { no MorphOS style syscalls supported. Only implemented to avoid abstract 
   method not implemented compiler warning. }
  internalerror(2005120401);
end;

begin
  ccallparanode:=tppccallparanode;
  ccallnode := tppccallnode;
end.

