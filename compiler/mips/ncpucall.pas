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


begin
  ccallnode := TMIPSELCallNode;
end.
