{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the PowerPC specific part of call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
  symdef, node, ncal, ncgcal;

type
  tppccallnode = class(tcgcallnode)
  end;

implementation

uses
  globtype, systems,
  cutils, verbose, globals,
  symconst, symbase, symsym, symtable, defutil, paramgr, parabase,
{$IFDEF GDB}
  strings,
  gdb,
{$ENDIF GDB}
  cgbase, pass_2,
  cpuinfo, cpubase, aasmbase, aasmtai, aasmcpu,
  nmem, nld, ncnv,
  ncgutil, cgutils, cgobj, tgobj, regvars, rgobj, rgcpu,
  cgcpu, cpupi, procinfo;


begin
  ccallnode := tppccallnode;
end.

