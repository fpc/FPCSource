{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the RiscV64 specific part of call nodes

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
unit nrv64cal;

{$I fpcdefs.inc}

interface

uses
  aasmdata, cgbase,
  symdef, node, ncal, ncgcal;

type

  trv64callparanode = class(tcgcallparanode)
  end;

  trv64ccallnode = class(tcgcallnode)
  end;

implementation

uses
  globtype, systems,
  cutils, verbose, globals,
  symconst, symbase, symsym, symtable, defutil, paramgr, parabase,
  pass_2,
  cpuinfo, cpubase, aasmbase, aasmtai, aasmcpu,
  nmem, nld, ncnv,
  ncgutil, cgutils, cgobj, tgobj, rgobj, rgcpu,
  cgcpu, cpupi, procinfo;

begin
  ccallparanode:=trv64callparanode;
  ccallnode := trv64ccallnode;
end.

