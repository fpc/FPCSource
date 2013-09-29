{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate MIPS assembler for nodes that handle loads and assignments

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
unit ncpuld;

{$I fpcdefs.inc}

interface

uses
  node, ncgld;

type
  tmipsloadnode = class(tcgloadnode)
    function pass_1 : tnode; override;
  end;

implementation

uses
  verbose,
  globtype,
  systems,
  cpubase,
  cgbase, cgutils, cgobj,
  aasmbase, aasmtai,aasmdata,
  symconst, symsym,
  procinfo,
  nld;

function tmipsloadnode.pass_1 : tnode;
begin
  pass_1 := inherited pass_1;
  case symtableentry.typ of
    staticvarsym,
    localvarsym,
    paravarsym :
      if([vo_is_dll_var,vo_is_external] * tabstractvarsym(symtableentry).varoptions <> []) then
        include(current_procinfo.flags,pi_needs_got);
  end;
end;


begin
  cloadnode := tmipsloadnode;
end.

