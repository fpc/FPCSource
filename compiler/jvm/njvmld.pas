{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM assembler for nodes that handle loads and assignments

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
unit njvmld;

{$I fpcdefs.inc}

interface

uses
  node, ncgld;

type
  tjvmloadnode = class(tcgloadnode)
    function is_addr_param_load: boolean; override;
  end;

implementation

uses
  nld,
  symsym,
  jvmdef;

function tjvmloadnode.is_addr_param_load: boolean;
  begin
    result:=
      inherited and
      not jvmimplicitpointertype(tparavarsym(symtableentry).vardef);
  end;


begin
  cloadnode:=tjvmloadnode;
end.

