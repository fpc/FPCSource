{
    Copyright (c) 2016 by Jonas Maebe

    Generate assembler for nodes that influence the flow

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
unit ncgnstfl;

{$i fpcdefs.inc}

interface

    uses
      node,
      nflw, ncgflw;

    type
      tcgnestgotonode = class(tcggotonode)
        function pass_typecheck: tnode; override;
      end;

implementation

    uses
      globtype,globals,
      nld,
      symdef,symsym,symcreat;


    function tcgnestgotonode.pass_typecheck: tnode;
      begin
        result:=inherited;
        if (m_non_local_goto in current_settings.modeswitches) and
           assigned(labelsym.jumpbuf) then
          begin
            { we will access this jumpbuf local variable from a nested context,
              but the loadnode to do so only gets created in pass_1. This is too
              late for us to detect that it should have been added to the
              parentfpstruct, so temporarily create and typecheckpass a load
              node for the jumpbuf here (so that it can be added now) }
            with cloadnode.create(labelsym.jumpbuf,labelsym.jumpbuf.Owner) do
              begin
                pass_typecheck;
                free
              end;
          end;
      end;


begin
  cgotonode:=tcgnestgotonode;
end.

