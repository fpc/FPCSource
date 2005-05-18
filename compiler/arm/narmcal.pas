{
    Copyright (c) 2002 by Florian Klaempfl

    Implements the ARM specific part of call nodes

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
unit narmcal;

{$i fpcdefs.inc}

interface

    uses
      symdef,node,ncal,ncgcal;

    type
       tarmcallnode = class(tcgcallnode)
          // procedure push_framepointer;override;
       end;

implementation

  uses
    paramgr;

(*
  procedure tarmcallnode.push_framepointer;
    begin
      framepointer_paraloc:=paramanager.getintparaloc(procdefinition.proccalloption,1);
    end;
*)

begin
   ccallnode:=tarmcallnode;
end.
