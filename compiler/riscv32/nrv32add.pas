{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the Risc-V32

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
unit nrv32add;

{$i fpcdefs.inc}

  interface

    uses
      node, ncgadd, aasmbase, nrvadd, cpubase;

    type
      trv32addnode = class(trvaddnode)
      protected
        function use_generic_mul32to64: boolean; override;
      end;

  implementation

    uses
      systems,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,cgcpu,cgutils,nadd,
      cpupara,
      ncon,nset,
      hlcgobj, ncgutil,cgobj;

    function trv32addnode.use_generic_mul32to64: boolean;
      begin
        result:=true;
      end;

begin
   caddnode:=trv32addnode;
end.
