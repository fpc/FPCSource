{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the Risc-V64

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
unit nrv64add;

{$I fpcdefs.inc}

  interface

    uses
      node, ncgadd, aasmbase, nrvadd, cpubase;

    type
      trv64addnode = class(trvaddnode)
      protected
        function pass_1: tnode; override;

        procedure second_add64bit; override;

        function use_generic_mul32to64: boolean; override;
      end;

  implementation

    uses
      systems,
      cutils,verbose,
      paramgr,procinfo,
      aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,cgcpu,cgutils,
      globals,
      pass_1,
      CPUInfo,cpupara,
      ncon,nset,nadd,
      symconst,
      hlcgobj, ncgutil,cgobj;

    function trv64addnode.pass_1: tnode;
      begin
        if (nodetype=muln) and
           (left.resultdef.typ=orddef) and (left.resultdef.typ=orddef) and
           (CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype]) then
          begin
            result:=nil;

            firstpass(left);
            firstpass(right);

            expectloc:=LOC_REGISTER;
          end
        else if (nodetype=muln) and
           (not (CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype])) and
           (is_64bit(left.resultdef) or
            is_64bit(right.resultdef)) then
          begin
            result:=first_add64bitint;
          end
        else
          Result:=inherited pass_1;

        if expectloc=LOC_FLAGS then
          expectloc:=LOC_REGISTER;
      end;


    procedure trv64addnode.second_add64bit;
      begin
        second_addordinal;
      end;


    function trv64addnode.use_generic_mul32to64: boolean;
      begin
        result:=false;
      end;

begin
  caddnode := trv64addnode;
end.

