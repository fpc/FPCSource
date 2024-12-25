{
    Copyright (c) 2002 by Florian Klaempfl

    RiscV64 specific calling conventions

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
unit cpupara;

{$I fpcdefs.inc}

  interface

    uses
      globtype,
      aasmtai,aasmdata,
      cpubase,
      symconst, symtype, symdef, symsym,
      paramgr, parabase, cgbase, cgutils,
      pararv;

    type
      tcpuparamanager = class(trvparamanager)
        function ret_in_param(def: tdef; pd: tabstractprocdef): boolean; override;

        function create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint; override;
      private
        function parseparaloc(p: tparavarsym; const s: string): boolean; override;
      end;

implementation

    uses
      verbose, systems,
      globals, cpuinfo,
      defutil,symtable,symcpu,
      procinfo, cpupi;

    function tcpuparamanager.ret_in_param(def: tdef; pd: tabstractprocdef): boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;

        { general rule: passed in registers -> returned in registers }
        result:=push_addr_param(vs_value,def,pd.proccalloption);
      end;


    function tcpuparamanager.create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg : tsuperregister;
      begin
        init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);

        result := create_paraloc_info_intern(p, side, p.paras, curintreg, curfloatreg, curmmreg, cur_stack_offset, false);

        create_funcretloc_info(p, side);
      end;


    function tcpuparamanager.parseparaloc(p: tparavarsym; const s: string): boolean;
      begin
        internalerror(200404182);
        result := true;
      end;


begin
  paramanager := tcpuparamanager.create;
end.

