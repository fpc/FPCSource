{
    Copyright (c) 2019 by Dmitry Boyarintsev

    WebAssembly-specific code for call nodes

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
unit nwasmcal;

{$i fpcdefs.inc}

interface

    uses
      cgbase,
      symtype,symdef,cgutils,parabase,
      node,ncal,ncgcal,hlcgobj,aasmcpu,cpubase;

    type
       { twasmcallparanode }

       twasmcallparanode = class(tcgcallparanode)
       end;

       { twasmcallnode }

       twasmcallnode = class(tcgcallnode)
       protected
         function can_call_ref(var ref: treference):boolean;override;
         //procedure extra_call_ref_code(var ref: treference);virtual;
         function do_call_ref(ref: treference): tcgpara; override;

         procedure set_result_location(realresdef: tstoreddef); override;
       end;


implementation

    uses
      globtype, aasmdata, defutil, tgobj, hlcgcpu;

      { twasmcallnode }

    function twasmcallnode.can_call_ref(var ref: treference): boolean;
      begin
        result:=true;
      end;

    function twasmcallnode.do_call_ref(ref: treference): tcgpara;
      begin
        thlcgwasm(hlcg).a_load_ref_stack(current_asmdata.CurrAsmList, s32inttype, ref, 0);
        // todo: determine the proper function type
        current_asmdata.CurrAsmList.Concat( taicpu.op_const(a_call_indirect, 0));
        result:=hlcg.get_call_result_cgpara(procdefinition,typedef)
      end;

    procedure twasmcallnode.set_result_location(realresdef: tstoreddef);
      begin
        // default implementation is placing the return value on LOC_REGISTER.
        // WebAssembly always returns the value on stack.
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1,[]);
        tg.gethltemp(current_asmdata.CurrAsmList,realresdef,retloc.intsize,tt_normal,location.reference);
      end;

begin
  ccallnode:=twasmcallnode;
  ccallparanode:=twasmcallparanode;
end.
