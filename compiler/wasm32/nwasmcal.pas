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
      node,ncal,ncgcal,hlcgobj,aasmcpu,cpubase, wasmdef;

    type
       { twasmcallparanode }

       twasmcallparanode = class(tcgcallparanode)
       private
         procedure secondpass_all;
         procedure push_all;
       public
         procedure secondcallparan;override;
       end;

       { twasmcallnode }

       twasmcallnode = class(tcgcallnode)
       protected
         function  pass_typecheck:tnode;override;
         procedure extra_post_call_code; override;
         procedure do_release_unused_return_value; override;
         procedure set_result_location(realresdef: tstoreddef); override;
       end;


implementation

    uses
      globals, globtype, verbose, aasmdata, defutil, tgobj, hlcgcpu, symconst, symsym, symcpu;

      { twasmcallparanode }

        procedure twasmcallparanode.secondpass_all;
          begin
            { Skip nothingn nodes which are used after disabling
              a parameter }
            if (left.nodetype<>nothingn) then
              secondcallparan_do_secondpass;

            { next parameter }
            if assigned(right) then
              twasmcallparanode(right).secondpass_all;
          end;

        procedure twasmcallparanode.push_all;
          begin
            { Skip nothingn nodes which are used after disabling
              a parameter }
            if (left.nodetype<>nothingn) then
              secondcallparan_after_secondpass;

            { next parameter }
            if assigned(right) then
              twasmcallparanode(right).push_all;
          end;

        procedure twasmcallparanode.secondcallparan;
          begin
            if not(assigned(parasym)) then
              internalerror(200304242);

            { On WebAssembly we generate code for evaluating all the parameters
              first, and then we push them only after we've evaluated them all.
              This is because the evaluation phase can generate labels, which
              wreaks havoc in our 'goto' label resolution algorithm, when there
              are labels at different stack heights. }
            secondpass_all;
            push_all;
          end;

      { twasmcallnode }

    function twasmcallnode.pass_typecheck:tnode;
      var
        p: tcallparanode;
        pvs: tparavarsym;
      begin
        result:=inherited;
        if codegenerror then
          exit;

        if assigned(procdefinition) then
          begin
            p:=tcallparanode(left);
            while assigned(p) do
              begin
                pvs:=p.parasym;
                if assigned(p.left) and is_wasm_reference_type(p.left.resultdef) and
                   assigned(pvs) and
                  ((pvs.varspez in [vs_var,vs_constref,vs_out]) or
                   ((pvs.varspez=vs_const) and (pvs.vardef.typ=formaldef))) then
                  CGMessage(parser_e_wasm_ref_types_can_only_be_passed_by_value);
                p:=tcallparanode(tcallparanode(p).right);
              end;
          end;
      end;

    procedure twasmcallnode.extra_post_call_code;
      begin
        thlcgwasm(hlcg).g_adjust_stack_after_call(current_asmdata.CurrAsmList,procdefinition);
        hlcg.g_maybe_checkforexceptions(current_asmdata.CurrAsmList);
      end;

    procedure twasmcallnode.do_release_unused_return_value;
      var
        ft: TWasmFuncType;
        i: Integer;
      begin
        if procdefinition.typ=procvardef then
          ft:=tcpuprocvardef(procdefinition).create_functype
        else
          ft:=tcpuprocdef(procdefinition).create_functype;
        for i:=1 to Length(ft.results) do
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_drop));
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          end;
        ft.free; // no nil needed
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
