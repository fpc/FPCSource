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
      symtype,symdef,cgutils,parabase,compilerbase,
      node,ncal,ncgcal,aasmcpu,cpubase, wasmdef;

    type
       { twasmcallparanode }

       twasmcallparanode = class(tcgcallparanode)
       private
         procedure secondpass_all(ctx:tpassgeneratecodecontext);
         procedure push_all(ctx:tpassgeneratecodecontext);
       public
         procedure secondcallparan(ctx:tpassgeneratecodecontext);override;
       end;

       { twasmcallnode }

       twasmcallnode = class(tcgcallnode)
       protected
         function  pass_typecheck:tnode;override;
         procedure extra_post_call_code(ctx:tpassgeneratecodecontext); override;
         procedure do_release_unused_return_value(ctx:tpassgeneratecodecontext); override;
         procedure set_result_location(realresdef: tstoreddef;ctx:tpassgeneratecodecontext); override;
       end;


implementation

    uses
      globals, globtype, verbose, aasmdata, defutil, tgobj, hlcgcpu, symconst, symsym, symcpu,
      pass_2_context, nodehelper, compiler;

      { twasmcallparanode }

        procedure twasmcallparanode.secondpass_all(ctx:tpassgeneratecodecontext);
          begin
            { Skip nothingn nodes which are used after disabling
              a parameter }
            if (left.nodetype<>nothingn) then
              secondcallparan_do_secondpass(ctx);

            { next parameter }
            if assigned(right) then
              twasmcallparanode(right).secondpass_all(ctx);
          end;

        procedure twasmcallparanode.push_all(ctx:tpassgeneratecodecontext);
          begin
            { Skip nothingn nodes which are used after disabling
              a parameter }
            if (left.nodetype<>nothingn) then
              secondcallparan_after_secondpass(ctx);

            { next parameter }
            if assigned(right) then
              twasmcallparanode(right).push_all(ctx);
          end;

        procedure twasmcallparanode.secondcallparan(ctx:tpassgeneratecodecontext);
          begin
            if not(assigned(parasym)) then
              internalerror(200304242);

            { On WebAssembly we generate code for evaluating all the parameters
              first, and then we push them only after we've evaluated them all.
              This is because the evaluation phase can generate labels, which
              wreaks havoc in our 'goto' label resolution algorithm, when there
              are labels at different stack heights. }
            secondpass_all(ctx);
            push_all(ctx);
          end;

      { twasmcallnode }

    function twasmcallnode.pass_typecheck:tnode;
      var
        p: tcallparanode;
        pvs: tparavarsym;
      begin
        result:=inherited;
        if compiler.verbose.codegenerror then
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
                  compiler.verbose.CGMessage(parser_e_wasm_ref_types_can_only_be_passed_by_value);
                p:=tcallparanode(tcallparanode(p).right);
              end;
          end;
      end;

    procedure twasmcallnode.extra_post_call_code(ctx:tpassgeneratecodecontext);
      begin
        thlcgwasm(ctx.hlcg).g_adjust_stack_after_call(ctx.CurrAsmList,procdefinition);
        ctx.hlcg.g_maybe_checkforexceptions(ctx.CurrAsmList);
      end;

    procedure twasmcallnode.do_release_unused_return_value(ctx:tpassgeneratecodecontext);
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
            ctx.CurrAsmList.concat(taicpu.op_none(a_drop));
            thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          end;
        ft.free; // no nil needed
      end;

    procedure twasmcallnode.set_result_location(realresdef: tstoreddef;ctx:tpassgeneratecodecontext);
      begin
        // default implementation is placing the return value on LOC_REGISTER.
        // WebAssembly always returns the value on stack.
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1,[]);
        ctx.tg.gethltemp(ctx.CurrAsmList,realresdef,retloc.intsize,tt_normal,location.reference);
      end;

begin
  ccallnode:=twasmcallnode;
  ccallparanode:=twasmcallparanode;
end.
