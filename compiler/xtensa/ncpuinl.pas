{
    Copyright (c) 1998-2017 by Florian Klaempfl

    Generates Xtensa inline nodes

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
unit ncpuinl;

{$i fpcdefs.inc}

  interface

    uses
      node,ninl,ncginl,aasmbase,compilerbase;

    type
      tcpuinlinenode = class(tcginlinenode)
        function first_abs_real: tnode; override;
        procedure second_abs_long(ctx:tpassgeneratecodecontext); override;
        procedure second_abs_real(ctx:tpassgeneratecodecontext); override;
        function first_fma: tnode; override;
        procedure second_fma(ctx:tpassgeneratecodecontext); override;
        function first_minmax: tnode; override;
        procedure second_minmax(ctx:tpassgeneratecodecontext); override;
        procedure second_prefetch(ctx:tpassgeneratecodecontext); override;
      end;

  implementation

    uses
      cpuinfo,
      verbose,globals,globtype,
      compinnr,
      aasmdata,
      aasmcpu,
      symdef,
      defutil,
      nodehelper,
      pass_2,pass_2_context,
      procinfo,
      cgbase, cgobj, cgutils,
      ncal,nutils,
      cpubase,
      compiler;

    procedure tcpuinlinenode.second_abs_long(ctx:tpassgeneratecodecontext);
      var
       hl: TAsmLabel;
      begin
        if is_64bitint(resultdef) then
          begin
            inherited;
            exit;
          end;

        secondpass(left,ctx);
        ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

        location:=left.location;
        location.register:=ctx.hlcg.getintregister(ctx.CurrAsmList,left.resultdef);

        if cs_check_overflow in compiler.globals.current_settings.localswitches then
          begin
            ctx.CurrAsmList.AsmData.getjumplabel(hl);

            ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,resultdef,OC_NE,$80000000,left.location.register,hl);
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_overflow',[],nil).resetiftemp;
            ctx.hlcg.a_label(ctx.CurrAsmList,hl);
          end;

        ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_ABS,location.register,left.location.register));
      end;


    function tcpuinlinenode.first_abs_real : tnode;
      begin
        result:=nil;
        if is_single(left.resultdef) and (FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) then
          expectloc:=LOC_FPUREGISTER
        else
          result:=inherited first_abs_real;
      end;


    procedure tcpuinlinenode.second_abs_real(ctx:tpassgeneratecodecontext);
      begin
        if not(is_single(resultdef)) then
          InternalError(2020091101);
        secondpass(left,ctx);
        ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,left.location,left.resultdef,true);
        location_reset(location,LOC_FPUREGISTER,OS_F32);
        location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);
        ctx.CurrAsmList.concat(setoppostfix(taicpu.op_reg_reg(A_ABS,location.register,left.location.register),PF_S));
      end;


    function tcpuinlinenode.first_fma : tnode;
      begin
        if is_single(resultdef) then
          begin
            expectloc:=LOC_FPUREGISTER;
            Result:=nil;
            if compiler.globals.needs_check_for_fpu_exceptions then
              Include(compiler.current_procinfo.flags,pi_do_call);
          end
        else
          Result:=inherited first_fma;
      end;


    procedure tcpuinlinenode.second_fma(ctx:tpassgeneratecodecontext);
      const
        op : array[false..true] of TAsmOp =
          (A_MADD,
           A_MSUB);

      var
        paraarray : array[1..3] of tnode;
        i : integer;
        negproduct : boolean;
        oppostfix : TOpPostfix;
        ai: taicpu;
      begin
         if is_single(resultdef)and
           (FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) then
           begin
             negproduct:=false;
             paraarray[1]:=tcallparanode(tcallparanode(tcallparanode(parameters).nextpara).nextpara).paravalue;
             paraarray[2]:=tcallparanode(tcallparanode(parameters).nextpara).paravalue;
             paraarray[3]:=tcallparanode(parameters).paravalue;

             { check if a neg. node can be removed
               this is possible because changing the sign of
               a floating point number does not affect its absolute
               value in any way
             }
             if paraarray[1].nodetype=unaryminusn then
               begin
                 paraarray[1]:=tunarynode(paraarray[1]).left;
                 { do not release the unused unary minus node, it is kept and release together with the other nodes,
                   only no code is generated for it }
                 negproduct:=not(negproduct);
               end;

             if paraarray[2].nodetype=unaryminusn then
               begin
                 paraarray[2]:=tunarynode(paraarray[2]).left;
                 { do not release the unused unary minus node, it is kept and release together with the other nodes,
                   only no code is generated for it }
                 negproduct:=not(negproduct);
               end;
              for i:=1 to 3 do
               secondpass(paraarray[i],ctx);

             { no memory operand is allowed }
             for i:=1 to 3 do
               begin
                 if not(paraarray[i].location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
                   ctx.hlcg.location_force_fpureg(ctx.CurrAsmList,paraarray[i].location,paraarray[i].resultdef,true);
               end;

             location_reset(location,LOC_FPUREGISTER,paraarray[1].location.size);
             location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);

             ctx.hlcg.a_loadfpu_reg_reg(ctx.CurrAsmList,paraarray[3].resultdef,resultdef,
               paraarray[3].location.register,location.register);

             ai:=taicpu.op_reg_reg_reg(op[negproduct],
               location.register,paraarray[1].location.register,paraarray[2].location.register);
             ai.oppostfix:=PF_S;
             ctx.CurrAsmList.concat(ai);

             ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
           end
         else
           internalerror(2020112401);
      end;


    function tcpuinlinenode.first_minmax : tnode;
      begin
        if is_32bitint(resultdef) then
          begin
            expectloc:=LOC_REGISTER;
            Result:=nil;
            if compiler.globals.needs_check_for_fpu_exceptions then
              Include(compiler.current_procinfo.flags,pi_do_call);
          end
        else
          Result:=inherited first_minmax;
      end;


    procedure tcpuinlinenode.second_minmax(ctx:tpassgeneratecodecontext);
      var
        paraarray : array[1..2] of tnode;
        i: Integer;
        ai: taicpu;
        op: TAsmOp;
      begin
         if is_32bitint(resultdef) then
           begin
             paraarray[1]:=tcallparanode(tcallparanode(parameters).nextpara).paravalue;
             paraarray[2]:=tcallparanode(parameters).paravalue;

              for i:=low(paraarray) to high(paraarray) do
               secondpass(paraarray[i],ctx);

             { no memory operand is allowed }
             for i:=low(paraarray) to high(paraarray) do
               begin
                 if not(paraarray[i].location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   ctx.hlcg.location_force_reg(ctx.CurrAsmList,paraarray[i].location,
                     paraarray[i].resultdef,resultdef,true);
               end;

             location_reset(location,LOC_REGISTER,paraarray[1].location.size);
             location.register:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);

             case inlinenumber of
               in_min_dword:
                 op:=A_MINU;
               in_max_dword:
                 op:=A_MAXU;
               in_min_longint:
                 op:=A_MIN;
               in_max_longint:
                 op:=A_MAX;
               else
                 Internalerror(2020120505);
             end;

             ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
               location.register,paraarray[1].location.register,paraarray[2].location.register));

             ctx.cg.maybe_check_for_fpu_exception(ctx.CurrAsmList);
           end
         else
           internalerror(2020120502);
      end;


    procedure tcpuinlinenode.second_prefetch(ctx:tpassgeneratecodecontext);
      var
        ref : treference;
        r : tregister;
        checkpointer_used : boolean;
      begin
        { do not call Checkpointer for left node }
        checkpointer_used:=(cs_checkpointer in compiler.globals.current_settings.localswitches);
        if checkpointer_used then
          node_change_local_switch(left,cs_checkpointer,false);
        secondpass(left,ctx);
        if checkpointer_used then
          node_change_local_switch(left,cs_checkpointer,false);
       case left.location.loc of
         LOC_CREFERENCE,
         LOC_REFERENCE:
           begin
             r:=ctx.cg.getintregister(ctx.CurrAsmList,OS_ADDR);
             ctx.cg.a_loadaddr_ref_reg(ctx.CurrAsmList,left.location.reference,r);
             reference_reset_base(ref,r,0,location.reference.temppos,left.location.reference.alignment,location.reference.volatility);
             ctx.CurrAsmList.concat(taicpu.op_reg_const(A_DPFR,ref.base,ref.offset));
           end;
         else
           { nothing to prefetch };
       end;
      end;


begin
  cinlinenode:=tcpuinlinenode;
end.
