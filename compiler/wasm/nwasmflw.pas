{
    Copyright (c) 2019 by Dmitry Boyarintsev

    Generate assembler for nodes that influence the flow for the JVM

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
unit nwasmflw;

interface

uses
  aasmbase,node,nflw,ncgflw, cutils;

type

   { twasmifnode }

   // Wasm doesn't have any jump(+offset) operations
   // It only provide structured blockes to handle jumps
   // (It's possible to jump-out-of-block at any time)
   // "If" is also implemented as a block, identical to high-level language.
   // Another thing to consider is "if" block also "returns" a value on the stack.
   // Such value should be substituteed (it's hard-coded to be type i32)
   twasmifnode = class(tcgifnode)
   public
     procedure pass_generate_code;override;
   end;

   { twasmwhilerepeatnode }

   twasmwhilerepeatnode = class(tcgwhilerepeatnode)
   public
     procedure pass_generate_code_condition;
     procedure pass_generate_code;override;
   end;

implementation

uses
  verbose,globals,systems,globtype,constexp,
  symconst,symdef,symsym,aasmtai,aasmdata,aasmcpu,defutil,defcmp,
  procinfo,cgbase,pass_1,pass_2,parabase,
  cpubase,cpuinfo,
  nbas,nld,ncon,ncnv,
  tgobj,paramgr,
  cgutils,hlcgobj,hlcgcpu;

{ twasmwhilerepeatnode }

procedure twasmwhilerepeatnode.pass_generate_code_condition;
begin
  secondpass(left);

  // reversing the condition
  // todo: there should be a better approach
  if not (lnf_checknegate in loopflags) then begin
    current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const,1) );
    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i32_xor) );
  end;
  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,1) );
end;

procedure twasmwhilerepeatnode.pass_generate_code;
var
   lcont,lbreak,lloop,
   oldclabel,oldblabel : tasmlabel;
   truelabel,falselabel : tasmlabel;
   oldflowcontrol : tflowcontrol;
begin
  location_reset(location,LOC_VOID,OS_NO);

  current_asmdata.getjumplabel(lloop);
  current_asmdata.getjumplabel(lcont);
  current_asmdata.getjumplabel(lbreak);

  oldflowcontrol:=flowcontrol;
  oldclabel:=current_procinfo.CurrContinueLabel;
  oldblabel:=current_procinfo.CurrBreakLabel;

  include(flowcontrol,fc_inflowcontrol);
  exclude(flowcontrol,fc_unwind_loop);

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_loop));

  if lnf_testatbegin in loopflags then
    pass_generate_code_condition;

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

  current_procinfo.CurrContinueLabel:=lcont;
  current_procinfo.CurrBreakLabel:=lbreak;

  secondpass(right);

  if (lnf_testatbegin in loopflags) then
    current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1) ); // jump back to the external loop

  current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('end of internal loop block')));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));
  if not (lnf_testatbegin in loopflags) then begin
    pass_generate_code_condition;
  end;
  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,0) ); // jump back to loop

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));

  current_procinfo.CurrContinueLabel:=oldclabel;
  current_procinfo.CurrBreakLabel:=oldblabel;
  { a break/continue in a while/repeat block can't be seen outside }
  flowcontrol:=oldflowcontrol+(flowcontrol-[fc_break,fc_continue,fc_inflowcontrol]);

end;

{ twasmifnode }

procedure twasmifnode.pass_generate_code;
var
  oldflowcontrol: tflowcontrol;
begin
  // left  - condition
  // right - then
  // t1    - else (optional)

  //todo: MOVE all current_asm_data actions to Wasm HL CodeGen

  secondpass(left); // condition exprssions

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if)); // IF
  thlcgwasm(hlcg).incblock;

  secondpass(right); // then branchs

  if Assigned(t1) then // else branch
    begin
      // 0 const on stack if used to return IF value
      current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_none(a_else));
      secondpass(t1);
    end
  else // else dummy-branch
    begin
      // dummy else branch! todo: to be removed, when it's decided
      // how to handle typeless-IF instructions (If without else)
      current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const, 0));
      current_asmdata.CurrAsmList.concat(taicpu.op_none(a_else));
      current_asmdata.CurrAsmList.concat(taicpu.op_none(a_nop));
    end;

  // 0 const on stack if used to return IF value
  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const, 0));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));
  thlcgwasm(hlcg).decblock;

  // clearing IF return value
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_drop));
end;

initialization
   //cfornode:=tjvmfornode;
   //craisenode:=tjvmraisenode;
   //ctryexceptnode:=tjvmtryexceptnode;
   //ctryfinallynode:=tjvmtryfinallynode;
   //connode:=tjvmonnode;
   cifnode:=twasmifnode;
   cwhilerepeatnode:=twasmwhilerepeatnode;

end.
