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

{$i fpcdefs.inc}

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
  if not (lnf_checknegate in loopflags) then
    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i32_eqz));

  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,1) );
  thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
end;

procedure twasmwhilerepeatnode.pass_generate_code;
var
   lcont,lbreak,lloop,
   oldclabel,oldblabel : tasmlabel;
   truelabel,falselabel : tasmlabel;
   oldflowcontrol : tflowcontrol;
   oldloopcontbroffset: Integer;
   oldloopbreakbroffset: Integer;
begin
  location_reset(location,LOC_VOID,OS_NO);

  current_asmdata.getjumplabel(lloop);
  current_asmdata.getjumplabel(lcont);
  current_asmdata.getjumplabel(lbreak);

  oldflowcontrol:=flowcontrol;

  oldloopcontbroffset:=thlcgwasm(hlcg).loopContBr;
  oldloopbreakbroffset:=thlcgwasm(hlcg).loopBreakBr;
  oldclabel:=current_procinfo.CurrContinueLabel;
  oldblabel:=current_procinfo.CurrBreakLabel;

  include(flowcontrol,fc_inflowcontrol);
  exclude(flowcontrol,fc_unwind_loop);

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
  thlcgwasm(hlcg).incblock;
  thlcgwasm(hlcg).loopBreakBr:=thlcgwasm(hlcg).br_blocks;
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_loop));
  thlcgwasm(hlcg).incblock;

  if lnf_testatbegin in loopflags then
  begin
    pass_generate_code_condition;
    thlcgwasm(hlcg).loopContBr:=thlcgwasm(hlcg).br_blocks;
  end else
    thlcgwasm(hlcg).loopContBr:=thlcgwasm(hlcg).br_blocks+1;

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
  thlcgwasm(hlcg).incblock;

  current_procinfo.CurrContinueLabel:=lcont;
  current_procinfo.CurrBreakLabel:=lbreak;

  secondpass(right);

  if (lnf_testatbegin in loopflags) then
    current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1) ); // jump back to the external loop

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
  thlcgwasm(hlcg).decblock;
  if not (lnf_testatbegin in loopflags) then begin
    pass_generate_code_condition;
  end;
  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,0) ); // jump back to loop

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_loop));
  thlcgwasm(hlcg).decblock;
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
  thlcgwasm(hlcg).decblock;

  current_procinfo.CurrContinueLabel:=oldclabel;
  current_procinfo.CurrBreakLabel:=oldblabel;
  thlcgwasm(hlcg).loopContBr:=oldloopcontbroffset;
  thlcgwasm(hlcg).loopBreakBr:=oldloopbreakbroffset;

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

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
  thlcgwasm(hlcg).incblock;
  thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

  if Assigned(right) then
    secondpass(right); // then branchs

  if Assigned(t1) then // else branch
    begin
      current_asmdata.CurrAsmList.concat(taicpu.op_none(a_else));
      secondpass(t1);
    end;

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));
  thlcgwasm(hlcg).decblock;
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
