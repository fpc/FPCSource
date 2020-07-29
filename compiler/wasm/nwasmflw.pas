unit nwasmflw;

interface

uses
  aasmbase,node,nflw,ncgflw;

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

procedure twasmwhilerepeatnode.pass_generate_code;
begin
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_loop));

  secondpass(left);

  // reversing the condition
  // todo: there should be a better approach
  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const,1) );
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i32_xor) );

  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,1) );

  secondpass(right);

  current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,0) );

  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end));
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

  secondpass(right); // then branchs

  if Assigned(t1) then // else branch
    begin
      // 0 const on stack if used to return IF value
      current_asmdata.CurrAsmList.concat(taicpu.op_const(a_i32_const, 0));
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
