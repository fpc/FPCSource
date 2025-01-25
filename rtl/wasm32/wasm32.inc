{%MainUnit system.pp}
{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    WebAssembly 32-bit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_WASM_THREADS}
procedure fpc_wasm32_init_tls(memory: Pointer);external name '__wasm_init_tls';
{$endif FPC_WASM_THREADS}

procedure fpc_wasm_invoke_helper(CodeAddress: CodePointer; Args: Pointer; Result: Pointer);assembler;nostackframe;
asm
  unreachable
end;

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;
  begin
  end;


{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);
  begin
    if count>0 then
      fpc_wasm32_memory_fill(PtrUInt(@x),value,count);
  end;


{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
  begin
    if count>0 then
      fpc_wasm32_memory_copy(PtrUInt(@dest),PtrUInt(@source),count);
  end;


{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
Function Get_pc_addr : CodePointer;
  begin
    { dummy, produces a small, fake backtrace, otherwise programs terminate
      with no output at all, in case of a runtime error }
    result:=CodePointer($eeeeeeef);
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:codepointer=nil):pointer;
  begin
    { dummy, produces a small, fake backtrace, otherwise programs terminate
      with no output at all, in case of a runtime error }
    if addr=CodePointer($eeeeeeef) then
      result:=CodePointer($eeeeeeee)
    else
      result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr : pointer; assembler; nostackframe;
  asm
    global.get $__stack_pointer
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint) : longint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
{$push}{$R-,Q-}
    Result:=fpc_wasm32_i32_atomic_rmw_sub(@Target,1)-1;
{$pop}
{$else FPC_WASM_THREADS}
    dec(Target);
    Result:=Target;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint) : longint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
{$push}{$R-,Q-}
    Result:=fpc_wasm32_i32_atomic_rmw_add(@Target,1)+1;
{$pop}
{$else FPC_WASM_THREADS}
    inc(Target);
    Result:=Target;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint) : longint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_xchg(@Target,LongWord(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    Target:=Source;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32'];
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Target,LongWord({$ifdef VER3_2}Comperand{$else}Comparand{$endif}),LongWord(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;

{$ifndef VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_64}
function fpc_atomic_cmp_xchg_64 (var Target: Int64; NewValue: Int64; Comparand: Int64) : Int64; [public,alias:'FPC_ATOMIC_CMP_XCHG_64'];
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=Int64(fpc_wasm32_i64_atomic_rmw_cmpxchg_u(@Target,QWord(Comparand),QWord(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target=Comparand then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;
{$endif VER3_2}

{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value : longint) : longint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_add(@Target,LongWord({$ifdef VER3_2}Source{$else}Value{$endif})));
{$else FPC_WASM_THREADS}
    Result:=Target;
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_16}
function fpc_atomic_dec_16 (var Target: smallint) : smallint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
{$push}{$R-,Q-}
    Result:=smallint(fpc_wasm32_i32_atomic_rmw16_sub_u(@Target,1)-1);
{$pop}
{$else FPC_WASM_THREADS}
    dec(Target);
    Result:=Target;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_16}
function fpc_atomic_inc_16 (var Target: smallint) : smallint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
{$push}{$R-,Q-}
    Result:=smallint(fpc_wasm32_i32_atomic_rmw16_add_u(@Target,1)+1);
{$pop}
{$else FPC_WASM_THREADS}
    inc(Target);
    Result:=Target;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_16}
function fpc_atomic_xchg_16 (var Target: smallint;Source : smallint) : smallint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Target,Word(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    Target:=Source;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_16}
function fpc_atomic_cmp_xchg_16 (var Target: smallint; NewValue: smallint; Comparand: smallint) : smallint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Target,Word({$ifdef VER3_2}Comperand{$else}Comparand{$endif}),Word(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_16}
function fpc_atomic_add_16 (var Target: smallint;Value : smallint) : smallint;
{$endif VER3_2}
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_add_u(@Target,Word({$ifdef VER3_2}Source{$else}Value{$endif})));
{$else FPC_WASM_THREADS}
    Result:=Target;
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});
{$endif FPC_WASM_THREADS}
  end;

{$ifndef VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_8}
function fpc_atomic_cmp_xchg_8 (var Target : shortint; NewValue : shortint; Comparand : shortint) : shortint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=ShortInt(fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(@Target, Byte(Comparand), Byte(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target=Comparand then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;
{$endif VER3_2}
