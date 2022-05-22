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
function Sptr : pointer;
  begin
    result:=nil;
  end;


function InterLockedDecrement (var Target: longint) : longint;
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


function InterLockedIncrement (var Target: longint) : longint;
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


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_xchg(@Target,LongWord(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    Target:=Source;
{$endif FPC_WASM_THREADS}
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Target,LongWord(Comperand),LongWord(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=LongInt(fpc_wasm32_i32_atomic_rmw_add(@Target,LongWord(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    inc(Target,Source);
{$endif FPC_WASM_THREADS}
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
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


function InterLockedIncrement (var Target: smallint) : smallint;
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


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Target,Word(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    Target:=Source;
{$endif FPC_WASM_THREADS}
  end;


function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Target,Word(Comperand),Word(NewValue)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;
{$endif FPC_WASM_THREADS}
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  begin
{$ifdef FPC_WASM_THREADS}
    Result:=SmallInt(fpc_wasm32_i32_atomic_rmw16_add_u(@Target,Word(Source)));
{$else FPC_WASM_THREADS}
    Result:=Target;
    inc(Target,Source);
{$endif FPC_WASM_THREADS}
  end;
