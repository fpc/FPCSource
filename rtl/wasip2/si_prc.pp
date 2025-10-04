{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Free Pascal development team

    This file implements the startup code for WebAssembly programs that
    don't link to the C library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit si_prc;

{$if defined(FPC_WASM_BRANCHFUL_EXCEPTIONS) or defined(FPC_WASM_LEGACY_EXCEPTIONS) or defined(FPC_WASM_EXNREF_EXCEPTIONS)}
  {$MODESWITCH EXCEPTIONS}
{$endif}

interface

procedure _start;

implementation

procedure PASCALMAIN; external 'PASCALMAIN';

{$if defined(FPC_WASM_BRANCHFUL_EXCEPTIONS) or defined(FPC_WASM_LEGACY_EXCEPTIONS) or defined(FPC_WASM_EXNREF_EXCEPTIONS)}
Procedure DoUnHandledException; external name 'FPC_DOUNHANDLEDEXCEPTION';

procedure _start_pascal;
begin
  try
    PASCALMAIN;
  except
    DoUnhandledException;
  end;
end;
{$else}
procedure _start_pascal;
begin
  PASCALMAIN;
end;
{$endif}

procedure SetInitialHeapBlockStart(p: Pointer); external name 'FPC_WASM_SETINITIALHEAPBLOCKSTART';

{ TODO: remove this, when calling SetInitialHeapBlockStart works directly from within inline asm }
procedure SetInitialHeapBlockStart2(p: Pointer);
begin
  SetInitialHeapBlockStart(p);
end;

procedure _start; assembler; nostackframe;
asm
  global.get $__stack_pointer
  call $SetInitialHeapBlockStart2

  call $_start_pascal
end;

exports
  _start;

end.
