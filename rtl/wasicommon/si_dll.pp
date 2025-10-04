{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021, 2022 by Free Pascal development team

    This file implements the startup code for WebAssembly programs that
    don't link to the C library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit si_dll;

{$if defined(FPC_WASM_BRANCHFUL_EXCEPTIONS) or defined(FPC_WASM_LEGACY_EXCEPTIONS) or defined(FPC_WASM_EXNREF_EXCEPTIONS)}
  {$MODESWITCH EXCEPTIONS}
{$endif}

interface

procedure _initialize;

implementation

procedure PASCALMAIN; external 'PASCALMAIN';

{$if defined(FPC_WASM_BRANCHFUL_EXCEPTIONS) or defined(FPC_WASM_LEGACY_EXCEPTIONS) or defined(FPC_WASM_EXNREF_EXCEPTIONS)}
Procedure DoUnHandledException; external name 'FPC_DOUNHANDLEDEXCEPTION';

procedure _initialize_pascal;
begin
  try
    PASCALMAIN;
  except
    DoUnhandledException;
  end;
end;
{$else}
procedure _initialize_pascal;
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

procedure _initialize; assembler; nostackframe;
asm
  global.get $__stack_pointer
  call $SetInitialHeapBlockStart2

  call $_initialize_pascal
end;

exports
  _initialize,
  _initialize name '_initialize_promising' promising;

end.
