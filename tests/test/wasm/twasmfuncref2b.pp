{ %cpu=wasm32 }
{ %fail }

program twasmfuncref2b;

{$MODE objfpc}

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;
  TRec = class
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: TWasmFuncRef;
  end;

begin
end.
