{ %cpu=wasm32 }
{ %fail }

program twasmfuncref2;

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;
  TRec = record
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: TWasmFuncRef;
  end;

begin
end.
