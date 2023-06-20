{ %cpu=wasm32 }
{ %fail }

program twasmfuncref2a;

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;
  TRec = object
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: TWasmFuncRef;
  end;

begin
end.
