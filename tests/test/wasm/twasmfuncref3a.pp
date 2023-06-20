{ %cpu=wasm32 }
{ %fail }

program twasmfuncref3a;

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;

const
  { Cannot take the size of WebAssembly reference types }
  Q = BitSizeOf(TWasmFuncRef);

begin
end.
