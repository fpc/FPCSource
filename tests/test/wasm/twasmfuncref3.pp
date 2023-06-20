{ %cpu=wasm32 }
{ %fail }

program twasmfuncref3;

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;

const
  { Cannot take the size of WebAssembly reference types }
  Q = SizeOf(TWasmFuncRef);

begin
end.
