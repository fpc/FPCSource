{ %cpu=wasm32 }
{ %fail }

program twasmexternref6a;

const
  { Cannot take the size of WebAssembly reference types }
  Q = BitSizeOf(WasmExternRef);

begin
end.
