{ %cpu=wasm32 }
{ %fail }

program twasmexternref6;

const
  { Cannot take the size of WebAssembly reference types }
  Q = SizeOf(WasmExternRef);

begin
end.
