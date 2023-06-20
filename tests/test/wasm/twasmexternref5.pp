{ %cpu=wasm32 }
{ %fail }

program twasmexternref5;

type
  TRec = record
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: WasmExternRef;
  end;

begin
end.
