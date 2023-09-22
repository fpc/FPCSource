{ %cpu=wasm32 }
{ %fail }

program twasmexternref5a;

type
  TRec = object
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: WasmExternRef;
  end;

begin
end.
