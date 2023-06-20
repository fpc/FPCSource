{ %cpu=wasm32 }
{ %fail }

program twasmexternref5b;

{$MODE objfpc}

type
  TRec = class
    { WebAssembly reference types cannot be used inside records, objects, or classes }
    q: WasmExternRef;
  end;

begin
end.
