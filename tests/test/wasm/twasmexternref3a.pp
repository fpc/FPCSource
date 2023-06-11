{ %cpu=wasm32 }
{ %fail }

program twasmexternref3a;

procedure testproc;
var
  p: WasmExternRef;
  q: Pointer;
begin
  { A pointer cannot be explicitly converted to WasmExternRef }
  p := WasmExternRef(q);
end;

begin
end.
