{ %cpu=wasm32 }
{ %fail }

program twasmexternref3c;

procedure testproc;
var
  p: WasmExternRef;
  q: Pointer;
begin
  { A pointer cannot be implicitly converted to WasmExternRef }
  p := q;
end;

begin
end.
