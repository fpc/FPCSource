{ %cpu=wasm32 }
{ %fail }

program twasmexternref3b;

procedure testproc;
var
  p: WasmExternRef;
  q: Pointer;
begin
  { WasmExternRef cannot be implicitly converted to pointer }
  q := p;
end;

begin
end.
