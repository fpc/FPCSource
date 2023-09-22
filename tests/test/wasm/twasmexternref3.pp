{ %cpu=wasm32 }
{ %fail }

program twasmexternref3;

procedure testproc;
var
  p: WasmExternRef;
  q: Pointer;
begin
  { WasmExternRef cannot be explicitly converted to pointer }
  q := Pointer(p);
end;

begin
end.
