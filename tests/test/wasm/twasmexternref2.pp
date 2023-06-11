{ %cpu=wasm32 }
{ %fail }

program twasmexternref2;

procedure testproc;
var
  p: WasmExternRef;
  q: Pointer;
begin
  { WasmExternRef cannot be address taken }
  q := @p;
end;

begin
end.
