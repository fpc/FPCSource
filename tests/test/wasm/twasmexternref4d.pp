{ %cpu=wasm32 }
{ %fail }

program twasmexternref4d;

procedure testproc(const p);
begin
end;

procedure testproc2;
var
  q: WasmExternRef;
begin
  { WasmExternRef cannot be passed as an untyped const parameter }
  testproc(q);
end;

begin
end.
