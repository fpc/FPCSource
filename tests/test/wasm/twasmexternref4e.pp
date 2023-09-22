{ %cpu=wasm32 }
{ %fail }

program twasmexternref4e;

procedure testproc(constref p);
begin
end;

procedure testproc2;
var
  q: WasmExternRef;
begin
  { WasmExternRef cannot be passed as an untyped constref parameter }
  testproc(q);
end;

begin
end.
