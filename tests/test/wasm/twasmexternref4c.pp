{ %cpu=wasm32 }
{ %fail }

program twasmexternref4c;

procedure testproc(var p);
begin
end;

procedure testproc2;
var
  q: WasmExternRef;
begin
  { WasmExternRef cannot be passed as an untyped var parameter }
  testproc(q);
end;

begin
end.
