{ %cpu=wasm32 }
{ %fail }

program twasmexternref4f;

{$MODE objfpc}

procedure testproc(out p);
begin
end;

procedure testproc2;
var
  q: WasmExternRef;
begin
  { WasmExternRef cannot be passed as an untyped out parameter }
  testproc(q);
end;

begin
end.
