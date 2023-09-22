{ %cpu=wasm32 }
{ %fail }

program twasmexternref4a;

{ WasmExternRef cannot be declared a constref parameter }
procedure testproc(constref p: WasmExternRef);
begin
end;

begin
end.
