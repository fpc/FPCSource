{ %cpu=wasm32 }
{ %fail }

program twasmexternref4;

{ WasmExternRef cannot be declared a var parameter }
procedure testproc(var p: WasmExternRef);
begin
end;

begin
end.
