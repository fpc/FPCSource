{ %cpu=wasm32 }
{ %fail }

program twasmexternref4b;

{$MODE objfpc}

{ WasmExternRef cannot be declared an out parameter }
procedure testproc(out p: WasmExternRef);
begin
end;

begin
end.
