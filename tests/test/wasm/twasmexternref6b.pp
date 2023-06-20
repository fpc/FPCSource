{ %cpu=wasm32 }
{ %fail }

program twasmexternref6b;

procedure test(para: WasmExternRef);
var
  a: longint;
begin
  { Cannot take the size of WebAssembly reference types }
  a := SizeOf(para);
end;

begin
end.
