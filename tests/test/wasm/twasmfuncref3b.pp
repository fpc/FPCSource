{ %cpu=wasm32 }
{ %fail }

program twasmfuncref3b;

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;

procedure test(para: TWasmFuncRef);
var
  a: longint;
begin
  { Cannot take the size of WebAssembly reference types }
  a := SizeOf(para);
end;

begin
end.
