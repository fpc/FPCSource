{ %cpu=wasm32 }
{ %norun }

program twasmexternref1;

procedure testproc;
var
  p: WasmExternRef;
begin
end;

procedure testproc2(q: WasmExternRef);
begin
end;

function testproc3: WasmExternRef;
begin
end;

function testproc4(a, b, c: longint; d: WasmExternRef; e: int64): WasmExternRef;
var
  q: WasmExternRef;
begin
  q := d;
  testproc4 := q;
end;

function testproc5(q: WasmExternRef): WasmExternRef;
var
  w: WasmExternRef;
begin
  w := nil;
  testproc5 := nil;
end;

begin
  testproc5(nil);
end.
