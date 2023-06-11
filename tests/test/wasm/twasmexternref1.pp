{ %cpu=wasm32 }
{ %norun }

program twasmexternref1;

var
  global_externref: WasmExternRef; section 'WebAssembly.Global';

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
  global_externref := d;
  q := global_externref;
end;

function testproc5(q: WasmExternRef): WasmExternRef;
var
  w: WasmExternRef;
begin
  w := nil;
  testproc5 := nil;
  global_externref := nil;
end;

function testproc6: Boolean;
var
  q: WasmExternRef;
begin
  testproc6 := q = nil;
  testproc6 := nil = q;
  testproc6 := global_externref = nil;
  testproc6 := nil = global_externref;
  testproc6 := q <> nil;
  testproc6 := nil <> q;
  testproc6 := global_externref <> nil;
  testproc6 := nil <> global_externref;
end;

procedure testproc7(const q: WasmExternRef);
begin
end;

begin
  testproc5(nil);
  testproc5(global_externref);
  testproc7(nil);
end.
