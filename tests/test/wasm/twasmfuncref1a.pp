{ %cpu=wasm32 }
{ %norun }

program twasmfuncref1a;

{$MODE tp}

type
  TWasmFuncRef = function(a: longint; b: int64): longint; WasmFuncRef;

procedure testproc;
var
  p: TWasmFuncRef;
begin
end;

procedure testproc2(q: TWasmFuncRef);
begin
end;

function testproc3: TWasmFuncRef;
begin
end;

function testproc4(a, b, c: longint; d: TWasmFuncRef; e: int64): TWasmFuncRef;
var
  q: TWasmFuncRef;
begin
  q := d;
  testproc4 := q;
end;

function testproc5(q: TWasmFuncRef): TWasmFuncRef;
var
  w: TWasmFuncRef;
begin
  w := nil;
  testproc5 := nil;
end;

begin
  testproc5(nil);
end.
