program gcd;

uses sysutils, classes,ctypes, wasmtime;

procedure exit_with_error(message : PAnsiChar; error : Pwasmtime_error_t; trap: Pwasm_trap_t); cdecl;

var
  error_message : Twasm_byte_vec_t ;
  S : AnsiString;
begin
  Writeln(stderr, 'error: ', message);

  if (error <> Nil)  then
    begin
    wasmtime_error_message(error, @error_message);
    wasmtime_error_delete(error)
    end
  else
    begin
    wasm_trap_message(trap, @error_message);
    wasm_trap_delete(trap);
    end;
  SetLength(S,error_message.size);
  Move(error_message.data^,S[1],error_message.size);
  Writeln(stderr, S);
  wasm_byte_vec_delete(@error_message);
  halt(1);
end;

Var
  engine : Pwasm_engine_t = Nil;
  store : Pwasmtime_store_t = Nil;
  context : Pwasmtime_context_t = Nil;
  F : TMemoryStream;
  wat : Twasm_byte_vec_t;
  wasm : twasm_byte_vec_t;
  module : Pwasmtime_module_t = Nil;
  error : Pwasmtime_error_t = Nil;
  trap : Pwasm_trap_t = Nil;
  instance : Twasmtime_instance_t;
  gcd_func : Twasmtime_extern_t;
  ok : Byte;

  a : longint = 6;
  b : longint = 27;
  params : Array [0..1] of twasmtime_val_t;
  results : array [0..0] of twasmtime_val_t;


begin
  Writeln('Loading wasm library');
  Loadwasmtime('./'+libwasmtime);
  Writeln('Initializing...');
  gcd_func:=Default(Twasmtime_extern_t);
  engine := wasm_engine_new();
  store:=wasmtime_store_new(engine, nil,nil);
  context:=wasmtime_store_context(store);
  F:=TMemoryStream.Create;
  try
    F.LoadFromFile('gcd.wat');
    wasm_byte_vec_new_uninitialized(@wat, F.Size);
    Move(F.Memory^,wat.data^,F.Size);
  finally
    F.Free;
  end;
  error:=wasmtime_wat2wasm(PAnsiChar(wat.data), wat.size, @wasm);
  if (error<>Nil) then
    exit_with_error('failed to parse wat', error, Nil);
  wasm_byte_vec_delete(@wat);

  Writeln('Compiling module...');

  error:=wasmtime_module_new(engine, Puint8_t(wasm.data), wasm.size, @module);
  wasm_byte_vec_delete(@wasm);
  if (error <> nil) then
    exit_with_error('failed to compile module', error, nil);

  Writeln('Instantiating module...');
  error:=wasmtime_instance_new(context, module, nil, 0, @instance, @trap);
  if (error<>nil) or (trap <>Nil) then
    exit_with_error('failed to instantiate', error, trap);

  Writeln('Extracting export...');
  ok:=wasmtime_instance_export_get(context, @instance, PAnsiChar('gcd'), 3, @gcd_func) ;
  if OK=0 then
    exit_with_error('failed to get gcd export', nil, nil);
  if gcd_func.kind<>WASMTIME_EXTERN_FUNC then
    exit_with_error('gcd is not a function', nil, nil);


  Writeln('Calling export...');
  params[0].kind:=WASMTIME_I32;
  params[0].of_.i32:=a;
  params[1].kind:=WASMTIME_I32;
  params[1].of_.i32:= b;

  error:=wasmtime_func_call(context, @gcd_func.of_.func, @params, 2, @results, 1, @trap);
  if (error<>nil) or (trap<>nil) then
    exit_with_error('failed to call function', error, trap);

  writeln(Format('gcd(%d, %d) = %d',[ a, b, results[0].of_.i32]));
  // Clean up after ourselves at this point
  Writeln('All finished!');
  wasmtime_module_delete(module);
  wasmtime_store_delete(store);
  wasm_engine_delete(engine);
end.

