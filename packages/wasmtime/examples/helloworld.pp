{$mode objfpc}
{$h+}
program helloworld;

uses classes,ctypes, wasmtime;

procedure exit_with_error(message : PAnsiChar; error : Pwasmtime_error_t; trap: Pwasm_trap_t); cdecl;

var
  error_message : Twasm_byte_vec_t ;
  S : AnsiString;

begin
  Writeln(stderr, 'error: ', message);
  S:='';
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

function hello_callback(env : Pointer; caller : Pwasmtime_caller_t; args : pwasmtime_val_t; nargs : size_t; results : pwasmtime_val_t; nresults : size_t) : pwasm_trap_t; cdecl;
begin
  Writeln('Calling back...');
  Writeln(' Hello World!');
  Result:=Nil;
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
  hello_ty : Pwasm_functype_t = nil;
  hello : Twasmtime_func_t;
  trap : Pwasm_trap_t = Nil;
  instance : Twasmtime_instance_t;
  import : Twasmtime_extern_t;
  run : Twasmtime_extern_t;
  ok : Byte;

begin
  Writeln('Loading wasm library');
  Loadwasmtime('./'+libwasmtime);
  Writeln('Initializing...');
  engine := wasm_engine_new();
  store:=wasmtime_store_new(engine, nil,nil);
  context:=wasmtime_store_context(store);
  F:=TMemoryStream.Create;
  try
    F.LoadFromFile('hello.wat');
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

  Writeln('Creating callback...');
  hello_ty:=wasm_functype_new_0_0();
  wasmtime_func_new(context, hello_ty, @hello_callback, Nil, Nil, @hello);

  Writeln('Instantiating module...');
  import.kind:=WASMTIME_EXTERN_FUNC;
  import.of_.func:=hello;
  error:=wasmtime_instance_new(context, module, @import, 1, @instance, @trap);
  if (error<>nil) or (trap <>Nil) then
    exit_with_error('failed to instantiate', error, trap);

  Writeln('Extracting export...');
  ok:=wasmtime_instance_export_get(context, @instance, PAnsiChar('run'), 3, @run) ;
  if OK=0 then
    exit_with_error('failed to get run export', nil, nil);
  if run.kind<>WASMTIME_EXTERN_FUNC then
    exit_with_error('run is not a function', nil, nil);

  Writeln('Calling export...');
  error:=wasmtime_func_call(context, @run.of_.func, nil, 0, nil, 0, @trap);
  if (error<>nil) or (trap<>nil) then
    exit_with_error('failed to call function', error, trap);

  Writeln('All finished!');
  wasmtime_module_delete(module);
  wasmtime_store_delete(store);
  wasm_engine_delete(engine);
end.

