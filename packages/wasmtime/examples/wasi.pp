{$mode objfpc}
{$h+}
program wasi;

uses classes,ctypes, wasmtime;

procedure exit_with_error(message : PAnsiChar; error : Pwasmtime_error_t; trap: Pwasm_trap_t); cdecl;

var
  error_message : Twasm_byte_vec_t ;
  S : AnsiString;
begin
  Writeln(stderr, 'error: ', message);
  S:='';
  if Trap<>nil then
    begin
    write('Trap: ');
    wasm_trap_message(trap, @error_message);
    wasm_trap_delete(trap);
    end
  else
    begin
    write('Error: ');
    wasmtime_error_message(error, @error_message);
    wasmtime_error_delete(error)
    end;
  SetLength(S,error_message.size);
  Move(error_message.data^,S[1],error_message.size);
  Writeln(stderr, '>>',S,'<<');
  wasm_byte_vec_delete(@error_message);
  halt(1);
end;

Var
  engine : Pwasm_engine_t = Nil;
  store : Pwasmtime_store_t = Nil;
  context : Pwasmtime_context_t = Nil;
  F : TMemoryStream;
  wasm : twasm_byte_vec_t;
  module : Pwasmtime_module_t = Nil;
  error : Pwasmtime_error_t = Nil;
  func : Twasmtime_func_t;
  trap : Pwasm_trap_t = Nil;
  linker : Pwasmtime_linker_t;
  wasi_config : Pwasi_config_t;
  status : cint;

begin
  Writeln('Loading wasm library');
  Loadwasmtime('./'+libwasmtime);
  Writeln('Initializing...');
  engine := wasm_engine_new();
  store:=wasmtime_store_new(engine, nil,nil);
  context:=wasmtime_store_context(store);

  linker:= wasmtime_linker_new(engine);
  error:=wasmtime_linker_define_wasi(linker);
  if (error<>Nil) then
    exit_with_error('failed to define link wasi', error, Nil);

  F:=TMemoryStream.Create;
  try
    F.LoadFromFile('hello.wasm');
    wasm_byte_vec_new_uninitialized(@wasm, F.Size);
    Move(F.Memory^,wasm.data^,F.Size);
  finally
    F.Free;
  end;

  // Now that we've got our binary webassembly we can compile our module.
  Writeln('Compiling module...');

  error:=wasmtime_module_new(engine, Puint8_t(wasm.data), wasm.size, @module);
  wasm_byte_vec_delete(@wasm);
  if (error <> nil) then
    exit_with_error('failed to compile module', error, nil);

  wasi_config:=wasi_config_new();
  if (wasi_config=nil) then
    exit_with_error('failed to create wasi config', Nil, nil);

  wasi_config_inherit_argv(wasi_config);
  wasi_config_inherit_env(wasi_config);
  wasi_config_inherit_stdin(wasi_config);
  wasi_config_inherit_stdout(wasi_config);
  wasi_config_inherit_stderr(wasi_config);
  wasi_config_preopen_dir(wasi_config,PAnsiChar('.'),PAnsiChar('.'));
  error:=wasmtime_context_set_wasi(context, wasi_config);
  if (error<>Nil) then
    exit_with_error('failed to instantiate WASI', error, nil);

  // Instantiate the module
  error:=wasmtime_linker_module(linker, context, Nil, 0, module);
  if (error<>nil) then
    exit_with_error('failed to instantiate module', Nil, Nil);


  error:=wasmtime_linker_get_default(linker, context, nil, 0, @func);
  if (error<>nil) then
    exit_with_error('failed to locate default export for module', error, nil);

  // And call it!
  Writeln('Calling export...');
  error:=wasmtime_func_call(context, @func, nil, 0, nil, 0, @trap);
  if wasmtime_error_exit_status(error,@status)<>0 then
    Writeln('Wasm program exited with status: ',Status)
  else 
    exit_with_error('Error while running default export for module', error, trap);
  // Clean up after ourselves at this point
  Writeln('All finished!\n');
  wasmtime_module_delete(module);
  wasmtime_store_delete(store);
  wasm_engine_delete(engine);

end.

