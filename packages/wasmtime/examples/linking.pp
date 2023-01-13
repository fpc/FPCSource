program linking;

uses classes,ctypes, wasmtime;

procedure exit_with_error(message : string; error : Pwasmtime_error_t; trap: Pwasm_trap_t); cdecl;

var
  error_message : Twasm_byte_vec_t ;
  S : AnsiString;
begin
  Writeln(stderr, 'error: ', message);
  S:='';
  if (error <> Nil)  then
    begin
    write('Error: ');
    wasmtime_error_message(error, @error_message);
    wasmtime_error_delete(error)
    end
  else
    begin
    write('Trap: ');
    wasm_trap_message(trap, @error_message);
    wasm_trap_delete(trap);
    end;
  SetLength(S,error_message.size);
  Move(error_message.data^,S[1],error_message.size);
  Writeln(stderr, '>>',S,'<<');
  wasm_byte_vec_delete(@error_message);
  halt(1);
end;

procedure checkerror(error : Pwasmtime_error_t; Message : string);

begin
  if Assigned(error) then
    exit_with_error(message,error,Nil);
end;

procedure checkerror(error : Pwasmtime_error_t; var trap : Pwasm_trap_t; Message : string);

begin
  if Assigned(error) or assigned(Trap) then
    exit_with_error(message,error,trap);
end;


Procedure read_wat_file(engine : Pwasm_engine_t; out bytes : twasm_byte_vec_t; aFile : String);

var
  wat : twasm_byte_vec_t;
  F : TMemoryStream;

begin
  F:=TMemoryStream.Create;
  try
    F.LoadFromFile(aFile);
    wasm_byte_vec_new_uninitialized(@wat, F.Size);
    Move(F.Memory^,wat.data^,F.Size);
  finally
    F.Free;
  end;
  CheckError(wasmtime_wat2wasm(PAnsiChar(wat.data), wat.size, @bytes),
            'failed to parse wat file '+aFile);
  wasm_byte_vec_delete(@wat);
end;

Var
  engine : Pwasm_engine_t = Nil;
  store : Pwasmtime_store_t = Nil;
  context : Pwasmtime_context_t = Nil;
  linking1_wasm, linking2_wasm : twasm_byte_vec_t;
  linking1_module,linking2_module : Pwasmtime_module_t;
  error : Pwasmtime_error_t = Nil;
  run : twasmtime_extern_t;
  trap : Pwasm_trap_t = Nil;
  linker : Pwasmtime_linker_t;
  wasi_config : Pwasi_config_t;
  status : cint;
  linking1, linking2 : twasmtime_instance_t;
  ok : Byte;

begin
  linking1_module:=nil;
  linking2_module:=Nil;
  Writeln('Loading wasm library');
  Loadwasmtime('./'+libwasmtime);
  Writeln('Initializing...');
  engine := wasm_engine_new();
  store:=wasmtime_store_new(engine, nil,nil);
  context:=wasmtime_store_context(store);

  read_wat_file(engine, linking1_wasm, 'linking1.wat');
  read_wat_file(engine, linking2_wasm, 'linking2.wat');


  // Now that we've got our binary webassembly we can compile our module.
  Writeln('Compiling module...');

  error:=wasmtime_module_new(engine, Puint8_t(linking1_wasm.data), linking1_wasm.size, @linking1_module);
  wasm_byte_vec_delete(@linking1_wasm);
  if (error <> nil) then
    exit_with_error('failed to compile module linking1', error, nil);

  error:=wasmtime_module_new(engine, Puint8_t(linking2_wasm.data), linking2_wasm.size, @linking2_module);
  wasm_byte_vec_delete(@linking2_wasm);
  if (error <> nil) then
    exit_with_error('failed to compile module linking2', error, nil);

  Writeln('Configuring WASI...');
  wasi_config:=wasi_config_new();
  if (wasi_config=nil) then
    exit_with_error('failed to create wasi config', Nil, nil);

  wasi_config_inherit_argv(wasi_config);
  wasi_config_inherit_env(wasi_config);
  wasi_config_inherit_stdin(wasi_config);
  wasi_config_inherit_stdout(wasi_config);
  wasi_config_inherit_stderr(wasi_config);
  wasi_config_preopen_dir(wasi_config,PAnsiChar('.'),PAnsiChar('.'));
  CheckError(wasmtime_context_set_wasi(context, wasi_config),
             'failed to instantiate WASI');

  Writeln('Creating linker...');
  linker:= wasmtime_linker_new(engine);
  CheckError(wasmtime_linker_define_wasi(linker),'failed to define link wasi');

  // Instantiate `linking2` with our linker.

  CheckError(wasmtime_linker_instantiate(linker, context, linking2_module, @linking2, @trap),Trap,
             'failed to instantiate linking2');
  // Register our new `linking2` instance with the linker
  CheckError(wasmtime_linker_define_instance(linker, context, PAnsiChar('linking2'), Length('linking2'), @linking2),
            'failed to link linking2');

  // Instantiate `linking1` with the linker now that `linking2` is defined
  CheckError(wasmtime_linker_instantiate(linker, context, linking1_module, @linking1, @trap),trap,
              'failed to instantiate linking1');

  Writeln('Extracting export...');
  ok:=wasmtime_instance_export_get(context, @linking1, PAnsiChar('run'), 3, @run) ;
  if OK=0 then
    exit_with_error('failed to get run export', nil, nil);
  if run.kind<>WASMTIME_EXTERN_FUNC then
    exit_with_error('run is not a function', nil, nil);
  // And call it!
  Writeln('Calling export...');
  error:=wasmtime_func_call(context, @run.of_.func, nil, 0, nil, 0, @trap);
  if (Trap<>Nil) then
    begin
    // exit_proc is reported as trap.
    if wasmtime_trap_exit_status(trap,@status)<>0 then
      Writeln('Wasm program exited with status: ',Status)
    else
      exit_with_error('failed to run default export for module', error, trap);
    end
  else if (error<>nil) then
    exit_with_error('failed to run default export for module', error, trap);

  // Clean up after ourselves at this point
  Writeln('All finished!');
  wasmtime_linker_delete(linker);
  wasmtime_module_delete(linking1_module);
  wasmtime_module_delete(linking2_module);
  wasmtime_store_delete(store);
  wasm_engine_delete(engine);

end.

