program memory;

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

procedure check(success : boolean);
begin
  if (not success) then
    begin
    Writeln('> Error, expected success');
    Halt(1);
    end;
end;

procedure check_call(store : pwasmtime_context_t;
                func : Pwasmtime_func_t;
                args : Pwasmtime_val_t;
                nargs : Tsize_t;
                expected : tint32_t);
Var
  results : Array[0..0] of twasmtime_val_t;
  trap : Pwasm_trap_t;
  error : Pwasmtime_error_t;
begin
  trap:=nil;
  error:=wasmtime_func_call(store, func, args, nargs, results, 1, @trap);
  if (error<>nil) or (trap<>nil) then
    exit_with_error('failed to call function', error, trap);
  if (results[0].of_.i32 <> expected) then
    begin
    writeln('> Error on result');
    halt(1);
    end;
end;

procedure check_trap(store : pwasmtime_context_t;
                func : Pwasmtime_func_t;
                args : Pwasmtime_val_t;
                nargs : Tsize_t;
                num_results : size_t);
var
  results : array [0..0] of twasmtime_val_t ;
  trap : Pwasm_trap_t;
  error : Pwasmtime_error_t;
begin
  assert(num_results <= 1);
  trap:=nil;;
  error:=wasmtime_func_call(store, func, args, nargs, results, num_results, @trap);
  if (error<>Nil) then
    exit_with_error('failed to call function', error, nil);
  if (trap = Nil) then
    begin
    Writeln('> Error on result, expected trap');
    halt(1);
    end;
  wasm_trap_delete(trap);
end;


procedure check_call0(store : Pwasmtime_context_t; func : Pwasmtime_func_t; expected : Tint32_t );

begin
  check_call(store, func, nil, 0, expected);
end;

procedure check_call1(store : Pwasmtime_context_t; func : Pwasmtime_func_t; arg : tint32_t; expected: tint32_t);
var
  args : array[0..0] of twasmtime_val_t;
begin
  args[0].kind:=WASMTIME_I32;
  args[0].of_.i32:=arg;
  check_call(store, func, @args, 1, expected);
end;


procedure check_ok(store : Pwasmtime_context_t; func : Pwasmtime_func_t; args : Pwasmtime_val_t;nargs: tsize_t);

var
  trap : Pwasm_trap_t;
  error : Pwasmtime_error_t;

begin
  trap:=nil;
  error:=wasmtime_func_call(store, func, args, nargs, nil, 0, @trap);
  if ((error<>nil) or (trap <> nil)) then
    exit_with_error('failed to call function', error, trap);
end;

procedure check_ok2(store : Pwasmtime_context_t; func : Pwasmtime_func_t; arg1,arg2 : tint32_t) ;
var
  args : array[0..1] of twasmtime_val_t;
begin
  args[0].kind:=WASMTIME_I32;
  args[0].of_.i32:=arg1;
  args[1].kind:=WASMTIME_I32;
  args[1].of_.i32:=arg2;
  check_ok(store, func, args, 2);
end;

procedure check_trap1(store : Pwasmtime_context_t; func : Pwasmtime_func_t; arg : tint32_t);

var
  args : array[0..0] of twasmtime_val_t;
begin
  args[0].kind := WASMTIME_I32;
  args[0].of_.i32 := arg;
  check_trap(store, func, args, 1, 1);
end;

procedure check_trap2(store : Pwasmtime_context_t; func : Pwasmtime_func_t; arg1,arg2 : tint32_t);

var
  args : array[0..1] of twasmtime_val_t;
begin
  args[0].kind:=WASMTIME_I32;
  args[0].of_.i32:=arg1;
  args[1].kind:=WASMTIME_I32;
  args[1].of_.i32:=arg2;
  check_trap(store, func, args, 2, 0);
end;




Function GetFunc(context : Pwasmtime_context_t; instance : Pwasmtime_instance_t; aName : string) : twasmtime_func_t;

var
  item : twasmtime_extern_t;
  Ok : Byte;
begin
  ok:=wasmtime_instance_export_get(context, instance, PAnsiChar(aName), Length(aName), @item) ;
  if OK=0 then
    exit_with_error('failed to get '+aName+'export', nil, nil);
  if item.kind<>WASMTIME_EXTERN_FUNC then
    exit_with_error(aName+' is not a function', nil, nil);
  Result:=item.of_.func;
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
  ok : Byte;
  mem,mem2 : twasmtime_memory_t ;
  size_func, load_func, store_func : twasmtime_func_t;
  item : twasmtime_extern_t;
  old_size : Tuint32_t;
  limits : twasm_limits_t;
  memorytype : pwasm_memorytype_t;
begin
  Writeln('Loading wasm library');
  Loadwasmtime('./'+libwasmtime);
  Writeln('Initializing...');

  engine := wasm_engine_new();
  store:=wasmtime_store_new(engine, nil,nil);
  context:=wasmtime_store_context(store);
  F:=TMemoryStream.Create;
  try
    F.LoadFromFile('memory.wat');
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

  error:=wasmtime_instance_new(context, module, Nil, 0, @instance, @trap);
  if (error<>nil) or (trap <>Nil) then
    exit_with_error('failed to instantiate', error, trap);

  Writeln('Extracting exports...');

  ok:=wasmtime_instance_export_get(context, @instance, PAnsiChar('memory'), 6, @item) ;
  if OK=0 then
    exit_with_error('failed to get run export', nil, nil);
  if item.kind<>WASMTIME_EXTERN_MEMORY then
    exit_with_error('mem is not memory', nil, nil);
  mem:=item.of_.memory;

  size_func:=GetFunc(context,@instance,'size');
  load_func:=GetFunc(context,@instance,'load');
  store_func:=GetFunc(context,@instance,'store');

  Writeln('Checking memory...');
  check(wasmtime_memory_size(context, @mem) = 2);
  check(wasmtime_memory_data_size(context, @mem) = $20000);
  check(wasmtime_memory_data(context, @mem)[0] = 0);
  check(wasmtime_memory_data(context, @mem)[$1000] = 1);
  check(wasmtime_memory_data(context, @mem)[$1003] = 4);

  check_call0(context, @size_func, 2);
  check_call1(context, @load_func, 0, 0);
  check_call1(context, @load_func, $1000, 1);
  check_call1(context, @load_func, $1003, 4);
  check_call1(context, @load_func, $1ffff, 0);
  check_trap1(context, @load_func, $20000);

  Writeln('Mutating memory...');
  wasmtime_memory_data(context, @mem)[$1003]:=5;
  check_ok2(context, @store_func, $1002, 6);
  check_trap2(context, @store_func, $20000, 0);

  check(wasmtime_memory_data(context, @mem)[$1002] = 6);
  check(wasmtime_memory_data(context, @mem)[$1003] = 5);
  check_call1(context, @load_func, $1002, 6);
  check_call1(context, @load_func, $1003, 5);

  writeln('Growing memory...');

  error:=wasmtime_memory_grow(context, @mem, 1, @old_size);
  if (error <> Nil) then
    exit_with_error('failed to grow memory', error, nil);
  check(wasmtime_memory_size(context, @mem) = 3);
  check(wasmtime_memory_data_size(context, @mem) = $30000);

  check_call1(context, @load_func, $20000, 0);
  check_ok2(context, @store_func, $20000, 0);
  check_trap1(context, @load_func, $30000);
  check_trap2(context, @store_func, $30000, 0);

  // We expect an error here !
  error:=wasmtime_memory_grow(context, @mem, 1, @old_size);
  if (error = Nil) then
    exit_with_error('failed to grow memory 2', nil, nil);
  wasmtime_error_delete(error);
  error:=wasmtime_memory_grow(context, @mem, 0, @old_size);
  if (error <>Nil) then
    exit_with_error('failed to grow memory 3', error, nil);

  writeln('Creating stand-alone memory...');
  limits.min:=5;
  limits.max:=5;
  memorytype:=wasm_memorytype_new(@limits);
  error:=wasmtime_memory_new(context, memorytype, @mem2);
  if (error<>nil) then
    exit_with_error('failed to create memory', error, nil);
  wasm_memorytype_delete(memorytype);
  check(wasmtime_memory_size(context, @mem2)=5);

  Writeln('All finished!');
  wasmtime_module_delete(module);
  wasmtime_store_delete(store);
  wasm_engine_delete(engine);
end.

