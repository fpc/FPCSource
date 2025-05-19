program runfib;

uses ctypes, libwasmedge;

var
  ConfCxt : PWasmEdge_ConfigureContext;
  VMCxt : PWasmEdge_VMContext;
  Returns, Params : Array[0..0] of TWasmEdge_Value;
  FuncName : TWasmEdge_String;
  Res : TWasmEdge_Result;

begin
  Loadlibwasmedge('libwasmedge.so');
  { Create the configure context and add the WASI support.
    This step is not necessary unless you need WASI support.}
  ConfCxt:=WasmEdge_ConfigureCreate();
  WasmEdge_ConfigureAddHostRegistration(ConfCxt, WasmEdge_HostRegistration_Wasi);
  { The configure and store context to the VM creation can be NULL. }
  VMCxt:=WasmEdge_VMCreate(ConfCxt,Nil);
  { The parameters }
  Params[0] := WasmEdge_ValueGenI32(32);
  { Function name. }
  FuncName:=WasmEdge_StringCreateByCString(Pcchar(Pansichar('fib')));
  { Run the WASM function from file. }
  Res := WasmEdge_VMRunWasmFromFile(VMCxt, pcchar(PAnsiChar(ParamStr(1))), FuncName, @Params, 1, @Returns, 1);
  if (WasmEdge_ResultOK(Res)) then
    Writeln('Get result: ', WasmEdge_ValueGetI32(Returns[0]))
  else
    Writeln('Error message: ', PAnsiChar(WasmEdge_ResultGetMessage(Res)));
  { Resources deallocations. }
  WasmEdge_VMDelete(VMCxt);
  WasmEdge_ConfigureDelete(ConfCxt);
  WasmEdge_StringDelete(FuncName);
end.

