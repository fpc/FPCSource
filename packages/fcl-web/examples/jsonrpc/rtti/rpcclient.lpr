program rpcclient;

{$if not defined(CPU386) and not defined(WIN64)}
{$define useffi}
{$endif}

uses
  sysutils, jsonparser, fprpcclient, {$ifdef useffi} ffi.manager,{$endif} myapi;


Procedure DoTestRPC(RPC : TFPRPCClient);

var
  client: IMyInterface;
  arr: TStringArray;
  s: String;
  res: Boolean;
begin
  client := RPC as IMyInterface;
  Writeln('===== Testing SayHello');
  client.SayHello;
  Writeln('===== Testing DoSum');
  Writeln(client.DoSum(2, 6));
  Writeln('===== Testing Split');
  arr := client.Split('Hello FPC World', ' ');
  Writeln('Split data:');
  for s in arr do
    Writeln(#9, s);
  Writeln('===== Testing DoVarTest');
  s := 'Foobar';
  res := client.DoVarTest(s);
  Writeln(res, ' ', s);
  s := 'Test';
  res := client.DoVarTest(s);
  Writeln(res, ' ', s);
//  Writeln('===== Testing Echo');
//  writeln(Client.Echo(['This','is','Sparta']));
end;

Procedure DoTestRPC2(RPC : TFPRPCClient);

var
  client: IMyOtherInterface;
begin
  Client:=RPC.Specialize CreateService<IMyotherInterface>('Service2');
  Writeln('===== Testing SayHello');
  Writeln('Sayhello: ',client.SayHello);
  Writeln('===== Testing DoEcho');
  Writeln('Sayhello: ',client.Echo(['This','is','Sparta']));
end;

var
  aRPCClient : TFPRPCClient;

begin
  RPCServiceRegistry.Add(TypeInfo(IMyInterface));
  RPCServiceRegistry.Add(TypeInfo(IMyOtherInterface),'Service2');
  aRPCClient:=TFPRPCClient.Create(Nil);
  try
    aRPCClient.BaseURL:=ParamStr(1);
    if (aRPCClient.BaseURL='') then
      aRPCClient.BaseURL:='http://localhost:8080/RPC';
    DoTestRPC(aRPCClient);
    DoTestRPC2(aRPCClient);
  finally
    aRPCClient.Free;
  end;
end.

