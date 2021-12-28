{
    This file is part of the Free Component Library

    Demonstrate client-side JSON-RPC functionality using Invoke.
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program rpcclient;

{$if not defined(CPU386) and not defined(WIN64)}
{$define useffi}
{$endif}

uses
  sysutils, jsonparser, {$ifdef useffi} ffi.manager,{$endif} myapi, fprpcclient;


Procedure DoTestRPC(RPC : TFPRPCClient);

var
  client: IMyInterface;
  arr: TStringArray;
  s: String;
  res: Boolean;
begin
  // Simple typecast to the needed interface
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
  // Explicitly create a service by name
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
    // Typecast
    DoTestRPC(aRPCClient);
    // Actually create service
    DoTestRPC2(aRPCClient);
  finally
    aRPCClient.Free;
  end;
end.

