program RPCCli;

uses SysUtils, Classes, fpAsync, xmlrpc;

type

  TClientApplication = class
    procedure WriteStringCompleted(AParser: TXMLRPCParser);
    procedure AddCompleted(AParser: TXMLRPCParser);
  public
    procedure Run;
  end;

procedure TClientApplication.Run;
var
  Client: TXMLRPCClient;
begin
  Client := TXMLRPCClient.Create(nil);
  try
    Client.Call(@WriteStringCompleted, 'WriteString', ['A test string']);
    Client.Call(@AddCompleted, 'Add', [123, 456]);
  finally
    Client.Free;
  end;
end;

procedure TClientApplication.WriteStringCompleted(AParser: TXMLRPCParser);
begin
  WriteLn('"WriteString" call completed');
end;

procedure TClientApplication.AddCompleted(AParser: TXMLRPCParser);
begin
  WriteLn('"Add" call completed. Result: ', AParser.GetNextInt);
end;

var
  App: TClientApplication;
begin
  App := TClientApplication.Create;
  try
    try
      App.Run;
    except
      on e: Exception do
        WriteLn('Error: ', e.Message);
    end;
  finally
    App.Free;
  end;
end.
