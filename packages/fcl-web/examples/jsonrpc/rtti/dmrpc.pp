unit dmRPC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, websession, jsonparser, fpHTTP, fpWeb, fpjsonrpc, webjsonrpc;

type
  TRPCModule = class(TJSONRPCModule)
  private

  public

  end;

var
  RPCModule: TRPCModule;

implementation

{$R *.lfm}

initialization
  TRPCModule.RegisterModule('RPC');
end.

