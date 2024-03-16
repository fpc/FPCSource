{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    JSON-RPC dispatcher for webclient.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit module.rpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, fpHTTP, fpWeb, fpjsonrpc, webjsonrpc;

type

  { TrpcModule }

  TrpcModule = class(TJSONRPCModule)
  private

  public

  end;

var
  rpcModule: TrpcModule;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule('RPC', TrpcModule);
end.

