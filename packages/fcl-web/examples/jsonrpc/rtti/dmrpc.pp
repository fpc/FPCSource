{
    This file is part of the Free Component Library

    This module routes the actual requests. See the options set on the datamodule

    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
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

