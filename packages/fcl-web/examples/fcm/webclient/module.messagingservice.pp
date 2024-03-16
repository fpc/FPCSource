{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM Messaging demo - web client : RPC client

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit module.messagingservice;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, fprpcclient, service.messagingserver;

type

  { TRPCModule }

  TRPCModule = class(TDataModule)
    Client: TPas2jsRPCClient;
    procedure DataModuleCreate(Sender: TObject);
  private
    FService: TMessagingService;
  public
    Property Service : TMessagingService Read FService;
  end;

var
  RPCModule: TRPCModule;

implementation

{$R *.lfm}

{ TRPCModule }

procedure TRPCModule.DataModuleCreate(Sender: TObject);
begin
  FService:=TMessagingService.Create(Self);
  FService.RPCClient:=Client;
end;

end.

