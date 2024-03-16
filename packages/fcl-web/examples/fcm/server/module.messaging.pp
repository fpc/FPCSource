{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM (Firebase Cloud Messaging) - JSON-RPC interface for webclient

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit module.messaging;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrpc, fpjson, fpfcmtypes, fpfcmsender;

type

  { TdmMessaging }

  TdmMessaging = class(TDataModule)
    RegisterSubscription: TJSONRPCHandler;
    SendNotification: TJSONRPCHandler;
    procedure RegisterSubscriptionExecute(Sender: TObject; const Params: TJSONData; out Res: TJSONData);
    procedure SendNotificationExecute(Sender: TObject; const Params: TJSONData; out Res: TJSONData);
  private
    class function ConfigDir: String;
    function AccessTokenFile: string;
    function DeviceTokensFileName: String;
    procedure HandleNewAccessToken(Sender: TObject; const aToken: TBearerToken);
    function LoadLastToken: UTF8String;
  public
    procedure SendMessage(Msg: TNotificationmessage);
    procedure SaveToken(const aToken: UTF8String);
  end;

var
  dmMessaging: TdmMessaging;

implementation

{$R *.lfm}

{ TdmMessaging }


class Function TdmMessaging.ConfigDir : String;

begin
  Result:=ExtractFilePath(ParamStr(0));
end;

Function TdmMessaging.DeviceTokensFileName : String;
begin
  Result:=ConfigDir+'device-tokens.txt';
end;

Function TdmMessaging.AccessTokenFile : string;

begin
  Result:=ConfigDir+'access-token.json';
end;

procedure TdmMessaging.HandleNewAccessToken(Sender: TObject; const aToken: TBearerToken);

begin
  aToken.SaveToFile(AccessTokenFile);
end;

procedure TdmMessaging.SaveToken(const aToken : UTF8String);

var
  L : TStrings;
  FN : String;

begin
  FN:=DeviceTokensFileName;
  L:=TStringList.Create;
  try
    if FileExists(FN) then
      L.LoadFromFile(FN);
    L.Add(aToken);
    L.SaveToFile(FN);
  finally
    L.Free;
  end;
end;

function TdmMessaging.LoadLastToken : UTF8String;

var
  L : TStrings;
  FN : String;

begin
  FN:=DeviceTokensFileName;
  L:=TStringList.Create;
  try
    if fileExists(fn) then
      L.LoadFromFile(FN);
    if L.Count=0 then
      Raise Exception.Create('No tokens registered');
    Result:=L[L.Count-1];
  finally
    L.Free;
  end;
end;

procedure TdmMessaging.RegisterSubscriptionExecute(Sender: TObject; const Params: TJSONData; out Res: TJSONData);

var
  Parms: TJSONArray absolute params;
  aToken : UTF8String;

begin
  If Parms.Count<>1 then
    Raise Exception.Create('Invalid param count');
  if Parms[0].JSONType=JTString then
    // FCM token
    aToken:=Parms[0].AsString
  else if Parms[0].JSONType=jtObject then
    aToken:=Parms[0].AsJSON
  else
    Raise Exception.Create('Invalid param type for token');
  SaveToken(aToken);
  Res:=TJSONBoolean.Create(True);
end;

procedure TdmMessaging.SendMessage(Msg : TNotificationmessage);

var
  Sender : TFCMClient;
  aConfig, aToken : String;

begin
  aToken:=LoadLastToken;
  Sender:=TFCMClient.Create(Self);
  try
    aConfig:=ChangeFileExt(paramstr(0),'-serviceaccount.json');
    Sender.LogFile:=ChangeFileExt(paramstr(0),'.log');
    Sender.InitServiceAccount(aConfig,'');
    Sender.OnNewBearerToken:=@HandleNewAccessToken;
    if FileExists(AccessTokenFile) then
      Sender.BearerToken.LoadFromFile(AccessTokenFile);
    Sender.Send(Msg,aToken);
  finally
    Sender.Free;
  end;
end;

procedure TdmMessaging.SendNotificationExecute(Sender: TObject; const Params: TJSONData; out Res: TJSONData);

var
  Parms: TJSONArray absolute params;
  Obj : TJSONObject;
  Msg : TNotificationMessage;

begin
  If Parms.Count<>1 then
    Raise Exception.Create('Invalid param count');
  if Parms[0].JSONType<>jtObject then
    Raise Exception.Create('Invalid notification');
  Obj:=Parms.Objects[0];
  Msg:=TNotificationMessage.Create;
  try
    Msg.Title:=Obj.Get('title',Msg.Title);
    Msg.Body:=Obj.Get('body',Msg.Body);
    Msg.Image:=Obj.Get('image',Msg.Image);
    SendMessage(Msg);
  finally
    Msg.Free;
  end;
end;

initialization
  JSONRPCHandlerManager.RegisterDatamodule(TdmMessaging, 'Messaging');
end.

