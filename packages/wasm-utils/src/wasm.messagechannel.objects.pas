{
    This file is part of the Free Component Library

    Webassembly MessageChannel API - object interface.
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.messagechannel.objects;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  System.Classes,
  {$ELSE}
  sysutils,
  classes,
  {$endif}
  wasm.messagechannel.shared, wasm.messagechannel.api;

Type
  EMessageChannel = class(Exception);
  TMessageChannelEvent = procedure(Sender : TObject; const aMessage : string) of object;

  { TWasmMessageChannel }

  TWasmMessageChannel = Class (TObject)
  private
    FID : TWasmMessageChannelID;
    FOnMessage: TMessageChannelEvent;
    procedure SetOnMessage(AValue: TMessageChannelEvent);
  Public
    constructor create(aType : TWasmMessageChannelType);
    destructor destroy; override;
    Procedure SendMessage(const S : String; aDeserialize : boolean);
    Property ID : TWasmMessageChannelID read FID;
    Property OnMessage : TMessageChannelEvent Read FOnMessage Write SetOnMessage;
  end;

  { TWasmBroadcastMessageChannel }

  TWasmBroadcastMessageChannel = class(TWasmMessageChannel)
  public
    constructor Create(const aName : UTF8String); reintroduce;
  end;

  { TWasmWorkerMessageChannel }

  TWasmWorkerMessageChannel = class(TWasmMessageChannel)
  public
    constructor Create(); reintroduce;
  end;

  { TMessageChannelController }

  TMessageChannelController = class sealed (TObject)
  private
    class var _Instance: TMessageChannelController;
  private
    FCurrentID : TWasmMessageChannelID;
    FList : TThreadList;
  protected
    function getNextID : TWasmMessageChannelID;
    function FindChannel(aID: TWasmMessageChannelID): TWasmMessageChannel;
    procedure HandleMessage(aID: TWasmMessageChannelID; S: String);
    procedure Register(aChannel : TWasmMessageChannel);
    procedure UnRegister(aChannel : TWasmMessageChannel);
  Public
    constructor create;
    destructor destroy;
    class constructor init;
    class destructor done;
    class property Instance : TMessageChannelController read _Instance;
  end;

implementation

uses wasm.logger.api;

{ TWasmMessageChannel }

procedure TWasmMessageChannel.SetOnMessage(AValue: TMessageChannelEvent);
const
  {$if SizeOf(Char)=2}
  UseUTF16 = True;
  {$ELSE}
  UseUTF16 = False;
  {$ENDIF}
begin
  if FOnMessage=AValue then Exit;
  FOnMessage:=AValue;
  __msgchannel_listen(ID,UseUTF16);
end;

constructor TWasmMessageChannel.create(aType: TWasmMessageChannelType);
begin
  FID:=TMessageChannelController.Instance.getNextID;
end;

destructor TWasmMessageChannel.destroy;
var
  lRes : TWasmMessageChannelResult;
begin
  lRes:=__msgchannel_deallocate(ID);
  if lRes<>WASMMSGCHANNEL_RESULT_SUCCESS then
    __wasm_log(wllError,className,'Failed to deallocate message %d. Error: %d',[Id,lRes]);
  TMessageChannelController.Instance.UnRegister(Self);
  Inherited destroy;
end;

procedure TWasmMessageChannel.SendMessage(const S: String; aDeserialize : boolean);
var
  lRes : TWasmMessageChannelResult;
begin
  {$if SizeOf(Char)=2}
  lRes:=__msgchannel_send_message_UTF16(ID,PUnicodeChar(S),Length(S),Ord(aDeserialize));
  {$ELSE}
  lRes:=__msgchannel_send_message_UTF8(ID,PByte(S),Length(S),Ord(aDeserialize));
  {$ENDIF}
  if lRes<>WASMMSGCHANNEL_RESULT_SUCCESS then
    Raise EMessageChannel.CreateFmt('Failed to send message on channel %d. Error: %d',[Id,lRes]);
end;

{ TWasmBroadcastMessageChannel }

constructor TWasmBroadcastMessageChannel.Create(const aName: UTF8String);
var
  lRes : TWasmMessageChannelResult;
begin
  inherited create(ctBroadcast);
  lRes:=__msgchannel_allocate(FID,Ord(ctBroadcast),PAnsiChar(aName),Length(aName));
  if lRes<>WASMMSGCHANNEL_RESULT_SUCCESS then
    Raise EMessageChannel.CreateFmt('Failed to create message channel. Error: %d',[lRes]);
  TMessageChannelController.Instance.Register(Self);
end;

{ TWasmWorkerMessageChannel }

constructor TWasmWorkerMessageChannel.Create();
var
  lRes : TWasmMessageChannelResult;
begin
  inherited create(ctWorker);
  lRes:=__msgchannel_allocate(FID,Ord(ctWorker),Nil,0);
  if lRes<>WASMMSGCHANNEL_RESULT_SUCCESS then
    Raise EMessageChannel.CreateFmt('Failed to create message channel. Error: %d',[lRes]);
  TMessageChannelController.Instance.Register(Self);
end;

{ TMessageChannelController }

function TMessageChannelController.getNextID: TWasmMessageChannelID;
begin
  Result:=InterlockedIncrement(FCurrentID);
end;

procedure TMessageChannelController.Register(aChannel: TWasmMessageChannel);
begin
  FList.Add(aChannel);
end;

procedure TMessageChannelController.UnRegister(aChannel: TWasmMessageChannel);
begin
  FList.Remove(aChannel);
end;

function TMessageChannelController.FindChannel(aID : TWasmMessageChannelID) : TWasmMessageChannel;

var
  l : TList;
  i : Integer;

begin
  Result:=nil;
  L:=FList.LockList;
  try
    I:=0;
    While (Result=Nil) and (I<L.Count) do
      begin
      Result:=TWasmMessageChannel(l[i]);
      if Result.ID<>aID then
        Result:=Nil;
      Inc(i);
      end;
  finally
    FList.UnlockList;
  end;
end;



procedure TMessageChannelController.HandleMessage(aID : TWasmMessageChannelID; S : String);
var
  lChannel : TWasmMessageChannel;
begin
  lChannel:=FindChannel(aID);
  if Assigned(lChannel) and Assigned(lChannel.OnMessage) then
    lChannel.OnMessage(lChannel,S);
end;

//procedure TMessageChannelController.

constructor TMessageChannelController.create;
begin
  FCurrentID:=0;
  FList:=TThreadList.Create;
{$IF SizeOf(char)=1}
  OnMessageUTF8:=@HandleMessage;
{$ELSE}
  OnMessageUTF16:=@HandleMessage;
{$ENDIF}
end;

destructor TMessageChannelController.destroy;
begin
  FreeAndNil(Flist);
end;

class constructor TMessageChannelController.init;
begin
  _instance:=TMessageChannelController.Create;
end;

class destructor TMessageChannelController.done;
begin
  FreeAndNil(_instance);
end;

end.

