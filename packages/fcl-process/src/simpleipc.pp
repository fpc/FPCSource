{
    This file is part of the Free Component library.
    Copyright (c) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team

    Unit implementing one-way IPC between 2 processes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit simpleipc;

{$mode objfpc}{$H+}

interface

uses
  Contnrs, Classes, SysUtils;

Const
  MsgVersion = 1;
  
  //Message types
  mtUnknown = 0;
  mtString = 1;

type
  TIPCMessageOverflowAction = (ipcmoaNone, ipcmoaDiscardOld, ipcmoaDiscardNew, ipcmoaError);

var
  // Currently implemented only for Windows platform!
  DefaultIPCMessageOverflowAction: TIPCMessageOverflowAction = ipcmoaNone;
  DefaultIPCMessageQueueLimit: Integer = 0;

Type

  TMessageType = LongInt;
  TMsgHeader = Packed record
    Version : Byte;
    MsgType : TMessageType;
    MsgLen  : Integer;
  end;

  TSimpleIPCServer = class;
  TSimpleIPCClient = class;

  TIPCServerMsg = class
  strict private
    FStream: TStream;
    FMsgType: TMessageType;
  public
    constructor Create;
    destructor Destroy; override;
    property Stream: TStream read FStream;
    property MsgType: TMessageType read FMsgType write FMsgType;
  end;

  TIPCServerMsgQueue = class
  strict private
    FList: TFPObjectList;
    FMaxCount: Integer;
    FMaxAction: TIPCMessageOverflowAction;
    function GetCount: Integer;
    procedure DeleteAndFree(Index: Integer);
    function PrepareToPush: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(AItem: TIPCServerMsg);
    function Pop: TIPCServerMsg;
    property Count: Integer read GetCount;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property MaxAction: TIPCMessageOverflowAction read FMaxAction write FMaxAction;
  end;

  { TIPCServerComm }
  
  TIPCServerComm = Class(TObject)
  Private
    FOwner  : TSimpleIPCServer;
  Protected  
    Function  GetInstanceID : String; virtual; abstract;
    Procedure DoError(const Msg : String; const Args : Array of const);
    Procedure PushMessage(Const Hdr : TMsgHeader; AStream : TStream);
    Procedure PushMessage(Msg : TIPCServerMsg);
  Public
    Constructor Create(AOwner : TSimpleIPCServer); virtual;
    Property Owner : TSimpleIPCServer read FOwner;
    Procedure StartServer; virtual; Abstract;
    Procedure StopServer;virtual; Abstract;
    // May push messages on the queue
    Function  PeekMessage(TimeOut : Integer) : Boolean;virtual; Abstract;
    // Must put message on the queue.
    Procedure ReadMessage ;virtual; Abstract;
    Property InstanceID : String read GetInstanceID;
  end;
  TIPCServerCommClass = Class of TIPCServerComm;

  { TSimpleIPC }
  TSimpleIPC = Class(TComponent)
  Private
    procedure SetActive(const AValue: Boolean);
    procedure SetServerID(const AValue: String);
  Protected
    FBusy: Boolean;
    FActive : Boolean;
    FServerID : String;
    Procedure DoError(const Msg: String; const Args: array of const);
    Procedure CheckInactive;
    Procedure CheckActive;
    Procedure Activate; virtual; abstract;
    Procedure Deactivate; virtual; abstract;
    Procedure Loaded; override;
    Property Busy : Boolean Read FBusy;
  Published
    Property Active : Boolean Read FActive Write SetActive;
    Property ServerID : String Read FServerID Write SetServerID;
  end;

  { TSimpleIPCServer }


  TSimpleIPCServer = Class(TSimpleIPC)
  protected
  Private
    FQueue : TIPCServerMsgQueue;
    FGlobal: Boolean;
    FOnMessage: TNotifyEvent;
    FMsgType: TMessageType;
    FMsgData : TStream;
    function GetInstanceID: String;
    function GetMaxAction: TIPCMessageOverflowAction;
    function GetStringMessage: String;
    procedure SetGlobal(const AValue: Boolean);
    procedure SetMaxAction(AValue: TIPCMessageOverflowAction);
  Protected
    FIPCComm: TIPCServerComm;
    Function CommClass : TIPCServerCommClass; virtual;
    Procedure PushMessage(Msg : TIPCServerMsg); virtual;
    function PopMessage: Boolean; virtual;
    Procedure Activate; override;
    Procedure Deactivate; override;
    Property Queue : TIPCServerMsgQueue Read FQueue;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure StartServer;
    Procedure StopServer;
    Function PeekMessage(TimeOut : Integer; DoReadMessage : Boolean): Boolean;
    Procedure ReadMessage;
    Property  StringMessage : String Read GetStringMessage;
    Procedure GetMessageData(Stream : TStream);
    Property  MsgType: TMessageType Read FMsgType;
    Property  MsgData : TStream Read FMsgData;
    Property  InstanceID : String Read GetInstanceID;
  Published
    Property Global : Boolean Read FGlobal Write SetGlobal;
    Property OnMessage : TNotifyEvent Read FOnMessage Write FOnMessage;
    property MaxAction: TIPCMessageOverflowAction read GetMaxAction write SetMaxAction;
  end;


  { TIPCClientComm}
  TIPCClientComm = Class(TObject)
  private
    FOwner: TSimpleIPCClient;
  protected
    Procedure DoError(const Msg : String; const Args : Array of const);
  Public
    Constructor Create(AOwner : TSimpleIPCClient); virtual;
    Property  Owner : TSimpleIPCClient read FOwner;
    Procedure Connect; virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    Function  ServerRunning : Boolean; virtual; abstract;
    Procedure SendMessage(MsgType : TMessageType; Stream : TStream);virtual;Abstract;
  end;
  TIPCClientCommClass = Class of TIPCClientComm;
  
  { TSimpleIPCClient }
  TSimpleIPCClient = Class(TSimpleIPC)
  Private
    FServerInstance: String;
    procedure SetServerInstance(const AValue: String);
  Protected
    FIPCComm : TIPCClientComm;
    Procedure Activate; override;
    Procedure Deactivate; override;
    Function CommClass : TIPCClientCommClass; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Connect;
    Procedure Disconnect;
    Function  ServerRunning : Boolean;
    Procedure SendMessage(MsgType : TMessageType; Stream: TStream);
    Procedure SendStringMessage(const Msg : String);
    Procedure SendStringMessage(MsgType : TMessageType; const Msg : String);
    Procedure SendStringMessageFmt(const Msg : String; Args : Array of const);
    Procedure SendStringMessageFmt(MsgType : TMessageType; const Msg : String; Args : Array of const);
    Property  ServerInstance : String Read FServerInstance Write SetServerInstance;
  end;


  EIPCError = Class(Exception);

Var
  DefaultIPCServerClass : TIPCServerCommClass = Nil;
  DefaultIPCClientClass : TIPCClientCommClass = Nil;

resourcestring
  SErrServerNotActive = 'Server with ID %s is not active.';
  SErrActive = 'This operation is illegal when the server is active.';
  SErrInActive = 'This operation is illegal when the server is inactive.';


implementation

{ ---------------------------------------------------------------------
  Include platform specific implementation. 
  Should implement the CommClass method of both server and client component, 
  as well as the communication class itself.
  
  This comes first, to allow the uses clause to be set.
  If the include file defines OSNEEDIPCINITDONE then the unit will
  call IPCInit and IPCDone in the initialization/finalization code.
  
  --------------------------------------------------------------------- }
{$UNDEF OSNEEDIPCINITDONE}

{$i simpleipc.inc}

Resourcestring
  SErrMessageQueueOverflow = 'Message queue overflow (limit %s)';

{ ---------------------------------------------------------------------
    TIPCServerMsg
  ---------------------------------------------------------------------}


constructor TIPCServerMsg.Create;
begin
  FMsgType := 0;
  FStream := TMemoryStream.Create;
end;

destructor TIPCServerMsg.Destroy;
begin
  FStream.Free;
end;

{ ---------------------------------------------------------------------
    TIPCServerMsgQueue
  ---------------------------------------------------------------------}

constructor TIPCServerMsgQueue.Create;
begin
  FMaxCount := DefaultIPCMessageQueueLimit;
  FMaxAction := DefaultIPCMessageOverflowAction;
  FList := TFPObjectList.Create(False); // FreeObjects = False!
end;

destructor TIPCServerMsgQueue.Destroy;
begin
  Clear;
  FList.Free;
end;

procedure TIPCServerMsgQueue.Clear;
begin
  while FList.Count > 0 do
    DeleteAndFree(FList.Count - 1);
end;

procedure TIPCServerMsgQueue.DeleteAndFree(Index: Integer);
begin
  FList[Index].Free; // Free objects manually!
  FList.Delete(Index);
end;

function TIPCServerMsgQueue.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIPCServerMsgQueue.PrepareToPush: Boolean;
begin
  Result := True;
  case FMaxAction of
    ipcmoaDiscardOld:
      begin
        while (FList.Count >= FMaxCount) do
          DeleteAndFree(FList.Count - 1);
      end;
    ipcmoaDiscardNew:
      begin
        Result := (FList.Count < FMaxCount);
      end;
    ipcmoaError:
      begin
        if (FList.Count >= FMaxCount) then
          // Caller is expected to catch this exception, so not using Owner.DoError()
          raise EIPCError.CreateFmt(SErrMessageQueueOverflow, [IntToStr(FMaxCount)]);
      end;
  end;
end;

procedure TIPCServerMsgQueue.Push(AItem: TIPCServerMsg);
begin
  if PrepareToPush then
    FList.Insert(0, AItem);
end;

function TIPCServerMsgQueue.Pop: TIPCServerMsg;
var
  Index: Integer;
begin
  Index := FList.Count - 1;
  if Index >= 0 then
  begin
    // Caller is responsible for freeing the object.
    Result := TIPCServerMsg(FList[Index]);
    FList.Delete(Index);
  end
  else
    Result := nil;
end;


{ ---------------------------------------------------------------------
    TIPCServerComm
  ---------------------------------------------------------------------}

constructor TIPCServerComm.Create(AOwner: TSimpleIPCServer);
begin
  FOwner:=AOWner;
end;

procedure TIPCServerComm.DoError(const Msg: String; const Args: array of const);

begin
  FOwner.DoError(Msg,Args);
end;

procedure TIPCServerComm.PushMessage(const Hdr: TMsgHeader; AStream: TStream);

Var
  M : TIPCServerMsg;

begin
  M:=TIPCServerMsg.Create;
  try
    M.MsgType:=Hdr.MsgType;
    if Hdr.MsgLen>0 then
      M.Stream.CopyFrom(AStream,Hdr.MsgLen);
  except
    M.Free;
    Raise;
  end;
  PushMessage(M);
end;

procedure TIPCServerComm.PushMessage(Msg: TIPCServerMsg);
begin
  FOwner.PushMessage(Msg);
end;

{ ---------------------------------------------------------------------
    TIPCClientComm
  ---------------------------------------------------------------------}
  
constructor TIPCClientComm.Create(AOwner: TSimpleIPCClient);
begin
  FOwner:=AOwner;
end;

Procedure TIPCClientComm.DoError(const Msg : String; const Args : Array of const);

begin
  FOwner.DoError(Msg,Args);
end;  

{ ---------------------------------------------------------------------
    TSimpleIPC
  ---------------------------------------------------------------------}

Procedure TSimpleIPC.DoError(const Msg: String; const Args: array of const);
var
  FullMsg: String;
begin
  if Length(Name) > 0
    then FullMsg := Name + ': '
    else FullMsg := '';
  FullMsg := FullMsg + Format(Msg, Args);
  raise EIPCError.Create(FullMsg);
end;

procedure TSimpleIPC.CheckInactive;
begin
  if not (csLoading in ComponentState) then
    If Active then
      DoError(SErrActive,[]);
end;

procedure TSimpleIPC.CheckActive;
begin
  if not (csLoading in ComponentState) then
    If Not Active then
      DoError(SErrInActive,[]);
end;

procedure TSimpleIPC.SetActive(const AValue: Boolean);
begin
  if (FActive<>AValue) then
    begin
    if ([]<>([csLoading,csDesigning]*ComponentState)) then
      FActive:=AValue
    else  
      If AValue then
        Activate
      else
        Deactivate;
    end;
end;

procedure TSimpleIPC.SetServerID(const AValue: String);
begin
  if (FServerID<>AValue) then
    begin
    CheckInactive;
    FServerID:=AValue
    end;
end;

Procedure TSimpleIPC.Loaded; 

Var
  B : Boolean;

begin
  Inherited;
  B:=FActive;
  if B then
    begin
    Factive:=False;
    Activate;
    end;
end;

{ ---------------------------------------------------------------------
    TSimpleIPCServer
  ---------------------------------------------------------------------}

constructor TSimpleIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlobal:=False;
  FActive:=False;
  FBusy:=False;
  FMsgData:=TStringStream.Create('');
  FQueue:=TIPCServerMsgQueue.Create;
end;

destructor TSimpleIPCServer.Destroy;
begin
  FreeAndNil(FQueue);
  Active:=False;
  FreeAndNil(FMsgData);
  inherited Destroy;
end;

procedure TSimpleIPCServer.SetGlobal(const AValue: Boolean);
begin
  if (FGlobal<>AValue) then
    begin
    CheckInactive;
    FGlobal:=AValue;
    end;
end;

procedure TSimpleIPCServer.SetMaxAction(AValue: TIPCMessageOverflowAction);
begin
  FQueue.MaxAction:=AValue;
end;

function TSimpleIPCServer.GetInstanceID: String;
begin
  Result:=FIPCComm.InstanceID;
end;

function TSimpleIPCServer.GetMaxAction: TIPCMessageOverflowAction;
begin
  Result:=FQueue.MaxAction;
end;


function TSimpleIPCServer.GetStringMessage: String;
begin
  Result:=TStringStream(FMsgData).DataString;
end;


procedure TSimpleIPCServer.StartServer;
begin
  if Not Assigned(FIPCComm) then
    begin
    If (FServerID='') then
      FServerID:=ApplicationName;
    FIPCComm:=CommClass.Create(Self);
    FIPCComm.StartServer;
    end;
  FActive:=True;
end;

procedure TSimpleIPCServer.StopServer;
begin
  If Assigned(FIPCComm) then
    begin
    FIPCComm.StopServer;
    FreeAndNil(FIPCComm);
    end;
  FQueue.Clear;
  FActive:=False;
end;

// TimeOut values:
//   >  0  -- umber of milliseconds to wait
//   =  0  -- return immediately
//   = -1  -- wait infinitely
//   < -1  -- wait infinitely (force to -1)
function TSimpleIPCServer.PeekMessage(TimeOut: Integer; DoReadMessage: Boolean): Boolean;
begin
  CheckActive;
  Result:=Queue.Count>0;
  If Not Result then
    begin
    if TimeOut < -1 then
      TimeOut := -1;
    FBusy:=True;
    Try
      Result:=FIPCComm.PeekMessage(Timeout);
    Finally
      FBusy:=False;
    end;
    end;
  If Result then
    If DoReadMessage then
      Readmessage;
end;

function TSimpleIPCServer.PopMessage: Boolean;

var
  MsgItem: TIPCServerMsg;

begin
  MsgItem:=FQueue.Pop;
  Result:=Assigned(MsgItem);
  if Result then
    try
      FMsgType := MsgItem.MsgType;
      MsgItem.Stream.Position := 0;
      FMsgData.Size := 0;
      FMsgData.CopyFrom(MsgItem.Stream, MsgItem.Stream.Size);
    finally
      MsgItem.Free;
    end;
end;

procedure TSimpleIPCServer.ReadMessage;

begin
  CheckActive;
  FBusy:=True;
  Try
    if (FQueue.Count=0) then
      // Readmessage pushes a message to the queue
      FIPCComm.ReadMessage;
    if PopMessage then
      If Assigned(FOnMessage) then
        FOnMessage(Self);
  Finally
    FBusy:=False;
  end;
end;

procedure TSimpleIPCServer.GetMessageData(Stream: TStream);
begin
  Stream.CopyFrom(FMsgData,0);
end;

procedure TSimpleIPCServer.Activate;
begin
  StartServer;
end;

procedure TSimpleIPCServer.Deactivate;
begin
  StopServer;
end;




{ ---------------------------------------------------------------------
    TSimpleIPCClient
  ---------------------------------------------------------------------}

procedure TSimpleIPCClient.SetServerInstance(const AValue: String);
begin
  CheckInactive;
  FServerInstance:=AVAlue;
end;

procedure TSimpleIPCClient.Activate;
begin
  Connect;
end;

procedure TSimpleIPCClient.Deactivate;
begin
  DisConnect;
end;
constructor TSimpleIPCClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSimpleIPCClient.destroy;
begin
  Active:=False;
  Inherited;
end;

procedure TSimpleIPCClient.Connect;
begin
  If Not assigned(FIPCComm) then
    begin
    FIPCComm:=CommClass.Create(Self);
    Try
      FIPCComm.Connect;
    Except
      FreeAndNil(FIPCComm);
      Raise;
    end;  
    FActive:=True;
    end;
end;

procedure TSimpleIPCClient.Disconnect;
begin
  If Assigned(FIPCComm) then
    Try
      FIPCComm.DisConnect;
    Finally
      FActive:=False;
      FreeAndNil(FIPCComm);
    end;  
end;

function TSimpleIPCClient.ServerRunning: Boolean;

begin
  If Assigned(FIPCComm) then
    Result:=FIPCComm.ServerRunning
  else
    With CommClass.Create(Self) do
      Try
        Result:=ServerRunning;
      finally
        Free;
      end;
end;

procedure TSimpleIPCClient.SendMessage(MsgType : TMessageType; Stream: TStream);

begin
  CheckActive;
  FBusy:=True;
  Try
    FIPCComm.SendMessage(MsgType,Stream);
  Finally
    FBusy:=False;
  end;
end;

procedure TSimpleIPCClient.SendStringMessage(const Msg: String);
begin
  SendStringMessage(mtString,Msg);
end;

procedure TSimpleIPCClient.SendStringMessage(MsgType: TMessageType; const Msg: String
  );
Var
  S : TStringStream;
begin
  S:=TStringStream.Create(Msg);
  try
    SendMessage(MsgType,S);
  finally
    S.free;
  end;
end;

procedure TSimpleIPCClient.SendStringMessageFmt(const Msg: String;
  Args: array of const);
begin
  SendStringMessageFmt(mtString,Msg,Args);
end;

procedure TSimpleIPCClient.SendStringMessageFmt(MsgType: TMessageType;
  const Msg: String; Args: array of const);
begin
  SendStringMessage(MsgType, Format(Msg,Args));
end;

{$IFDEF OSNEEDIPCINITDONE}
initialization
  IPCInit;
finalization
  IPCDone;
{$ENDIF}  
end.

