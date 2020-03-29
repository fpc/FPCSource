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
  Contnrs, SyncObjs, Classes, SysUtils;

const
  MsgVersion = 1;

  { IPC message types }
  mtUnknown = 0;
  mtString = 1;

type
  TIPCMessageOverflowAction = (ipcmoaNone, ipcmoaDiscardOld, ipcmoaDiscardNew, ipcmoaError);

  TMessageType = LongInt;

  TMsgHeader = Packed record
    Version : Byte;
    MsgType : TMessageType;
    MsgLen  : Integer;
  end;

  TSimpleIPCServer = class;
  TSimpleIPCClient = class;

  { TIPCServerMsg }
  TIPCServerMsg = class
  private type
    TStreamClass = class of TStream;
  private const
    // TMemoryStream uses an effecient grow algorithm.
    DefaultStreamClass: TStreamClass = TMemoryStream;
  strict private
    FStream: TStream;
    FOwnsStream: Boolean;
    FMsgType: TMessageType;
    function GetStringMessage: String;
  public
    constructor Create;
    constructor Create(AStream: TStream; AOwnsStream: Boolean = True);
    destructor Destroy; override;
    property Stream: TStream read FStream;
    property MsgType: TMessageType read FMsgType write FMsgType;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
    property StringMessage: String read GetStringMessage;
  end;

  { TIPCServerMsgQueue }
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
    // Check for new messages, may read and push messages to the queue.
    Function PeekMessage(Timeout: Integer): Boolean; virtual; Abstract;
    // Read and push new message to the queue, if not done by PeekMessage.
    Procedure ReadMessage; virtual; Abstract;
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
    procedure PrepareServerID;
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

  TMessageQueueEvent = Procedure(Sender: TObject; Msg: TIPCServerMsg) of object;

  { TSimpleIPCServer }
  TSimpleIPCServer = Class(TSimpleIPC)
  private const
    DefaultThreaded = False;
    DefaultThreadTimeout = 50;
    DefaultSynchronizeEvents = True;
    DefaultMaxAction = ipcmoaNone;
    DefaultMaxQueue = 0;
  private
    FOnMessageError: TMessageQueueEvent;
    FOnMessageQueued: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FOnThreadError: TNotifyEvent;
    FQueue: TIPCServerMsgQueue;
    FQueueLock: TCriticalSection;
    FQueueAddEvent: TSimpleEvent;
    FGlobal: Boolean;
    // Access to the message is not locked by design!
    // In the threaded mode, it must be accessed only during event callbacks.
    FMessage: TIPCServerMsg;
    FTempMessage: TIPCServerMsg;
    FThreaded: Boolean;
    FThreadTimeout: Integer;
    FThreadError: String;
    FThreadExecuting: Boolean;
    FThreadReadyEvent: TSimpleEvent;
    FThread: TThread;
    FSynchronizeEvents: Boolean;
    procedure DoOnMessage;
    procedure DoOnMessageQueued;
    procedure DoOnMessageError(Msg: TIPCServerMsg);
    procedure DoOnThreadError;
    procedure InternalDoOnMessage;
    procedure InternalDoOnMessageQueued;
    procedure InternalDoOnMessageError;
    procedure InternalDoOnThreadError;
    function GetInstanceID: String;
    function GetMaxAction: TIPCMessageOverflowAction;
    function GetMaxQueue: Integer;
    function GetStringMessage: String;
    procedure SetGlobal(const AValue: Boolean);
    procedure SetMaxAction(AValue: TIPCMessageOverflowAction);
    procedure SetMaxQueue(AValue: Integer);
    procedure SetThreaded(AValue: Boolean);
    procedure SetThreadTimeout(AValue: Integer);
    procedure SetSynchronizeEvents(AValue: Boolean);
    function WaitForReady(Timeout: Integer = -1): Boolean;
    function GetMsgType: TMessageType;
    function GetMsgData: TStream;
  protected
    FIPCComm: TIPCServerComm;
    Function CommClass : TIPCServerCommClass; virtual;
    Procedure PushMessage(Msg : TIPCServerMsg); virtual;
    function PopMessage: Boolean; virtual;
    procedure StartComm; virtual;
    procedure StopComm; virtual;
    function StartThread: Boolean; virtual;
    procedure StopThread; virtual;
    Procedure Activate; override;
    Procedure Deactivate; override;
    function ProcessMessage(Timeout: Integer): Boolean;
    Property Queue : TIPCServerMsgQueue Read FQueue;
    Property Thread : TThread Read FThread;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure StartServer;
    Procedure StartServer(AThreaded: Boolean);
    Procedure StopServer;
    Function PeekMessage(Timeout: Integer; DoReadMessage: Boolean): Boolean;
    Function ReadMessage: Boolean;
    Property  StringMessage : String Read GetStringMessage;
    Procedure GetMessageData(Stream : TStream);
    Property  Message: TIPCServerMsg read FMessage;
    Property  MsgType: TMessageType Read GetMsgType;
    Property  MsgData: TStream Read GetMsgData;
    Property  InstanceID : String Read GetInstanceID;
    property  ThreadExecuting: Boolean read FThreadExecuting;
    property  ThreadError: String read FThreadError;
  Published
    Property Global : Boolean Read FGlobal Write SetGlobal;
    // Called during ReadMessage
    Property OnMessage : TNotifyEvent Read FOnMessage Write FOnMessage;
    // Called when a message is pushed on the queue.
    Property OnMessageQueued : TNotifyEvent Read FOnMessageQueued Write FOnMessageQueued;
    // Called when the queue overflows and  MaxAction = ipcmoaError.
    Property OnMessageError : TMessageQueueEvent Read FOnMessageError Write FOnMessageError;
    // Called when the server thread catches an exception.
    property OnThreadError: TNotifyEvent read FOnThreadError write FOnThreadError;
    // Maximum number of messages to keep in the queue
    property MaxQueue: Integer read GetMaxQueue write SetMaxQueue default DefaultMaxQueue;
    // What to do when the queue overflows
    property MaxAction: TIPCMessageOverflowAction read GetMaxAction write SetMaxAction default DefaultMaxAction;
    // Instruct IPC server to operate in a threaded mode.
    property Threaded: Boolean read FThreaded write SetThreaded;
    // Amount of time thread waits for a message before checking for termination.
    property ThreadTimeout: Integer read FThreadTimeout write SetThreadTimeout default DefaultThreadTimeout;
    // Synchronize events with the main thread when in threaded mode.
    property SynchronizeEvents: Boolean read FSynchronizeEvents write SetSynchronizeEvents default DefaultSynchronizeEvents;
  end;

  { TIPCClientComm }
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

var
  DefaultIPCServerClass : TIPCServerCommClass = Nil;
  DefaultIPCClientClass : TIPCClientCommClass = Nil;

var
  DefaultIPCMessageOverflowAction: TIPCMessageOverflowAction = TSimpleIPCServer.DefaultMaxAction;
  DefaultIPCMessageQueueLimit: Integer = TSimpleIPCServer.DefaultMaxQueue;

resourcestring
  SErrServerNotActive = 'Server with ID %s is not active.';
  SErrActive = 'This operation is illegal when the server is active.';
  SErrInActive = 'This operation is illegal when the server is inactive.';
  SErrThreadContext = 'This operation is illegal outside of IPC thread context.';
  SErrThreadFailure = 'IPC thread failure.';
  SErrMessageQueueOverflow = 'Message queue overflow (limit %s)';


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

// Convert content of any stream type to a string.
function FastStreamToString(Stream: TStream): String;
var
  CharCount, CharSize: Integer;
  StringStream: TStringStream;
  OldPosition: Int64;
begin
  // Optimized for TStringStream
  if Stream is TStringStream then
  begin
    Result := TStringStream(Stream).DataString;
  end
  // Optimized for TCustomMemoryStream
  else if Stream is TCustomMemoryStream then
  begin
    Result := '';
    CharSize := StringElementSize(Result);
    CharCount := Stream.Size div CharSize;
    SetLength(Result, CharCount);
    Move(TCustomMemoryStream(Stream).Memory^, Result[1], CharCount * CharSize);
  end
  // Any other stream type
  else
  begin
    OldPosition := Stream.Position;
    try
      StringStream := TStringStream.Create('');
      try
        Stream.Position := 0;
        StringStream.CopyFrom(Stream, Stream.Size);
        Result := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      Stream.Position := OldPosition;
    end;
  end;
end;

// Timeout values:
//   >  0  -- Number of milliseconds to wait
//   =  0  -- return immediately
//   = -1  -- wait infinitely (converted to INFINITE)
//   < -1  -- wait infinitely (converted to INFINITE)
function IPCTimeoutToEventTimeout(Timeout: Integer): Cardinal; inline;
begin
  if Timeout >= 0 then
    Result := Timeout
  else
    Result := SyncObjs.INFINITE;
end;

// Timeout values:
//   >  0  -- Number of milliseconds to wait
//   =  0  -- return immediately
//   = -1  -- wait infinitely
//   < -1  -- wait infinitely (force to -1)
function IPCTimeoutSanitized(Timeout: Integer): Integer; inline;
begin
  if Timeout >= 0 then
    Result := Timeout
  else
    Result := -1;
end;

{$REGION 'TIPCServerMsg'}

constructor TIPCServerMsg.Create;
begin
  FMsgType := mtUnknown;
  FStream := Self.DefaultStreamClass.Create;
  FOwnsStream := True;
end;

constructor TIPCServerMsg.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  FMsgType := mtUnknown;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TIPCServerMsg.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
end;

function TIPCServerMsg.GetStringMessage: String;
begin
  Result := FastStreamToString(FStream);
end;

{$ENDREGION}

{$REGION 'TIPCServerMsgQueue'}

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
  Inherited;
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
  // PrepareToPush may throw an exception, e.g. if message queue is full.
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

{$ENDREGION}

{$REGION 'TIPCServerComm'}

constructor TIPCServerComm.Create(AOwner: TSimpleIPCServer);
begin
  FOwner:=AOWner;
end;

procedure TIPCServerComm.DoError(const Msg: String; const Args: array of const);
begin
  FOwner.DoError(Msg,Args);
end;

procedure TIPCServerComm.PushMessage(const Hdr: TMsgHeader; AStream: TStream);
var
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

{$ENDREGION}

{$REGION 'TIPCClientComm'}
  
constructor TIPCClientComm.Create(AOwner: TSimpleIPCClient);
begin
  FOwner:=AOwner;
end;

Procedure TIPCClientComm.DoError(const Msg : String; const Args : Array of const);

begin
  FOwner.DoError(Msg,Args);
end;  

{$ENDREGION}

{$REGION 'TSimpleIPC'}

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
    FServerID:=AValue;
  end;
end;

procedure TSimpleIPC.PrepareServerID;
begin
  if FServerID = '' then
    FServerID := ApplicationName;
  // Extra precaution for thread-safety
  UniqueString(FServerID);
end;

procedure TSimpleIPC.Loaded;
var
  B : Boolean;
begin
  inherited;
  B:=FActive;
  if B then
  begin
    FActive:=False;
    Activate;
  end;
end;

{$ENDREGION}

{$REGION 'TIPCServerThread'}

type
  TIPCServerThread = class(TThread)
  private
    FServer: TSimpleIPCServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TSimpleIPCServer);
    property Server: TSimpleIPCServer read FServer;
  end;

constructor TIPCServerThread.Create(AServer: TSimpleIPCServer);
begin
  inherited Create(True); // CreateSuspended = True
  FServer := AServer;
end;

procedure TIPCServerThread.Execute;
begin
  FServer.FThreadExecuting := True;
  try
    FServer.StartComm;
    try
      // Notify server that thread has started.
      FServer.FThreadReadyEvent.SetEvent;
      // Run message loop
      while not Terminated do
        FServer.ProcessMessage(FServer.ThreadTimeout);
    finally
      FServer.StopComm;
    end;
  except on E: Exception do
    begin
      FServer.FThreadExecuting := False;
      FServer.FThreadError := E.Message;
      // Trigger event to wake up the caller from potentially indefinite wait.
      FServer.FThreadReadyEvent.SetEvent;
      FServer.DoOnThreadError;
    end;
  end;
  FServer.FThreadExecuting := False;
end;

{$ENDREGION}

{$REGION 'TSimpleIPCServer'}

constructor TSimpleIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlobal:=False;
  FActive:=False;
  FBusy:=False;
  FMessage:=nil;
  FQueue:=TIPCServerMsgQueue.Create;
  FThreaded:=DefaultThreaded;
  FThreadTimeout:=DefaultThreadTimeout;
  FSynchronizeEvents:=DefaultSynchronizeEvents;
end;

destructor TSimpleIPCServer.Destroy;
begin
  Active:=False;
  FreeAndNil(FQueue);
  if Assigned(FMessage) then
    FreeAndNil(FMessage);
  inherited Destroy;
end;

procedure TSimpleIPCServer.SetGlobal(const AValue: Boolean);
begin
  CheckInactive;
  FGlobal:=AValue;
end;

procedure TSimpleIPCServer.SetThreaded(AValue: Boolean);
begin
  CheckInactive;
  FThreaded:=AValue;
end;

procedure TSimpleIPCServer.SetThreadTimeout(AValue: Integer);
begin
  CheckInactive;
  FThreadTimeout:=AValue;
end;

procedure TSimpleIPCServer.SetSynchronizeEvents(AValue: Boolean);
begin
  CheckInactive;
  FSynchronizeEvents:=AValue;
end;

procedure TSimpleIPCServer.SetMaxAction(AValue: TIPCMessageOverflowAction);
begin
  CheckInactive;
  FQueue.MaxAction:=AValue;
end;

procedure TSimpleIPCServer.SetMaxQueue(AValue: Integer);
begin
  CheckInactive;
  FQueue.MaxCount:=AValue;
end;

function TSimpleIPCServer.GetInstanceID: String;
begin
  Result:=FIPCComm.InstanceID;
end;

function TSimpleIPCServer.GetMaxAction: TIPCMessageOverflowAction;
begin
  Result:=FQueue.MaxAction;
end;

function TSimpleIPCServer.GetMaxQueue: Integer;
begin
  Result:=FQueue.MaxCount;
end;

procedure TSimpleIPCServer.StartComm;
begin
  if Assigned(FIPCComm) then
    FreeAndNil(FIPCComm);
  FIPCComm := CommClass.Create(Self);
  FIPCComm.StartServer;
end;

procedure TSimpleIPCServer.StopComm;
begin
  if Assigned(FIPCComm) then
  begin
    FIPCComm.StopServer;
    FreeAndNil(FIPCComm);
  end;
end;

function TSimpleIPCServer.StartThread: Boolean;
begin
  FThreadError := '';
  FQueueLock := SyncObjs.TCriticalSection.Create;
  FQueueAddEvent := SyncObjs.TSimpleEvent.Create;
  FThreadReadyEvent := SyncObjs.TSimpleEvent.Create;
  FThread := TIPCServerThread.Create(Self);
  FThread.Start;
  Result := WaitForReady;
end;

procedure TSimpleIPCServer.StopThread;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  if Assigned(FThreadReadyEvent) then
    FreeAndNil(FThreadReadyEvent);
  if Assigned(FQueueAddEvent) then
    FreeAndNil(FQueueAddEvent);
  if Assigned(FQueueLock) then
    FreeAndNil(FQueueLock);
end;

function TSimpleIPCServer.WaitForReady(Timeout: Integer = -1): Boolean;
begin
  if FThreadReadyEvent.WaitFor(IPCTimeoutToEventTimeout(Timeout)) = wrSignaled then
    Result := FThreadExecuting
  else
    Result := False;
end;

procedure TSimpleIPCServer.StartServer;
begin
  StartServer(FThreaded);
end;

procedure TSimpleIPCServer.StartServer(AThreaded: Boolean);
begin
  CheckInactive;
  FActive := True;
  try
    PrepareServerID;
    FThreaded := AThreaded;
    if FThreaded then
    begin
      if not StartThread then
        raise EIPCError.Create(SErrThreadFailure);
    end
    else
      StartComm;
  except
    FActive := False;
    raise;
  end;
end;

procedure TSimpleIPCServer.StopServer;
begin
  StopThread;
  StopComm;
  FQueue.Clear;
  FActive := False;
end;

function TSimpleIPCServer.ProcessMessage(Timeout: Integer): Boolean;
begin
  FBusy := True;
  try
    // Check for new messages (may push several messages to the queue)
    Result := FIPCComm.PeekMessage(IPCTimeoutSanitized(Timeout));
    // Push new message to the queue (explicitly)
    if Result then
      FIPCComm.ReadMessage;
  finally
    FBusy := False;
  end;
end;

// Timeout values:
//   >  0  -- Number of milliseconds to wait
//   =  0  -- return immediately
//   = -1  -- wait infinitely
//   < -1  -- wait infinitely (force to -1)
function TSimpleIPCServer.PeekMessage(Timeout: Integer; DoReadMessage: Boolean): Boolean;
begin
  CheckActive;

  if Threaded then
  begin
    // Check if have messages in the queue
    FQueueLock.Acquire;
    try
      Result:=FQueue.Count>0;
      // Reset queue add event
      if not Result then
        FQueueAddEvent.ResetEvent;
    finally
      FQueueLock.Release;
    end;
    // Wait for queue add event
    if not Result and (Timeout <> 0) then
      Result := FQueueAddEvent.WaitFor(IPCTimeoutToEventTimeout(Timeout)) = wrSignaled;
  end
  else
  begin
    // Check if have messages in the queue
    Result:=FQueue.Count>0;
    // If queue is empty, process new messages via IPC driver
    if not Result then
      Result := ProcessMessage(Timeout);
  end;

  // Read message if available (be aware of a race condition in threaded mode)
  If Result then
    If DoReadMessage then
      ReadMessage;
end;

function TSimpleIPCServer.ReadMessage: Boolean;
begin
  // Pop a message from the queue
  Result := PopMessage;
  if Result then
    DoOnMessage;
end;

function TSimpleIPCServer.PopMessage: Boolean;
begin
  if Threaded then
    FQueueLock.Acquire;
  try
    if Assigned(FMessage) then
      FreeAndNil(FMessage);
    FMessage := FQueue.Pop;
    Result := Assigned(FMessage);
  finally
    if Threaded then
      FQueueLock.Release;
  end;
end;

procedure TSimpleIPCServer.PushMessage(Msg: TIPCServerMsg);
var
  PushFailed: Boolean;
begin
  if Threaded then
    FQueueLock.Acquire;
  try
    PushFailed := False;
    try
      // Queue.Push may throw an exception, e.g. if message queue is full.
      FQueue.Push(Msg);
    except
      PushFailed := True;
    end;
    // Notify a waiting PeekMessage in threaded mode
    if Threaded and not PushFailed then
      FQueueAddEvent.SetEvent;
  finally
    if Threaded then
      FQueueLock.Release;
  end;

  if PushFailed then
    // Handler must free the Msg, because it is not owned by anybody.
    DoOnMessageError(Msg)
  else
    DoOnMessageQueued;
end;

function TSimpleIPCServer.GetMsgType: TMessageType;
begin
  // Access to the message is not locked by design!
  if Assigned(FMessage) then
    Result := FMessage.MsgType
  else
    Result := mtUnknown;
end;

function TSimpleIPCServer.GetMsgData: TStream;
begin
  // Access to the message is not locked by design!
  if Assigned(FMessage) then
    Result := FMessage.Stream
  else
    Result := nil;
end;

procedure TSimpleIPCServer.GetMessageData(Stream: TStream);
begin
  // Access to the message is not locked by design!
  if Assigned(FMessage) then
    Stream.CopyFrom(FMessage.Stream, 0);
end;

function TSimpleIPCServer.GetStringMessage: String;
begin
  // Access to the message is not locked by design!
  if Assigned(FMessage) then
    Result := FMessage.StringMessage
  else
    Result := '';
end;

procedure TSimpleIPCServer.Activate;
begin
  StartServer;
end;

procedure TSimpleIPCServer.Deactivate;
begin
  StopServer;
end;

procedure TSimpleIPCServer.DoOnMessage;
begin
  if Assigned(FOnMessage) then
  begin
    if FSynchronizeEvents and Assigned(FThread) then
      TThread.Synchronize(FThread, @InternalDoOnMessage)
    else
      InternalDoOnMessage;
  end;
end;

procedure TSimpleIPCServer.InternalDoOnMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self);
end;

procedure TSimpleIPCServer.DoOnMessageQueued;
begin
  if Assigned(FOnMessageQueued) then
  begin
    if FSynchronizeEvents and Assigned(FThread) then
      TThread.Synchronize(FThread, @InternalDoOnMessageQueued)
    else
      InternalDoOnMessageQueued;
  end;
end;

procedure TSimpleIPCServer.InternalDoOnMessageQueued;
begin
  if Assigned(FOnMessageQueued) then
    FOnMessageQueued(Self);
end;

procedure TSimpleIPCServer.DoOnMessageError(Msg: TIPCServerMsg);
begin
  try
    if Assigned(FOnMessageError) then
    begin
      // Temp message (class instance variable) is used to pass
      // a parameter to a synchronized thread method.
      FTempMessage := Msg;
      if FSynchronizeEvents and Assigned(FThread) then
        TThread.Synchronize(FThread, @InternalDoOnMessageError)
      else
        InternalDoOnMessageError;
    end;
  finally
    // Must free the message because it is not owned by anybody.
    FTempMessage := nil;
    FreeAndNil(Msg);
  end;
end;

procedure TSimpleIPCServer.InternalDoOnMessageError;
begin
  if Assigned(FOnMessageError) then
    FOnMessageError(Self, FTempMessage);
end;

procedure TSimpleIPCServer.DoOnThreadError;
begin
  if Assigned(FOnThreadError) then
  begin
    if FSynchronizeEvents and Assigned(FThread) then
      TThread.Synchronize(FThread, @InternalDoOnThreadError)
    else
      InternalDoOnThreadError;
  end;
end;

procedure TSimpleIPCServer.InternalDoOnThreadError;
begin
  if Assigned(FOnThreadError) then
    FOnThreadError(Self);
end;

{$ENDREGION}

{$REGION 'TSimpleIPCClient'}

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

destructor TSimpleIPCClient.Destroy;
begin
  Active:=False;
  Inherited;
end;

procedure TSimpleIPCClient.Connect;
begin
  If Not assigned(FIPCComm) then
  begin
    PrepareServerID;
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
var
  TempComm: TIPCClientComm;
begin
  If Assigned(FIPCComm) then
    Result:=FIPCComm.ServerRunning
  else
  begin
    PrepareServerID;
    TempComm := CommClass.Create(Self);
    Try
      Result := TempComm.ServerRunning;
    finally
      TempComm.Free;
    end;
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

procedure TSimpleIPCClient.SendStringMessage(MsgType: TMessageType; const Msg: String);
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

{$ENDREGION}

{$IFDEF OSNEEDIPCINITDONE}
initialization
  IPCInit;
finalization
  IPCDone;
{$ENDIF}

end.

