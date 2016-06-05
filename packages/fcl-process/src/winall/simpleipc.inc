{
    This file is part of the Free Component library.
    Copyright (c) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team

    Windows implementation of one-way IPC between 2 processes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses Windows,messages;

const
  MsgWndClassName: WideString = 'FPCMsgWindowCls';

resourcestring
  SErrFailedToRegisterWindowClass = 'Failed to register message window class';
  SErrFailedToCreateWindow = 'Failed to create message window %s';

var
  MsgWindowClass: TWndClassW = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: nil);

type
  TWinMsgServerComm = Class(TIPCServerComm)
  strict private
    FHWND : HWND;
    FWindowName : String;
    FWndProcException: Boolean;
    FWndProcExceptionMsg: String;
    function AllocateHWnd(const aWindowName: WideString) : HWND;
    procedure ProcessMessages;
    procedure ProcessMessagesWait(TimeOut: Integer);
    procedure HandlePostedMessage(const Msg: TMsg); inline;
    function HaveQueuedMessages: Boolean; inline;
    function CountQueuedMessages: Integer; inline;
    procedure CheckWndProcException; inline;
  private
    procedure ReadMsgData(var Msg: TMsg);
    function TryReadMsgData(var Msg: TMsg; out Error: String): Boolean;
    procedure SetWndProcException(const ErrorMsg: String); inline;
  public
    constructor Create(AOwner : TSimpleIPCServer); override;
    destructor Destroy; override;
    Procedure StartServer; override;
    Procedure StopServer; override;
    Function  PeekMessage(TimeOut : Integer) : Boolean; override;
    Procedure ReadMessage ; override;
    Function GetInstanceID : String;override;
    Property WindowName : String Read FWindowName;
  end;

{ ---------------------------------------------------------------------
    MsgWndProc
  ---------------------------------------------------------------------}

function MsgWndProc(Window: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; {$ifdef wince}cdecl;{$else}stdcall;{$endif}
Var
  Server: TWinMsgServerComm;
  Msg: TMsg;
  MsgError: String;
begin
  Result:=0;
  if (uMsg=WM_COPYDATA) then
  begin
    // Post WM_USER to wake up GetMessage call.
    PostMessage(Window, WM_USER, 0, 0);
    // Read message data and add to message queue.
    Server:=TWinMsgServerComm({$ifdef wince}GetWindowLong{$else}GetWindowLongPtr{$endif}(Window,GWL_USERDATA));
    if Assigned(Server) then
    begin
      Msg.Message:=uMsg;
      Msg.wParam:=wParam;
      Msg.lParam:=lParam;
      // Exceptions thrown inside WindowProc may not propagate back
      // to the caller in some circumstances (according to MSDN),
      // so capture it and raise it outside of WindowProc!
      if Server.TryReadMsgData(Msg, MsgError) then
        Result:=1 // True
      else
      begin
        Result:=0; // False
        Server.SetWndProcException(MsgError);
      end;
    end;
  end
  else
  begin
    Result:=DefWindowProcW(Window,uMsg,wParam,lParam);
  end;
end;

{ ---------------------------------------------------------------------
    TWinMsgServerComm
  ---------------------------------------------------------------------}

function TWinMsgServerComm.AllocateHWnd(const aWindowName: WideString): HWND;
var
  cls: TWndClassW;
  isreg : Boolean;
begin
  MsgWindowClass.lpfnWndProc:=@MsgWndProc;
  MsgWindowClass.hInstance := HInstance;
  MsgWindowClass.lpszClassName:=PWideChar(MsgWndClassName);
  isreg:=GetClassInfoW(HInstance,PWideChar(MsgWndClassName),@cls);
  if not isreg then
    if (Windows.RegisterClassW(MsgWindowClass)=0) then
      Owner.DoError(SErrFailedToRegisterWindowClass,[]);
  Result:=CreateWindowExW(WS_EX_TOOLWINDOW, PWideChar(MsgWndClassName),
    PWideChar(aWindowName), WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if (Result=0) then
    Owner.DoError(SErrFailedToCreateWindow,[aWindowName]);
  {$ifdef wince}SetWindowLong{$else}SetWindowLongPtr{$endif}(Result,GWL_USERDATA,PtrInt(Self));
end;

constructor TWinMsgServerComm.Create(AOwner: TSimpleIPCServer);
begin
  inherited Create(AOwner);
  FWindowName := Owner.ServerID;
  If not Owner.Global then
    FWindowName := FWindowName+'_'+InstanceID;
  FWndProcException := False;
  FWndProcExceptionMsg := '';
end;

destructor TWinMsgServerComm.Destroy;
begin
  StopServer;
  inherited;
end;

procedure TWinMsgServerComm.StartServer;
begin
  StopServer;
  FHWND := AllocateHWND(WideString(FWindowName));
end;

procedure TWinMsgServerComm.StopServer;
begin
  if FHWND <> 0 then
  begin
    DestroyWindow(FHWND);
    FHWND := 0;
  end;
end;

procedure TWinMsgServerComm.SetWndProcException(const ErrorMsg: String); inline;
begin
  FWndProcException := True;
  FWndProcExceptionMsg := ErrorMsg;
end;

procedure TWinMsgServerComm.CheckWndProcException; inline;
var
  Msg: String;
begin
  if FWndProcException then
  begin
    Msg := FWndProcExceptionMsg;
    FWndProcException := False;
    FWndProcExceptionMsg := '';
    Owner.DoError(Msg, []);
  end;
end;

function TWinMsgServerComm.HaveQueuedMessages: Boolean; inline;
begin
  Result := (Owner.Queue.Count > 0);
end;

function TWinMsgServerComm.CountQueuedMessages: Integer; inline;
begin
  Result := Owner.Queue.Count;
end;

procedure TWinMsgServerComm.HandlePostedMessage(const Msg: TMsg); inline;
begin
  if Msg.message <> WM_USER then
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end
end;

procedure TWinMsgServerComm.ProcessMessages;
var
  Msg: TMsg;
begin
  // Windows.PeekMessage dispatches incoming sent messages by directly
  // calling associated WindowProc, and then checks the thread message queue
  // for posted messages and retrieves a message if any available.
  // Note: WM_COPYDATA is a sent message, not posted, so it will be processed
  // directly via WindowProc inside of Windows.PeekMessage call.
  while Windows.PeekMessage(Msg, FHWND, 0, 0, PM_REMOVE) do
  begin
    // Empty the message queue by processing all posted messages.
    HandlePostedMessage(Msg);
  end;
end;

procedure TWinMsgServerComm.ProcessMessagesWait(TimeOut: Integer);
var
  Msg: TMsg;
  TimerID: UINT_PTR;
  GetMessageResult: BOOL;
begin
  // Not allowed to wait.
  if TimeOut = 0 then
    Exit;

  // Setup a timer to post WM_TIMER to wake up GetMessage call.
  if TimeOut > 0 then
    TimerID := SetTimer(FHWND, 0, TimeOut, nil)
  else
    TimerID := 0;

  // Wait until a message arrives.
  try
    // We either need to wait infinitely or we have a timer.
    if (TimeOut < 0) or (TimerID <> 0) then
    begin
      // Windows.GetMessage dispatches incoming sent messages until a posted
      // message is available for retrieval. Note: WM_COPYDATA will not actually
      // wake up Windows.GetMessage, so we must post a dummy message when
      // we receive WM_COPYDATA inside of WindowProc.
      GetMessageResult := Windows.GetMessage(Msg, FHWND, 0, 0);
      case LongInt(GetMessageResult) of
        -1, 0: ;
        else HandlePostedMessage(Msg);
      end;
    end;
  finally
    // Destroy timer.
    if TimerID <> 0 then
      KillTimer(FHWND, TimerID);
  end;
end;

function TWinMsgServerComm.PeekMessage(TimeOut: Integer): Boolean;
begin
  // Process incoming messages.
  ProcessMessages;

  // Do we have queued messages?
  Result := HaveQueuedMessages;

  // Wait for incoming messages.
  if (not Result) and (TimeOut <> 0) then
  begin
    ProcessMessagesWait(TimeOut);
    Result := HaveQueuedMessages;
  end;

  // Check for exception raised inside WindowProc.
  CheckWndProcException;
end;

procedure TWinMsgServerComm.ReadMsgData(var Msg: TMsg);
var
  CDS: PCopyDataStruct;
  MsgItem: TIPCServerMsg;

begin
  CDS := PCopyDataStruct(Msg.lParam);
  MsgItem := TIPCServerMsg.Create;
  try
    MsgItem.MsgType := CDS^.dwData;
    MsgItem.Stream.WriteBuffer(CDS^.lpData^,CDS^.cbData);
  except
    FreeAndNil(MsgItem);
    // Caller is expected to catch this exception, so not using Owner.DoError()
    raise;
  end;
  PushMessage(MsgItem);
end;

function TWinMsgServerComm.TryReadMsgData(var Msg: TMsg; out Error: String): Boolean;
begin
  Result := True;
  try
    ReadMsgData(Msg);
  except on E: Exception do
    begin
      Result := False;
      Error := E.Message;
    end;
  end;
end;

procedure TWinMsgServerComm.ReadMessage;
begin
  // Do nothing, PeekMessages has pushed messages to the queue.
end;

function TWinMsgServerComm.GetInstanceID: String;
begin
  Result:=IntToStr(HInstance);
end;

{ ---------------------------------------------------------------------
    TWinMsgClientComm
  ---------------------------------------------------------------------}

Type
  TWinMsgClientComm = Class(TIPCClientComm)
  Private
    FWindowName: String;
    FHWND : HWND;
    function FindServerWindow: HWND;
    function FindServerWindow(const aWindowName: WideString): HWND;
  Public
    Constructor Create(AOWner : TSimpleIPCClient); override;
    Procedure Connect; override;
    Procedure Disconnect; override;
    Procedure SendMessage(MsgType : TMessageType; Stream : TStream); override;
    Function  ServerRunning : Boolean; override;
    Property WindowName : String Read FWindowName;
  end;


constructor TWinMsgClientComm.Create(AOWner: TSimpleIPCClient);
begin
  inherited Create(AOWner);
  FWindowName:=Owner.ServerID;
  If (Owner.ServerInstance<>'') then
    FWindowName:=FWindowName+'_'+Owner.ServerInstance;
end;

function TWinMsgClientComm.FindServerWindow: HWND;
begin
  Result := FindServerWindow(WideString(FWindowName));
end;

function TWinMsgClientComm.FindServerWindow(const aWindowName: WideString): HWND;
begin
  Result := FindWindowW(PWideChar(MsgWndClassName), PWideChar(aWindowName));
end;

procedure TWinMsgClientComm.Connect;
begin
  FHWND:=FindServerWindow;
  If (FHWND=0) then
    Owner.DoError(SErrServerNotActive,[Owner.ServerID]);
end;

procedure TWinMsgClientComm.Disconnect;
begin
  FHWND:=0;
end;

procedure TWinMsgClientComm.SendMessage(MsgType: TMessageType; Stream: TStream);
var
  CDS : TCopyDataStruct;
  Data,FMemstr : TMemorySTream;
begin
  if Stream is TMemoryStream then
  begin
    Data:=TMemoryStream(Stream);
    FMemStr:=nil;
  end
  else
  begin
    FMemStr:=TMemoryStream.Create;
    Data:=FMemstr;
  end;
  try
    if Assigned(FMemStr) then
    begin
      FMemStr.CopyFrom(Stream,0);
      FMemStr.Seek(0,soFromBeginning);
    end;
    CDS.dwData:=MsgType;
    CDS.lpData:=Data.Memory;
    CDS.cbData:=Data.Size;
    Windows.SendMessage(FHWND,WM_COPYDATA,0,PtrInt(@CDS));
  finally
    FreeAndNil(FMemStr);
  end;
end;

function TWinMsgClientComm.ServerRunning: Boolean;
begin
  Result:=FindServerWindow<>0;
end;

{ ---------------------------------------------------------------------
    Set TSimpleIPCClient / TSimpleIPCServer defaults.
  ---------------------------------------------------------------------}


Function TSimpleIPCServer.CommClass : TIPCServerCommClass;

begin
  if (DefaultIPCServerClass<>Nil) then
    Result:=DefaultIPCServerClass
  else
    Result:=TWinMsgServerComm;
end;

Function TSimpleIPCClient.CommClass : TIPCClientCommClass;

begin
  if (DefaultIPCClientClass<>Nil) then
    Result:=DefaultIPCClientClass
  else
    Result:=TWinMsgClientComm;
end;

