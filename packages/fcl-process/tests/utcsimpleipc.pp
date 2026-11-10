{ Tests of TSimpleIPCServer message dispatching and queue limits }
unit utcsimpleipc;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, fpcunit, testregistry, simpleipc;

type

  { TTestIPCMsgQueue }

  TTestIPCMsgQueue = class(TTestCase)
  private
    FQueue: TIPCServerMsgQueue;
    procedure PushStrings(aCount: Integer);
    procedure AssertPopped(const aMsg: String; aExpected: array of String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnlimitedNone;
    procedure TestUnlimitedDiscardOld;
    procedure TestUnlimitedDiscardNew;
    procedure TestUnlimitedError;
    procedure TestLimitDiscardOld;
    procedure TestLimitDiscardNew;
    procedure TestLimitError;
  end;

  { TTestSimpleIPC }

  TTestSimpleIPC = class(TTestCase)
  private
    FServer: TSimpleIPCServer;
    FClient: TSimpleIPCClient;
    FReceived: TStrings;
    FErrorCount: Integer;
    FDepth: Integer;
    FMaxDepth: Integer;
    FPumpInMessage: Integer;
    procedure DoMessage(Sender: TObject);
    procedure DoMessageError(Sender: TObject; Msg: TIPCServerMsg);
    procedure DoMessageQueuedRead(Sender: TObject);
    procedure SendStrings(aCount: Integer);
    procedure StartServer(aMaxQueue: Integer; aMaxAction: TIPCMessageOverflowAction);
    procedure ReadAllMessages;
    procedure CheckUnlimited(aMaxAction: TIPCMessageOverflowAction);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnlimitedNone;
    procedure TestUnlimitedDiscardOld;
    procedure TestUnlimitedDiscardNew;
    procedure TestUnlimitedError;
    procedure TestReadMessageInOnMessageQueued;
{$IFDEF WINDOWS}
    procedure TestOnMessageWithoutReadMessage;
    procedure TestOnMessageFromOtherThread;
    procedure TestPollingWithoutOnMessage;
    procedure TestNestedMessageLoop;
{$ENDIF}
  end;

implementation

var
  ServerCounter: Integer = 0;

{ Returns a server ID that is unique for this process }
function NewServerID: String;

begin
  Inc(ServerCounter);
  Result:='fpctestipc'+IntToStr(GetProcessID)+'_'+IntToStr(ServerCounter);
end;


{ Returns a message with the given text }
function NewMessage(const aText: String): TIPCServerMsg;

begin
  Result:=TIPCServerMsg.Create;
  Result.Stream.WriteBuffer(aText[1],Length(aText));
end;


{$IFDEF WINDOWS}
type

  { TClientThread }

  TClientThread = class(TThread)
  private
    FServerID: String;
    FPrefix: String;
    FCount: Integer;
    FDelay: Integer;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that sends aCount messages aPrefix1.. to aServerID after aDelay milliseconds
    constructor Create(const aServerID, aPrefix: String; aCount, aDelay: Integer);
  end;


constructor TClientThread.Create(const aServerID, aPrefix: String; aCount, aDelay: Integer);

begin
  FServerID:=aServerID;
  FPrefix:=aPrefix;
  FCount:=aCount;
  FDelay:=aDelay;
  inherited Create(False);
end;


procedure TClientThread.Execute;

var
  lClient: TSimpleIPCClient;
  i: Integer;

begin
  if FDelay>0 then
    Sleep(FDelay);
  lClient:=TSimpleIPCClient.Create(nil);
  try
    lClient.ServerID:=FServerID;
    lClient.Connect;
    for i:=1 to FCount do
      lClient.SendStringMessage(FPrefix+IntToStr(i));
  finally
    lClient.Free;
  end;
end;


{ Runs a plain Windows message loop for aDuration milliseconds or until aReceived has aStopCount lines }
procedure PumpWindowsMessages(aDuration: Integer; aReceived: TStrings; aStopCount: Integer);

var
  lMsg: TMsg;
  lStop: QWord;

begin
  lStop:=GetTickCount64+aDuration;
  while (GetTickCount64<lStop) and ((aReceived=nil) or (aReceived.Count<aStopCount)) do
    begin
    while Windows.PeekMessage(lMsg,0,0,0,PM_REMOVE) do
      begin
      TranslateMessage(lMsg);
      DispatchMessage(lMsg);
      end;
    Sleep(5);
    end;
end;
{$ENDIF}


{ TTestIPCMsgQueue }

procedure TTestIPCMsgQueue.SetUp;

begin
  FQueue:=TIPCServerMsgQueue.Create;
end;


procedure TTestIPCMsgQueue.TearDown;

begin
  FreeAndNil(FQueue);
end;


procedure TTestIPCMsgQueue.PushStrings(aCount: Integer);

var
  i: Integer;

begin
  for i:=1 to aCount do
    FQueue.Push(NewMessage('m'+IntToStr(i)));
end;


procedure TTestIPCMsgQueue.AssertPopped(const aMsg: String; aExpected: array of String);

var
  lMsg: TIPCServerMsg;
  i: Integer;

begin
  AssertEquals(aMsg+': queue count',Length(aExpected),FQueue.Count);
  for i:=0 to Length(aExpected)-1 do
    begin
    lMsg:=FQueue.Pop;
    try
      AssertEquals(aMsg+': message '+IntToStr(i),aExpected[i],lMsg.StringMessage);
    finally
      lMsg.Free;
    end;
    end;
end;


procedure TTestIPCMsgQueue.TestUnlimitedNone;

begin
  FQueue.MaxCount:=0;
  FQueue.MaxAction:=ipcmoaNone;
  PushStrings(5);
  AssertPopped('MaxCount 0 with ipcmoaNone keeps all messages',['m1','m2','m3','m4','m5']);
end;


procedure TTestIPCMsgQueue.TestUnlimitedDiscardOld;

begin
  FQueue.MaxCount:=0;
  FQueue.MaxAction:=ipcmoaDiscardOld;
  PushStrings(5);
  AssertPopped('MaxCount 0 with ipcmoaDiscardOld keeps all messages',['m1','m2','m3','m4','m5']);
end;


procedure TTestIPCMsgQueue.TestUnlimitedDiscardNew;

begin
  FQueue.MaxCount:=0;
  FQueue.MaxAction:=ipcmoaDiscardNew;
  PushStrings(5);
  AssertPopped('MaxCount 0 with ipcmoaDiscardNew keeps all messages',['m1','m2','m3','m4','m5']);
end;


procedure TTestIPCMsgQueue.TestUnlimitedError;

begin
  FQueue.MaxCount:=0;
  FQueue.MaxAction:=ipcmoaError;
  PushStrings(5);
  AssertPopped('MaxCount 0 with ipcmoaError keeps all messages',['m1','m2','m3','m4','m5']);
end;


procedure TTestIPCMsgQueue.TestLimitDiscardOld;

begin
  FQueue.MaxCount:=2;
  FQueue.MaxAction:=ipcmoaDiscardOld;
  PushStrings(4);
  AssertPopped('MaxCount 2 with ipcmoaDiscardOld keeps the newest messages',['m3','m4']);
end;


procedure TTestIPCMsgQueue.TestLimitDiscardNew;

begin
  FQueue.MaxCount:=2;
  FQueue.MaxAction:=ipcmoaDiscardNew;
  PushStrings(4);
  AssertPopped('MaxCount 2 with ipcmoaDiscardNew keeps the oldest messages',['m1','m2']);
end;


procedure TTestIPCMsgQueue.TestLimitError;

var
  lMsg: TIPCServerMsg;
  lRaised: Boolean;

begin
  FQueue.MaxCount:=2;
  FQueue.MaxAction:=ipcmoaError;
  PushStrings(2);
  lMsg:=NewMessage('m3');
  lRaised:=False;
  try
    FQueue.Push(lMsg);
  except
    on EIPCError do
      lRaised:=True;
  end;
  lMsg.Free;
  AssertTrue('Push on a full queue with ipcmoaError raises EIPCError',lRaised);
  AssertPopped('Full queue with ipcmoaError is unchanged',['m1','m2']);
end;


{ TTestSimpleIPC }

procedure TTestSimpleIPC.SetUp;

begin
  FReceived:=TStringList.Create;
  FErrorCount:=0;
  FDepth:=0;
  FMaxDepth:=0;
  FPumpInMessage:=0;
  FServer:=TSimpleIPCServer.Create(nil);
  FServer.ServerID:=NewServerID;
  FServer.Global:=True;
  FServer.OnMessageError:=@DoMessageError;
  FClient:=TSimpleIPCClient.Create(nil);
  FClient.ServerID:=FServer.ServerID;
end;


procedure TTestSimpleIPC.TearDown;

begin
  FreeAndNil(FClient);
  FreeAndNil(FServer);
  FreeAndNil(FReceived);
end;


procedure TTestSimpleIPC.DoMessage(Sender: TObject);

begin
  Inc(FDepth);
  try
    if FDepth>FMaxDepth then
      FMaxDepth:=FDepth;
    FReceived.Add(FServer.StringMessage);
{$IFDEF WINDOWS}
    if (FReceived.Count=1) and (FPumpInMessage>0) then
      PumpWindowsMessages(FPumpInMessage,nil,0);
{$ENDIF}
  finally
    Dec(FDepth);
  end;
end;


procedure TTestSimpleIPC.DoMessageError(Sender: TObject; Msg: TIPCServerMsg);

begin
  Inc(FErrorCount);
end;


procedure TTestSimpleIPC.DoMessageQueuedRead(Sender: TObject);

begin
  FServer.ReadMessage;
end;


procedure TTestSimpleIPC.SendStrings(aCount: Integer);

var
  i: Integer;

begin
  for i:=1 to aCount do
    FClient.SendStringMessage('m'+IntToStr(i));
end;


procedure TTestSimpleIPC.StartServer(aMaxQueue: Integer; aMaxAction: TIPCMessageOverflowAction);

begin
  FServer.MaxQueue:=aMaxQueue;
  FServer.MaxAction:=aMaxAction;
  FServer.StartServer(False);
  FClient.Connect;
end;


procedure TTestSimpleIPC.ReadAllMessages;

begin
  while FServer.PeekMessage(100,False) do
    begin
    FServer.ReadMessage;
    FReceived.Add(FServer.StringMessage);
    end;
end;


procedure TTestSimpleIPC.CheckUnlimited(aMaxAction: TIPCMessageOverflowAction);

begin
  StartServer(0,aMaxAction);
  SendStrings(5);
  ReadAllMessages;
  AssertEquals('No OnMessageError with MaxQueue 0',0,FErrorCount);
  AssertEquals('All messages received with MaxQueue 0','m1,m2,m3,m4,m5',FReceived.CommaText);
end;


procedure TTestSimpleIPC.TestUnlimitedNone;

begin
  CheckUnlimited(ipcmoaNone);
end;


procedure TTestSimpleIPC.TestUnlimitedDiscardOld;

begin
  CheckUnlimited(ipcmoaDiscardOld);
end;


procedure TTestSimpleIPC.TestUnlimitedDiscardNew;

begin
  CheckUnlimited(ipcmoaDiscardNew);
end;


procedure TTestSimpleIPC.TestUnlimitedError;

begin
  CheckUnlimited(ipcmoaError);
end;


procedure TTestSimpleIPC.TestReadMessageInOnMessageQueued;

var
  i: Integer;

begin
  FServer.OnMessage:=@DoMessage;
  FServer.OnMessageQueued:=@DoMessageQueuedRead;
  StartServer(0,ipcmoaNone);
  SendStrings(3);
  for i:=1 to 10 do
    FServer.PeekMessage(10,False);
  AssertEquals('ReadMessage in OnMessageQueued fires OnMessage once per message','m1,m2,m3',FReceived.CommaText);
end;


{$IFDEF WINDOWS}
procedure TTestSimpleIPC.TestOnMessageWithoutReadMessage;

begin
  FServer.OnMessage:=@DoMessage;
  StartServer(0,ipcmoaNone);
  SendStrings(3);
  AssertEquals('OnMessage fires without ReadMessage','m1,m2,m3',FReceived.CommaText);
end;


procedure TTestSimpleIPC.TestOnMessageFromOtherThread;

var
  lThread: TClientThread;

begin
  FServer.OnMessage:=@DoMessage;
  StartServer(0,ipcmoaNone);
  lThread:=TClientThread.Create(FServer.ServerID,'t',3,0);
  try
    PumpWindowsMessages(5000,FReceived,3);
    lThread.WaitFor;
  finally
    lThread.Free;
  end;
  AssertEquals('OnMessage fires from the application message loop','t1,t2,t3',FReceived.CommaText);
end;


procedure TTestSimpleIPC.TestPollingWithoutOnMessage;

var
  lThread: TClientThread;

begin
  StartServer(0,ipcmoaNone);
  lThread:=TClientThread.Create(FServer.ServerID,'t',3,0);
  try
    PumpWindowsMessages(1000,nil,0);
    lThread.WaitFor;
  finally
    lThread.Free;
  end;
  ReadAllMessages;
  AssertEquals('Messages received through the message loop without OnMessage stay queued for PeekMessage','t1,t2,t3',FReceived.CommaText);
end;


procedure TTestSimpleIPC.TestNestedMessageLoop;

var
  lFirst, lSecond: TClientThread;

begin
  FServer.OnMessage:=@DoMessage;
  FPumpInMessage:=1000;
  StartServer(0,ipcmoaNone);
  lFirst:=TClientThread.Create(FServer.ServerID,'a',1,0);
  lSecond:=TClientThread.Create(FServer.ServerID,'b',1,300);
  try
    PumpWindowsMessages(5000,FReceived,2);
    lFirst.WaitFor;
    lSecond.WaitFor;
  finally
    lFirst.Free;
    lSecond.Free;
  end;
  AssertEquals('Message received during OnMessage is dispatched after it','a1,b1',FReceived.CommaText);
  AssertEquals('OnMessage is not called recursively',1,FMaxDepth);
end;
{$ENDIF}


initialization
  RegisterTests([TTestIPCMsgQueue,TTestSimpleIPC]);
end.
