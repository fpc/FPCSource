unit FPHTTPClientAsyncPool;

{
  Default HTTP Client asynchronous pool.
  Events are not synchronized - the program has to synchronize within the events itself if necessary
    (e.g. with critical sections).

  If you are looking for a client pool that can be used in an LCL application and that does the synchronization for you,
    check (TODO: URL)
}

{$IF (FPC_FULLVERSION >= 30301)}
  {$define use_functionreferences}
{$ENDIF}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$IFDEF use_functionreferences}
  {$modeswitch functionreferences}
{$ENDIF}

interface

uses
  Classes, SysUtils, fphttpclient, httpprotocol, URIParser, syncobjs, DateUtils, FPHTTPClientPool;

type
  TFPHTTPClientPoolMethodResult = (mrSuccess, mrAbortedByClient, mrAbortedWithException);

  TFPHTTPClientAbstractAsyncPoolRequest = class;

  TFPHTTPClientPoolResult = class(TPersistent)
  private
    fExceptionClass: TClass;
    fExceptionMessage: string;

    fRequest: TFPHTTPClientAbstractAsyncPoolRequest;
    fMethodResult: TFPHTTPClientPoolMethodResult;
    fResponseHeaders: TStringList;
    fResponseStatusCode: Integer;
    fResponseStatusText: string;

    fResponseStream: TStream;
    fOwnsResponseStream: Boolean;
    function GetResponseContentType: string;
    function GetResponseEncoding: TEncoding;
    function GetResponseString: string;
    function GetResponseBytes: TBytes;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Request: TFPHTTPClientAbstractAsyncPoolRequest read fRequest;
    property MethodResult: TFPHTTPClientPoolMethodResult read fMethodResult write fMethodResult;
    property ResponseStatusCode: Integer read fResponseStatusCode write fResponseStatusCode;
    property ResponseStatusText: string read fResponseStatusText write fResponseStatusText;
    // ResponseEncoding - must be destroyed after use if it is not a standard encoding
    property ResponseEncoding: TEncoding read GetResponseEncoding;

    property ResponseHeaders: TStringList read fResponseHeaders;
    property ResponseStream: TStream read fResponseStream write fResponseStream;
    property OwnsResponseStream: Boolean read fOwnsResponseStream write fOwnsResponseStream;
    property ResponseString: string read GetResponseString;
    property ResponseBytes: TBytes read GetResponseBytes;

    property ResponseContentType: string read GetResponseContentType;

    property ExceptionClass: TClass read fExceptionClass write fExceptionClass;
    property ExceptionMessage: string read fExceptionMessage write fExceptionMessage;
  public
    constructor Create(const aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
    destructor Destroy; override;
  end;

  TFPHTTPClientAsyncPoolRequestThread = class;

  TFPHTTPClientPoolProgressDirection = (pdDataSent, pdDataReceived);

{$IFDEF use_functionreferences}
  TFPHTTPClientAsyncPoolRequestRef = class;
  TFPHTTPClientPoolInitRef = reference to procedure(const aRequest: TFPHTTPClientAsyncPoolRequestRef; const aClient: TFPHTTPClient);
  TFPHTTPClientPoolFinishRef = reference to procedure(const aResult: TFPHTTPClientPoolResult);
  TFPHTTPClientPoolProgressRef = reference to procedure(
    Sender: TFPHTTPClientAsyncPoolRequestThread;
    const aDirection: TFPHTTPClientPoolProgressDirection;
    const aPosition, aContentLength: Int64; var ioStop: Boolean);
  TFPHTTPClientPoolSimpleCallbackRef = reference to procedure;
{$ENDIF}
  TFPHTTPClientAsyncPoolRequest = class;
  TFPHTTPClientPoolInit = procedure(const aRequest: TFPHTTPClientAsyncPoolRequest; const aClient: TFPHTTPClient) of object;
  TFPHTTPClientPoolFinish = procedure(const aResult: TFPHTTPClientPoolResult) of object;
  TFPHTTPClientPoolProgress = procedure(
    Sender: TFPHTTPClientAsyncPoolRequestThread;
    const aDirection: TFPHTTPClientPoolProgressDirection;
    const aPosition, aContentLength: Int64; var ioStop: Boolean) of object;
  TNotifyComponentEvent = procedure(AOwner: TComponent) of object;

  TFPCustomHTTPClientAsyncPool = class;

  TFPHTTPClientAbstractAsyncPoolRequest = class(TPersistent)
  public
    // if Owner gets destroyed, the request will be aborted (=rsAbortedByClient)
    //  especially needed in an LCL application where e.g. the form can get closed while the request is still running
    Owner: TComponent;
    // if aBlocker <> nil, when sending the request, all running requests with the same aBlocker will be aborted (=rsAbortedByClient)
    Blocker: TObject;

    // parameters for TFPHTTPClient.HTTPMethod
    Method, URL: string;
    URLData: TBytes;
    ContentType: string;
    Headers: string;
    ResponseStream: TStream;
    OwnsResponseStream: Boolean;
    AllowedResponseCodes: array of Integer;

    // EVENTS
    // should OnInit be synchronized with the main thread?
    SynchronizeOnInit: Boolean;
    // should OnFinish be synchronized with the main thread?
    SynchronizeOnFinish: Boolean;
    // should OnFinish be executed when Owner is destroyed during the request
    ExecuteOnFinishOnOwnerDestroy: Boolean;

    // TIMEOUTS in ms
    // timeout to find a free client in the pool. Default=0 (infinite)
    ClientTimeout: Integer;
    // TFPHTTPClient.ConnectTimeout. Default=3000
    ConnectTimeout: Integer;
    // TFPHTTPClient.IOTimeout. Default=0 (infinite)
    IOTimeout: Integer;
  private
    function GetHost: string;
    function GetURLDataString: string;
    procedure SetURLDataString(const aURLDataString: string);

  protected
    procedure OwnerDestroyed; virtual;

    procedure DoOnInit(const aClient: TFPHTTPClient); virtual; abstract;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); virtual; abstract;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); virtual; abstract;
  public
    constructor Create;
  public
    property URLDataString: string read GetURLDataString write SetURLDataString;
    property Host: string read GetHost;
  end;

  TFPHTTPClientAsyncPoolRequest = class(TFPHTTPClientAbstractAsyncPoolRequest)
  protected
    procedure OwnerDestroyed; override;

    procedure DoOnInit(const aClient: TFPHTTPClient); override;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); override;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); override;
  public
    // EVENTS
    // setup custom client properties
    OnInit: TFPHTTPClientPoolInit;
    // read out the result
    OnFinish: TFPHTTPClientPoolFinish;
    // progress callback
    OnProgress: TFPHTTPClientPoolProgress;
  end;

{$IFDEF use_functionreferences}
  TFPHTTPClientAsyncPoolRequestRef = class(TFPHTTPClientAbstractAsyncPoolRequest)
  protected
    procedure OwnerDestroyed; override;

    procedure DoOnInit(const aClient: TFPHTTPClient); override;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); override;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); override;
  public
    // EVENTS
    // setup custom client properties
    OnInit: TFPHTTPClientPoolInitRef;
    // read out the result
    OnFinish: TFPHTTPClientPoolFinishRef;
    // progress callback
    OnProgress: TFPHTTPClientPoolProgressRef;
  end;
{$ENDIF}

  TFPHTTPClientAsyncPoolRequestQueueItem = class;
  TFPHTTPClientAsyncPoolThread = class(TThread)
  private
    fPool: TFPCustomHTTPClientAsyncPool;
    fCSProperties: TCriticalSection;
  protected
    // access only through LockProperties
    procedure OwnerDestroyed; virtual;
  public
    property Pool: TFPCustomHTTPClientAsyncPool read fPool;

    // lock&unlock read/write properties (properties are written currently only in OwnerDestroyed)
    procedure LockProperties;
    procedure UnlockProperties;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool);
    destructor Destroy; override;
  end;

  TFPHTTPClientAsyncPoolCustomWaitForAllThread = class(TFPHTTPClientAsyncPoolThread)
  private
    fTimeoutMS: Integer;
    fOwner: TComponent;
    fSynchronizeOnAllDone: Boolean;

    procedure ExecOnAllDone;
  protected
    procedure DoOnAllDone; virtual; abstract;

    procedure Execute; override;

    // access only through LockProperties
    procedure OwnerDestroyed; override;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
  end;

  TFPHTTPClientAsyncPoolWaitForAllThread = class(TFPHTTPClientAsyncPoolCustomWaitForAllThread)
  private
    fOnAllDone: TNotifyEvent;
  protected
    procedure DoOnAllDone; override;
    procedure OwnerDestroyed; override;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; aOnAllDone: TNotifyEvent;
      const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
  end;

  {$IFDEF use_functionreferences}
  TFPHTTPClientAsyncPoolWaitForAllThreadRef = class(TFPHTTPClientAsyncPoolCustomWaitForAllThread)
  private
    fOnAllDone: TFPHTTPClientPoolSimpleCallbackRef;
  protected
    procedure DoOnAllDone; override;
    procedure OwnerDestroyed; override;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; aOnAllDone: TFPHTTPClientPoolSimpleCallbackRef;
      const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
  end;
  {$ENDIF}

  TFPHTTPClientAsyncPoolRequestThread = class(TFPHTTPClientAsyncPoolThread)
  private
    fItem: TFPHTTPClientAsyncPoolRequestQueueItem;

    fClient: TFPHTTPClient;

    function GetRequest: TFPHTTPClientAbstractAsyncPoolRequest;
    function GetResult: TFPHTTPClientPoolResult;
    procedure OnDataReceived(Sender: TObject; const aContentLength, aCurrentPos: Int64);
    procedure OnDataSent(Sender: TObject; const aContentLength, aCurrentPos: Int64);

    // the ExecOn* methods call their DoOn* counterparts - do the synchronisation here
    procedure ExecOnInit;
    procedure ExecOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
      const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
  protected
    // access only through LockProperties
    procedure OwnerDestroyed; override;
  protected
    procedure OnDataReceivedSend(Sender: TObject; const aDirection: TFPHTTPClientPoolProgressDirection; const aCurrentPos, aContentLength: Int64); virtual;
  protected
    property Client: TFPHTTPClient read fClient;
    property Result: TFPHTTPClientPoolResult read GetResult;
    procedure TerminatedSet; override;

    // the DoOn* methods do the actual work and can be synchronised by their ExecOn* counterparts
    // DoOnInit - executed when the request aquired a TFPHTTPClient to setup its extra properties
    //  should not be synchronized with Synchronize() - it slows down the execution. Better to use CriticalSections
    procedure DoOnInit; virtual;
    // DoOnProgress - show progress during upload&download
    //  should not be synchronized with Synchronize() - it slows down the execution. Better to use CriticalSections or Application.QueueAsyncCall in an LCL application
    procedure DoOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
      const aCurrentPos, aContentLength: Integer; var ioStop: Boolean); virtual;
  protected
    procedure Execute; override;

  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; aItem: TFPHTTPClientAsyncPoolRequestQueueItem;
      aClient: TFPHTTPClient); virtual;
    destructor Destroy; override;
  public
    // access only through LockProperties
    property Request: TFPHTTPClientAbstractAsyncPoolRequest read GetRequest;
  end;

  TFPHTTPClientAsyncPoolRequestQueueItem = class(TComponent)
  private
    fBreakUTC: TDateTime;
    fClients: TFPCustomHTTPClients;
    fPool: TFPCustomHTTPClientAsyncPool;
    fRequest: TFPHTTPClientAbstractAsyncPoolRequest;
    fRequestOwner: TComponent;
    fResult: TFPHTTPClientPoolResult;

    Thread: TFPHTTPClientAsyncPoolRequestThread;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Pool: TFPCustomHTTPClientAsyncPool read fPool;
    property Clients: TFPCustomHTTPClients read fClients;
    property BreakUTC: TDateTime read fBreakUTC;
    property Request: TFPHTTPClientAbstractAsyncPoolRequest read fRequest;
    property Result: TFPHTTPClientPoolResult read fResult;

    procedure DoOnFinish;
    procedure ExecOnFinish;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; aClients: TFPCustomHTTPClients;
      aBreakUTC: TDateTime; aRequest: TFPHTTPClientAbstractAsyncPoolRequest); reintroduce;
    destructor Destroy; override;
  end;

  TFPHTTPClientAsyncPoolRequestQueue = class(TList)
  private
    function GetItem(Index: Integer): TFPHTTPClientAsyncPoolRequestQueueItem;
  public
    property Items[Index: Integer]: TFPHTTPClientAsyncPoolRequestQueueItem read GetItem; default;
  end;

  TFPCustomHTTPClientAsyncPool = class(TComponent)
  private
    fHttpPool: TFPCustomHTTPClientPool;

    // do not access fQueue directly, use LockQueue() instead
    fListCS: TCriticalSection;
    fQueue: TFPHTTPClientAsyncPoolRequestQueue;

    fBlockRequestsCounter: Integer;
    function GetActiveAsyncMethodCount: Integer;
    function GetClientCount: Integer;
    function GetMaxClientsPerServer: Integer;
    function GetWaitingAsyncMethodCount: Integer;
    function GetQueueCount: Integer;
    procedure SetMaxClientsPerServer(const aMaxClientsPerServer: Integer);

  protected
    function CreatePool: TFPCustomHTTPClientPool; virtual;
    function CreateRequestThread(aItem: TFPHTTPClientAsyncPoolRequestQueueItem; aRequest: TFPHTTPClientAbstractAsyncPoolRequest;
      aClient: TFPHTTPClient): TFPHTTPClientAsyncPoolRequestThread; virtual;
    function CreateWaitForAllRequestsThread(const aOnAllDone: TNotifyEvent;
      const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolCustomWaitForAllThread; virtual;
    {$IFDEF use_functionreferences}
    function CreateWaitForAllRequestsThreadRef(const aOnAllDoneRef: TFPHTTPClientPoolSimpleCallbackRef;
      const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolCustomWaitForAllThread; virtual;
    {$ENDIF}

    // support for MaxClientsPerServer (add requests that wait for a client to a queue)
    function AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime; const aRequest: TFPHTTPClientAbstractAsyncPoolRequest): TFPHTTPClientAsyncPoolRequestQueueItem;
    procedure RemoveFromQueue(const aItem: TFPHTTPClientAsyncPoolRequestQueueItem);
    procedure ReleaseClient(const aRequest: TFPHTTPClientAbstractAsyncPoolRequest; const aClient: TFPHTTPClient);

    procedure LockQueue(out outQueue: TFPHTTPClientAsyncPoolRequestQueue);
    procedure UnlockQueue;
  public
    // send an asynchronous HTTP request
    procedure AsyncMethod(aRequest: TFPHTTPClientAbstractAsyncPoolRequest);

    // stop all requests with Blocker
    procedure StopRequests(const aBlocker: TObject);

    procedure BlockNewRequests;
    procedure UnblockNewRequests;

    // wait until all requests are finished
    //  all new requests will be blocked in between
    procedure WaitForAllRequests(const aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer);
    {$IFDEF use_functionreferences}
    procedure WaitForAllRequests(const aOnAllDoneRef: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    property ClientCount: Integer read GetClientCount;
    property ActiveAsyncMethodCount: Integer read GetActiveAsyncMethodCount;
    property WaitingAsyncMethodCount: Integer read GetWaitingAsyncMethodCount;
    property QueueCount: Integer read GetQueueCount;
    property MaxClientsPerServer: Integer read GetMaxClientsPerServer write SetMaxClientsPerServer;
  end;

implementation

{ TFPHTTPClientAsyncPoolRequestQueue }

function TFPHTTPClientAsyncPoolRequestQueue.GetItem(Index: Integer): TFPHTTPClientAsyncPoolRequestQueueItem;
begin
  Result := TFPHTTPClientAsyncPoolRequestQueueItem(inherited Items[Index]);
end;

{$IFDEF use_functionreferences}
{ TFPHTTPClientAsyncPoolRequestRef }

procedure TFPHTTPClientAsyncPoolRequestRef.DoOnFinish(const aResult: TFPHTTPClientPoolResult);
begin
  if Assigned(OnFinish) then
  begin
    OnFinish(aResult);
    OnFinish := nil;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestRef.DoOnInit(const aClient: TFPHTTPClient);
begin
  if Assigned(OnInit) then
    OnInit(Self, aClient);
end;

procedure TFPHTTPClientAsyncPoolRequestRef.DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread;
  const aDirection: TFPHTTPClientPoolProgressDirection; const aPosition, aContentLength: Int64; var ioStop: Boolean);
begin
  if Assigned(OnProgress) then
    OnProgress(Sender, aDirection, aPosition, aContentLength, ioStop);
end;

procedure TFPHTTPClientAsyncPoolRequestRef.OwnerDestroyed;
begin
  inherited;
  if not ExecuteOnFinishOnOwnerDestroy then
    OnFinish := nil;
  OnProgress := nil;
end;
{$ENDIF}

{ TFPHTTPClientAsyncPoolRequest }

procedure TFPHTTPClientAsyncPoolRequest.DoOnFinish(const aResult: TFPHTTPClientPoolResult);
begin
  if Assigned(OnFinish) then
  begin
    OnFinish(aResult);
    OnFinish := nil;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequest.DoOnInit(const aClient: TFPHTTPClient);
begin
  if Assigned(OnInit) then
    OnInit(Self, aClient);
end;

procedure TFPHTTPClientAsyncPoolRequest.DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread;
  const aDirection: TFPHTTPClientPoolProgressDirection; const aPosition, aContentLength: Int64; var ioStop: Boolean);
begin
  if Assigned(OnProgress) then
    OnProgress(Sender, aDirection, aPosition, aContentLength, ioStop);
end;

procedure TFPHTTPClientAsyncPoolRequest.OwnerDestroyed;
begin
  inherited;
  if not ExecuteOnFinishOnOwnerDestroy then
    OnFinish := nil;
  OnProgress := nil;
end;

{ TFPHTTPClientAsyncPoolRequestQueueItem }

constructor TFPHTTPClientAsyncPoolRequestQueueItem.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aClients: TFPCustomHTTPClients; aBreakUTC: TDateTime; aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
begin
  inherited Create(nil);

  fBreakUTC := aBreakUTC;
  fClients := aClients;
  fPool := aPool;
  fRequest := aRequest;

  fRequestOwner := fRequest.Owner;
  if Assigned(fRequestOwner) then
    FreeNotification(fRequestOwner);

  fResult := TFPHTTPClientPoolResult.Create(aRequest);
  if Assigned(aRequest.ResponseStream) then
  begin
    fResult.ResponseStream := aRequest.ResponseStream;
    fResult.OwnsResponseStream := aRequest.OwnsResponseStream;
  end else
  begin
    fResult.ResponseStream := TBytesStream.Create;
    fResult.OwnsResponseStream := True;
  end;
end;

destructor TFPHTTPClientAsyncPoolRequestQueueItem.Destroy;
begin
  {$IFDEF DEBUG}
  Assert(not Assigned(Thread)); // for debugging
  {$ENDIF}

  if Assigned(Request) then
  begin
    ExecOnFinish;
    fRequest.Free;
    fRequest := nil;
  end;
  fResult.Free;

  Pool.RemoveFromQueue(Self);
  inherited Destroy;
end;

procedure TFPHTTPClientAsyncPoolRequestQueueItem.DoOnFinish;
begin
  fRequest.DoOnFinish(fResult);
end;

procedure TFPHTTPClientAsyncPoolRequestQueueItem.ExecOnFinish;
begin
  if fRequest.SynchronizeOnFinish and (ThreadID<>MainThreadID) then
  begin
    TThread.Synchronize(nil, @DoOnFinish);
  end else
    DoOnFinish;
end;

procedure TFPHTTPClientAsyncPoolRequestQueueItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation=opRemove) and (fRequestOwner=AComponent) then
  begin
    fRequestOwner := nil;
    fResult.MethodResult := mrAbortedByClient;
    if Assigned(Thread) then
    begin
      Thread.LockProperties;
      Thread.OwnerDestroyed;
      ExecOnFinish;
      Thread.UnlockProperties;
      Thread.Terminate;
    end else
      Self.Free;
  end;
end;

{ TFPHTTPClientAsyncPoolWaitForAllThread }

constructor TFPHTTPClientAsyncPoolWaitForAllThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  fOnAllDone := aOnAllDone;
  inherited Create(aPool, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.DoOnAllDone;
begin
  if Assigned(fOnAllDone) then
    fOnAllDone(Self);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.OwnerDestroyed;
begin
  fOnAllDone := nil;
  inherited OwnerDestroyed;
end;

{$IFDEF use_functionreferences}
{ TFPHTTPClientAsyncPoolWaitForAllThreadRef }

constructor TFPHTTPClientAsyncPoolWaitForAllThreadRef.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aOnAllDone: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent;
  const aTimeoutMS: Integer);
begin
  fOnAllDone := aOnAllDone;
  inherited Create(aPool, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThreadRef.DoOnAllDone;
begin
  if Assigned(fOnAllDone) then
    fOnAllDone();
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThreadRef.OwnerDestroyed;
begin
  fOnAllDone := nil;
  inherited OwnerDestroyed;
end;
{$ENDIF}

{ TFPHTTPClientAsyncPoolCustomWaitForAllThread }

constructor TFPHTTPClientAsyncPoolCustomWaitForAllThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  fOwner := aOwner;
  fSynchronizeOnAllDone := aSynchronizeOnAllDone;
  fTimeoutMS := aTimeoutMS;

  inherited Create(aPool);
end;

procedure TFPHTTPClientAsyncPoolCustomWaitForAllThread.ExecOnAllDone;
begin
  if fSynchronizeOnAllDone then
    Synchronize(@DoOnAllDone)
  else
    DoOnAllDone;
end;

procedure TFPHTTPClientAsyncPoolCustomWaitForAllThread.Execute;
var
  xBreak: TDateTime;
begin
  try
    Pool.BlockNewRequests;
    try
      if fTimeoutMS>0 then
        xBreak := IncMilliSecond(NowUTC, fTimeoutMS);
      while not Terminated and (Pool.ActiveAsyncMethodCount>0) and (Pool.WaitingAsyncMethodCount>0) and ((fTimeoutMS=0) or (NowUTC<xBreak)) do
        Sleep(10);
    finally
      Pool.UnblockNewRequests;
    end;
    if not Terminated then
      ExecOnAllDone;
  except
  end;
end;

procedure TFPHTTPClientAsyncPoolCustomWaitForAllThread.OwnerDestroyed;
begin
  fOwner := nil;
  inherited OwnerDestroyed;
end;

{ TFPHTTPClientAsyncPoolThread }

constructor TFPHTTPClientAsyncPoolThread.Create(aPool: TFPCustomHTTPClientAsyncPool);
begin
  fPool := aPool;
  FreeOnTerminate := True;
  fCSProperties := TCriticalSection.Create;

  inherited Create(False);
end;

destructor TFPHTTPClientAsyncPoolThread.Destroy;
begin
  fCSProperties.Free;
  inherited Destroy;
end;

procedure TFPHTTPClientAsyncPoolThread.LockProperties;
begin
  fCSProperties.Enter;
end;

procedure TFPHTTPClientAsyncPoolThread.OwnerDestroyed;
begin
  // nothing here
end;

procedure TFPHTTPClientAsyncPoolThread.UnlockProperties;
begin
  fCSProperties.Leave;
end;

{ TFPHTTPClientAbstractAsyncPoolRequest }

constructor TFPHTTPClientAbstractAsyncPoolRequest.Create;
begin
  inherited Create;

  ConnectTimeout := 3000;
end;

function TFPHTTPClientAbstractAsyncPoolRequest.GetHost: string;
var
  xURI: TURI;
begin
  xURI := ParseURI(URL, False);
  Result := xURI.Host;
end;

function TFPHTTPClientAbstractAsyncPoolRequest.GetURLDataString: string;
begin
  Result := TEncoding.SystemEncoding.GetAnsiString(URLData);
end;

procedure TFPHTTPClientAbstractAsyncPoolRequest.OwnerDestroyed;
begin
  Owner := nil;
end;

procedure TFPHTTPClientAbstractAsyncPoolRequest.SetURLDataString(const aURLDataString: string);
begin
  URLData := TEncoding.SystemEncoding.GetAnsiBytes(aURLDataString);
end;

{ TFPHTTPClientPoolResult }

constructor TFPHTTPClientPoolResult.Create(const aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
begin
  inherited Create;

  fRequest := aRequest;
  fResponseHeaders := TStringList.Create;
end;

procedure TFPHTTPClientPoolResult.AssignTo(Dest: TPersistent);
var
  xDest: TFPHTTPClientPoolResult;
begin
  if Dest is TFPHTTPClientPoolResult then
  begin
    xDest := TFPHTTPClientPoolResult(Dest);
    xDest.fExceptionClass := fExceptionClass;
    xDest.fExceptionMessage := fExceptionMessage;

    xDest.fMethodResult := fMethodResult;
    xDest.fResponseHeaders.Assign(fResponseHeaders);
    xDest.fResponseStatusCode := fResponseStatusCode;
    xDest.fResponseStatusText := fResponseStatusText;
  end else
    inherited AssignTo(Dest);
end;

destructor TFPHTTPClientPoolResult.Destroy;
begin
  fResponseHeaders.Free;
  if OwnsResponseStream then
    ResponseStream.Free;

  inherited Destroy;
end;

function TFPHTTPClientPoolResult.GetResponseBytes: TBytes;
begin
  Result := nil;
  if ResponseStream.Size=0 then
    Exit;

  SetLength(Result, ResponseStream.Size);
  ResponseStream.Position := 0;
  if Length(Result)>0 then
    ResponseStream.ReadBuffer(Result, Length(Result));
end;

function TFPHTTPClientPoolResult.GetResponseContentType: string;
begin
  Result := TFPCustomHTTPClient.GetHeader(fResponseHeaders, HeaderContentType);
end;

function TFPHTTPClientPoolResult.GetResponseEncoding: TEncoding;
var
  xContentType, xEncodingName: string;
  xStrL: TStringList;
  I: Integer;
begin
  xContentType := GetResponseContentType;
  xContentType := 'Content-Type: text/html; charset=utf-8';
  xStrL := TStringList.Create;
  try
    xStrL.Delimiter := ';';
    xStrL.NameValueSeparator := '=';
    xStrL.StrictDelimiter := False;
    xStrL.DelimitedText := xContentType;
    for I := 0 to xStrL.Count-1 do
    begin
      if SameText(xStrL.Names[I], 'charset') then
      begin
        xEncodingName := xStrL.ValueFromIndex[I];
        Exit(TEncoding.GetEncoding(UnicodeString(xEncodingName)));
      end;
    end;
  finally
    xStrL.Free;
  end;
  Result := nil;
end;

function TFPHTTPClientPoolResult.GetResponseString: string;
var
  xEncoding: TEncoding;
  xBytes: TBytes;
begin
  if not Assigned(ResponseStream) or (ResponseStream.Size=0) then
    Exit('');
  xEncoding := ResponseEncoding;
  try
    if Assigned(xEncoding) then
    begin
      if ResponseStream is TBytesStream then
        Result := xEncoding.GetAnsiString(TBytesStream(ResponseStream).Bytes, 0, TBytesStream(ResponseStream).Size)
      else
      begin
        xBytes := nil;
        SetLength(xBytes, ResponseStream.Size);
        ResponseStream.Position := 0;
        ResponseStream.ReadBuffer(xBytes[0], Length(xBytes));
        Result := xEncoding.GetAnsiString(xBytes);
      end;
    end else
    begin
      SetLength(Result, ResponseStream.Size);
      ResponseStream.Position := 0;
      if Length(Result)>0 then
        ResponseStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    if Assigned(xEncoding) and  not TEncoding.IsStandardEncoding(xEncoding) then
      xEncoding.Free;
  end;
end;

{ TFPCustomHTTPClientAsyncPool }

procedure TFPCustomHTTPClientAsyncPool.AsyncMethod(aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
var
  xClients: TFPCustomHTTPClients;
  xBreakUTC: TDateTime;
  xURI: TURI;
  xClient: TFPHTTPClient;
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  xResult: TFPHTTPClientPoolResult;
begin
  try
    if InterlockedExchangeAdd(fBlockRequestsCounter, 0)<>0 then
    begin
      xResult := TFPHTTPClientPoolResult.Create(aRequest);
      try
        xResult.MethodResult := mrAbortedByClient;
        aRequest.DoOnFinish(xResult);
      finally
        xResult.Free;
      end;
      Exit;
    end;

    if Assigned(aRequest.Blocker) then
      StopRequests(aRequest.Blocker);

    xURI := ParseURI(aRequest.URL, False);
    xClients := fHttpPool.GetCreateServerClients(xURI.Host, xURI.Port);
    if aRequest.ClientTimeout>0 then
      xBreakUTC := IncMilliSecond(NowUTC, aRequest.ClientTimeout)
    else
      xBreakUTC := 0;
    xClient := xClients.GetClient;
    LockQueue(xQueue);
    xItem := AddToQueue(xClients, xBreakUTC, aRequest);
    if Assigned(xClient) then // client is available -> create request thread
      xItem.Thread := CreateRequestThread(xItem, aRequest, xClient);
    UnlockQueue;
    aRequest := nil; // don't destroy aRequest
  finally
    aRequest.Free;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.BlockNewRequests;
begin
  InterlockedIncrement(fBlockRequestsCounter);
end;

function TFPCustomHTTPClientAsyncPool.CreatePool: TFPCustomHTTPClientPool;
begin
  Result := TFPCustomHTTPClientPool.Create(Self);
end;

function TFPCustomHTTPClientAsyncPool.CreateRequestThread(aItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  aRequest: TFPHTTPClientAbstractAsyncPoolRequest; aClient: TFPHTTPClient): TFPHTTPClientAsyncPoolRequestThread;
begin
  Result := TFPHTTPClientAsyncPoolRequestThread.Create(Self, aItem, aClient);
end;

{$IFDEF use_functionreferences}
function TFPCustomHTTPClientAsyncPool.CreateWaitForAllRequestsThreadRef(
  const aOnAllDoneRef: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean;
  const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolCustomWaitForAllThread;
begin
  Result := TFPHTTPClientAsyncPoolWaitForAllThreadRef.Create(Self, aOnAllDoneRef, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;
{$ENDIF}

function TFPCustomHTTPClientAsyncPool.CreateWaitForAllRequestsThread(const aOnAllDone: TNotifyEvent;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent;
  const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolCustomWaitForAllThread;
begin
  Result := TFPHTTPClientAsyncPoolWaitForAllThread.Create(Self, aOnAllDone, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

constructor TFPCustomHTTPClientAsyncPool.Create(AOwner: TComponent);
begin
  fListCS := TCriticalSection.Create;
  fQueue := TFPHTTPClientAsyncPoolRequestQueue.Create;
  fHttpPool := CreatePool;

  inherited Create(AOwner);
end;

function TFPCustomHTTPClientAsyncPool.AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime;
  const aRequest: TFPHTTPClientAbstractAsyncPoolRequest): TFPHTTPClientAsyncPoolRequestQueueItem;
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
begin
  LockQueue(xQueue);
  try
    Result := TFPHTTPClientAsyncPoolRequestQueueItem.Create(Self, aClients, aBreakUTC, aRequest);
    xQueue.Add(Result);
  finally
    UnlockQueue;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.ReleaseClient(const aRequest: TFPHTTPClientAbstractAsyncPoolRequest;
  const aClient: TFPHTTPClient);
var
  xURI: TURI;
  xClients: TFPCustomHTTPClients;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  I: Integer;
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
begin
  LockQueue(xQueue);
  try
    // remove old item
    for I := 0 to xQueue.Count-1 do
    begin
      xItem := xQueue[I];
      if xItem.Request=aRequest then
      begin
        xQueue.Delete(I);
        break;
      end;
    end;

    xURI := ParseURI(aRequest.URL, False);
    xClients := fHttpPool.GetCreateServerClients(xURI.Host, xURI.Port);

    // find next to start
    I := 0;
    while I<xQueue.Count do
    begin
      xItem := xQueue[I];
      if not Assigned(xItem.Thread) then
      begin
        if (CompareDateTime(xItem.BreakUTC, 0)<>0) and (CompareDateTime(xItem.BreakUTC, NowUTC)<0) then
        begin // timeout is over
          xItem.Free;
          xQueue.Delete(I);
        end else
        if xClients=xItem.Clients then
        begin // found a request waiting in queue
          xItem.Thread := CreateRequestThread(xItem, xItem.Request, aClient);
          Exit;
        end else
          Inc(I);
      end else
        Inc(I);
    end;

    // no waiting request found - release the client
    fHttpPool.ReleaseClient(xURI.Host, xURI.Port, aClient);
  finally
    UnlockQueue;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.RemoveFromQueue(const aItem: TFPHTTPClientAsyncPoolRequestQueueItem);
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
  I: Integer;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
begin
  LockQueue(xQueue);
  try
    // remove old item
    for I := 0 to xQueue.Count-1 do
    begin
      xItem := xQueue[I];
      if xItem=aItem then
      begin
        xQueue.Delete(I);
        break;
      end;
    end;
  finally
    UnlockQueue;
  end;
end;

destructor TFPCustomHTTPClientAsyncPool.Destroy;
  procedure _TerminateAll(_List: TFPHTTPClientAsyncPoolRequestQueue);
  var
    I: Integer;
  begin
    for I := 0 to _List.Count-1 do
      if Assigned(_List[I].Thread) then
        _List[I].Thread.Terminate;
  end;
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
begin
  BlockNewRequests;
  LockQueue(xQueue);
  try
    _TerminateAll(xQueue);
  finally
    UnlockQueue;
  end;
  while QueueCount>0 do
  begin
    if (ThreadID=MainThreadID) then // we are synchronizing events - call CheckSynchronize to prevent deadlock in the main thread
      CheckSynchronize(10)
    else
      Sleep(10);
  end;
  fQueue.Free;
  fListCS.Free;

  inherited Destroy;
end;

function TFPCustomHTTPClientAsyncPool.GetActiveAsyncMethodCount: Integer;
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
  I: Integer;
begin
  LockQueue(xQueue);
  Result := 0;
  for I := 0 to xQueue.Count-1 do
    if Assigned(xQueue[I].Thread) then
      Inc(Result);
  UnlockQueue;
end;

function TFPCustomHTTPClientAsyncPool.GetClientCount: Integer;
begin
  Result := fHttpPool.ClientCount;
end;

function TFPCustomHTTPClientAsyncPool.GetMaxClientsPerServer: Integer;
begin
  Result := fHttpPool.MaxClientsPerServer;
end;

function TFPCustomHTTPClientAsyncPool.GetQueueCount: Integer;
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
begin
  LockQueue(xQueue);
  Result := xQueue.Count;
  UnlockQueue;
end;

function TFPCustomHTTPClientAsyncPool.GetWaitingAsyncMethodCount: Integer;
var
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
  I: Integer;
begin
  LockQueue(xQueue);
  Result := 0;
  for I := 0 to xQueue.Count-1 do
    if not Assigned(xQueue[I].Thread) then
      Inc(Result);
  UnlockQueue;
end;

procedure TFPCustomHTTPClientAsyncPool.LockQueue(out outQueue: TFPHTTPClientAsyncPoolRequestQueue);
begin
  fListCS.Enter;
  outQueue := fQueue;
end;

procedure TFPCustomHTTPClientAsyncPool.SetMaxClientsPerServer(const aMaxClientsPerServer: Integer);
begin
  fHttpPool.MaxClientsPerServer := aMaxClientsPerServer;
end;

procedure TFPCustomHTTPClientAsyncPool.StopRequests(const aBlocker: TObject);
var
  I: Integer;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  xQueue: TFPHTTPClientAsyncPoolRequestQueue;
begin
  LockQueue(xQueue);
  try
    for I := xQueue.Count-1 downto 0 do
    begin
      xItem := xQueue[I];
      if (xItem.Request.Blocker=aBlocker) then
      begin
        if Assigned(xItem.Thread) then
        begin
          xItem.Thread.Terminate;
        end else
        begin
          xItem.Free;
          xQueue.Delete(I);
        end;
      end;
    end;
  finally
    UnlockQueue;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.UnblockNewRequests;
begin
  InterlockedDecrement(fBlockRequestsCounter);
end;

procedure TFPCustomHTTPClientAsyncPool.UnlockQueue;
begin
  fListCS.Leave;
end;

{$IFDEF use_functionreferences}
procedure TFPCustomHTTPClientAsyncPool.WaitForAllRequests(const aOnAllDoneRef: TFPHTTPClientPoolSimpleCallbackRef;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  if ActiveAsyncMethodCount=0 then
  begin
    if Assigned(aOnAllDoneRef) then
      aOnAllDoneRef();
    Exit;
  end;

  CreateWaitForAllRequestsThreadRef(aOnAllDoneRef, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;
{$ENDIF}

procedure TFPCustomHTTPClientAsyncPool.WaitForAllRequests(const aOnAllDone: TNotifyEvent;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  if ActiveAsyncMethodCount=0 then
  begin
    if Assigned(aOnAllDone) then
      aOnAllDone(Self);
    Exit;
  end;

  CreateWaitForAllRequestsThread(aOnAllDone, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

{ TFPHTTPClientAsyncPoolRequestThread }

constructor TFPHTTPClientAsyncPoolRequestThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aItem: TFPHTTPClientAsyncPoolRequestQueueItem; aClient: TFPHTTPClient);
begin
  fItem := aItem;
  fClient := aClient;

  inherited Create(aPool);
end;

destructor TFPHTTPClientAsyncPoolRequestThread.Destroy;
begin
  fItem.Thread := nil;
  fItem.Free;

  inherited Destroy;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OnDataReceived(Sender: TObject; const aContentLength, aCurrentPos: Int64);
begin
  OnDataReceivedSend(Sender, pdDataReceived, aCurrentPos, aContentLength);
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OnDataReceivedSend(Sender: TObject;
  const aDirection: TFPHTTPClientPoolProgressDirection; const aCurrentPos, aContentLength: Int64);
var
  xStop: Boolean;
begin
  LockProperties;
  try
    xStop := False;
    ExecOnProgress(aDirection, aCurrentPos, aContentLength, xStop);

    if xStop or Terminated then
      (Sender as TFPCustomHTTPClient).Terminate;
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OnDataSent(Sender: TObject; const aContentLength, aCurrentPos: Int64);
begin
  OnDataReceivedSend(Sender, pdDataSent, aContentLength, aCurrentPos);
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OwnerDestroyed;
begin
  inherited;

  if Assigned(Request) then
    Request.OwnerDestroyed;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.TerminatedSet;
begin
  inherited TerminatedSet;
  fClient.Terminate;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnInit;
begin
  if Assigned(Request) then
    Request.DoOnInit(fClient);
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
  const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
begin
  if Assigned(Request) then
    Request.DoOnProgress(Self, aDirection, aCurrentPos, aContentLength, ioStop);
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnInit;
begin
  LockProperties;
  try
    if fItem.Request.SynchronizeOnInit then
      Synchronize(@DoOnInit)
    else
      DoOnInit;
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
  const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
begin
  DoOnProgress(aDirection, aCurrentPos, aContentLength, ioStop);
end;

procedure TFPHTTPClientAsyncPoolRequestThread.Execute;
begin
  // don't LockProperties here - Request.Headers/ContentType/URLData/Method/URL/ResponseStream/AllowedResponseCodes are read-only
  try
    try
      fClient.ConnectTimeout := Request.ConnectTimeout;
      fClient.IOTimeout := Request.IOTimeout;

      fClient.RequestHeaders.Text := Request.Headers;
      if Request.ContentType<>'' then
        fClient.AddHeader(fClient.RequestHeaders, HeaderContentType, Request.ContentType);
      if Length(Request.URLData)>0 then
        fClient.RequestBody := TBytesStream.Create(Request.URLData);

      ExecOnInit;

      fClient.OnDataReceived := @OnDataReceived;
      fClient.OnDataSent := @OnDataSent;

      if Terminated then
      begin
        Result.MethodResult := mrAbortedByClient;
        Exit;
      end;

      try
        fClient.HTTPMethod(Request.Method, Request.URL, Result.ResponseStream, Request.AllowedResponseCodes);
      finally
        fClient.RequestBody.Free;
        fClient.RequestBody := nil;
      end;
      Result.ResponseStream.Position := 0;
      if Terminated then
      begin
        Result.MethodResult := mrAbortedByClient;
      end else
      begin
        Result.MethodResult := mrSuccess;
        Result.ResponseStatusCode := fClient.ResponseStatusCode;
        Result.ResponseStatusText := fClient.ResponseStatusText;
        Result.ResponseHeaders.Assign(fClient.ResponseHeaders);
      end;
    except
      on E: TObject do
      begin
        if Terminated then // client terminated the connection -> it has priority above mrAbortedWithException
          Result.MethodResult := mrAbortedByClient
        else
          Result.MethodResult := mrAbortedWithException;
        Result.ExceptionClass := E.ClassType;
        if E is Exception then
          Result.ExceptionMessage := Exception(E).Message;
      end;
    end;
  finally
    try
      Pool.ReleaseClient(fItem.Request, fClient);
      fClient := nil; // do not use fClient - it doesn't belong here anymore
    except
    end;
  end;
end;

function TFPHTTPClientAsyncPoolRequestThread.GetRequest: TFPHTTPClientAbstractAsyncPoolRequest;
begin
  if Assigned(fItem) then
    Result := fItem.Request
  else
    Result := nil;
end;

function TFPHTTPClientAsyncPoolRequestThread.GetResult: TFPHTTPClientPoolResult;
begin
  if Assigned(fItem) then
    Result := fItem.Result
  else
    Result := nil;
end;

end.

