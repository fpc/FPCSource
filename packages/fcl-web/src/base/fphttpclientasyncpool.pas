unit FPHTTPClientAsyncPool;

{
  Default HTTP Client asynchronous pool.
  Events are not synchronized - the program has to synchronize within the events itself if necessary
    (e.g. with critical sections).

  If you are looking for a client pool that can be used in an LCL application and that does the synchronization for you,
    check (TODO: URL)
}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fphttpclient, httpprotocol, URIParser, syncobjs, DateUtils, FPHTTPClientPool;

type
  TFPHTTPClientPoolMethodResult = (mrSuccess, mrAbortedByClient, mrAbortedWithException);

  TFPHTTPClientAsyncPoolRequest = class;

  TFPHTTPClientPoolResult = class(TPersistent)
  private
    fExceptionClass: TClass;
    fExceptionMessage: string;

    fRequest: TFPHTTPClientAsyncPoolRequest;
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

    //class function Create(const aFromThread: TOHttpPoolThread): TFPHTTPClientPoolResult; static;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Request: TFPHTTPClientAsyncPoolRequest read fRequest;
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
    constructor Create(const aRequest: TFPHTTPClientAsyncPoolRequest);
    destructor Destroy; override;
  end;

  TFPHTTPClientAsyncPoolRequestThread = class;

  TFPHTTPClientPoolInit = procedure(const aRequest: TFPHTTPClientAsyncPoolRequest; const aClient: TFPHTTPClient) of object;
  TFPHTTPClientPoolFinish = procedure(const aResult: TFPHTTPClientPoolResult) of object;
  TFPHTTPClientPoolProgressDirection = (pdDataSent, pdDataReceived);
  TFPHTTPClientPoolProgress = procedure(
    Sender: TFPHTTPClientAsyncPoolRequestThread;
    const aDirection: TFPHTTPClientPoolProgressDirection;
    const aPosition, aContentLength: Int64; var ioStop: Boolean) of object;

  TFPCustomHTTPClientAsyncPool = class;
  TFPHTTPClientAsyncPoolRequest = class(TPersistent)
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
    // setup custom client properties
    OnInit: TFPHTTPClientPoolInit;
    // should OnInit be synchronized with the main thread?
    SynchronizeOnInit: Boolean;
    // read out the result
    OnFinish: TFPHTTPClientPoolFinish;
    // should OnFinish be synchronized with the main thread?
    SynchronizeOnFinish: Boolean;
    // progress callback
    OnProgress: TFPHTTPClientPoolProgress;

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
    function GetSelf: TFPHTTPClientAsyncPoolRequest;
  public
    constructor Create;
  public
    property URLDataString: string read GetURLDataString write SetURLDataString;
    property Host: string read GetHost;
  end;

  TFPHTTPClientAsyncPoolThread = class(TThread)
  strict private
    fPool: TFPCustomHTTPClientAsyncPool;
    fCSProperties: TCriticalSection;
  protected
    // access only through LockProperties
    procedure OwnerDestroyed; virtual;
  public
    property Pool: TFPCustomHTTPClientAsyncPool read fPool;

    // access only through LockProperties
    function GetOwner: TComponent; virtual; abstract;

    // lock&unlock read/write properties (properties are written currently only in OwnerDestroyed)
    procedure LockProperties;
    procedure UnlockProperties;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool);
    destructor Destroy; override;
  end;

  TFPHTTPClientAsyncPoolWaitForAllThread = class(TFPHTTPClientAsyncPoolThread)
  private
    fTimeoutMS: Integer;
    fOwner: TComponent;
    fOnAllDone: TNotifyEvent;
    fSynchronizeOnAllDone: Boolean;

    procedure ExecOnAllDone;
  protected

    procedure DoOnAllDone; virtual;

    procedure Execute; override;

    // access only through LockProperties
    procedure OwnerDestroyed; override;
  public
    // access only through LockProperties
    function GetOwner: TComponent; override;
  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool; aOnAllDone: TNotifyEvent;
      const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer);
  end;

  TFPHTTPClientAsyncPoolRequestThread = class(TFPHTTPClientAsyncPoolThread)
  private
    fRequest: TFPHTTPClientAsyncPoolRequest;

    fClient: TFPHTTPClient;
    fResult: TFPHTTPClientPoolResult;

    procedure OnDataReceived(Sender: TObject; const aContentLength, aCurrentPos: Int64);
    procedure OnDataSent(Sender: TObject; const aContentLength, aCurrentPos: Int64);
    procedure OnIdle(Sender: TObject);

    // the ExecOn* methods call their DoOn* counterparts - do the synchronisation here
    procedure ExecOnInit;
    procedure ExecOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
      const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
    procedure ExecOnFinish;
  protected
    // access only through LockProperties
    procedure OwnerDestroyed; override;
  protected
    procedure OnDataReceivedSend(Sender: TObject; const aDirection: TFPHTTPClientPoolProgressDirection; const aCurrentPos, aContentLength: Int64); virtual;
  protected
    property Client: TFPHTTPClient read fClient;
    property Result: TFPHTTPClientPoolResult read fResult;

    // the DoOn* methods do the actual work and can be synchronised by their ExecOn* counterparts
    // DoOnInit - executed when the request aquired a TFPHTTPClient to setup its extra properties
    //  should not be synchronized with Synchronize() - it slows down the execution. Better to use CriticalSections
    procedure DoOnInit; virtual;
    // DoOnProgress - show progress during upload&download
    //  should not be synchronized with Synchronize() - it slows down the execution. Better to use CriticalSections or Application.QueueAsyncCall in an LCL application
    procedure DoOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
      const aCurrentPos, aContentLength: Integer; var ioStop: Boolean); virtual;
    // DoOnFinish - executed when the request is done
    //  can happily be synchronized with Synchronize() because when called, the request connection is already released back to pool for reuse
    procedure DoOnFinish; virtual;
  protected
    procedure Execute; override;

  public
    constructor Create(aPool: TFPCustomHTTPClientAsyncPool;
      aRequest: TFPHTTPClientAsyncPoolRequest; aClient: TFPHTTPClient); virtual;
    destructor Destroy; override;
  public
    // access only through LockProperties
    property Request: TFPHTTPClientAsyncPoolRequest read fRequest;
    function GetOwner: TComponent; override;
  end;

  TFPHTTPClientAsyncPoolRequestQueueItem = class(TObject)
  public
    Pool: TFPCustomHTTPClientAsyncPool;
    Clients: TFPCustomHTTPClients;
    BreakUTC: TDateTime;
    Request: TFPHTTPClientAsyncPoolRequest;
  public
    destructor Destroy; override;
  end;

  TFPCustomHTTPClientAsyncPool = class(TComponent)
  private
    fHttpPool: TFPCustomHTTPClientPool;

    // do not access fWaitingQueue directly, use LockWorkingThreads() instead
    fWorkingThreads: TThreadList;
    fWaitingQueue: TList;

    fBlockRequestsCounter: Integer;
    function GetActiveAsyncMethodCount: Integer;
    function GetClientCount: Integer;
    function GetMaxClientsPerServer: Integer;
    function GetWaitingAsyncMethodCount: Integer;
    procedure SetMaxClientsPerServer(const aMaxClientsPerServer: Integer);

  private
    fDoOnAbortedFinishSynchronizedCS: TCriticalSection;
    fDoOnAbortedFinishSynchronizedRequest: TFPHTTPClientAsyncPoolRequest;
    procedure ExecOnAbortedFinish(var ioRequest: TFPHTTPClientAsyncPoolRequest);
    procedure DoOnAbortedFinishSynchronized;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CreateRequestThread(aRequest: TFPHTTPClientAsyncPoolRequest; aClient: TFPHTTPClient): TFPHTTPClientAsyncPoolRequestThread; virtual;
    function CreateWaitForAllRequestsThread(const aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolWaitForAllThread; virtual;
    procedure WaitForThreadsToFinish; virtual;

    // support for MaxClientsPerServer (add requests that wait for a client to a queue)
    procedure AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime; const aRequest: TFPHTTPClientAsyncPoolRequest);
    procedure ReleaseClient(const aURL: string; const aClient: TFPHTTPClient);
    procedure DoOnAbortedFinish(var ioRequest: TFPHTTPClientAsyncPoolRequest); virtual;

    procedure LockWorkingThreads(out outWorkingThreads, outWaitingQueue: TList);
    procedure UnlockWorkingThreads;
  public
    // send an asynchronous HTTP request
    procedure AsyncMethod(aRequest: TFPHTTPClientAsyncPoolRequest); overload;

    // stop all requests with Blocker
    procedure StopRequests(const aBlocker: TObject);
    // stop all requests with Owner and don't send results to Owner
    procedure OwnerDestroyed(const aOwner: TObject);

    procedure BlockNewRequests;
    procedure UnblockNewRequests;

    // wait until all requests are finished
    //  all new requests will be blocked in between
    procedure WaitForAllRequests(const aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    property ClientCount: Integer read GetClientCount;
    property ActiveAsyncMethodCount: Integer read GetActiveAsyncMethodCount;
    property WaitingAsyncMethodCount: Integer read GetWaitingAsyncMethodCount;
    property MaxClientsPerServer: Integer read GetMaxClientsPerServer write SetMaxClientsPerServer;
  end;

implementation

{ TFPHTTPClientAsyncPoolRequestQueueItem }

destructor TFPHTTPClientAsyncPoolRequestQueueItem.Destroy;
begin
  if Assigned(Request) then
  begin
    Pool.DoOnAbortedFinish(Request);
    Request.Free;
  end;
  inherited Destroy;
end;

{ TFPHTTPClientAsyncPoolWaitForAllThread }

constructor TFPHTTPClientAsyncPoolWaitForAllThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  fOnAllDone := aOnAllDone;
  fSynchronizeOnAllDone := aSynchronizeOnAllDone;
  fTimeoutMS := aTimeoutMS;
  fOwner := aOwner;

  inherited Create(aPool);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.DoOnAllDone;
begin
  if Assigned(fOnAllDone) then
    fOnAllDone(Self);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.ExecOnAllDone;
begin
  if not Assigned(fOnAllDone) then
    Exit;

  if fSynchronizeOnAllDone then
    Synchronize(@DoOnAllDone)
  else
    DoOnAllDone;
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.Execute;
var
  xBreak: TDateTime;
begin
  try
    Pool.BlockNewRequests;
    try
      if fTimeoutMS>0 then
        xBreak := IncMilliSecond(NowUTC, fTimeoutMS);
      while not Terminated and (Pool.ActiveAsyncMethodCount>0) and ((fTimeoutMS=0) or (NowUTC<xBreak)) do
        Sleep(10);
    finally
      Pool.UnblockNewRequests;
    end;
    if not Terminated then
      ExecOnAllDone;
  except
  end;
end;

function TFPHTTPClientAsyncPoolWaitForAllThread.GetOwner: TComponent;
begin
  Result := fOwner;
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.OwnerDestroyed;
begin
  inherited OwnerDestroyed;

  fOwner := nil;
end;

{ TFPHTTPClientAsyncPoolThread }

constructor TFPHTTPClientAsyncPoolThread.Create(aPool: TFPCustomHTTPClientAsyncPool);
begin
  fPool := aPool;
  fPool.fWorkingThreads.Add(Self);
  FreeOnTerminate := True;
  fCSProperties := TCriticalSection.Create;

  inherited Create(False);
end;

destructor TFPHTTPClientAsyncPoolThread.Destroy;
begin
  fPool.fWorkingThreads.Remove(Self);
  fCSProperties.Free;
  inherited Destroy;
end;

procedure TFPHTTPClientAsyncPoolThread.LockProperties;
begin
  fCSProperties.Enter;
end;

procedure TFPHTTPClientAsyncPoolThread.OwnerDestroyed;
begin
  Terminate;
end;

procedure TFPHTTPClientAsyncPoolThread.UnlockProperties;
begin
  fCSProperties.Leave;
end;

{ TFPHTTPClientAsyncPoolRequest }

constructor TFPHTTPClientAsyncPoolRequest.Create;
begin
  inherited Create;

  ConnectTimeout := 3000;
end;

function TFPHTTPClientAsyncPoolRequest.GetHost: string;
var
  xURI: TURI;
begin
  xURI := ParseURI(URL, False);
  Result := xURI.Host;
end;

function TFPHTTPClientAsyncPoolRequest.GetSelf: TFPHTTPClientAsyncPoolRequest;
begin
  Result := Self;
end;

function TFPHTTPClientAsyncPoolRequest.GetURLDataString: string;
begin
  Result := TEncoding.SystemEncoding.GetAnsiString(URLData);
end;

procedure TFPHTTPClientAsyncPoolRequest.SetURLDataString(const aURLDataString: string);
begin
  URLData := TEncoding.SystemEncoding.GetAnsiBytes(aURLDataString);
end;

{ TFPHTTPClientPoolResult }

constructor TFPHTTPClientPoolResult.Create(const aRequest: TFPHTTPClientAsyncPoolRequest);
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
  fRequest.Free;

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

procedure TFPCustomHTTPClientAsyncPool.AsyncMethod(aRequest: TFPHTTPClientAsyncPoolRequest);
var
  xClients: TFPCustomHTTPClients;
  xBreakUTC: TDateTime;
  xURI: TURI;
  xClient: TFPHTTPClient;
begin
  try
    if InterlockedExchangeAdd(fBlockRequestsCounter, 0)<>0 then
    begin
      DoOnAbortedFinish(aRequest);
      Exit;
    end;

    if Assigned(aRequest.Blocker) then
      StopRequests(aRequest.Blocker);
    if Assigned(aRequest.Owner) then
    begin
      FreeNotification(aRequest.Owner);
      // We do not remove the notification with RemoveFreeNotification().
      // It would be unsafe if more requests are sent with the same owner.
      // That is fine - it will be removed automatically when the owner is destroyed.
    end;

    xURI := ParseURI(aRequest.URL, False);
    xClients := fHttpPool.GetCreateServerClients(xURI.Host, xURI.Port);
    if aRequest.ClientTimeout>0 then
      xBreakUTC := IncMilliSecond(NowUTC, aRequest.ClientTimeout)
    else
      xBreakUTC := 0;
    xClient := xClients.GetClient;
    if Assigned(xClient) then
      // client is available -> create request thread
      CreateRequestThread(aRequest, xClient)
    else
      // no client available -> add to queue
      AddToQueue(xClients, xBreakUTC, aRequest);
    aRequest := nil; // don't destroy aRequest
  finally
    aRequest.Free;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.BlockNewRequests;
begin
  InterlockedIncrement(fBlockRequestsCounter);
end;

function TFPCustomHTTPClientAsyncPool.CreateRequestThread(aRequest: TFPHTTPClientAsyncPoolRequest;
  aClient: TFPHTTPClient): TFPHTTPClientAsyncPoolRequestThread;
begin
  Result := TFPHTTPClientAsyncPoolRequestThread.Create(Self, aRequest, aClient);
end;

function TFPCustomHTTPClientAsyncPool.CreateWaitForAllRequestsThread(const aOnAllDone: TNotifyEvent;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent;
  const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolWaitForAllThread;
begin
  Result := TFPHTTPClientAsyncPoolWaitForAllThread.Create(Self, aOnAllDone, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

constructor TFPCustomHTTPClientAsyncPool.Create(AOwner: TComponent);
begin
  fWorkingThreads := TThreadList.Create;
  fWaitingQueue := TList.Create;
  fHttpPool := TFPCustomHTTPClientPool.Create(Self);
  fDoOnAbortedFinishSynchronizedCS := TCriticalSection.Create;

  inherited Create(AOwner);
end;

procedure TFPCustomHTTPClientAsyncPool.AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime;
  const aRequest: TFPHTTPClientAsyncPoolRequest);
var
  xNewItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  xThreads, xQueue: TList;
begin
  LockWorkingThreads(xThreads, xQueue);
  try
    xNewItem := TFPHTTPClientAsyncPoolRequestQueueItem.Create;
    xNewItem.Pool := Self;
    xNewItem.Clients := aClients;
    xNewItem.BreakUTC := aBreakUTC;
    xNewItem.Request := aRequest;
    xQueue.Add(xNewItem);
  finally
    UnlockWorkingThreads;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.ReleaseClient(const aURL: string; const aClient: TFPHTTPClient);
var
  xURI: TURI;
  xClients: TFPCustomHTTPClients;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
  xRequest: TFPHTTPClientAsyncPoolRequest;
  I: Integer;
  xThreads, xQueue: TList;
begin
  LockWorkingThreads(xThreads, xQueue);
  try
    xURI := ParseURI(aURL, False);
    xClients := fHttpPool.GetCreateServerClients(xURI.Host, xURI.Port);

    I := 0;
    while I<xQueue.Count do
    begin
      xItem := TFPHTTPClientAsyncPoolRequestQueueItem(xQueue[I]);
      if (CompareDateTime(xItem.BreakUTC, 0)<>0) and (CompareDateTime(xItem.BreakUTC, NowUTC)<0) then
      begin // timeout is over
        xItem.Free;
        xQueue.Delete(I);
      end else
      if xClients=xItem.Clients then
      begin // found a request waiting in queue
        xRequest := xItem.Request;
        xItem.Request := nil; // do not destroy/abort request
        xItem.Free;
        xQueue.Delete(I);

        CreateRequestThread(xRequest, aClient);
        Exit;
      end else
        Inc(I);
    end;

    // no waiting request found - release the client
    fHttpPool.ReleaseClient(xURI.Host, xURI.Port, aClient);
  finally
    UnlockWorkingThreads;
  end;
end;

destructor TFPCustomHTTPClientAsyncPool.Destroy;
  procedure _TerminateAll(_List: TList);
  var
    I: Integer;
  begin
    for I := 0 to _List.Count-1 do
      TThread(_List[I]).Terminate;
  end;
  procedure _ClearWaitingQueue(_List: TList);
  var
    I: Integer;
  begin
    for I := 0 to _List.Count-1 do
      TObject(_List[I]).Free;
    _List.Clear;
  end;
var
  xThreads, xQueue: TList;
begin
  LockWorkingThreads(xThreads, xQueue);
  try
    _TerminateAll(xThreads);
    _ClearWaitingQueue(xQueue);
  finally
    UnlockWorkingThreads;
  end;
  while ActiveAsyncMethodCount>0 do
  begin
    if (ThreadID=MainThreadID) then // we are synchronizing events - call CheckSynchronize to prevent deadlock in the main thread
      CheckSynchronize(10)
    else
      Sleep(10);
  end;
  fWorkingThreads.Free;
  fWaitingQueue.Free;
  fDoOnAbortedFinishSynchronizedCS.Free;

  inherited Destroy;
end;

procedure TFPCustomHTTPClientAsyncPool.DoOnAbortedFinish(var ioRequest: TFPHTTPClientAsyncPoolRequest);
var
  xResult: TFPHTTPClientPoolResult;
begin
  if Assigned(ioRequest.OnFinish) then
  begin
    xResult := TFPHTTPClientPoolResult.Create(ioRequest);
    try
      xResult.MethodResult := mrAbortedByClient;
      ioRequest.OnFinish(xResult);
      ioRequest := nil; // ioRequest gets destroyed in xResult.Free
    finally
      xResult.Free;
    end;
  end else
  begin
    ioRequest.Free;
    ioRequest := nil;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.DoOnAbortedFinishSynchronized;
begin
  DoOnAbortedFinish(fDoOnAbortedFinishSynchronizedRequest);
end;

procedure TFPCustomHTTPClientAsyncPool.ExecOnAbortedFinish(var ioRequest: TFPHTTPClientAsyncPoolRequest);
begin
  // always synchronize - even if OnFinish is nil, so that ioRequest gets destroyed in the main thread
  //  if somebody had the idea to do something with the LCL in a custom request destructor
  //  -- don't do: if not Assigned(ioRequest.OnFinish) then Exit;

  if ioRequest.SynchronizeOnFinish and (ThreadID<>MainThreadID) then
  begin
    fDoOnAbortedFinishSynchronizedCS.Enter; // we need to protect fDoOnAbortedFinishSynchronizedRequest
    try
      fDoOnAbortedFinishSynchronizedRequest := ioRequest;
      TThread.Synchronize(nil, @DoOnAbortedFinishSynchronized);
      ioRequest := nil;
    finally
      fDoOnAbortedFinishSynchronizedCS.Free;
    end;
  end else
    DoOnAbortedFinish(ioRequest);
end;

function TFPCustomHTTPClientAsyncPool.GetActiveAsyncMethodCount: Integer;
var
  xThreads, xQueue: TList;
begin
  LockWorkingThreads(xThreads, xQueue);
  Result := xThreads.Count;
  UnlockWorkingThreads;
end;

function TFPCustomHTTPClientAsyncPool.GetClientCount: Integer;
begin
  Result := fHttpPool.ClientCount;
end;

function TFPCustomHTTPClientAsyncPool.GetMaxClientsPerServer: Integer;
begin
  Result := fHttpPool.MaxClientsPerServer;
end;

function TFPCustomHTTPClientAsyncPool.GetWaitingAsyncMethodCount: Integer;
var
  xThreads, xQueue: TList;
begin
  LockWorkingThreads(xThreads, xQueue);
  Result := xQueue.Count;
  UnlockWorkingThreads;
end;

procedure TFPCustomHTTPClientAsyncPool.LockWorkingThreads(out outWorkingThreads, outWaitingQueue: TList);
begin
  outWorkingThreads := fWorkingThreads.LockList;
  outWaitingQueue := fWaitingQueue;
end;

procedure TFPCustomHTTPClientAsyncPool.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation=opRemove then
    OwnerDestroyed(AComponent);

  inherited Notification(AComponent, Operation);
end;

procedure TFPCustomHTTPClientAsyncPool.SetMaxClientsPerServer(const aMaxClientsPerServer: Integer);
begin
  fHttpPool.MaxClientsPerServer := aMaxClientsPerServer;
end;

procedure TFPCustomHTTPClientAsyncPool.StopRequests(const aBlocker: TObject);
var
  I: Integer;
  xThreads, xQueue: TList;
  xThread: TFPHTTPClientAsyncPoolRequestThread;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
begin
  LockWorkingThreads(xThreads, xQueue);
  try
    for I := 0 to xThreads.Count-1 do
    begin
      if TObject(xThreads[I]) is TFPHTTPClientAsyncPoolRequestThread then
      begin
        xThread := TFPHTTPClientAsyncPoolRequestThread(TObject(xThreads[I]));
        xThread.LockProperties;
        try
          if xThread.Request.Blocker=aBlocker then
            xThread.Terminate;
        finally
          xThread.UnlockProperties;
        end;
      end;
    end;

    for I := xQueue.Count-1 downto 0 do
    begin
      xItem := TFPHTTPClientAsyncPoolRequestQueueItem(xQueue[I]);
      if xItem.Request.Blocker=aBlocker then
      begin // found a request waiting in queue
        xItem.Free;
        xQueue.Delete(I);
      end;
    end;
  finally
    UnlockWorkingThreads;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.OwnerDestroyed(const aOwner: TObject);
var
  I: Integer;
  xList, xQueue: TList;
  xThread: TFPHTTPClientAsyncPoolThread;
  xItem: TFPHTTPClientAsyncPoolRequestQueueItem;
begin
  LockWorkingThreads(xList, xQueue);
  try
    for I := 0 to xList.Count-1 do
    begin
      if TObject(xList[I]) is TFPHTTPClientAsyncPoolThread then
      begin
        xThread := TFPHTTPClientAsyncPoolThread(TObject(xList[I]));
        xThread.LockProperties;
        try
          if xThread.GetOwner=aOwner then
            xThread.OwnerDestroyed;
        finally
          xThread.UnlockProperties;
        end;
      end;
    end;

    for I := xQueue.Count-1 downto 0 do
    begin
      xItem := TFPHTTPClientAsyncPoolRequestQueueItem(xQueue[I]);
      if xItem.Request.Owner=aOwner then
      begin // found a request waiting in queue
        xItem.Free;
        xQueue.Delete(I);
      end;
    end;
  finally
    UnlockWorkingThreads;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.UnblockNewRequests;
begin
  InterlockedDecrement(fBlockRequestsCounter);
end;

procedure TFPCustomHTTPClientAsyncPool.UnlockWorkingThreads;
begin
  fWorkingThreads.UnlockList;
end;

procedure TFPCustomHTTPClientAsyncPool.WaitForAllRequests(const aOnAllDone: TNotifyEvent;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  if ActiveAsyncMethodCount=0 then
  begin
    if Assigned(aOnAllDone) then
      aOnAllDone(Self);
    Exit;
  end;

  if Assigned(aOwner) then
  begin
    FreeNotification(aOwner);
    // We do not remove the notification with RemoveFreeNotification().
    // It would be unsafe if more requests are sent with the same owner.
    // That is fine - it will be removed automatically when the owner is destroyed.
  end;
  CreateWaitForAllRequestsThread(aOnAllDone, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

procedure TFPCustomHTTPClientAsyncPool.WaitForThreadsToFinish;
begin
  Sleep(10);
end;

{ TFPHTTPClientAsyncPoolRequestThread }

constructor TFPHTTPClientAsyncPoolRequestThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aRequest: TFPHTTPClientAsyncPoolRequest; aClient: TFPHTTPClient);
begin
  fRequest := aRequest;
  fResult := TFPHTTPClientPoolResult.Create(fRequest);
  fClient := aClient;

  if Assigned(aRequest.ResponseStream) then
  begin
    fResult.ResponseStream := aRequest.ResponseStream;
    fResult.OwnsResponseStream := aRequest.OwnsResponseStream;
  end else
  begin
    fResult.ResponseStream := TBytesStream.Create;
    fResult.OwnsResponseStream := True;
  end;

  inherited Create(aPool);
end;

destructor TFPHTTPClientAsyncPoolRequestThread.Destroy;
begin
  fResult.Free;
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
    if Assigned(Request.OnProgress) then
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

procedure TFPHTTPClientAsyncPoolRequestThread.OnIdle(Sender: TObject);
begin
  if Terminated then
    (Sender as TFPCustomHTTPClient).Terminate;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OwnerDestroyed;
begin
  inherited;

  fRequest.Owner := nil;
  fRequest.OnFinish := nil;
  fRequest.OnProgress := nil;
  fRequest.OnInit := nil;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnInit;
begin
  LockProperties;
  try
    if Assigned(Request.OnInit) then
      Request.OnInit(Request, fClient);
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
  const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
begin
  LockProperties;
  try
    if Assigned(Request.OnProgress) then
      Request.OnProgress(Self, aDirection, aCurrentPos, aContentLength, ioStop);
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnFinish;
var
  xUnlocked: Boolean;
begin
  xUnlocked := False;
  LockProperties;
  try
    if Request.SynchronizeOnFinish then
    begin
      UnlockProperties;
      xUnlocked := True;
      Synchronize(@DoOnFinish)
    end
    else
      DoOnFinish;
  finally
    if not xUnlocked then
      UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnInit;
var
  xUnlocked: Boolean;
begin
  xUnlocked := False;
  LockProperties;
  try
    if not Assigned(Request.OnInit) then
      Exit;

    if Request.SynchronizeOnInit then
    begin
      UnlockProperties;
      xUnlocked := True;
      Synchronize(@DoOnInit);
    end else
      DoOnInit;
  finally
    if not xUnlocked then
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
    fClient.OnIdle := @OnIdle;

    if Terminated then
    begin
      fResult.MethodResult := mrAbortedByClient;
      Exit;
    end;

    try
      fClient.HTTPMethod(Request.Method, Request.URL, fResult.ResponseStream, Request.AllowedResponseCodes);
    finally
      fClient.RequestBody.Free;
      fClient.RequestBody := nil;
    end;
    fResult.ResponseStream.Position := 0;
    if Terminated then
    begin
      fResult.MethodResult := mrAbortedByClient;
    end else
    begin
      fResult.MethodResult := mrSuccess;
      fResult.ResponseStatusCode := fClient.ResponseStatusCode;
      fResult.ResponseStatusText := fClient.ResponseStatusText;
      fResult.ResponseHeaders.Assign(fClient.ResponseHeaders);
    end;
  except
    on E: TObject do
    begin
      if Terminated then // client terminated the connection -> it has priority above mrAbortedWithException
        fResult.MethodResult := mrAbortedByClient
      else
        fResult.MethodResult := mrAbortedWithException;
      fResult.ExceptionClass := E.ClassType;
      if E is Exception then
        fResult.ExceptionMessage := Exception(E).Message;
    end;
  end;
  try
    Pool.ReleaseClient(Request.URL, fClient);
    fClient := nil; // do not use fClient - it doesn't belong here anymore
    ExecOnFinish;
  except
  end;
end;

function TFPHTTPClientAsyncPoolRequestThread.GetOwner: TComponent;
begin
  Result := fRequest.Owner;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnFinish;
begin
  LockProperties;
  try
    if Assigned(Request.OnFinish) then
      Request.OnFinish(fResult);
    // always destroy fResult so that the Request's destructor is synchronised if DoOnFinish is synchronised
    fResult.Free;
    fResult := nil;
  finally
    UnlockProperties;
  end;
end;

end.

