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
  Classes, SysUtils, fphttpclient, httpprotocol, URIParser, syncobjs, ssockets, DateUtils, FPHTTPClientPool;

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
    procedure DoOnInit(const aClient: TFPHTTPClient); virtual; abstract;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); virtual; abstract;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); virtual; abstract;
    function HasProgress: Boolean; virtual; abstract;
    procedure OwnerDestroyed; virtual;
  public
    constructor Create;
  public
    property URLDataString: string read GetURLDataString write SetURLDataString;
    property Host: string read GetHost;
  end;

  TFPHTTPClientAsyncPoolRequest = class(TFPHTTPClientAbstractAsyncPoolRequest)
  protected
    procedure DoOnInit(const aClient: TFPHTTPClient); override;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); override;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); override;
    function HasProgress: Boolean; override;
    procedure OwnerDestroyed; override;
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
    procedure DoOnInit(const aClient: TFPHTTPClient); override;
    procedure DoOnFinish(const aResult: TFPHTTPClientPoolResult); override;
    procedure DoOnProgress(Sender: TFPHTTPClientAsyncPoolRequestThread; const aDirection: TFPHTTPClientPoolProgressDirection;
      const aPosition, aContentLength: Int64; var ioStop: Boolean); override;
    function HasProgress: Boolean; override;
    procedure OwnerDestroyed; override;
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

  TFPHTTPClientAsyncPoolThread = class(TThread)
  private
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
    // access only through LockProperties
    function GetOwner: TComponent; override;

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
    fRequest: TFPHTTPClientAbstractAsyncPoolRequest;

    fClient: TFPHTTPClient;
    fResult: TFPHTTPClientPoolResult;

    procedure OnDataReceived(Sender: TObject; const aContentLength, aCurrentPos: Int64);
    procedure OnDataSent(Sender: TObject; const aContentLength, aCurrentPos: Int64);

    // the ExecOn* methods call their DoOn* counterparts - do the synchronisation here
    procedure ExecOnInit;
    procedure ExecOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
      const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
    procedure ExecOnFinish;
    procedure OnIdle(Sender: TObject; AOperation: TSocketOperationType; var AAbort: Boolean);
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
      aRequest: TFPHTTPClientAbstractAsyncPoolRequest; aClient: TFPHTTPClient); virtual;
    destructor Destroy; override;
  public
    // access only through LockProperties
    property Request: TFPHTTPClientAbstractAsyncPoolRequest read fRequest;
    function GetOwner: TComponent; override;
  end;

  TFPHTTPClientAsyncPoolRequestQueueItem = class(TObject)
  public
    Pool: TFPCustomHTTPClientAsyncPool;
    Clients: TFPCustomHTTPClients;
    BreakUTC: TDateTime;
    Request: TFPHTTPClientAbstractAsyncPoolRequest;
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
    fDoOnAbortedFinishSynchronizedRequest: TFPHTTPClientAbstractAsyncPoolRequest;
    procedure ExecOnAbortedFinish(var ioRequest: TFPHTTPClientAbstractAsyncPoolRequest);
    procedure DoOnAbortedFinishSynchronized;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CreatePool: TFPCustomHTTPClientPool; virtual;
    function CreateRequestThread(aRequest: TFPHTTPClientAbstractAsyncPoolRequest; aClient: TFPHTTPClient): TFPHTTPClientAsyncPoolRequestThread; virtual;
    function CreateWaitForAllRequestsThread(const aOnAllDone: TNotifyEvent; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolWaitForAllThread; virtual;
    {$IFDEF use_functionreferences}
    function CreateWaitForAllRequestsThreadRef(const aOnAllDone: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean;
      const aOwner: TComponent; const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolWaitForAllThreadRef; virtual;
    {$ENDIF}

    // support for MaxClientsPerServer (add requests that wait for a client to a queue)
    procedure AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime; const aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
    procedure ReleaseClient(const aURL: string; const aClient: TFPHTTPClient);
    procedure DoOnAbortedFinish(var ioRequest: TFPHTTPClientAbstractAsyncPoolRequest); virtual;

    procedure LockWorkingThreads(out outWorkingThreads, outWaitingQueue: TList);
    procedure UnlockWorkingThreads;
  public
    // send an asynchronous HTTP request
    procedure AsyncMethod(aRequest: TFPHTTPClientAbstractAsyncPoolRequest); overload;

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
    property MaxClientsPerServer: Integer read GetMaxClientsPerServer write SetMaxClientsPerServer;
  end;

implementation

{ TFPHTTPClientAsyncPoolRequestRef }

procedure TFPHTTPClientAsyncPoolRequestRef.DoOnFinish(const aResult: TFPHTTPClientPoolResult);
begin
  if Assigned(OnFinish) then
    OnFinish(aResult);
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

function TFPHTTPClientAsyncPoolRequestRef.HasProgress: Boolean;
begin
  Result := Assigned(OnProgress);
end;

procedure TFPHTTPClientAsyncPoolRequestRef.OwnerDestroyed;
begin
  inherited OwnerDestroyed;

  OnInit := nil;
  OnProgress := nil;
  if not ExecuteOnFinishOnOwnerDestroy then
    OnFinish := nil;
end;

{ TFPHTTPClientAsyncPoolRequest }

procedure TFPHTTPClientAsyncPoolRequest.DoOnFinish(const aResult: TFPHTTPClientPoolResult);
begin
  if Assigned(OnFinish) then
    OnFinish(aResult);
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

function TFPHTTPClientAsyncPoolRequest.HasProgress: Boolean;
begin
  Result := Assigned(OnProgress);
end;

procedure TFPHTTPClientAsyncPoolRequest.OwnerDestroyed;
begin
  inherited OwnerDestroyed;

  OnInit := nil;
  OnProgress := nil;
  if not ExecuteOnFinishOnOwnerDestroy then
    OnFinish := nil;
end;

{ TFPHTTPClientAsyncPoolWaitForAllThreadRef }

constructor TFPHTTPClientAsyncPoolWaitForAllThreadRef.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aOnAllDone: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent;
  const aTimeoutMS: Integer);
begin

end;

procedure TFPHTTPClientAsyncPoolWaitForAllThreadRef.DoOnAllDone;
begin
  if Assigned(fOnAllDone) then
    fOnAllDone;
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThreadRef.OwnerDestroyed;
begin
  inherited OwnerDestroyed;

  fOnAllDone := nil;
end;

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

  inherited Create(aPool, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.DoOnAllDone;
begin
  if Assigned(fOnAllDone) then
    fOnAllDone(Self);
end;

procedure TFPHTTPClientAsyncPoolWaitForAllThread.OwnerDestroyed;
begin
  inherited OwnerDestroyed;

  fOnAllDone := nil;
end;

{ TFPHTTPClientAsyncPoolCustomWaitForAllThread }

constructor TFPHTTPClientAsyncPoolCustomWaitForAllThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  inherited Create(aPool);

  fSynchronizeOnAllDone := aSynchronizeOnAllDone;
  fOwner := aOwner;
  fTimeoutMS := aTimeoutMS;
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

function TFPHTTPClientAsyncPoolCustomWaitForAllThread.GetOwner: TComponent;
begin
  Result := fOwner;
end;

procedure TFPHTTPClientAsyncPoolCustomWaitForAllThread.OwnerDestroyed;
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

procedure TFPCustomHTTPClientAsyncPool.AsyncMethod(aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
var
  xClients: TFPCustomHTTPClients;
  xBreakUTC: TDateTime;
  xURI: TURI;
  xClient: TFPHTTPClient;
begin
  fWorkingThreads.LockList;
  try
    if fBlockRequestsCounter<>0 then
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
    fWorkingThreads.UnlockList;
    aRequest.Free;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.BlockNewRequests;
begin
  fWorkingThreads.LockList;
  Inc(fBlockRequestsCounter);
  fWorkingThreads.UnlockList;
end;

function TFPCustomHTTPClientAsyncPool.CreatePool: TFPCustomHTTPClientPool;
begin
  Result := TFPCustomHTTPClientPool.Create(Self);
end;

function TFPCustomHTTPClientAsyncPool.CreateRequestThread(aRequest: TFPHTTPClientAbstractAsyncPoolRequest;
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

function TFPCustomHTTPClientAsyncPool.CreateWaitForAllRequestsThreadRef(
  const aOnAllDone: TFPHTTPClientPoolSimpleCallbackRef; const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent;
  const aTimeoutMS: Integer): TFPHTTPClientAsyncPoolWaitForAllThreadRef;
begin
  Result := TFPHTTPClientAsyncPoolWaitForAllThreadRef.Create(Self, aOnAllDone, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
end;

constructor TFPCustomHTTPClientAsyncPool.Create(AOwner: TComponent);
begin
  fWorkingThreads := TThreadList.Create;
  fWaitingQueue := TList.Create;
  fHttpPool := CreatePool;
  fDoOnAbortedFinishSynchronizedCS := TCriticalSection.Create;

  inherited Create(AOwner);
end;

procedure TFPCustomHTTPClientAsyncPool.AddToQueue(const aClients: TFPCustomHTTPClients; const aBreakUTC: TDateTime;
  const aRequest: TFPHTTPClientAbstractAsyncPoolRequest);
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
  xRequest: TFPHTTPClientAbstractAsyncPoolRequest;
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

procedure TFPCustomHTTPClientAsyncPool.DoOnAbortedFinish(var ioRequest: TFPHTTPClientAbstractAsyncPoolRequest);
var
  xResult: TFPHTTPClientPoolResult;
begin
  xResult := TFPHTTPClientPoolResult.Create(ioRequest);
  try
    xResult.MethodResult := mrAbortedByClient;
    ioRequest.DoOnFinish(xResult);
    ioRequest := nil; // ioRequest gets destroyed in xResult.Free
  finally
    xResult.Free;
  end;
end;

procedure TFPCustomHTTPClientAsyncPool.DoOnAbortedFinishSynchronized;
begin
  DoOnAbortedFinish(fDoOnAbortedFinishSynchronizedRequest);
end;

procedure TFPCustomHTTPClientAsyncPool.ExecOnAbortedFinish(var ioRequest: TFPHTTPClientAbstractAsyncPoolRequest);
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
  fWorkingThreads.LockList;
  Dec(fBlockRequestsCounter);
  fWorkingThreads.UnlockList;
end;

procedure TFPCustomHTTPClientAsyncPool.UnlockWorkingThreads;
begin
  fWorkingThreads.UnlockList;
end;

procedure TFPCustomHTTPClientAsyncPool.WaitForAllRequests(const aOnAllDoneRef: TFPHTTPClientPoolSimpleCallbackRef;
  const aSynchronizeOnAllDone: Boolean; const aOwner: TComponent; const aTimeoutMS: Integer);
begin
  if ActiveAsyncMethodCount=0 then
  begin
    if Assigned(aOnAllDoneRef) then
      aOnAllDoneRef;
    Exit;
  end;

  if Assigned(aOwner) then
  begin
    FreeNotification(aOwner);
    // We do not remove the notification with RemoveFreeNotification().
    // It would be unsafe if more requests are sent with the same owner.
    // That is fine - it will be removed automatically when the owner is destroyed.
  end;
  CreateWaitForAllRequestsThreadRef(aOnAllDoneRef, aSynchronizeOnAllDone, aOwner, aTimeoutMS);
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

{ TFPHTTPClientAsyncPoolRequestThread }

constructor TFPHTTPClientAsyncPoolRequestThread.Create(aPool: TFPCustomHTTPClientAsyncPool;
  aRequest: TFPHTTPClientAbstractAsyncPoolRequest; aClient: TFPHTTPClient);
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
    if Request.HasProgress then
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

procedure TFPHTTPClientAsyncPoolRequestThread.OnIdle(Sender: TObject; AOperation: TSocketOperationType;
  var AAbort: Boolean);
begin
  if Terminated then
    AAbort := True;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.OwnerDestroyed;
begin
  inherited;

  fRequest.OwnerDestroyed;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnInit;
begin
  LockProperties;
  try
    Request.DoOnInit(fClient);
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.DoOnProgress(const aDirection: TFPHTTPClientPoolProgressDirection;
  const aCurrentPos, aContentLength: Integer; var ioStop: Boolean);
begin
  LockProperties;
  try
    if Request.HasProgress then
      Request.DoOnProgress(Self, aDirection, aCurrentPos, aContentLength, ioStop);
  finally
    UnlockProperties;
  end;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnFinish;
begin
  if Request.SynchronizeOnFinish then
    Synchronize(@DoOnFinish)
  else
    DoOnFinish;
end;

procedure TFPHTTPClientAsyncPoolRequestThread.ExecOnInit;
begin
  if Request.SynchronizeOnInit then
    Synchronize(@DoOnInit)
  else
    DoOnInit;
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
      fClient.OnIdle := @OnIdle;

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
  finally
    try
      Pool.ReleaseClient(Request.URL, fClient);
      fClient := nil; // do not use fClient - it doesn't belong here anymore
      ExecOnFinish;
    except
    end;
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
    Request.DoOnFinish(fResult);
    // always destroy fResult so that the Request's destructor is synchronised if DoOnFinish is synchronised
    fResult.Free;
    fResult := nil;
  finally
    UnlockProperties;
  end;
end;

end.



