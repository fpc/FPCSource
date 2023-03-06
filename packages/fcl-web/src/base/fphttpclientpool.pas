{$IFNDEF FPC_DOTTEDUNITS}
unit FPHTTPClientPool;
{$ENDIF FPC_DOTTEDUNITS}

{
  Simple thread-safe HTTP Client pool.

  The idea is that a thread asks in a loop for an available client with GetClient() until it gets one
  or forces a new client with aForceNewClient.
  Once the thread finished working with the client, it pushes the client back with ReleaseClient().
}

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.Http.Client, Fcl.UriParser, System.SyncObjs, System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fphttpclient, URIParser, syncobjs, DateUtils;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFPHTTPClientList = class(TList)
  private
    function GetItem(Index: Integer): TFPHTTPClient;
  public
    property Items[Index: Integer]: TFPHTTPClient read GetItem; default;
  end;

  TFPCustomHTTPClientPool = class;

  TFPCustomHTTPClients = class
  private
    fAvailableClients: TFPHTTPClientList;
    fBusyClients: TFPHTTPClientList;
    fPool: TFPCustomHTTPClientPool;

    fCS: TCriticalSection;
    function GetAvailableClientCount: Integer;
    function GetBusyClientCount: Integer;
  public
    constructor Create(const aPool: TFPCustomHTTPClientPool);
    destructor Destroy; override;
  public
    property AvailableClientCount: Integer read GetAvailableClientCount;
    property BusyClientCount: Integer read GetBusyClientCount;

    function GetClient(const aForceNewClient: Boolean = False): TFPHTTPClient;
    procedure ReleaseClient(const aClient: TFPHTTPClient);
  end;

  TFPCustomHTTPClientPool = class(TComponent)
  private
    fMaxClientsPerServer: Integer;
    fServerClients: TStringList; // key = host:port; object = TFPCustomHTTPClients
    fServerClientsCS: TCriticalSection;

    function CreateClient: TFPHTTPClient;
    function GetAvailableClientCount: Integer;
    function GetBusyClientCount: Integer;
    function GetClientCount: Integer;
    function GetServerClients(I: Integer): TFPCustomHTTPClients;
  protected
    function DoCreateClient: TFPHTTPClient; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    // client count that is available for GetClient (all servers)
    property AvailableClientCount: Integer read GetAvailableClientCount;
    // client count that is currently busy (all servers)
    property BusyClientCount: Integer read GetBusyClientCount;
    // AvailableClientCount + BusyClientCount
    property ClientCount: Integer read GetClientCount;
  public
    // a limit for clients per server
    property MaxClientsPerServer: Integer read fMaxClientsPerServer write fMaxClientsPerServer;
    // find a free client to be reused
    //  create a new one if there are no clients available but the client limit hasn't been reached
    //  if the limit has been reached and there are no available clients, return nil
    //  set aForceNewClient to True if you want to override the max client count
    function GetClient(const aURL: string; const aForceNewClient: Boolean = False): TFPHTTPClient; overload;
    function GetClient(const aHost: string; const aPort: Word; const aForceNewClient: Boolean = False): TFPHTTPClient; overload;
    // every client must be pushed back when it is done
    procedure ReleaseClient(const aURL: string; const aClient: TFPHTTPClient); overload;
    procedure ReleaseClient(const aHost: string; const aPort: Word; const aClient: TFPHTTPClient); overload;
    // get the client list for a host
    function GetCreateServerClients(const aHost: string; const aPort: Word): TFPCustomHTTPClients;
  end;

  EHTTPPool = class(Exception);

implementation

resourcestring
  SErrCannotPushClient = 'Cannot push client.';

{ TFPHTTPClientList }

function TFPHTTPClientList.GetItem(Index: Integer): TFPHTTPClient;
begin
  Result := TFPHTTPClient(inherited Items[Index]);
end;

{ TFPCustomHTTPClients }

constructor TFPCustomHTTPClients.Create(const aPool: TFPCustomHTTPClientPool);
begin
  inherited Create;

  fAvailableClients := TFPHTTPClientList.Create;
  fBusyClients := TFPHTTPClientList.Create;
  fCS := TCriticalSection.Create;

  fPool := aPool;
end;

destructor TFPCustomHTTPClients.Destroy;
begin
  fAvailableClients.Free;
  fBusyClients.Free;
  fCS.Free;

  inherited Destroy;
end;

function TFPCustomHTTPClients.GetClient(const aForceNewClient: Boolean): TFPHTTPClient;
begin
  fCS.Enter;
  try
    if fAvailableClients.Count>0 then
    begin // pick one of the available clients
      Result := TFPHTTPClient(fAvailableClients[fAvailableClients.Count-1]);
      fAvailableClients.Delete(fAvailableClients.Count-1);
    end else
    if aForceNewClient or (fPool.MaxClientsPerServer<=0) or (fBusyClients.Count<fPool.MaxClientsPerServer) then
      Result := fPool.CreateClient
    else
      Result := nil;
    if Assigned(Result) then
      fBusyClients.Add(Result);
  finally
    fCS.Leave;
  end;
end;

function TFPCustomHTTPClients.GetAvailableClientCount: Integer;
begin
  fCS.Enter;
  try
    Result := fAvailableClients.Count;
  finally
    fCS.Leave;
  end;
end;

function TFPCustomHTTPClients.GetBusyClientCount: Integer;
begin
  fCS.Enter;
  try
    Result := fBusyClients.Count;
  finally
    fCS.Leave;
  end;
end;

procedure TFPCustomHTTPClients.ReleaseClient(const aClient: TFPHTTPClient);
var
  I: Integer;
begin
  fCS.Enter;
  try
    I := fBusyClients.IndexOf(aClient);
    if I<0 then
      raise EHTTPPool.Create(SErrCannotPushClient);
    fBusyClients.Delete(I);
    fAvailableClients.Add(aClient);
  finally
    fCS.Leave;
  end;
end;

{ TFPCustomHTTPClientPool }

constructor TFPCustomHTTPClientPool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fServerClients := TStringList.Create;
  fServerClients.OwnsObjects := True;
  fServerClients.Duplicates := dupError;
  fServerClients.Sorted := True;

  fServerClientsCS := TCriticalSection.Create;
end;

function TFPCustomHTTPClientPool.CreateClient: TFPHTTPClient;
begin
  Result := DoCreateClient;
  Result.KeepConnection := True;
end;

destructor TFPCustomHTTPClientPool.Destroy;
begin
  fServerClientsCS.Free;
  fServerClients.Free;

  inherited Destroy;
end;

function TFPCustomHTTPClientPool.DoCreateClient: TFPHTTPClient;
begin
  Result := TFPHTTPClient.Create(Self);
end;

function TFPCustomHTTPClientPool.GetAvailableClientCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  fServerClientsCS.Enter;
  try
    for I := 0 to fServerClients.Count-1 do
      Inc(Result, GetServerClients(I).AvailableClientCount);
  finally
    fServerClientsCS.Leave;
  end;
end;

function TFPCustomHTTPClientPool.GetBusyClientCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  fServerClientsCS.Enter;
  try
    for I := 0 to fServerClients.Count-1 do
      Inc(Result, GetServerClients(I).BusyClientCount);
  finally
    fServerClientsCS.Leave;
  end;
end;

function TFPCustomHTTPClientPool.GetClientCount: Integer;
begin
  Result := BusyClientCount+AvailableClientCount;
end;

function TFPCustomHTTPClientPool.GetCreateServerClients(const aHost: string; const aPort: Word): TFPCustomHTTPClients;
var
  xKey: string;
  I: Integer;
begin
  fServerClientsCS.Enter;
  try
    xKey := LowerCase(aHost)+':'+IntToStr(aPort);
    I := fServerClients.IndexOf(xKey);
    if I>=0 then
      Result := GetServerClients(I)
    else
    begin
      Result := TFPCustomHTTPClients.Create(Self);
      fServerClients.AddObject(xKey, Result);
    end;
  finally
    fServerClientsCS.Leave;
  end;
end;

function TFPCustomHTTPClientPool.GetServerClients(I: Integer): TFPCustomHTTPClients;
begin
  Result := TFPCustomHTTPClients(fServerClients.Objects[I]);
end;

function TFPCustomHTTPClientPool.GetClient(const aURL: string; const aForceNewClient: Boolean): TFPHTTPClient;
var
  xURI: TURI;
begin
  xURI := ParseURI(aURL, False);
  Result := GetClient(xURI.Host, xURI.Port, aForceNewClient);
end;

function TFPCustomHTTPClientPool.GetClient(const aHost: string; const aPort: Word; const aForceNewClient: Boolean): TFPHTTPClient;
var
  xClients: TFPCustomHTTPClients;
begin
  xClients := GetCreateServerClients(aHost, aPort);
  Result := xClients.GetClient(aForceNewClient);
end;

procedure TFPCustomHTTPClientPool.ReleaseClient(const aURL: string; const aClient: TFPHTTPClient);
var
  xURI: TURI;
begin
  xURI := ParseURI(aURL, False);
  ReleaseClient(xURI.Host, xURI.Port, aClient);
end;

procedure TFPCustomHTTPClientPool.ReleaseClient(const aHost: string; const aPort: Word; const aClient: TFPHTTPClient);
var
  xClients: TFPCustomHTTPClients;
begin
  xClients := GetCreateServerClients(aHost, aPort);
  xClients.ReleaseClient(aClient);
end;

end.

