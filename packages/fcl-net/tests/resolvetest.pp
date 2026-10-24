{ Byte order of addresses returned by netdb and resolve, issue #34531 }
unit resolvetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, resolve
{$ifdef unix}
  , netdb
{$endif}
  ;

{$ifdef unix}
const
  // Address the fake DNS server knows
  FakeAddress = '192.0.2.10';
  // Name the fake DNS server returns for FakeAddress
  FakeName = 'fake.example';

type
  { DNS server on a loopback UDP port that answers A and PTR queries for FakeName and FakeAddress }
  TFakeDNSServer = class(TThread)
  private
    FSocket: TSocket;
    FPort: Word;
    FLastQuery: string;
    procedure Answer(var aBuf: array of Byte; aLen: Integer; const aFrom: TInetSockAddr);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    // Stops the server thread
    procedure Stop;
    // UDP port the server listens on, in host order
    property Port: Word read FPort;
    // Name of the last question received
    property LastQuery: string read FLastQuery;
  end;
{$endif}

type
  { TResolveByteOrderTest }

  TResolveByteOrderTest = class(TTestCase)
{$ifdef unix}
  private
    FServer: TFakeDNSServer;
    FOldServers: TDNSServerArray;
    FOldPort: Word;
    FOldCheckAge: Boolean;
    FOldDomainList: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveHostByAddr;
    procedure TestResolveHostByName;
    procedure TestResolverAddressLookup;
    procedure TestResolverNameLookup;
{$else}
  published
    procedure TestResolverAddressLookupLocal;
{$endif}
  end;

implementation

{$ifdef unix}

{ Encodes aName as DNS labels into aBuf at aPos, returns the position after it }
function PutName(var aBuf: array of Byte; aPos: Integer; const aName: string): Integer;

var
  lParts: TStringArray;
  lPart: string;
  i: Integer;
begin
  lParts:=aName.Split(['.']);
  for lPart in lParts do
    begin
    aBuf[aPos]:=Length(lPart);
    Inc(aPos);
    for i:=1 to Length(lPart) do
      begin
      aBuf[aPos]:=Ord(lPart[i]);
      Inc(aPos);
      end;
    end;
  aBuf[aPos]:=0;
  Result:=aPos+1;
end;


{ Decodes the uncompressed DNS name at aPos in aBuf, aPos is moved past it }
function GetName(const aBuf: array of Byte; aLen: Integer; var aPos: Integer): string;

var
  lCount, i: Integer;
begin
  Result:='';
  while (aPos<aLen) and (aBuf[aPos]<>0) do
    begin
    lCount:=aBuf[aPos];
    Inc(aPos);
    if Result<>'' then
      Result:=Result+'.';
    for i:=1 to lCount do
      if aPos<aLen then
        begin
        Result:=Result+Chr(aBuf[aPos]);
        Inc(aPos);
        end;
    end;
  Inc(aPos);
end;


{ Writes the 16-bit aValue in net order into aBuf at aPos }
procedure PutWord(var aBuf: array of Byte; aPos: Integer; aValue: Word);

begin
  aBuf[aPos]:=Hi(aValue);
  aBuf[aPos+1]:=Lo(aValue);
end;


{ Returns the reverse lookup name of FakeAddress }
function FakeReverseName: string;

var
  lAddr: TNetAddr;
begin
  lAddr:=StrToNetAddr(FakeAddress);
  Result:=Format('%d.%d.%d.%d.in-addr.arpa',[lAddr.s_bytes[4],lAddr.s_bytes[3],lAddr.s_bytes[2],lAddr.s_bytes[1]]);
end;


{ TFakeDNSServer }

constructor TFakeDNSServer.Create;

var
  lAddr: TInetSockAddr;
  lLen: TSockLen;
begin
  FSocket:=fpSocket(AF_INET,SOCK_DGRAM,0);
  if FSocket<0 then
    raise Exception.Create('Cannot create the fake DNS server socket');
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  lAddr.sin_port:=0;
  if fpBind(FSocket,@lAddr,SizeOf(lAddr))<>0 then
    raise Exception.Create('Cannot bind the fake DNS server socket');
  lLen:=SizeOf(lAddr);
  fpGetSockName(FSocket,@lAddr,@lLen);
  FPort:=NToHs(lAddr.sin_port);
  inherited Create(False);
end;


destructor TFakeDNSServer.Destroy;

begin
  inherited Destroy;
  CloseSocket(FSocket);
end;


procedure TFakeDNSServer.Stop;

var
  lAddr: TInetSockAddr;
  lSock: TSocket;
  lByte: Byte;
begin
  Terminate;
  lSock:=fpSocket(AF_INET,SOCK_DGRAM,0);
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  lAddr.sin_port:=HToNs(FPort);
  lByte:=0;
  fpSendTo(lSock,@lByte,1,0,@lAddr,SizeOf(lAddr));
  CloseSocket(lSock);
  WaitFor;
end;


procedure TFakeDNSServer.Answer(var aBuf: array of Byte; aLen: Integer; const aFrom: TInetSockAddr);

var
  lPos, lType, lRDStart: Integer;
  lName: string;
  lAddr: TNetAddr;
begin
  lPos:=12;
  lName:=GetName(aBuf,aLen,lPos);
  FLastQuery:=lName;
  lType:=aBuf[lPos]*256+aBuf[lPos+1];
  Inc(lPos,4);
  { response, recursion desired and available }
  aBuf[2]:=$81;
  aBuf[3]:=$80;
  PutWord(aBuf,6,0);
  PutWord(aBuf,8,0);
  PutWord(aBuf,10,0);
  if ((lType=DNSQRY_PTR) and SameText(lName,FakeReverseName))
     or ((lType=DNSQRY_A) and SameText(lName,FakeName)) then
    begin
    PutWord(aBuf,6,1);
    { pointer to the question name }
    aBuf[lPos]:=$C0;
    aBuf[lPos+1]:=12;
    PutWord(aBuf,lPos+2,lType);
    PutWord(aBuf,lPos+4,1);
    PutWord(aBuf,lPos+6,0);
    PutWord(aBuf,lPos+8,60);
    lRDStart:=lPos+12;
    if lType=DNSQRY_A then
      begin
      lAddr:=StrToNetAddr(FakeAddress);
      Move(lAddr,aBuf[lRDStart],4);
      lPos:=lRDStart+4;
      end
    else
      lPos:=PutName(aBuf,lRDStart,FakeName);
    PutWord(aBuf,lRDStart-2,lPos-lRDStart);
    end
  else
    { name error }
    aBuf[3]:=$83;
  fpSendTo(FSocket,@aBuf[0],lPos,0,@aFrom,SizeOf(aFrom));
end;


procedure TFakeDNSServer.Execute;

var
  lBuf: array[0..1023] of Byte;
  lFrom: TInetSockAddr;
  lFromLen: TSockLen;
  lLen: Integer;
begin
  while not Terminated do
    begin
    lFromLen:=SizeOf(lFrom);
    lLen:=fpRecvFrom(FSocket,@lBuf,SizeOf(lBuf),0,@lFrom,@lFromLen);
    if Terminated then
      break;
    if lLen>=12 then
      Answer(lBuf,lLen,lFrom);
    end;
end;


{ TResolveByteOrderTest }

procedure TResolveByteOrderTest.SetUp;

begin
  FOldServers:=Copy(DNSServers);
  FOldPort:=DNSServerPort;
  FOldCheckAge:=CheckResolveFileAge;
  FOldDomainList:=DefaultDomainList;
  FServer:=TFakeDNSServer.Create;
  CheckResolveFileAge:=False;
  DefaultDomainList:='';
  SetLength(DNSServers,1);
  DNSServers[0]:=StrToNetAddr('127.0.0.1');
  DNSServerPort:=FServer.Port;
end;


procedure TResolveByteOrderTest.TearDown;

begin
  FServer.Stop;
  FreeAndNil(FServer);
  DNSServers:=FOldServers;
  DNSServerPort:=FOldPort;
  CheckResolveFileAge:=FOldCheckAge;
  DefaultDomainList:=FOldDomainList;
end;


procedure TResolveByteOrderTest.TestResolveHostByAddr;

var
  H: THostEntry;
begin
  AssertTrue('ResolveHostByAddr finds the fake address',ResolveHostByAddr(StrToHostAddr(FakeAddress),H));
  AssertEquals('Reverse query name',FakeReverseName,FServer.LastQuery);
  AssertEquals('Name',FakeName,H.Name);
  AssertEquals('H.Addr is in net order',FakeAddress,NetAddrToStr(H.Addr));
end;


procedure TResolveByteOrderTest.TestResolveHostByName;

var
  H: THostEntry;
begin
  AssertTrue('ResolveHostByName finds the fake name',ResolveHostByName(FakeName,H));
  AssertEquals('H.Addr is in net order',FakeAddress,NetAddrToStr(H.Addr));
end;


procedure TResolveByteOrderTest.TestResolverAddressLookup;

var
  lResolver: THostResolver;
begin
  lResolver:=THostResolver.Create(nil);
  try
    AssertTrue('AddressLookup finds the fake address',lResolver.AddressLookup(FakeAddress));
    AssertEquals('ResolvedName',FakeName,lResolver.ResolvedName);
    AssertEquals('AddressAsString',FakeAddress,lResolver.AddressAsString);
    AssertEquals('HostAddress is in host order',FakeAddress,HostAddrToStr(lResolver.HostAddress));
    AssertEquals('NetHostAddress is in net order',FakeAddress,NetAddrToStr(lResolver.NetHostAddress));
  finally
    lResolver.Free;
  end;
end;


procedure TResolveByteOrderTest.TestResolverNameLookup;

var
  lResolver: THostResolver;
begin
  lResolver:=THostResolver.Create(nil);
  try
    AssertTrue('NameLookup finds the fake name',lResolver.NameLookup(FakeName));
    AssertEquals('AddressAsString',FakeAddress,lResolver.AddressAsString);
    AssertEquals('NetHostAddress is in net order',FakeAddress,NetAddrToStr(lResolver.NetHostAddress));
  finally
    lResolver.Free;
  end;
end;

{$else}

procedure TResolveByteOrderTest.TestResolverAddressLookupLocal;

var
  lResolver: THostResolver;
begin
  lResolver:=THostResolver.Create(nil);
  try
    AssertTrue('AddressLookup finds the loopback address',lResolver.AddressLookup('127.0.0.1'));
    AssertTrue('ResolvedName is set',lResolver.ResolvedName<>'');
    AssertEquals('AddressAsString','127.0.0.1',lResolver.AddressAsString);
  finally
    lResolver.Free;
  end;
end;

{$endif}

initialization
  RegisterTest(TResolveByteOrderTest);
end.
