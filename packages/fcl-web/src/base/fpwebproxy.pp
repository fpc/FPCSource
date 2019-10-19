{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Classes to implement a proxy mechanism.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpwebproxy;

{$mode objfpc}{$H+}

// Define this to output debug info on console
{ $DEFINE DEBUGPROXY}

interface

uses
  Classes, SysUtils, fphttp, httpdefs, httpprotocol, fphttpclient;

Type
  TProxyRequestLog = Procedure(Sender : TObject; Const Method,Location,FromURL,ToURL : String) of object;

  { TProxyLocation }

  TProxyLocation = Class(TCollectionItem)
  private
    FAppendPathInfo: Boolean;
    FEnabled: Boolean;
    FPath: String;
    FRedirect: Boolean;
    FURL: String;
  Published
    Property Path : String Read FPath Write FPath;
    Property URL : String Read FURL Write FURL;
    Property Enabled : Boolean Read FEnabled Write FEnabled;
    Property Redirect : Boolean Read FRedirect Write FRedirect;
    Property AppendPathInfo : Boolean Read FAppendPathInfo Write FAppendPathInfo;
  end;

  { TProxyLocations }

  TProxyLocations = Class(TCollection)
  private
    function GetL(AIndex : Integer): TProxyLocation;
    procedure SetL(AIndex : Integer; AValue: TProxyLocation);
  Public
    Function IndexOfLocation(Const APath : String) : Integer;
    Function FindLocation(Const APath : String) : TProxyLocation;
    Property Locations [AIndex : Integer] : TProxyLocation Read GetL Write SetL; default;
  end;

  { TProxyWebModule }

  TProxyWebModule = Class(TCustomHTTPModule)
  protected
    Procedure DoLog(Const aMethod,aLocation,aFromURL,aToURL : String);
    procedure ClientToResponse(T: TFPHTTPClient; aResponse: TResponse); virtual;
    procedure RequestToClient(T: TFPHTTPClient; aRequest: TRequest); virtual;
    procedure ReRouteRequest(L: TProxyLocation; ARequest: TRequest;  AResponse: TResponse);virtual;
  Public
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  end;

   { TProxyManager }

   TProxyManager = Class(TObject)
   private
     FLocations : TProxyLocations;
     FOnLog: TProxyRequestLog;
     function GetLocation(AIndex : Integer): TProxyLocation;
     function GetLocationCount: Integer;
   Public
     Constructor create;
     Destructor Destroy; override;
     Function RegisterLocation(Const APath,AURL : String) : TProxyLocation;
     Function UnRegisterLocation(Const APath : String) : boolean;
     Function FindLocation(Const APath : String) : TProxyLocation;
     Property LocationCount : Integer Read GetLocationCount;
     Property Locations[AIndex : Integer] : TProxyLocation Read GetLocation;
     Property OnLog : TProxyRequestLog Read FOnLog Write FOnLog;
   end;

   EWAProxy = Class(Exception);

Function ProxyManager: TProxyManager;

implementation

uses StrUtils;

Resourcestring
  SErrDuplicateProxy = 'Duplicate proxy location: "%s"';

Var
  PM : TProxyManager;


Function ProxyManager: TProxyManager;

begin
  If PM=Nil then
    PM:=TProxyManager.Create;
  Result:=PM;
end;

{ TProxyManager }

function TProxyManager.GetLocation(AIndex : Integer): TProxyLocation;
begin
  Result:=FLocations[AIndex];
end;

function TProxyManager.GetLocationCount: Integer;
begin
  Result:=FLocations.Count;
end;

constructor TProxyManager.create;
begin
  inherited create;
  FLocations:=TProxyLocations.Create(TProxyLocation);
end;

destructor TProxyManager.Destroy;
begin
  FreeAndNil(FLocations);
  inherited Destroy;
end;

function TProxyManager.RegisterLocation(const APath, AURL: String
  ): TProxyLocation;
begin
  Result:=FLocations.FindLocation(APAth);
  if Result<>Nil then
    Raise EWAProxy.CreateFmt(SErrDuplicateProxy,[APath]);
  Result:=FLocations.Add as TProxyLocation;
  Result.Path:=APath;
  Result.URL:=AURL;
  Result.Enabled:=True;
end;

function TProxyManager.UnRegisterLocation(const APath : String): boolean;

Var
  l : TProxyLocation;
begin
  L:=FLocations.FindLocation(APath);
  Result:=L<>Nil;
  If Result then
    L.Free;
end;

function TProxyManager.FindLocation(const APath: String): TProxyLocation;
begin
  Result:=FLocations.FindLocation(APath);
end;

{ TProxyLocations }

function TProxyLocations.GetL(AIndex : Integer): TProxyLocation;

begin
  Result:=Items[AIndex] as TProxyLocation;
end;

procedure TProxyLocations.SetL(AIndex : Integer; AValue: TProxyLocation);

begin
  Items[AIndex]:=AValue;
end;

function TProxyLocations.IndexOfLocation(const APath: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetL(Result).Path,APath)<>0) do
    Dec(Result);
end;

function TProxyLocations.FindLocation(const APath: String): TProxyLocation;

Var
  I : Integer;

begin
  I:=IndexOfLocation(APath);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetL(I);
end;

{ TProxyWebModule }

procedure TProxyWebModule.RequestToClient(T : TFPHTTPClient; aRequest : TRequest);

Var
  H : THeader;
  I : Integer;
  N,V : String;

begin
  // Transfer known headers
  for H in THeader do
    if (hdRequest in HTTPHeaderDirections[H]) then
      if aRequest.HeaderIsSet(H) then
        if H<>hhHost then
         begin
         {$ifdef DEBUGPROXY}Writeln('Sending header: ',HTTPHeaderNames[H],': ',aRequest.GetHeader(H));{$ENDIF}
         T.AddHeader(HTTPHeaderNames[H],aRequest.GetHeader(H));
         end;
  // Transfer custom headers
  For I:=0 to aRequest.CustomHeaders.Count-1 do
    begin
    aRequest.CustomHeaders.GetNameValue(I,N,V);
    {$ifdef DEBUGPROXY}Writeln('Sending custom header: ',N,': ',V);{$ENDIF}
    T.AddHeader(N,V);
    end;
  if (Length(ARequest.Content)>0) then
    begin
    T.RequestBody:=TMemoryStream.Create;
    T.RequestBody.WriteBuffer(ARequest.Content[1],Length(ARequest.Content));
    T.RequestBody.Position:=0;
    end;
end;

procedure TProxyWebModule.DoLog(const aMethod,aLocation, aFromURL, aToURL: String);
begin
  If Assigned(ProxyManager) and Assigned(ProxyManager.OnLog) then;
    ProxyManager.OnLog(Self,aMethod,aLocation,aFromURl,aToURL);
end;

procedure TProxyWebModule.ClientToResponse(T : TFPHTTPClient; aResponse : TResponse);

Var
  N,H : String;
  HT : THeader;

begin
  for N in T.ResponseHeaders do
    begin
    H:=ExtractWord(1,N,[':']);
    HT:=HeaderType(H);
    if not (HT in [hhContentLength]) then
      begin
      {$IFDEF DEBUGPROXY}Writeln('Returning header: ',N);{$ENDIF}
      AResponse.CustomHeaders.Add(N);
      end;
    end;
  AResponse.Code:=T.ResponseStatusCode;
  AResponse.CodeText:=T.ResponseStatusText;
  AResponse.ContentLength:=AResponse.ContentStream.Size;
end;

procedure TProxyWebModule.ReRouteRequest(L : TProxyLocation; ARequest: TRequest; AResponse: TResponse);

Var
  T : TFPHTTPClient;
  P,URL : String;

begin
  URL:=L.URL;
  if L.AppendPathInfo then
    begin
    P:=ARequest.PathInfo;
    if (P<>'') then
      URL:=IncludeHTTPPathDelimiter(URL)+P;
    end;
  if (ARequest.QueryString<>'') then
    URL:=URL+'?'+ARequest.QueryString;
  DoLog(aRequest.Method, L.Path,ARequest.URL, URL);
  T:=TFPHTTPClient.Create(Self);
  try
    RequestToClient(T,aRequest);
    aResponse.FreeContentStream:=True;
    aResponse.ContentStream:=TMemoryStream.Create;
    T.AllowRedirect:=True;
    T.HTTPMethod(ARequest.Method,URL,AResponse.ContentStream,[]);
    ClientToResponse(T,aResponse);
    AResponse.SendContent;
  finally
    T.RequestBody.Free;
    T.Free;
  end;
end;

procedure TProxyWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  P : String;
  L : TProxyLocation;

begin
  P:=ARequest.GetNextPathInfo;
  L:=ProxyManager.FindLocation(P);
  if (L=Nil) or (Not L.Enabled) then
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='Location not found : '+P;
    AResponse.SendContent;
    end
  else if L.Redirect then
    begin
    DoLog(L.Path,aRequest.method, ARequest.URL, L.URL);
    AResponse.SendRedirect(L.URL);
    AResponse.SendContent;
    end
  else
    begin
    ReRouteRequest(L,ARequest,AResponse);
    if not AResponse.ContentSent then
      AResponse.SendContent;
    end;
end;

finalization
  FreeAndNil(PM);
end.

