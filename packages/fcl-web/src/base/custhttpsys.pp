{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017-2018 by the Free Pascal development team

    Windows HTTP Server API based TCustomWebApplication

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit custHTTPSys;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, HttpApi, custWeb, HTTPDefs;

type

  { THTTPSysRequest }

  THTTPSysRequest = class(TRequest)
  private
    fHandle: THandle;
    fRequestId: HTTP_REQUEST_ID;
    function GetBaseUrl(const aUrl: AnsiString): AnsiString;
    procedure FillHeader(aRequest: PHTTP_REQUEST);
    procedure FillHTTPVariables(aRequest: PHTTP_REQUEST);
    procedure InitFromRequest(aRequest: PHTTP_REQUEST);
  protected
    procedure ReadContent; override;
  public
    constructor CreateReq(aHandle: THandle; const aUrl: String; aRequest: PHTTP_REQUEST);
  end;
  THTTPSysRequestClass = class of THTTPSysRequest;

  { THTTPSysResponse }

  THTTPSysResponse = class(TResponse)
  protected
    fHandle: THandle;
    fRequestId: HTTP_REQUEST_ID;
    fRequestVersion: HTTP_VERSION;
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  end;
  THTTPSysResponseClass = class of THTTPSysResponse;

  { THTTPSysHandler }

  THTTPSysHandler = class(TWebHandler)
  private
    fUrls: TStrings;
    fHandle: THandle;
    fServerSession: HTTP_SERVER_SESSION_ID;
    fUrlGroup: HTTP_URL_GROUP_ID;
    fBuffer: PHTTP_REQUEST;
    fBufferSize: LongWord;
    procedure InitUrls;
  protected
    function CreateRequest(aRequest: PHTTP_REQUEST; const aUrl: String): THTTPSysRequest; virtual;
    function CreateResponse(aRequest: THTTPSysRequest): THTTPSysResponse; virtual;
    procedure ProcessRequest(aBuffer: PHTTP_REQUEST; aSize: LongWord; out aRequest: TRequest; out aResponse: TResponse);
    function WaitForRequest(out aRequest: TRequest; out aResponse: TResponse): Boolean; override;
  public
    procedure Terminate; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  THTTPSysHandlerClass = class of THTTPSysHandler;

  { TCustomHTTPSysApplication }

  TCustomHTTPSysApplication = class(TCustomWebApplication)
  private
    fUrls: TStrings;
  protected
    function InitializeWebHandler: TWebHandler; override;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Urls: TStrings read fUrls;
  end;

  EHTTPSys = class(EFPWebError);

var
  HTTPSysRequestClass: THTTPSysRequestClass = THTTPSysRequest;
  HTTPSysResponseClass: THTTPSysResponseClass = THTTPSysResponse;
  HTTPSysHandlerClass: THTTPSysHandlerClass = THTTPSysHandler;

implementation

uses
  Windows, httpprotocol, WinSock2;

resourcestring
  SErrReceiveRequest = 'Failed to receive HTTP request (Errorcode: 0x%x)';
  SErrReceiveRequestBody = 'Failed to receive body of HTTP request (Errorcode: 0x%x)';
  SErrSendResponse = 'Failed to send HTTP response (Errorcode: 0x%x)';
  SErrSendResponseBody = 'Failed to send body of HTTP response (Errorcode: 0x%x)';
  SErrInitializeHttpApi = 'Failed to initialize HTTP API (Errorcode: 0x%x)';
  SErrCreateRequestQueue = 'Failed to create request queue (Errorcode: 0x%x)';
  SErrCreateServerSession = 'Failed to create server session (Errorcode: 0x%x)';
  SErrCreateUrlGroup = 'Failed to create URL group (Errorcode: 0x%x)';
  SErrAddUrl = 'Failed to add URL ''%s'' to URL group (Errorcode: 0x%x)';
  SErrBindGroupToQueue = 'Failed to bind URL group to queue (Errorcode: 0x%x)';

function HeaderToHttpHeaderId(aHeader: THeader; out aId: HTTP_HEADER_ID): Boolean;
begin
  Result := True;
  case aHeader of
    hhAccept:
      aId := HttpHeaderAccept;
    hhAcceptCharset:
      aId := HttpHeaderAcceptCharset;
    hhAcceptEncoding:
      aId := HttpHeaderAcceptEncoding;
    hhAcceptLanguage:
      aId := HttpHeaderAcceptLanguage;
    hhAcceptRanges:
      aId := HttpHeaderAcceptRanges;
    hhAge:
      aId := HttpHeaderAge;
    hhAllow:
      aId := HttpHeaderAllow;
    hhAuthorization:
      aId := HttpHeaderAuthorization;
    hhCacheControl:
      aId := HttpHeaderCacheControl;
    hhConnection:
      aId := HttpHeaderConnection;
    hhContentEncoding:
      aId := HttpHeaderContentEncoding;
    hhContentLanguage:
      aId := HttpHeaderContentLanguage;
    hhContentLength:
      aId := HttpHeaderContentLength;
    hhContentLocation:
      aId := HttpHeaderContentLocation;
    hhContentMD5:
      aId := HttpHeaderContentMd5;
    hhContentRange:
      aId := HttpHeaderContentRange;
    hhContentType:
      aId := HttpHeaderContentType;
    hhDate:
      aId := HttpHeaderDate;
    hhETag:
      aId := HttpHeaderEtag;
    hhExpires:
      aId := HttpHeaderExpires;
    hhExpect:
      aId := HttpHeaderExpect;
    hhFrom:
      aId := HttpHeaderFrom;
    hhHost:
      aId := HttpHeaderHost;
    hhIfMatch:
      aId := HttpHeaderIfMatch;
    hhIfModifiedSince:
      aId := HttpHeaderIfModifiedSince;
    hhIfNoneMatch:
      aId := HttpHeaderIfNoneMatch;
    hhIfRange:
      aId := HttpHeaderIfRange;
    hhIfUnModifiedSince:
      aId := HttpHeaderIfUnmodifiedSince;
    hhLastModified:
      aId := HttpHeaderLastModified;
    hhLocation:
      aId := HttpHeaderLocation;
    hhMaxForwards:
      aId := HttpHeaderMaxForwards;
    hhPragma:
      aId := HttpHeaderPragma;
    //hhProxyAuthenticate: ;
    //hhProxyAuthorization: ;
    hhRange:
      aId := HttpHeaderRange;
    hhReferer:
      aId := HttpHeaderReferer;
    hhRetryAfter:
      aId := HttpHeaderRetryAfter;
    hhServer:
      aId := HttpHeaderServer;
    hhTE:
      aId := HttpHeaderTe;
    hhTrailer:
      aId := HttpHeaderTrailer;
    hhTransferEncoding:
      aId := HttpHeaderTransferEncoding;
    hhUpgrade:
      aId := HttpHeaderUpgrade;
    hhUserAgent:
      aId := HttpHeaderUserAgent;
    hhVary:
      aId := HttpHeaderVary;
    hhVia:
      aId := HttpHeaderVia;
    hhWarning:
      aId := HttpHeaderWarning;
    hhWWWAuthenticate:
      aId := HttpHeaderWwwAuthenticate;
    otherwise
      Result := False;
  end;
end;

function IgnoreHttpHeaderForRequest(aHeader: THeader): Boolean;
begin
  case aHeader of
    hhAcceptRanges,
    hhAge,
    hhETag,
    hhLocation,
    hhProxyAuthenticate,
    hhRetryAfter,
    hhServer,
    hhVary,
    hhWWWAuthenticate:
      Result := True;
    otherwise
      Result := False;
  end;
end;

{ THTTPSysResponse }

procedure THTTPSysResponse.DoSendHeaders(aHeaders: TStrings);
var
  resp: HTTP_RESPONSE;
  flags, bytessend: LongWord;
  i, colonidx: LongInt;
  headerstr, headerval: String;
  res: ULONG;
  hh: THeader;
  headerid: HTTP_HEADER_ID;
  headerstrs, unknownheaders: TStrings;
  unknownheadersarr: array of HTTP_UNKNOWN_HEADER;
begin
  resp := Default(HTTP_RESPONSE);
  resp.Version := fRequestVersion;
  resp.StatusCode := Code;
  if CodeText <> '' then begin
    resp.pReason := PChar(CodeText);
    resp.ReasonLength := Length(CodeText);
  end;

  flags := 0;
  if (Assigned(ContentStream) and (ContentStream.Size > 0)) or (Contents.Count > 0) then
    flags := flags or HTTP_SEND_RESPONSE_FLAG_MORE_DATA;

  unknownheaders := Nil;
  headerstrs := TStringList.Create;
  try
    unknownheaders := TStringList.Create;

    for i := 0 to aHeaders.Count - 1 do begin
      colonidx := Pos(':', aHeaders[i]);
      if colonidx = 0 then
        Continue;
      headerstr := Copy(aHeaders[i], 1, colonidx - 1);
      headerval := Trim(Copy(aHeaders[i], colonidx + 1, Length(aHeaders[i]) - colonidx));

      hh := HeaderType(headerstr);
      if hh = hhUnknown then begin
        unknownheaders.Values[headerstr] := headerval;
        Continue;
      end;

      if not (hdResponse in HTTPHeaderDirections[hh]) then begin
        unknownheaders.Values[headerstr] := headerval;
        Continue;
      end;

      if not HeaderToHttpHeaderId(hh, headerid) then begin
        unknownheaders.Values[headerstr] := headerval;
        Continue;
      end;

      if headerid >= HttpHeaderResponseMaximum then begin
        unknownheaders.Values[headerstr] := headerval;
        Continue;
      end;

      headerstrs.Add(headerval);

      resp.Headers.KnownHeaders[Ord(headerid)].RawValueLength := Length(headerval);
      resp.Headers.KnownHeaders[Ord(headerid)].pRawValue := PAnsiChar(headerstrs[headerstrs.Count - 1]);
    end;

    SetLength(unknownheadersarr, unknownheaders.Count);
    for i := 0 to unknownheaders.Count - 1 do begin
      headerstr := unknownheaders.Names[i];
      headerval := unknownheaders.ValueFromIndex[i];

      headerstrs.Add(headerstr);
      unknownheadersarr[i].NameLength := Length(headerstr);
      unknownheadersarr[i].pName := PAnsiChar(headerstrs[headerstrs.Count - 1]);

      headerstrs.Add(headerval);
      unknownheadersarr[i].RawValueLength := Length(headerval);
      unknownheadersarr[i].pRawValue := PAnsiChar(headerstrs[headerstrs.Count - 1]);
    end;

    if unknownheaders.Count > 0 then begin
      resp.Headers.UnknownHeaderCount := unknownheaders.Count;
      resp.Headers.pUnknownHeaders := @unknownheadersarr[0];
    end;

    res := HttpSendHttpResponse(fHandle, fRequestId, flags, @resp, Nil, @bytessend, Nil, 0, Nil, Nil);
    if res <> NO_ERROR then
      raise EHTTPSys.CreateFmtHelp(SErrSendResponse, [res], 500);
  finally
    unknownheaders.Free;
    headerstrs.Free;
  end;
end;

procedure THTTPSysResponse.DoSendContent;
var
  chunk: HTTP_DATA_CHUNK;
  bytessend: LongWord;
  memstrm: TMemoryStream;
  res: ULONG;
begin
  if not (Assigned(ContentStream) and (ContentStream.Size > 0)) and not (Contents.Count > 0) then
    Exit;

  memstrm := TMemoryStream.Create;
  try
    if Assigned(ContentStream) then
      memstrm.CopyFrom(ContentStream, ContentStream.Size)
    else
      Contents.SaveToStream(memstrm);

    chunk := Default(HTTP_DATA_CHUNK);
    chunk.DataChunkType := HttpDataChunkFromMemory;
    chunk.FromMemory.pBuffer := memstrm.Memory;
    chunk.FromMemory.BufferLength := memstrm.Size;

    res := HttpSendResponseEntityBody(fHandle, fRequestId, 0, 1, @chunk, @bytessend, Nil, Nil, Nil, Nil);
    if res <> NO_ERROR then
      raise EHTTPSys.CreateFmtHelp(SErrSendResponseBody, [res], 500);
  finally
    memstrm.Free;
  end;
end;

{ THTTPSysRequest }

function THTTPSysRequest.GetBaseUrl(const aUrl: AnsiString): AnsiString;
const
  ProtocolHttp = 'http://';
  ProtocolHttps = 'https://';
var
  prefix: AnsiString;
  slashidx: LongInt;
begin
  prefix := aUrl;
  if Copy(prefix, 1, Length(ProtocolHttp)) = ProtocolHttp then
    Delete(prefix, 1, Length(ProtocolHttp))
  else if Copy(prefix, 1, Length(ProtocolHttps)) = ProtocolHttps then
    Delete(prefix, 1, Length(ProtocolHttps))
  else
    Exit('');

  slashidx := Pos('/', prefix);
  if slashidx = 0 then
    Exit('');

  Delete(prefix, 1, slashidx - 1);

  Result := prefix;
end;

procedure THTTPSysRequest.FillHeader(aRequest: PHTTP_REQUEST);
var
  hh: THeader;
  hid: HTTP_HEADER_ID;
  unkheader: PHTTP_UNKNOWN_HEADER;
  i: LongInt;
  name, value: AnsiString;
  hv: THTTPVariableType;
begin
  for hh := Low(THeader) to High(THeader) do begin
    if not (hdRequest in HTTPHeaderDirections[hh]) or IgnoreHttpHeaderForRequest(hh) then
      Continue;
    if not HeaderToHttpHeaderId(hh, hid) then
      Continue;
    if aRequest^.Headers.KnownHeaders[Ord(hid)].RawValueLength > 0 then
      SetHeader(hh, StrPas(aRequest^.Headers.KnownHeaders[Ord(hid)].pRawValue));
  end;

  for i := 0 to aRequest^.Headers.UnknownHeaderCount - 1 do begin
    unkheader := @aRequest^.Headers.pUnknownHeaders[i];
    if (unkheader^.NameLength > 0) and Assigned(unkheader^.pName) then begin
      name := StrPas(unkheader^.pName);
      value := StrPas(unkheader^.pRawValue);
      if name = HeaderProxyAuthenticate then
        hh := hhProxyAuthenticate
      else if name = HeaderProxyAuthorization then
        hh := hhProxyAuthorization
      else begin
        hh := hhUnknown;
        hv := hvUnknown;
        if name = HeaderSetCookie then
          hv := hvSetCookie
        else if name = HeaderCookie then
          hv := hvCookie
        else if name = HeaderXRequestedWith then
          hv := hvXRequestedWith;
        if hv <> hvUnknown then
          SetHTTPVariable(hvSetCookie, value)
        else
          SetCustomHeader(name, value);
      end;
      if hh <> hhUnknown then
        SetHeader(hh, value);
    end;
  end;
end;

procedure THTTPSysRequest.FillHTTPVariables(aRequest: PHTTP_REQUEST);

  function GetMethodStr(aRequest: PHTTP_REQUEST): String;
  begin
    case aRequest^.Verb of
      HttpVerbOPTIONS:
        Result := 'OPTIONS';
      HttpVerbGET:
        Result := 'GET';
      HttpVerbHEAD:
        Result := 'HEAD';
      HttpVerbPOST:
        Result := 'POST';
      HttpVerbPUT:
        Result := 'PUT';
      HttpVerbDELETE:
        Result := 'DELETE';
      HttpVerbTRACE:
        Result := 'TRACE';
      HttpVerbCONNECT:
        Result := 'CONNECT';
      HttpVerbTRACK:
        Result := 'TRACK';
      HttpVerbMOVE:
        Result := 'MOVE';
      HttpVerbCOPY:
        Result := 'COPY';
      HttpVerbPROPFIND:
        Result := 'PROPFIND';
      HttpVerbPROPPATCH:
        Result := 'PROPPATCH';
      HttpVerbMKCOL:
        Result := 'MKCOL';
      HttpVerbLOCK:
        Result := 'LOCK';
      HttpVerbUNLOCK:
        Result := 'UNLOCK';
      HttpVerbSEARCH:
        Result := 'SEARCH';
      otherwise
        if (aRequest^.UnknownVerbLength > 0) and Assigned(aRequest^.pUnknownVerb) then
          Result := StrPas(aRequest^.pUnknownVerb)
        else
          Result := '';
    end;
  end;

  function GetRemoteAddress: String;
  var
    len, size: DWord;
  begin
    if not Assigned(aRequest^.Address.pRemoteAddress) then
      Exit('');

    if aRequest^.Address.pRemoteAddress^.sa_family = AF_INET then
      size := SizeOf(TSockAddrIn)
    else if aRequest^.Address.pRemoteAddress^.sa_family = AF_INET6 then
      size := SizeOf(TSockAddrIn6)
    else
      Exit('');

    len := 32;
    SetLength(Result, len - 1);

    if WSAAddressToString(aRequest^.Address.pRemoteAddress^, size, Nil, PChar(Result), len) <> 0 then begin
      //Writeln('Failed to retrieve address string; error: ', WSAGetLastError);
      Exit('');
    end;

    SetLength(Result, len - 1);
  end;

var
  s: AnsiString;
  urlstr, urlprefix: UTF8String;
  idx: LongInt;
begin
  SetHTTPVariable(hvHTTPVersion, IntToStr(aRequest^.Version.MajorVersion) + '.' + IntToStr(aRequest^.Version.MinorVersion));
  SetHTTPVariable(hvMethod, GetMethodStr(aRequest));

  urlstr := Utf8String(StrPas(aRequest^.CookedUrl.pAbsPath));
  urlprefix := ReturnedPathInfo;

  SetHTTPVariable(hvURL, urlstr);

  if Copy(urlstr, 1, Length(urlprefix)) = urlprefix then
    Delete(urlstr, 1, Length(urlprefix));

  idx := Pos('?', urlstr);
  if idx > 0 then begin
    SetHTTPVariable(hvPathInfo, Copy(urlstr, 1, idx - 1));
    SetHTTPVariable(hvQuery, Copy(urlstr, idx + 1, Length(urlstr) - idx));
  end else
    SetHTTPVariable(hvPathInfo, urlstr);

  // ToDo
  {s := GetRemoteAddress;
  if s <> '' then
    SetHTTPVariable(hvRemoteAddress, s)}
end;

procedure THTTPSysRequest.InitFromRequest(aRequest: PHTTP_REQUEST);
begin
  FillHeader(aRequest);
  FillHTTPVariables(aRequest);
  ParseCookies;
  ReadContent;
  InitRequestVars;
end;

procedure THTTPSysRequest.ReadContent;
const
  BufLen = 4096;
var
  ss: TStringStream;
  res, bytesreturned: ULONG;
  buf: PByte;
  e: EHTTPSys;
  s: AnsiString;
begin
  buf := Nil;
  ss := TStringStream.Create('');
  try
    buf := GetMem(BufLen);

    repeat
      res := HttpReceiveRequestEntityBody(fHandle, fRequestId, 0, buf, BufLen, @bytesreturned, Nil);
      if res = NO_ERROR then
        ss.Write(buf^, bytesreturned)
      else if res <> ERROR_HANDLE_EOF then begin
        e := EHTTPSys.CreateFmt(SErrReceiveRequestBody, [res]);
        e.StatusCode := 500;
        raise e;
      end;
    until res = ERROR_HANDLE_EOF;

    s := ss.DataString;
    InitContent(s);
  finally
    Freemem(buf);
    ss.Free;
  end;
end;

constructor THTTPSysRequest.CreateReq(aHandle: THandle; const aUrl: String;
  aRequest: PHTTP_REQUEST);
begin
  fHandle := aHandle;
  fRequestId := aRequest^.RequestId;
  ReturnedPathInfo := GetBaseUrl(aUrl);
  inherited Create;
  InitFromRequest(aRequest);
end;

{ THTTPSysHandler }

function THTTPSysHandler.CreateRequest(aRequest: PHTTP_REQUEST;
  const aUrl: String): THTTPSysRequest;
var
  c: THTTPSysRequestClass;
begin
  c := HTTPSysRequestClass;
  if not Assigned(c) then
    c := THTTPSysRequest;
  Result := c.CreateReq(fHandle, aUrl, aRequest);
end;

function THTTPSysHandler.CreateResponse(aRequest: THTTPSysRequest
  ): THTTPSysResponse;
var
  c: THTTPSysResponseClass;
begin
  c := HTTPSysResponseClass;
  if not Assigned(c) then
    c := THTTPSysResponse;
  Result := c.Create(aRequest);
end;

procedure THTTPSysHandler.ProcessRequest(aBuffer: PHTTP_REQUEST;
  aSize: LongWord; out aRequest: TRequest; out aResponse: TResponse);
var
  locrequest: THTTPSysRequest;
  locresponse: THTTPSysResponse;
  url: String;
begin
  if aBuffer^.UrlContext < fUrls.Count then
    url := fUrls[aBuffer^.UrlContext];
  locrequest := CreateRequest(aBuffer, url);
  InitRequest(locrequest);

  locresponse := CreateResponse(locrequest);
  InitResponse(locresponse);
  locresponse.fRequestId := aBuffer^.RequestId;
  locresponse.fRequestVersion := aBuffer^.Version;
  locresponse.fHandle := fHandle;

  aRequest := locrequest;
  aResponse := locresponse;
end;

function THTTPSysHandler.WaitForRequest(out aRequest: TRequest; out
  aResponse: TResponse): Boolean;
var
  readsize: ULONG;
  res: ULONG;
begin
  if not Assigned(fBuffer) then begin
    InitUrls;

    fBufferSize := 4096;
    fBuffer := GetMem(fBufferSize);
  end;

  repeat
    repeat
      res := HttpReceiveHttpRequest(fHandle, HTTP_NULL_ID, 0, fBuffer, fBufferSize, @readsize, Nil);
      if res = ERROR_MORE_DATA then begin
        FreeMem(fBuffer);
        fBufferSize := fBufferSize + 4096;
        fBuffer := GetMem(fBufferSize);
      end;
    until res <> ERROR_MORE_DATA;
    if res <> NO_ERROR then
      DoError(SErrReceiveRequest, [res])
    else begin
      ProcessRequest(fBuffer, readsize, aRequest, aResponse);
      Result := True;
    end;
  until Result or (fHandle = INVALID_HANDLE_VALUE);
end;

procedure THTTPSysHandler.InitUrls;
var
  i: LongInt;
  res: ULONG;
  binding: HTTP_BINDING_INFO;
  s: String;
begin
  for i := 0 to fUrls.Count - 1 do begin
    s := fUrls[i];
    Log(etInfo, 'Adding URL ' + s);
    res := HttpAddUrlToUrlGroup(fUrlGroup, PWideChar(WideString(s)), i, 0);
    if res <> NO_ERROR then
      DoError(SErrAddUrl, [s, res]);
  end;

  binding := Default(HTTP_BINDING_INFO);
  set_Present(binding.Flags, 1);
  binding.RequestQueueHandle := fHandle;

  res := HttpSetUrlGroupProperty(fUrlGroup, HttpServerBindingProperty, @binding, SizeOf(binding));
  if res <> NO_ERROR then
    DoError(SErrBindGroupToQueue, [res]);
end;

procedure THTTPSysHandler.Terminate;
begin
  inherited Terminate;
end;

constructor THTTPSysHandler.Create(AOwner: TComponent);
var
  res: ULONG;
begin
  fUrls := TStringList.Create;

  inherited Create(AOwner);

  fHandle := INVALID_HANDLE_VALUE;

  res := HttpCreateRequestQueue(HTTPAPI_VERSION_2, Nil, Nil, 0, @fHandle);
  if res <> NO_ERROR then
    DoError(SErrCreateRequestQueue, [res]);

  res := HttpCreateServerSession(HTTPAPI_VERSION_2, @fServerSession, 0);
  if res <> NO_ERROR then
    DoError(SErrCreateServerSession, [res]);

  res := HttpCreateUrlGroup(fServerSession, @fUrlGroup, 0);
  if res <> NO_ERROR then
    DoError(SErrCreateUrlGroup, [res]);
end;

destructor THTTPSysHandler.Destroy;
begin
  if fUrlGroup <> HTTP_NULL_ID then
    HttpCloseUrlGroup(fUrlGroup);

  if fServerSession <> HTTP_NULL_ID then
    HttpCloseServerSession(fServerSession);

  if fHandle <> INVALID_HANDLE_VALUE then
    HttpCloseRequestQueue(fHandle);

  FreeMem(fBuffer);
  fUrls.Free;

  inherited Destroy;
end;

{ TCustomHTTPSysApplication }

function TCustomHTTPSysApplication.InitializeWebHandler: TWebHandler;
var
  c: THTTPSysHandlerClass;
begin
  c := HTTPSysHandlerClass;
  if not Assigned(c) then
    c := THTTPSysHandler;
  Result := c.Create(Self);
end;

procedure TCustomHTTPSysApplication.DoRun;
begin
  if WebHandler is THTTPSysHandler then
    THTTPSysHandler(WebHandler).fUrls.Assign(fUrls);
  inherited DoRun;
end;

constructor TCustomHTTPSysApplication.Create(AOwner: TComponent);
var
  res: ULONG;
begin
  fUrls := TStringList.Create;

  res := HttpInitialize(HTTPAPI_VERSION_2, HTTP_INITIALIZE_SERVER, Nil);
  if res <> NO_ERROR then
    raise Exception.CreateFmt(SErrInitializeHttpApi, [res]);

  inherited Create(AOwner);
end;

destructor TCustomHTTPSysApplication.Destroy;
begin
  fUrls.Free;
  HttpTerminate(HTTP_INITIALIZE_SERVER, Nil);
  inherited Destroy;
end;

end.

