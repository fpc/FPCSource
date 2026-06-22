{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2026 - by the Free Pascal development team

    HTTP/2 request/response adaptation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fphttp2request;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.Http.Defs, FpWeb.Http.Protocol,
  FpWeb.Http.Server, FpWeb.Http2.Consts, FpWeb.Http2.Frames,
  FpWeb.Http2.Connection, FpWeb.Uhpack;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, httpdefs, httpprotocol, fphttpserver, fphttp2consts,
  fphttp2frames, fphttp2connection, uhpack;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Interface between TRequest/TResponse and HTTP/2 handling:

    An existing fcl-web handler (OnRequest / web module) is dispatched UNCHANGED
    over HTTP/2, but the contract it observes differs from HTTP/1.1 in several ways:

    - No Connection / Keep-Alive / Transfer-Encoding headers ever arrive
    - No Connection / Keep-Alive / Transfer-Encoding headers are emitted
      (they are illegal in HTTP/2 (§8.2.2).
    - Request bodies arrive via flow-controlled DATA frames and are fully buffered
      they are not streamed; the complete body is in Request.ContentBytes when OnRequest fires.
    - The HTTP/2 :scheme / :authority pseudo-headers REPLACE the HTTP/1.1 absolute-URI / Host conventions.
      :authority is surfaced as Request.Host;
      :scheme has no TRequest counterpart.
    - Trailers exist in HTTP/2 but are not delivered to the handler here.
  }

  { TH2Request - adapts a decoded HTTP/2 stream into the EXISTING TRequest
    pipeline. It descends from TFPHTTPConnectionRequest (NOT TRequest) so a
    variable of that exact type can hold it and be passed to the var-parameter
    Server.HandleRequest. }

  TH2Request = class(TFPHTTPConnectionRequest)
  public
    constructor CreateFromH2(aHeaders: THPackHeaderTextList; const aBody: TBytes);
  end;

  { TH2Response - frames the reply as an HPACK HEADERS frame (carrying :status)
    followed by flow-controlled DATA, NEVER an HTTP/1.1 status line. }

  TH2Response = class(TFPHTTPConnectionResponse)
  private
    FH2Conn: TH2Connection;
    FStreamID: Cardinal;
  protected
    procedure DoSendHeaders(Headers: TStrings); override;
    procedure DoSendContent; override;
  public
    constructor CreateH2(aRequest: TRequest; aConn: TH2Connection; aStreamID: Cardinal);
    property H2Connection: TH2Connection read FH2Conn;
    property StreamID: Cardinal read FStreamID;
  end;

  { TH2Dispatcher -
    Turns a completed stream (TH2Connection.OnRequestReady) into
    a dispatch through the Server.HandleRequest pipeline. }

  TH2Dispatcher = class
  private
    FServer: TFPCustomHttpServer;
  public
    constructor Create(aServer: TFPCustomHttpServer);
    // Signature-compatible with TH2RequestEvent (fphttp2connection).
    procedure HandleRequestReady(aConn: TH2Connection; aStreamID: Cardinal;
      aHeaders: THPackHeaderTextList; const aBody: TBytes);
    property Server: TFPCustomHttpServer read FServer;
  end;

// §8 rejection checking. Raises EH2StreamError(ecProtocolError, aStreamID) on the
// FIRST RFC 9113 §8 violation in the decoded request header list
procedure ValidateH2RequestHeaders(aHeaders: THPackHeaderTextList; aStreamID: Cardinal);

// h2c (RFC 7540 §3.2.1) - convert the upgraded HTTP/1.1 request into the RFC 9113
// §8-CLEAN HTTP/2 header list that becomes stream 1, plus its body (out param).
function H2HeadersFromHTTPRequest(aRequest: TRequest; out aBody: TBytes): THPackHeaderTextList;

implementation

type
  TServerCracker = class(TFPCustomHttpServer);

// True for the response headers HTTP/2 forbids
function IsForbiddenResponseHeader(const aLowerName: string): Boolean;
begin
  Result := (aLowerName = 'connection')
         or (aLowerName = 'keep-alive')
         or (aLowerName = 'proxy-connection')
         or (aLowerName = 'transfer-encoding')
         or (aLowerName = 'upgrade')
         or (aLowerName = 'status');
end;

// True for the §8.2.2 connection-specific request headers HTTP/2 forbids
function IsForbiddenRequestHeader(const aLowerName: string): Boolean;
begin
  Result := (aLowerName = 'connection')
         or (aLowerName = 'keep-alive')
         or (aLowerName = 'proxy-connection')
         or (aLowerName = 'transfer-encoding')
         or (aLowerName = 'upgrade');
end;

{ TH2Request }

constructor TH2Request.CreateFromH2(aHeaders: THPackHeaderTextList;
  const aBody: TBytes);
var
  I, P: Integer;
  Name, Value, Scheme, PathPart: RawByteString;
begin
  inherited Create;
  // Scheme is a managed string -> auto-initialised to '' on entry.
  for I := 0 to aHeaders.Count - 1 do
  begin
    Name := aHeaders[I]^.HeaderName;
    Value := aHeaders[I]^.HeaderValue;
    if (Length(Name) > 0) and (Name[1] = ':') then
    begin
      // Pseudo-headers (§8.1.2.1) - mapped onto request fields in this ONE place.
      if Name = H2_PSEUDO_METHOD then
        Method := Value
      else if Name = H2_PSEUDO_PATH then
        URL := Value                 // origin-form incl. ?query; split by InitRequestVars
      else if Name = H2_PSEUDO_AUTHORITY then
        Host := Value
      else if Name = H2_PSEUDO_SCHEME then
        Scheme := Value;
    end
    else
      // Regular header: known -> slot, known var -> var, else -> custom header
      // (so unknown headers survive). [httpdefs SetFieldByName]
      SetFieldByName(Name, Value);
  end;
  HttpVersion := '2.0';
  PathPart := URL;
  P := Pos('?', PathPart);
  if P > 0 then
    PathPart := Copy(PathPart, 1, P - 1);
  if (Length(PathPart) > 1) and (PathPart[1] <> '/') then
    PathPart := '/' + PathPart
  else if PathPart = '/' then
    PathPart := '';
  PathInfo := PathPart;
  ContentBytes := aBody;
  InitRequestVars;
end;

{ TH2Response }

constructor TH2Response.CreateH2(aRequest: TRequest; aConn: TH2Connection;
  aStreamID: Cardinal);
begin
  inherited Create(aRequest);
  FH2Conn := aConn;
  FStreamID := aStreamID;
end;

procedure TH2Response.DoSendHeaders(Headers: TStrings);
var
  List: THPackHeaderTextList;
  I, P: Integer;
  Line, Name, Value: string;
begin
  // Build the HPACK header list.
  List := THPackHeaderTextList.Create;
  try
    List.Add(H2_PSEUDO_STATUS, IntToStr(Code));
    for I := 0 to Headers.Count - 1 do
    begin
      Line := Headers[I];
      P := Pos(':', Line);            // 'Name: Value' (CollectHeaders uses ': ')
      if P < 1 then
        Continue;                     // blank trailing line / not a header
      Name := LowerCase(Trim(Copy(Line, 1, P - 1)));
      if Name = '' then
        Continue;
      if IsForbiddenResponseHeader(Name) then
        Continue;
      Value := Trim(Copy(Line, P + 1, Length(Line) - P));
      List.Add(Name, Value);
    end;
    // END_HEADERS is set automatically by SendHeaders; NO END_STREAM here
    FH2Conn.SendHeaders(FStreamID, List, 0);
  finally
    List.Free;
  end;
end;

procedure TH2Response.DoSendContent;
var
  Body: TBytes;
begin
  // Body bytes, kept as TBytes end-to-end 
  if Assigned(ContentStream) then
  begin
    ContentStream.Position := 0;
    SetLength(Body, ContentStream.Size);
    if ContentStream.Size > 0 then
      ContentStream.ReadBuffer(Body[0], ContentStream.Size);
  end
  else
  begin
    SetLength(Body, Length(Content));
    if Length(Content) > 0 then
      Move(Content[1], Body[0], Length(Content));
  end;
  // Emit DATA with END_STREAM last. An empty body still closes the stream via the
  // window-exempt zero-length END_STREAM DATA frame (1.7).
  FH2Conn.SendData(FStreamID, Body, True);
end;

// §8 validation - ONE ordered pass over the decoded header list plus a pseudo-header count.
procedure ValidateH2RequestHeaders(aHeaders: THPackHeaderTextList; aStreamID: Cardinal);
var
  I, J: Integer;
  Name, LName, Value, Scheme, Path, Authority: RawByteString;
  SeenRegular, HasMethod, HasScheme, HasPath, HasAuthority: Boolean;
  Ch: Char;

  procedure Reject(const aReason: string);
  begin
    raise EH2StreamError.Create(ecProtocolError, aStreamID, aReason);
  end;

begin
  SeenRegular := False;
  HasMethod := False; HasScheme := False; HasPath := False; HasAuthority := False;
  Scheme := ''; Path := ''; Authority := '';
  for I := 0 to aHeaders.Count - 1 do
  begin
    Name := aHeaders[I]^.HeaderName;
    Value := aHeaders[I]^.HeaderValue;
    // (#5) field names MUST be lowercase (§8.2.1).e.
    for J := 1 to Length(Name) do
    begin
      Ch := Name[J];
      if (Ch >= 'A') and (Ch <= 'Z') then
        Reject('uppercase field name (§8.2.1): ' + Name);
    end;
    if (Length(Name) > 0) and (Name[1] = ':') then
    begin
      if SeenRegular then
        Reject('pseudo-header after regular header (§8.3.1): ' + Name);
      if Name = H2_PSEUDO_METHOD then
      begin
        if HasMethod then Reject('duplicate :method (§8.3.1)');
        HasMethod := True;
      end
      else if Name = H2_PSEUDO_SCHEME then
      begin
        if HasScheme then Reject('duplicate :scheme (§8.3.1)');
        HasScheme := True; Scheme := Value;
      end
      else if Name = H2_PSEUDO_AUTHORITY then
      begin
        if HasAuthority then Reject('duplicate :authority (§8.3.1)');
        HasAuthority := True; Authority := Value;
      end
      else if Name = H2_PSEUDO_PATH then
      begin
        if HasPath then Reject('duplicate :path (§8.3.1)');
        HasPath := True; Path := Value;
      end
      else
        Reject('unknown/undefined request pseudo-header (§8.3.1): ' + Name);
    end
    else
    begin
      SeenRegular := True;
      LName := Name;   // already proven lowercase above
      if IsForbiddenRequestHeader(LName) then
        Reject('connection-specific header (§8.2.2): ' + LName);
      if (LName = 'te') and (Value <> 'trailers') then
        Reject('TE must be "trailers" (§8.2.2): ' + Value);
    end;
  end;
  if not HasMethod then Reject('missing :method (§8.3.1)');
  if not HasScheme then Reject('missing :scheme (§8.3.1)');
  if not HasPath   then Reject('missing :path (§8.3.1)');
  if (Scheme <> 'http') and (Scheme <> 'https') then
    Reject('bad :scheme (§8.3.1): ' + Scheme);
  if Path = '' then
    Reject('empty :path (§8.3.1)');
  if (Path[1] <> '/') and (Path <> '*') then
    Reject('malformed :path, not origin-form (§8.3.1): ' + Path);
  if Pos('@', Authority) > 0 then
    Reject('malformed :authority, userinfo present (§8.3.1): ' + Authority);
end;

// True for the request field names that MUST NOT cross the h2c upgrade boundary
function IsDroppedUpgradeHeader(const aLowerName: string): Boolean;
begin
  Result := (aLowerName = 'connection')
         or (aLowerName = 'keep-alive')
         or (aLowerName = 'proxy-connection')
         or (aLowerName = 'transfer-encoding')
         or (aLowerName = 'upgrade')
         or (aLowerName = 'http2-settings')
         or (aLowerName = 'host');
end;

function H2HeadersFromHTTPRequest(aRequest: TRequest; out aBody: TBytes): THPackHeaderTextList;
var
  H: THeader;
  I, ContentLen: Integer;
  N, V: string;
  BodyStr: RawByteString;

  // Add one §8-clean regular header: lowercase the name
  procedure AddRegular(const aName, aValue: string);
  var
    Lower: string;
  begin
    Lower := LowerCase(aName);
    if Lower = '' then
      Exit;
    if IsDroppedUpgradeHeader(Lower) then
      Exit;
    if (Lower = 'te') and (aValue <> 'trailers') then
      Exit;
    Result.Add(Lower, aValue);
  end;

begin
  Result := THPackHeaderTextList.Create;
  Result.Add(H2_PSEUDO_METHOD, aRequest.Method);
  Result.Add(H2_PSEUDO_SCHEME, 'http');
  Result.Add(H2_PSEUDO_AUTHORITY, aRequest.Host);
  Result.Add(H2_PSEUDO_PATH, aRequest.URL);
  for H in THeader do
    if (hdRequest in HTTPHeaderDirections[H]) and aRequest.HeaderIsSet(H) then
      AddRegular(HTTPHeaderNames[H], aRequest.GetHeader(H));
  if Assigned(aRequest.CustomHeaders) then
    for I := 0 to aRequest.CustomHeaders.Count - 1 do
    begin
      aRequest.CustomHeaders.GetNameValue(I, N, V);
      if (N <> '') and (V <> '') then
        AddRegular(N, V);
    end;
  BodyStr := aRequest.Content;
  ContentLen := Length(BodyStr);
  SetLength(aBody, ContentLen);
  if ContentLen > 0 then
    Move(BodyStr[1], aBody[0], ContentLen);
end;

{ TH2Dispatcher }

constructor TH2Dispatcher.Create(aServer: TFPCustomHttpServer);
begin
  inherited Create;
  FServer := aServer;
end;

procedure TH2Dispatcher.HandleRequestReady(aConn: TH2Connection;
  aStreamID: Cardinal; aHeaders: THPackHeaderTextList; const aBody: TBytes);
var
  Req: TFPHTTPConnectionRequest;      // EXACT var-param types
  Resp: TFPHTTPConnectionResponse;
begin
  // §8 rejection check
  ValidateH2RequestHeaders(aHeaders, aStreamID);
  Req := TH2Request.CreateFromH2(aHeaders, aBody);
  try
    Resp := TH2Response.CreateH2(Req, aConn, aStreamID);
    try
      // Reuse the existing, unmodified pipeline
      TServerCracker(FServer).HandleRequest(Req, Resp);
      // Ensure the response is flushed even if the handler did not send it
      if not Resp.ContentSent then
        Resp.SendContent;
    finally
      Resp.Free;
    end;
  finally
    Req.Free;
  end;
end;

end.
