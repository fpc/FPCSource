{

    HTTP Servlet Classes
    Copyright (c) 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit HTTPSvlt;

interface

uses SysUtils, Classes, fpAsync, fpSock, HTTPBase, Servlets;

resourcestring
  SErrUnknownMethod = 'Unknown HTTP method "%s" used';
  SErrUnsupportedMethod = 'HTTP method "%s" is not supported for this URL';

type

  THttpSession = class
  public
    property Attributes[const AName: String]: TObject;  // !!!: Implement this  rw
    property CreationTime: TDateTime;   // !!!: Implement this
    property ID: String;                // !!!: Implement this
    property LastAccessedTime: TDateTime;       // !!!: Implement this
    property MaxInactiveInterval: TDateTime;    // !!!: Implement this  rw
    property ServletContext: TServletContext;   // !!!: Implement this
    property IsNew: Boolean;            // !!!: Implement this

    // procedure Invalidate;            // !!!: Implement this
    // procedure RemoveAttribute(const AName: String);  // !!!: Implement this
  end;

  THttpServletRequest = class(TServletRequest)
  private
    RequestHeader: THTTPRequestHeader;
  protected
    function GetContentLength: Integer; override;
    function GetContentType: String; override;
    function GetProtocol: String; override;
    function GetMethod: String;
    function GetRequestURI: String;
    function GetQueryString: String;
  public
    constructor Create(ARequestHeader: THTTPRequestHeader; AInputStream: TStream;
      const AScheme, APathInfo: String);
    // GetSession

    // function IsRequestedSessionIdFromCookie: Boolean;        // !!!: Implement this
    // function IsRequestedSessionIdFromURL: Boolean;   // !!!: Implement this
    // function IsRequestedSessionIdValid: Boolean;     // !!!: Implement this

    property AuthType: String;          // !!!: How to implement?
    property ContextPath: String;       // !!!: How to implement?
    property CookieCount: Integer;      // !!!: How to implement?
    property Cookies[Index: Integer]: Pointer;  // !!!: How to implement?
    property DateHeaders[const AName: String]: TDateTime;       // !!!: Implement this
    property Headers[const AName: String]: String;      // !!!: Implement this
    property IntHeaders[const AName: String]: Integer;  // !!!: Implement this
    property Method: String read GetMethod;
    property PathInfo: String read FPathInfo;
    property PathTranslated: String;    // !!!: How to implement?
    property QueryString: String read GetQueryString;
    property RemoteUser: String;        // !!!: How to implement?
    property RequestedSessionID: String;        // !!!: How to implement?
    property RequestURI: String read GetRequestURI;
    property RequestURL: String;        // !!!: How to implement?
    property ServletPath: String;       // !!!: How to implement?
  end;

  THttpServletResponse = class(TServletResponse)
  private
    ResponseHeader: THTTPResponseHeader;
  protected
    procedure SetContentType(const Value: String); override;
    procedure SetContentLength(Value: Int64); override;
  public
    constructor Create(AResponseHeader: THTTPResponseHeader;
      AOutputStream: TStream);
    // procedure AddCookie(Cookie: TCookie);    // !!!: Implement this
    // procedure AddDateHeader(const AName: String; ADate: TDateTime);  // !!!: Implement this
    // procedure AddHeader(const AName, AValue: String);        // !!!: Implement this
    // procedure AddIntHeader(const AName: String; AValue: Int64);      // !!!: Implement this
    // function ContainsHeader(const AName: String): Boolean;   // !!!: Implement this
    // function EncodeRedirectURL(const URL: String): String;   // !!!: Implement this
    // function EncodeURL(const URL: String): String;   // !!!: Implement this
    // procedure SendError(StatusCode: Integer);        // !!!: Implement this
    // procedure SendError(StatusCode: Integer; const Msg: String);     // !!!: Implement this
    // procedure SendRedirect(const Location: String);  // !!!: Implement this
    // procedure SetDateHeader(const AName: String; ADate: TDateTime);  // !!!: Implement this
    // procedure SetHeader(const AName, AValue: String);        // !!!: Implement this
    // procedure SetIntHeader(const AName: String; AValue: Int64);      // !!!: Implement this
    // procedure SetStatus(StatusCode: Integer);        // !!!: Implement this
    // procedure SetStatus(StatusCode: Integer; const Msg: String);     // !!!: Implement this
  end;

  THttpServlet = class(TGenericServlet)
  protected
    // function GetLastModified(Req: THttpServletRequest): TDateTime;
    // Handlers for HTTP methods
    procedure DoDelete(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoGet(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoHead(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoOptions(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoPost(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoPut(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure DoTrace(Req: THttpServletRequest; Resp: THttpServletResponse);
      virtual; abstract;
    procedure Service(Req: THttpServletRequest; Resp: THttpServletResponse); virtual;
  end;

  // A simple file retreiving servlet
  TCustomFileServlet = class(THttpServlet)
  private
    FPath: String;
  protected
    procedure DoGet(Req: THttpServletRequest; Resp: THttpServletResponse); override;
    property Path: String read FPath write FPath;
  end;

  TFileServlet = class(TCustomFileServlet)
  published
    property Path;
  end;


  // HTTP server (servlet container)

  TServletMapping = class(TCollectionItem)
  private
    FServlet: TGenericServlet;
    FURLPattern: String;
  published
    property Servlet: TGenericServlet read FServlet write FServlet;
    property URLPattern: String read FURLPattern write FURLPattern;
  end;

  TServletMappings = class(TCollection)
  private
    function GetItem(Index: Integer): TServletMapping;
    procedure SetItem(Index: Integer; Value: TServletMapping);
  public
    property Items[Index: Integer]: TServletMapping read GetItem write SetItem;
      default;
  end;

  THttpServer = class(TCustomTCPServer)
  private
    Connections: TList;         // List of THttpServerConnection objects
    FServletMappings: TServletMappings;
  protected
    procedure DoConnect(AStream: TSocketStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddServlet(AServlet: THttpServlet; const AURLPattern: String);
    // procedure RemoveServlet(const APathName: String);
  published
    property Active;
    property Port;
    property OnQueryConnect;
    property OnConnect;
    property ServletMappings: TServletMappings
      read FServletMappings write FServletMappings;
  end;


{ No, this one really doesn't belong to here - but as soon as we don't have a
  nice solution for platform-independent component streaming in the FCL classes
  unit, it will be left here. }
function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;



implementation


constructor THttpServletRequest.Create(ARequestHeader: THTTPRequestHeader;
  AInputStream: TStream; const AScheme, APathInfo: String);
begin
  inherited Create(AInputStream, AScheme, APathInfo);
  RequestHeader := ARequestHeader;
end;

function THttpServletRequest.GetContentLength: Integer;
begin
  Result := RequestHeader.ContentLength;
end;

function THttpServletRequest.GetContentType: String;
begin
  Result := RequestHeader.ContentType;
end;

function THttpServletRequest.GetProtocol: String;
begin
  Result := 'HTTP/' + RequestHeader.HttpVersion;
end;

function THttpServletRequest.GetMethod: String;
begin
  Result := RequestHeader.Command;
end;

function THttpServletRequest.GetRequestURI: String;
begin
  Result := RequestHeader.URI;
end;

function THttpServletRequest.GetQueryString: String;
begin
  Result := RequestHeader.QueryString;
end;


constructor THttpServletResponse.Create(AResponseHeader: THTTPResponseHeader;
  AOutputStream: TStream);
begin
  inherited Create(AOutputStream);
  ResponseHeader := AResponseHeader;
end;

procedure THttpServletResponse.SetContentType(const Value: String);
begin
  ResponseHeader.ContentType := Value;
end;

procedure THttpServletResponse.SetContentLength(Value: Int64);
begin
  ResponseHeader.ContentLength := Value;
end;


procedure THttpServlet.Service(Req: THttpServletRequest; Resp: THttpServletResponse);
var
  Method: String;
begin
  Method := Req.Method;
  try
    if Method = 'DELETE' then
      DoDelete(Req, Resp)
    else if Method = 'GET' then
      DoGet(Req, Resp)
    else if Method = 'HEAD' then
      DoHead(Req, Resp)
    else if Method = 'OPTIONS' then
      DoOptions(Req, Resp)
    else if Method = 'POST' then
      DoPost(Req, Resp)
    else if Method = 'PUT' then
      DoPut(Req, Resp)
    else if Method = 'TRACE' then
      DoTrace(Req, Resp)
    else
      raise EServlet.CreateFmt(SErrUnknownMethod, [Method]);
  except
    on e: EAbstractError do
      raise EServlet.CreateFmt(SErrUnsupportedMethod, [Method]);
  end;
end;


procedure TCustomFileServlet.DoGet(Req: THttpServletRequest;
  Resp: THttpServletResponse);
var
  f: TStream;
  s: String;
  i, LastStart: Integer;
begin
  s := Req.PathInfo;
  i := 1;
  LastStart := 1;
  while i <= Length(s) do
  begin
    if (s[i] = '/') or (s[i] = '\') then
      LastStart := i + 1
    else if (i = LastStart) and (s[i] = '.') and (i < Length(s)) and
      (s[i + 1] = '..') then
      exit;             // !!!: are ".." allowed in URLs?
    Inc(i);
  end;

  if s = '' then
    s := 'index.html';

  f := TFileStream.Create(Path + '/' + s, fmOpenRead);
  try
    Resp.OutputStream.CopyFrom(f, f.Size);
  finally
    f.Free;
  end;
end;


// HTTP Server

function TServletMappings.GetItem(Index: Integer): TServletMapping;
begin
  Result := TServletMapping(inherited GetItem(Index));
end;

procedure TServletMappings.SetItem(Index: Integer; Value: TServletMapping);
begin
  inherited SetItem(Index, Value);
end;

type
  THttpServerConnection = class
  private
    Server: THttpServer;
    Stream: TSocketStream;
    RequestHeader: THttpRequestHeader;
    RequestStream: TMemoryStream;
    ResponseHeader: THttpResponseHeader;
    ResponseStream: TMemoryStream;
    BytesToRead, BytesToWrite: Integer;
    DataAvailableNotifyHandle: Pointer;
    CanSendNotifyHandle: Pointer;
    SendBuffer: Pointer;
    procedure RequestHeaderReceived(Sender: TObject);
    procedure DataAvailable(Sender: TObject);
    procedure RequestStreamReceived;
    procedure ResponseHeaderSent(Sender: TObject);
    procedure CanSend(Sender: TObject);
  public
    constructor Create(AServer: THttpServer; AStream: TSocketStream);
    destructor Destroy; override;
  end;


constructor THttpServerConnection.Create(AServer: THttpServer;
  AStream: TSocketStream);
begin
  inherited Create;
  Server := AServer;
  Stream := AStream;
  RequestHeader := THttpRequestHeader.Create;
  RequestHeader.OnCompleted := @RequestHeaderReceived;
  RequestHeader.AsyncReceive(Server.EventLoop, Stream);
end;

destructor THttpServerConnection.Destroy;
begin
  if Assigned(DataAvailableNotifyHandle) then
    Server.EventLoop.ClearDataAvailableNotify(DataAvailableNotifyHandle);
  if Assigned(CanSendNotifyHandle) then
    Server.EventLoop.ClearCanWriteNotify(CanSendNotifyHandle);
  RequestHeader.Free;
  RequestStream.Free;
  ResponseHeader.Free;
  ResponseStream.Free;
  Stream.Free;
  Server.Connections.Remove(Self);
  inherited Destroy;
end;

procedure THttpServerConnection.RequestHeaderReceived(Sender: TObject);
var
  BytesInBuffer: Integer;
  NeedMoreData: Boolean;
begin
  // WriteLn('HTTP-Header empfangen');

  BytesInBuffer:= RequestHeader.Reader.BytesInBuffer;
  BytesToRead := RequestHeader.ContentLength;
  // WriteLn('Content-Length: ', BytesToRead, ', noch im Puffer: ', BytesInBuffer);

  RequestStream := TMemoryStream.Create;

  NeedMoreData := RequestHeader.Command = 'POST';

  if BytesInBuffer > 0 then
  begin
    RequestStream.Write(RequestHeader.Reader.Buffer^, BytesInBuffer);
    if BytesToRead > 0 then
      Dec(BytesToRead, BytesInBuffer);

    if BytesInBuffer = RequestHeader.ContentLength then
      NeedMoreData := False;
  end;

  if NeedMoreData then
    DataAvailableNotifyHandle := Server.EventLoop.SetDataAvailableNotify(
      Stream.Handle, @DataAvailable, nil)
  else
    RequestStreamReceived;
end;

procedure THttpServerConnection.DataAvailable(Sender: TObject);
var
  Buffer: array[0..4095] of Byte;
  ReadNow, BytesRead: Integer;
begin
  ReadNow := SizeOf(Buffer);
  if (BytesToRead > 0) and (ReadNow > BytesToRead) then
    ReadNow := BytesToRead;

  BytesRead := Stream.Read(Buffer, ReadNow);
  // WriteLn('Sollte ', ReadNow, ' Bytes lesen, ', BytesRead, ' wurden gelesen');

  RequestStream.Write(Buffer, BytesRead);
  if BytesToRead > 0 then
  begin
    Dec(BytesToRead, BytesRead);
    if BytesToRead = 0 then
    begin
      Server.EventLoop.ClearDataAvailableNotify(DataAvailableNotifyHandle);
      DataAvailableNotifyHandle := nil;
      RequestStreamReceived;
    end;
  end;
end;

procedure THttpServerConnection.RequestStreamReceived;
var
  i : Integer;
  s, URI: String;
  Servlet: TGenericServlet;
  Request: THttpServletRequest;
  Response: THttpServletResponse;
begin
  // WriteLn('Stream received: ', RequestStream.Size, ' bytes');

  URI := UpperCase(RequestHeader.URI);
  I:=0;
  Servlet:=Nil;
  While (Servlet=Nil) and (I<Server.ServletMappings.Count) do
    begin
    s := UpperCase(Server.ServletMappings[i].URLPattern);
    if ((s[Length(s)] = '*') and (Copy(s, 1, Length(s) - 1) =
      Copy(URI, 1, Length(s) - 1))) or (s = URI) then
      Servlet:=Server.ServletMappings[i].Servlet;
    inc(I);   
    end;

  if RequestHeader.ContentLength = 0 then
    RequestHeader.ContentLength := RequestStream.Size;
  RequestStream.Position := 0;

  if s[Length(s)] = '*' then
    s := Copy(s, 1, Length(s) - 1);
  Request := THttpServletRequest.Create(RequestHeader, RequestStream, 'http',
    Copy(RequestHeader.URI, Length(s) + 1, Length(RequestHeader.URI)));
  ResponseHeader := THTTPResponseHeader.Create;
  ResponseHeader.Connection := 'Keep-Alive';
  ResponseStream := TMemoryStream.Create;
  Response := THttpServletResponse.Create(ResponseHeader, ResponseStream);

  try
    try
      if Assigned(Servlet) then
        if Servlet.InheritsFrom(THttpServlet) then
          THttpServlet(Servlet).Service(Request, Response)
        else
          Servlet.Service(Request, Response)
      else
      begin
        ResponseHeader.ContentType := 'text/plain';
        s := 'Invalid URL';
        ResponseStream.Write(s[1], Length(s));
      end;
    except
      on e: Exception do
      begin
        s := 'An error occured: ' + ' ' + e.Message;
        ResponseHeader.ContentType := 'text/plain';
        ResponseStream.Write(s[1], Length(s));
      end;
    end;

    BytesToWrite := ResponseStream.Size;
    SendBuffer := ResponseStream.Memory;
    ResponseStream.Position := 0;
    ResponseHeader.ContentLength := BytesToWrite;
    ResponseHeader.OnCompleted := @ResponseHeaderSent;
    ResponseHeader.AsyncSend(Server.EventLoop, Stream);
  finally
    Response.Free;
    Request.Free;
  end;
  // WriteLn('Antwort wurde generiert');
end;

procedure THttpServerConnection.ResponseHeaderSent(Sender: TObject);
begin
  // WriteLn('Antwortheader geschickt');
  if BytesToWrite > 0 then
    CanSendNotifyHandle := Server.EventLoop.SetCanWriteNotify(Stream.Handle,
      @CanSend, nil);
end;

procedure THttpServerConnection.CanSend(Sender: TObject);
var
  BytesWritten: Integer;
begin
  BytesWritten := Stream.Write(SendBuffer^, BytesToWrite);
  Dec(BytesToWrite, BytesWritten);
  Inc(SendBuffer, BytesWritten);
  if BytesToWrite = 0 then
  begin
    // WriteLn('Antwortdaten geschickt');
    Free;
  end;
end;


constructor THttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ServletMappings := TServletMappings.Create(TServletMapping);
end;

destructor THttpServer.Destroy;
var
  i: Integer;
begin
  ServletMappings.Free;
  if Assigned(Connections) then
  begin
    for i := 0 to Connections.Count - 1 do
      THttpServerConnection(Connections[i]).Free;
    Connections.Free;
  end;
  inherited Destroy;
end;

procedure THttpServer.AddServlet(AServlet: THttpServlet;
  const AURLPattern: String);
var
  Mapping: TServletMapping;
begin
  Mapping := TServletMapping(ServletMappings.Add);
  Mapping.Servlet := AServlet;
  Mapping.URLPattern := AURLPattern;
end;

{procedure THttpServer.RemoveServlet(const APathName: String);
var
  i: Integer;
begin
  for i := 0 to Servlets.Count - 1 do
    if TServletInfo(Servlets[i]).PathName = APathName then
    begin
      TServletInfo(Servlets[i]).Free;
      Servlets.Delete(i);
      break;
    end;
end;}

procedure THttpServer.DoConnect(AStream: TSocketStream);
begin
  // WriteLn('Incoming HTTP connection');
  if not Assigned(Connections) then
    Connections := TList.Create;
  Connections.Add(THttpServerConnection.Create(Self, AStream));
end;




function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

  function DoInitClass(ClassType: TClass): Boolean;
  var
    Filename: String;
    TextStream, BinStream: TStream;
  begin
    Result := False;
    if (ClassType <> TComponent) and (ClassType <> RootAncestor) then
    begin
      { Init the parent class first }
      Result := DoInitClass(ClassType.ClassParent);

      Filename := LowerCase(Copy(ClassType.ClassName, 2, 255)) + '.frm';

      TextStream := nil;
      BinStream := nil;
      try
        try
          TextStream := TFileStream.Create(Filename, fmOpenRead);
        except
          exit;
        end;
        BinStream := TMemoryStream.Create;
        ObjectTextToBinary(TextStream, BinStream);
        BinStream.Position := 0;
        BinStream.ReadComponent(Instance);
        Result := True;
      finally
        TextStream.Free;
        BinStream.Free;
      end;
    end;
  end;

begin
  {!!!: GlobalNameSpace.BeginWrite;
  try}
    if (Instance.ComponentState * [csLoading, csInline]) = [] then
    begin
      BeginGlobalLoading;
      try
        Result := DoInitClass(Instance.ClassType);
        NotifyGlobalLoading;
      finally
        EndGlobalLoading;
      end;
    end else
      Result := DoInitClass(Instance.ClassType);
  {finally
    GlobalNameSpace.EndWrite;
  end;}
end;

end.
