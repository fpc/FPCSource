{
    $Id$

    HTTP: Classes for dealing with HTTP requests
    Copyright (C) 2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}
{$H+}

unit HTTP;

interface

uses Classes, AsyncIO, SSockets;

const

  fieldAccept = 'Accept';
  fieldAcceptCharset = 'Accept-Charset';
  fieldAcceptEncoding = 'Accept-Encoding';
  fieldAcceptLanguage = 'Accept-Language';
  fieldAuthorization = 'Authorization';
  fieldContentEncoding = 'Content-Encoding';
  fieldContentLanguage = 'Content-Language';
  fieldContentLength = 'Content-Length';
  fieldContentType = 'Content-Type';
  fieldCookie = 'Cookie';
  fieldDate = 'Date';
  fieldExpires = 'Expires';
  fieldFrom = 'From';
  fieldIfModifiedSince = 'If-Modified-Since';
  fieldLastModified = 'Last-Modified';
  fieldLocation = 'Location';
  fieldPragma = 'Pragma';
  fieldReferer = 'Referer';
  fieldRetryAfter = 'Retry-After';
  fieldServer = 'Server';
  fieldSetCookie = 'Set-Cookie';
  fieldUserAgent = 'User-Agent';
  fieldWWWAuthenticate = 'WWW-Authenticate';

type

  PHttpField = ^THttpField;
  THttpField = record
    Name, Value: String;
  end;


  THttpHeader = class
  protected
    FReader: TAsyncStreamLineReader;
    FWriter: TAsyncWriteStream;
    FOnCompleted: TNotifyEvent;
    FFields: TList;
    CmdReceived: Boolean;

    procedure ParseFirstHeaderLine(const line: String); virtual; abstract;
    procedure LineReceived(const line: String);
    function  GetFirstHeaderLine: String; virtual; abstract;
    procedure WriterCompleted(ASender: TObject);

    function  GetFieldCount: Integer;
    function  GetFields(AIndex: Integer): String;
    function  GetFieldNames(AIndex: Integer): String;
    procedure SetFieldNames(AIndex: Integer; const AName: String);
    function  GetFieldValues(AIndex: Integer): String;
    procedure SetFieldValues(AIndex: Integer; const AValue: String);


    function  GetAccept: String;
    procedure SetAccept(const AValue: String);
    function  GetAcceptCharset: String;
    procedure SetAcceptCharset(const AValue: String);
    function  GetAcceptEncoding: String;
    procedure SetAcceptEncoding(const AValue: String);
    function  GetAcceptLanguage: String;
    procedure SetAcceptLanguage(const AValue: String);
    function  GetAuthorization: String;
    procedure SetAuthorization(const AValue: String);
    function  GetContentEncoding: String;
    procedure SetContentEncoding(const AValue: String);
    function  GetContentLanguage: String;
    procedure SetContentLanguage(const AValue: String);
    function  GetContentLength: Integer;
    procedure SetContentLength(AValue: Integer);
    function  GetContentType: String;
    procedure SetContentType(const AValue: String);
    function  Get_Cookie: String;
    procedure Set_Cookie(const AValue: String);
    function  GetDate: String;
    procedure SetDate(const AValue: String);
    function  GetExpires: String;
    procedure SetExpires(const AValue: String);
    function  GetFrom: String;
    procedure SetFrom(const AValue: String);
    function  GetIfModifiedSince: String;
    procedure SetIfModifiedSince(const AValue: String);
    function  GetLastModified: String;
    procedure SetLastModified(const AValue: String);
    function  GetLocation: String;
    procedure SetLocation(const AValue: String);
    function  GetPragma: String;
    procedure SetPragma(const AValue: String);
    function  GetReferer: String;
    procedure SetReferer(const AValue: String);
    function  GetRetryAfter: String;
    procedure SetRetryAfter(const AValue: String);
    function  GetServer: String;
    procedure SetServer(const AValue: String);
    function  Get_SetCookie: String;
    procedure Set_SetCookie(const AValue: String);
    function  GetUserAgent: String;
    procedure SetUserAgent(const AValue: String);
    function  GetWWWAuthenticate: String;
    procedure SetWWWAuthenticate(const AValue: String);

  public
    HttpVersion: String;

    constructor Create;
    destructor Destroy; override;
    procedure SetFieldByName(const AName, AValue: String);
    function  GetFieldByName(const AName: String): String;

    procedure AsyncSend(AManager: TAsyncIOManager; AStream: THandleStream);
    procedure AsyncReceive(AManager: TAsyncIOManager; AStream: THandleStream);

    property Reader: TAsyncStreamLineReader read FReader;
    property Writer: TAsyncWriteStream read FWriter;
    property FieldCount: Integer read GetFieldCount;
    property Fields[AIndex: Integer]: String read GetFields;
    property FieldNames[AIndex: Integer]: String read GetFieldNames write SetFieldNames;
    property FieldValues[AIndex: Integer]: String read GetFieldValues write SetFieldValues;

    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;

    property Accept: String read GetAccept write SetAccept;
    property AcceptCharset: String read GetAcceptCharset write SetAcceptCharset;
    property AcceptEncoding: String read GetAcceptEncoding write SetAcceptEncoding;
    property AcceptLanguage: String read GetAcceptLanguage write SetAcceptLanguage;
    property Authorization: String read GetAuthorization write SetAuthorization;
    property ContentEncoding: String read GetContentEncoding write SetContentEncoding;
    property ContentLanguage: String read GetContentLanguage write SetContentLanguage;
    property ContentLength: Integer read GetContentLength write SetContentLength;
    property ContentType: String read GetContentType write SetContentType;
    property Cookie: String read Get_Cookie write Set_Cookie;
    property Date: String read GetDate write SetDate;
    property Expires: String read GetExpires write SetExpires;
    property From: String read GetFrom write SetFrom;
    property IfModifiedSince: String read GetIfModifiedSince write SetIfModifiedSince;
    property LastModified: String read GetLastModified write SetLastModified;
    property Location: String read GetLocation write SetLocation;
    property Pragma: String read GetPragma write SetPragma;
    property Referer: String read GetReferer write SetReferer;
    property RetryAfter: String read GetRetryAfter write SetRetryAfter;
    property Server: String read GetServer write SetServer;
    property SetCookie: String read Get_SetCookie write Set_SetCookie;
    property UserAgent: String read GetUserAgent write SetUserAgent;
    property WWWAuthenticate: String read GetWWWAuthenticate write SetWWWAuthenticate;
  end;


  THttpRequestHeader = class(THttpHeader)
  protected
    procedure ParseFirstHeaderLine(const line: String); override;
    function  GetFirstHeaderLine: String; override;
  public
    CommandLine: String;
    Command: String;
    URI: String;                // Uniform Resource Identifier
  end;


  THttpAnswerHeader = class(THttpHeader)
  protected
    procedure ParseFirstHeaderLine(const line: String); override;
    function  GetFirstHeaderLine: String; override;
  public
    Code: Integer;
    CodeText: String;
    constructor Create;
  end;


  TCustomHttpConnection = class
  protected
    FManager: TAsyncIOManager;
    FSocket: TInetSocket;
    SendBuffer: TAsyncWriteStream;
    FOnHeaderSent, FOnStreamSent, FOnHeaderReceived, FOnStreamReceived: TNotifyEvent;
    RecvSize: Integer;  // How many bytes are still to be read. -1 if unknown.

    procedure HeaderToSendCompleted(Sender: TObject);
    procedure StreamToSendCompleted(Sender: TObject);
    procedure ReceivedHeaderCompleted(Sender: TObject);
    procedure DataAvailable(Sender: TObject);
    procedure ReceivedStreamCompleted(Sender: TObject);

    property OnHeaderSent: TNotifyEvent read FOnHeaderSent write FOnHeaderSent;
    property OnStreamSent: TNotifyEvent read FOnStreamSent write FOnStreamSent;
    property OnHeaderReceived: TNotifyEvent read FOnHeaderReceived write FOnHeaderReceived;
    property OnStreamReceived: TNotifyEvent read FOnStreamReceived write FOnStreamReceived;

  public
    HeaderToSend: THttpHeader;
    StreamToSend: TStream;
    ReceivedHeader: THttpHeader;
    ReceivedStream: TStream;

    constructor Create(AManager: TAsyncIOManager; ASocket: TInetSocket);
    destructor Destroy; override;
    procedure Start;
  end;

  THttpConnection = class(TCustomHttpConnection)
  public
    property OnHeaderSent;
    property OnStreamSent;
    property OnHeaderReceived;
    property OnStreamReceived;
  end;



// ===================================================================
// ===================================================================

implementation

uses SysUtils;


// -------------------------------------------------------------------
//   THttpHeader
// -------------------------------------------------------------------

procedure THttpHeader.LineReceived(const line: String);
var
  i: Integer;
begin
  if Length(line) = 0 then
  begin
    FReader.OnLine := nil;      // Stop receiving
    if Assigned(FOnCompleted) then
      FOnCompleted(Self);
    FReader.Free;
    FReader := nil;
  end else
    if not CmdReceived then
    begin
      CmdReceived := True;
      ParseFirstHeaderLine(line);
    end else
    begin
      i := Pos(':', line);
      SetFieldByName(Trim(Copy(line, 1, i - 1)), Trim(Copy(line, i + 1, Length(line))));
    end;
end;

procedure THttpHeader.WriterCompleted(ASender: TObject);
begin
  if Assigned(FOnCompleted) then
    FOnCompleted(Self);
end;

function THttpHeader.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function THttpHeader.GetFields(AIndex: Integer): String;
begin
  Result := FieldNames[AIndex] + ': ' + FieldValues[AIndex];
end;

function THttpHeader.GetFieldNames(AIndex: Integer): String;
begin
  Result := PHttpField(FFields.Items[AIndex])^.Name;
end;

procedure THttpHeader.SetFieldNames(AIndex: Integer; const AName: String);
begin
  PHttpField(FFields.Items[AIndex])^.Name := AName;
end;

function THttpHeader.GetFieldValues(AIndex: Integer): String;
begin
  Result := PHttpField(FFields.Items[AIndex])^.Value;
end;

procedure THttpHeader.SetFieldValues(AIndex: Integer; const AValue: String);
begin
  PHttpField(FFields.Items[AIndex])^.Name := AValue;
end;

function  THttpHeader.GetAccept: String; begin Result := GetFieldByName(fieldAccept) end;
procedure THttpHeader.SetAccept(const AValue: String); begin SetFieldByName(fieldAccept, AValue) end;
function  THttpHeader.GetAcceptCharset: String; begin Result := GetFieldByName(fieldAcceptCharset) end;
procedure THttpHeader.SetAcceptCharset(const AValue: String); begin SetFieldByName(fieldAcceptCharset, AValue) end;
function  THttpHeader.GetAcceptEncoding: String; begin Result := GetFieldByName(fieldAcceptEncoding) end;
procedure THttpHeader.SetAcceptEncoding(const AValue: String); begin SetFieldByName(fieldAcceptEncoding, AValue) end;
function  THttpHeader.GetAcceptLanguage: String; begin Result := GetFieldByName(fieldAcceptLanguage) end;
procedure THttpHeader.SetAcceptLanguage(const AValue: String); begin SetFieldByName(fieldAcceptLanguage, AValue) end;
function  THttpHeader.GetAuthorization: String; begin Result := GetFieldByName(fieldAuthorization) end;
procedure THttpHeader.SetAuthorization(const AValue: String); begin SetFieldByName(fieldAuthorization, AValue) end;
function  THttpHeader.GetContentEncoding: String; begin Result := GetFieldByName(fieldContentEncoding) end;
procedure THttpHeader.SetContentEncoding(const AValue: String); begin SetFieldByName(fieldContentEncoding, AValue) end;
function  THttpHeader.GetContentLanguage: String; begin Result := GetFieldByName(fieldContentLanguage) end;
procedure THttpHeader.SetContentLanguage(const AValue: String); begin SetFieldByName(fieldContentLanguage, AValue) end;
function  THttpHeader.GetContentLength: Integer; var s: String; begin s := GetFieldByName(fieldContentLength); if Length(s) = 0 then Result := -1 else Result := StrToInt(s) end;
procedure THttpHeader.SetContentLength(AValue: Integer); begin SetFieldByName(fieldContentLength, IntToStr(AValue)) end;
function  THttpHeader.GetContentType: String; begin Result := GetFieldByName(fieldContentType) end;
procedure THttpHeader.SetContentType(const AValue: String); begin SetFieldByName(fieldContentType, AValue) end;
function  THttpHeader.Get_Cookie: String; begin Result := GetFieldByName(fieldCookie) end;
procedure THttpHeader.Set_Cookie(const AValue: String); begin SetFieldByName(fieldCookie, AValue) end;
function  THttpHeader.GetDate: String; begin Result := GetFieldByName(fieldDate) end;
procedure THttpHeader.SetDate(const AValue: String); begin SetFieldByName(fieldDate, AValue) end;
function  THttpHeader.GetExpires: String; begin Result := GetFieldByName(fieldExpires) end;
procedure THttpHeader.SetExpires(const AValue: String); begin SetFieldByName(fieldExpires, AValue) end;
function  THttpHeader.GetFrom: String; begin Result := GetFieldByName(fieldFrom) end;
procedure THttpHeader.SetFrom(const AValue: String); begin SetFieldByName(fieldFrom, AValue) end;
function  THttpHeader.GetIfModifiedSince: String; begin Result := GetFieldByName(fieldIfModifiedSince) end;
procedure THttpHeader.SetIfModifiedSince(const AValue: String); begin SetFieldByName(fieldIfModifiedSince, AValue) end;
function  THttpHeader.GetLastModified: String; begin Result := GetFieldByName(fieldLastModified) end;
procedure THttpHeader.SetLastModified(const AValue: String); begin SetFieldByName(fieldLastModified, AValue) end;
function  THttpHeader.GetLocation: String; begin Result := GetFieldByName(fieldLocation) end;
procedure THttpHeader.SetLocation(const AValue: String); begin SetFieldByName(fieldLocation, AValue) end;
function  THttpHeader.GetPragma: String; begin Result := GetFieldByName(fieldPragma) end;
procedure THttpHeader.SetPragma(const AValue: String); begin SetFieldByName(fieldPragma, AValue) end;
function  THttpHeader.GetReferer: String; begin Result := GetFieldByName(fieldReferer) end;
procedure THttpHeader.SetReferer(const AValue: String); begin SetFieldByName(fieldReferer, AValue) end;
function  THttpHeader.GetRetryAfter: String; begin Result := GetFieldByName(fieldRetryAfter) end;
procedure THttpHeader.SetRetryAfter(const AValue: String); begin SetFieldByName(fieldRetryAfter, AValue) end;
function  THttpHeader.GetServer: String; begin Result := GetFieldByName(fieldServer) end;
procedure THttpHeader.SetServer(const AValue: String); begin SetFieldByName(fieldServer, AValue) end;
function  THttpHeader.Get_SetCookie: String; begin Result := GetFieldByName(fieldSetCookie) end;
procedure THttpHeader.Set_SetCookie(const AValue: String); begin SetFieldByName(fieldSetCookie, AValue) end;
function  THttpHeader.GetUserAgent: String; begin Result := GetFieldByName(fieldUserAgent) end;
procedure THttpHeader.SetUserAgent(const AValue: String); begin SetFieldByName(fieldUserAgent, AValue) end;
function  THttpHeader.GetWWWAuthenticate: String; begin Result := GetFieldByName(fieldWWWAuthenticate) end;
procedure THttpHeader.SetWWWAuthenticate(const AValue: String); begin SetFieldByName(fieldWWWAuthenticate, AValue) end;

constructor THttpHeader.Create;
begin
  inherited Create;
  FFields := TList.Create;
  HttpVersion := '1.0';
end;

destructor THttpHeader.Destroy;
var
  i: Integer;
  field: PHttpField;
begin
  FReader.Free;
  FWriter.Free;
  for i := 0 to FFields.Count - 1 do begin
    field := PHttpField(FFields.Items[i]);
    SetLength(field^.Name, 0);
    SetLength(field^.Value, 0);
    Dispose(field);
  end;
  FFields.Free;
  inherited Destroy;
end;

function THttpHeader.GetFieldByName(const AName: String): String;
var
  i: Integer;
  name: String;
begin
  name := UpperCase(AName);
  for i := 0 to FFields.Count - 1 do
    if UpperCase(FieldNames[i]) = name then begin
      Result := FieldValues[i];
      exit;
    end;
  SetLength(Result, 0);
end;

procedure THttpHeader.SetFieldByName(const AName, AValue: String);
var
  i: Integer;
  name: String;
  field: PHttpField;
begin
  name := UpperCase(AName);
  for i := 0 to FFields.Count - 1 do
    if UpperCase(FieldNames[i]) = name then begin
      FieldNames[i] := AName;   // preserve case
      FieldValues[i] := AValue;
      exit;
    end;
  New(field);
  FillChar(field^, SizeOf(field^), 0);
  field^.Name := AName;
  field^.Value := AValue;
  FFields.Add(field);
end;

procedure THttpHeader.AsyncSend(AManager: TAsyncIOManager; AStream: THandleStream);
var
  i: Integer;
begin
  FWriter.Free;
  FWriter := TAsyncWriteStream.Create(AManager, AStream);
  FWriter.OnBufferEmpty := @WriterCompleted;
  FWriter.EndOfLineMarker := #13#10;
  FWriter.WriteLine(GetFirstHeaderLine);
  for i := 0 to FFields.Count - 1 do
    FWriter.WriteLine(Fields[i]);
  FWriter.WriteLine('');
end;

procedure THttpHeader.AsyncReceive(AManager: TAsyncIOManager; AStream: THandleStream);
begin
  CmdReceived := False;
  FReader.Free;
  FReader := TAsyncStreamLineReader.Create(AManager, AStream);
  FReader.OnLine := @LineReceived;
end;


// -------------------------------------------------------------------
//   THttpRequestHeader
// -------------------------------------------------------------------

procedure THttpRequestHeader.ParseFirstHeaderLine(const line: String);
var
  i: Integer;
begin
  CommandLine := line;
  i := Pos(' ', line);
  Command := Copy(line, 1, i - 1);
  URI := Copy(line, i + 1, Length(line));
  i := Pos(' ', URI);
  if i > 0 then begin
    HttpVersion := Copy(URI, i + 1, Length(URI));
    URI := Copy(URI, 1, i - 1);
    HttpVersion := Copy(HttpVersion, Pos('/', HttpVersion) + 1, Length(HttpVersion));
  end;
end;

function THttpRequestHeader.GetFirstHeaderLine: String;
begin
  Result := Command + ' ' + URI;
  if Length(HttpVersion) > 0 then
    Result := Result + ' HTTP/' + HttpVersion;
end;


// -------------------------------------------------------------------
//   THttpAnswerHeader
// -------------------------------------------------------------------

procedure THttpAnswerHeader.ParseFirstHeaderLine(const line: String);
var
  i: Integer;
  s: String;
begin
  i := Pos('/', line);
  s := Copy(line, i + 1, Length(line));
  i := Pos(' ', s);
  HttpVersion := Copy(s, 1, i - 1);
  s := Copy(s, i + 1, Length(s));
  i := Pos(' ', s);
  if i > 0 then begin
    CodeText := Copy(s, i + 1, Length(s));
    s := Copy(s, 1, i - 1);
  end;
  Code := StrToInt(s);
end;

function THttpAnswerHeader.GetFirstHeaderLine: String;
begin
  Result := Format('HTTP/%s %d %s', [HttpVersion, Code, CodeText]);
end;

constructor THttpAnswerHeader.Create;
begin
  inherited Create;
  Code := 200;
  CodeText := 'OK';
end;


// -------------------------------------------------------------------
//   TCustomHttpConnection
// -------------------------------------------------------------------

procedure TCustomHttpConnection.HeaderToSendCompleted(Sender: TObject);
begin
  //WriteLn('TCustomHttpConnection.HeaderToSendCompleted');
  if Assigned(FOnHeaderSent) then
    FOnHeaderSent(Self);
  if Assigned(StreamToSend) then begin
    SendBuffer := TAsyncWriteStream.Create(FManager, FSocket);
    SendBuffer.CopyFrom(StreamToSend, StreamToSend.Size);
    SendBuffer.OnBufferEmpty := @StreamToSendCompleted;
    SendBuffer.Run;
  end else
    StreamToSendCompleted(nil);
end;

procedure TCustomHttpConnection.StreamToSendCompleted(Sender: TObject);
begin
  if Assigned(FOnStreamSent) then
    FOnStreamSent(Self);
  //WriteLn('TCustomHttpConnection.StreamToSendCompleted');
  SendBuffer.Free;
  SendBuffer := nil;
end;

procedure TCustomHttpConnection.ReceivedHeaderCompleted(Sender: TObject);
var
  BytesInBuffer: Integer;
begin
  //WriteLn('TCustomHttpConnection.ReceivedHeaderCompleted');
  BytesInBuffer := ReceivedHeader.Reader.BytesInBuffer;
  //WriteLn('BytesInBuffer: ', BytesInBuffer, ', Content length: ', ReceivedHeader.ContentLength);
  if Assigned(FOnHeaderReceived) then
    FOnHeaderReceived(Self);
  RecvSize := ReceivedHeader.ContentLength;
  if Assigned(ReceivedStream) then
  begin
    if BytesInBuffer > 0 then
    begin
      ReceivedStream.Write(ReceivedHeader.Reader.Buffer^, BytesInBuffer);
      if RecvSize > 0 then
        Dec(RecvSize, BytesInBuffer);
      if BytesInBuffer = ReceivedHeader.ContentLength then
      begin
        ReceivedStreamCompleted(nil);
        exit;
      end;
    end;
    FManager.SetReadHandler(FSocket.Handle, @DataAvailable, nil);
  end else
    ReceivedStreamCompleted(nil);
end;

procedure TCustomHttpConnection.DataAvailable(Sender: TObject);
var
  FirstRun: Boolean;
  ReadNow, BytesRead: Integer;
  buf: array[0..1023] of Byte;
begin
  FirstRun := True;
  while True do
  begin
    if RecvSize >= 0 then
    begin
      ReadNow := RecvSize;
      if ReadNow > 1024 then
        ReadNow := 1024;
    end else
      ReadNow := 1024;
    BytesRead := FSocket.Read(buf, ReadNow);
    //WriteLn('TCustomHttpConnection.DataAvailable: Read ', BytesRead, ' bytes; RecvSize=', RecvSize);
    if BytesRead <= 0 then
    begin
      if FirstRun then
        ReceivedStreamCompleted(nil);
      break;
    end;
    FirstRun := False;
    ReceivedStream.Write(buf, BytesRead);
    if RecvSize > 0 then
      Dec(RecvSize, BytesRead);
    if RecvSize = 0 then
    begin
      ReceivedStreamCompleted(nil);
      break;
    end;
  end;
end;

procedure TCustomHttpConnection.ReceivedStreamCompleted(Sender: TObject);
begin
  //WriteLn('TCustomHttpConnection.ReceivedStreamCompleted');
  if Assigned(FOnStreamReceived) then
    FOnStreamReceived(Self);
  FManager.ClearReadHandler(FSocket.Handle);
end;

constructor TCustomHttpConnection.Create(AManager: TAsyncIOManager; ASocket: TInetSocket);
begin
  inherited Create;
  FManager := AManager;
  FSocket := ASocket;
end;

destructor TCustomHttpConnection.Destroy;
begin
  FManager.ClearReadHandler(FSocket.Handle);
  inherited Destroy;
end;

procedure TCustomHttpConnection.Start;
begin
  // Start receiver
  if Assigned(ReceivedHeader) then begin
    ReceivedHeader.OnCompleted := @ReceivedHeaderCompleted;
    ReceivedHeader.AsyncReceive(FManager, FSocket);
  end;

  // Start sender
  if Assigned(HeaderToSend) then begin
    // Set the 'Content-Length' field automatically, if possible
    if (HeaderToSend.ContentLength = -1) and Assigned(StreamToSend) then
      HeaderToSend.ContentLength := StreamToSend.Size;

    HeaderToSend.OnCompleted := @HeaderToSendCompleted;
    HeaderToSend.AsyncSend(FManager, FSocket)
  end;
end;


end.


{
  $Log$
  Revision 1.3  2002-09-07 15:15:24  peter
    * old logs removed and tabs fixed

}
