{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team

    HTTP client component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fphttpclient;

{ ---------------------------------------------------------------------
  Todo:
  * Proxy support ?
  * Easy calls for POST/DELETE/etc.
  ---------------------------------------------------------------------}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, httpdefs, uriparser, base64;

Const
  ReadBufLen = 4096;

Type
  { TFPCustomHTTPClient }
  TFPCustomHTTPClient = Class(TComponent)
  private
    FHTTPVersion: String;
    FRequestBody: TStream;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
    FResponseStatusCode: Integer;
    FResponseStatusText: String;
    FServerHTTPVersion: String;
    FSocket : TInetSocket;
    FBuffer : Ansistring;
    function CheckContentLength: Integer;
    procedure SetRequestHeaders(const AValue: TStrings);
  protected
    // Parse response status line. Saves status text and protocol, returns numerical code. Exception if invalid line.
    Function ParseStatusLine(AStatusLine : String) : Integer;
    // Construct server URL for use in request line.
    function GetServerURL(URI: TURI): String;
    // Read 1 line of response. Fills FBuffer
    function ReadString: String;
    // Check if response code is in AllowedResponseCodes. if not, an exception is raised.
    function CheckResponseCode(ACode: Integer;  const AllowedResponseCodes: array of Integer): Boolean; virtual;
    // Read response from server, and write any document to Stream.
    procedure ReadResponse(Stream: TStream;  const AllowedResponseCodes: array of Integer); virtual;
    // Read server response line and headers. Returns status code.
    Function ReadResponseHeaders : integer; virtual;
    // Allow header in request ? (currently checks only if non-empty and contains : token)
    function AllowHeader(var AHeader: String): Boolean; virtual;
    // Connect to the server. Must initialize FSocket.
    procedure ConnectToServer(const AHost: String; APort: Integer); virtual;
    // Disconnect from server. Must free FSocket.
    procedure DisconnectFromServer; virtual;
    // Run method AMethod, using request URL AURL. Write Response to Stream, and headers in ResponseHeaders.
    // If non-empty, AllowedResponseCodes contains an array of response codes considered valid responses.
    Procedure DoMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Send request to server: construct request line and send headers and request body.
    procedure SendRequest(const AMethod: String; URI: TURI); virtual;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // General-purpose call.
    Procedure HTTPMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Execute GET on server, store result in Stream, File, StringList or string
    Procedure Get(Const AURL : String; Stream : TStream);
    Procedure Get(Const AURL : String; const LocalFileName : String);
    Procedure Get(Const AURL : String; Response : TStrings);
    Function Get(Const AURL : String) : String;
  Protected
    // Before request properties.
    // Additional headers for request. Host; and Authentication are automatically added.
    Property RequestHeaders : TStrings Read FRequestHeaders Write SetRequestHeaders;
    // Optional body to send (mainly in POST request)
    Property RequestBody : TStream read FRequestBody Write FRequestBody;
    // used HTTP version when constructing the request.
    Property HTTPversion : String Read FHTTPVersion Write FHTTPVersion;
    // After request properties.
    // After request, this contains the headers sent by server.
    Property ResponseHeaders : TStrings Read FResponseHeaders;
    // After request, HTTP version of server reply.
    Property ServerHTTPVersion : String Read FServerHTTPVersion;
    // After request, HTTP response status of the server.
    Property ResponseStatusCode : Integer Read FResponseStatusCode;
    // After request, HTTP response status text of the server.
    Property ResponseStatusText : String Read FResponseStatusText;
  end;
  TFPHTTPClient = Class(TFPCustomHTTPClient)
  Public
    Property RequestHeaders;
    Property RequestBody;
    Property ResponseHeaders;
    Property HTTPversion;
    Property ServerHTTPVersion;
    Property ResponseStatusCode;
    Property ResponseStatusText;
  end;
  EHTTPClient = Class(Exception);

implementation

resourcestring
  SErrInvalidProtocol = 'Invalid protocol : "%s"';
  SErrReadingSocket = 'Error reading data from socket';
  SErrInvalidProtocolVersion = 'Invalid protocol version in response: "%s"';
  SErrInvalidStatusCode = 'Invalid response status code: %s';
  SErrUnexpectedResponse = 'Unexpected response status code: %d';

Const
  CRLF = #13#10;

{ TFPCustomHTTPClient }

procedure TFPCustomHTTPClient.SetRequestHeaders(const AValue: TStrings);
begin
  if FRequestHeaders=AValue then exit;
  FRequestHeaders.Assign(AValue);
end;

Function TFPCustomHTTPClient.GetServerURL(URI : TURI) : String;

Var
  D : String;

begin
  D:=URI.Path;
  If (D[1]<>'/') then
    D:='/'+D;
  If (D[Length(D)]<>'/') then
    D:=D+'/';
  Result:=D+URI.Document;
end;

procedure TFPCustomHTTPClient.ConnectToServer(Const AHost : String; APort : Integer);

begin
  if Aport=0 then
    Aport:=80;
  FSocket:=TInetSocket.Create(AHost,APort);
end;

procedure TFPCustomHTTPClient.DisconnectFromServer;

begin
  FreeAndNil(FSocket);
end;

function TFPCustomHTTPClient.AllowHeader(Var AHeader : String) : Boolean;

begin
  Result:=(AHeader<>'') and (Pos(':',AHeader)<>0);
end;

procedure TFPCustomHTTPClient.SendRequest(Const AMethod : String; URI : TURI);

Var
  S,L : String;
  I : Integer;

begin
  S:=Uppercase(AMethod)+' '+GetServerURL(URI)+' '+'HTTP/'+FHTTPVersion+CRLF;
  If (URI.Username<>'') then
    S:=S+'Authorization: Basic ' + EncodeStringBase64(URI.UserName+ ':' + URI.Password)+CRLF;
  S:=S+'Host: '+URI.Host;
  If (URI.Port<>0) then
    S:=S+':'+IntToStr(URI.Port);
  S:=S+CRLF;
  For I:=0 to FRequestHeaders.Count-1 do
    begin
    l:=FRequestHeaders[i];
    If AllowHeader(L) then
      S:=S+L+CRLF;
    end;
  S:=S+CRLF;
  FSocket.WriteBuffer(S[1],Length(S));
  If Assigned(FRequestBody) then
    FSocket.CopyFrom(FRequestBody,FRequestBody.Size);
end;

function TFPCustomHTTPClient.ReadString : String;

  Procedure FillBuffer;

  Var
    R : Integer;

  begin
    SetLength(FBuffer,ReadBufLen);
    r:=FSocket.Read(FBuffer[1],ReadBufLen);
    If r<0 then
      Raise EHTTPClient.Create(SErrReadingSocket);
    if (r<ReadBuflen) then
      SetLength(FBuffer,r);
  end;

Var
  CheckLF,Done : Boolean;
  P,L : integer;

begin
  Result:='';
  Done:=False;
  CheckLF:=False;
  Repeat
    if Length(FBuffer)=0 then
      FillBuffer;
    if Length(FBuffer)=0 then
      Done:=True
    else if CheckLF then
      begin
      If (FBuffer[1]<>#10) then
        Result:=Result+#13
      else
        begin
        Delete(FBuffer,1,1);
        Done:=True;
        end;
      end;
    if not Done then
      begin
      P:=Pos(#13#10,FBuffer);
      If P=0 then
        begin
        L:=Length(FBuffer);
        CheckLF:=FBuffer[L]=#13;
        if CheckLF then
          Result:=Result+Copy(FBuffer,1,L-1)
        else
          Result:=Result+FBuffer;
        FBuffer:='';
        end
      else
        begin
        Result:=Result+Copy(FBuffer,1,P-1);
        Delete(FBuffer,1,P+1);
        Done:=True;
        end;
      end;
  until Done;
end;
Function GetNextWord(Var S : String) : string;

Const
  WhiteSpace = [' ',#9];

Var
  P : Integer;

begin
  While (Length(S)>0) and (S[1] in WhiteSpace) do
    Delete(S,1,1);
  P:=Pos(' ',S);
  If (P=0) then
   P:=Pos(#9,S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=Copy(S,1,P-1);
  Delete(S,1,P);
end;

Function TFPCustomHTTPClient.ParseStatusLine(AStatusLine : String) : Integer;

Var
  S : String;

begin
  S:=Uppercase(GetNextWord(AStatusLine));
  If (Copy(S,1,5)<>'HTTP/') then
    Raise EHTTPClient.CreateFmt(SErrInvalidProtocolVersion,[S]);
  Delete(S,1,5);
  FServerHTTPVersion:=S;
  S:=GetNextWord(AStatusLine);
  Result:=StrToIntDef(S,-1);
  if Result=-1 then
   Raise EHTTPClient.CreateFmt(SErrInvalidStatusCode,[S]);
  FResponseStatusText:=AStatusLine;
end;

Function TFPCustomHTTPClient.ReadResponseHeaders : Integer;

Var
  StatusLine,S : String;
begin
  StatusLine:=ReadString;
  Result:=ParseStatusLine(StatusLine);

  Repeat
    S:=ReadString;
    if (S<>'') then
      ResponseHeaders.Add(S);
  Until (S='');
end;

Function TFPCustomHTTPClient.CheckResponseCode(ACode : Integer; Const AllowedResponseCodes : Array of Integer) : Boolean;

Var
  I : Integer;

begin
  Result:=(High(AllowedResponseCodes)=-1);
  if not Result then
    begin
    I:=Low(AllowedResponseCodes);
    While (Not Result) and (I<=High(AllowedResponseCodes)) do
      begin
      Result:=(AllowedResponseCodes[i]=FResponseStatusCode);
      Inc(I);
      end
    end;
end;

Function TFPCustomHTTPClient.CheckContentLength: Integer;

Const CL ='content-length:';

Var
  S : String;
  I : integer;

begin
  Result:=-1;
  I:=0;
  While (Result=-1) and (I<FResponseHeaders.Count) do
    begin
    S:=Trim(LowerCase(FResponseHeaders[i]));
    If (Copy(S,1,Length(Cl))=Cl) then
      begin
      Delete(S,1,Length(CL));
      Result:=StrToIntDef(Trim(S),-1);
      end;
    Inc(I);
    end;
end;

procedure TFPCustomHTTPClient.ReadResponse(Stream: TStream; Const AllowedResponseCodes : Array of Integer);

Var
  L,LB,R : Integer;
  ResponseOK : Boolean;

begin
  FResponseStatusCode:=ReadResponseHeaders;
  if not CheckResponseCode(FResponseStatusCode,AllowedResponseCodes) then
    Raise EHTTPClient.CreateFmt(SErrUnexpectedResponse,[ResponseStatusCode]);
  // Write remains of buffer to output.
  LB:=Length(FBuffer);
  If (LB>0) then
    Stream.WriteBuffer(FBuffer[1],LB);
  // Now write the rest, if any.
  L:=CheckContentLength;
  If (L>LB) then
    Stream.CopyFrom(FSocket,L-LB)
  else if L<0 then
    // No content-length, so we read till no more data available.
    Repeat
      SetLength(FBuffer,ReadBufLen);
      R:=FSocket.Read(FBuffer[1],ReadBufLen);
      If R<0 then
        Raise EHTTPClient.Create(SErrReadingSocket);
      if (R>0) then
        Stream.Write(FBuffer[1],R);
    until (R=0);
end;

procedure TFPCustomHTTPClient.DoMethod(Const AMethod,AURL: String; Stream: TStream; Const AllowedResponseCodes : Array of Integer);

Var
  URI : TURI;

begin
  FResponseHeaders.Clear;
  URI:=ParseURI(AURL);
  If (Lowercase(URI.Protocol)<>'http') then
   Raise EHTTPClient.CreateFmt(SErrInvalidProtocol,[URI.Protocol]);
  ConnectToServer(URI.Host,URI.Port);
  try
    SendRequest(AMethod,URI);
    ReadResponse(Stream,AllowedResponseCodes);
  finally
    DisconnectFromServer;
  end;
end;

constructor TFPCustomHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequestHeaders:=TStringList.Create;
  FResponseHeaders:=TStringList.Create;
  FHTTPVersion:='1.1';
end;

destructor TFPCustomHTTPClient.Destroy;
begin
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FResponseHeaders);
  inherited Destroy;
end;

procedure TFPCustomHTTPClient.HTTPMethod(const AMethod, AURL: String;
  Stream: TStream; const AllowedResponseCodes: array of Integer);
begin
  DoMethod(AMethod,AURL,Stream,AllowedResponseCodes);
end;

procedure TFPCustomHTTPClient.Get(Const AURL: String; Stream: TStream);
begin
  DoMethod('GET',AURL,Stream,[200]);
end;

procedure TFPCustomHTTPClient.Get(Const AURL: String; const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Get(AURL,F);
  finally
    F.Free;
  end;
end;

procedure TFPCustomHTTPClient.Get(const AURL: String; Response: TStrings);
begin
  Response.Text:=Get(AURL);
end;

function TFPCustomHTTPClient.Get(Const AURL: String): String;

Var
  SS : TStringStream;

begin
  SS:=TStringStream.Create('');
  try
    Get(AURL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

end.

