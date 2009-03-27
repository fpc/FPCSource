{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2009 by the Free Pascal development team

    TFCgiApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ $define CGIDEBUG}
{$mode objfpc}
{$H+}

unit custfcgi;

Interface

uses
  Classes,SysUtils, httpdefs,custweb, custcgi, fastcgi;

Type
  { TFCGIRequest }
  TCustomFCgiApplication = Class;

  TFCGIRequest = Class(TCGIRequest)
  Private
    FHandle: THandle;
    FKeepConnectionAfterRequest: boolean;
    FRequestID : Word;
    FCGIParams : TSTrings;
    procedure GetNameValuePairsFromContentRecord(const ARecord : PFCGI_ContentRecord; NameValueList : TStrings);
  Protected
    Function GetFieldValue(Index : Integer) : String; override;
    procedure ReadContent; override;
  Public
    destructor Destroy; override;
    function ProcessFCGIRecord(AFCGIRecord : PFCGI_Header) : boolean; virtual;
    property RequestID : word read FRequestID write FRequestID;
    property Handle : THandle read FHandle write FHandle;
    property KeepConnectionAfterRequest : boolean read FKeepConnectionAfterRequest;
  end;

  { TFCGIResponse }

  TFCGIResponse = Class(TCGIResponse)
  private
    procedure Write_FCGIRecord(ARecord : PFCGI_Header);
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
  end;

  TReqResp = record
             Request : TFCgiRequest;
             Response : TFCgiResponse;
             end;

  { TCustomFCgiApplication }

  TCustomFCgiApplication = Class(TCustomWebApplication)
  Private
    FRequestsArray : Array of TReqResp;
    FRequestsAvail : integer;
    FHandle : THandle;
    function Read_FCGIRecord : PFCGI_Header;
  protected
    function WaitForRequest(var ARequest : TRequest; var AResponse : TResponse) : boolean; override;
    procedure EndRequest(ARequest : TRequest;AResponse : TResponse); override;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

ResourceString
  SNoInputHandle = 'Failed to open input-handle passed from server. Socket Error: %d';

Implementation

uses
{$ifdef CGIDEBUG}
  dbugintf,
{$endif}
  BaseUnix, Sockets;

{ TFCGIHTTPRequest }

procedure TFCGIRequest.ReadContent;
begin
  // Nothing has to be done. This should never be called
end;

destructor TFCGIRequest.Destroy;
begin
  FCGIParams.Free;
  inherited Destroy;
end;

function TFCGIRequest.ProcessFCGIRecord(AFCGIRecord: PFCGI_Header): boolean;
var cl,rcl : Integer;
begin
  Result := False;
  case AFCGIRecord^.reqtype of
    FCGI_BEGIN_REQUEST : FKeepConnectionAfterRequest := (PFCGI_BeginRequestRecord(AFCGIRecord)^.body.flags and FCGI_KEEP_CONN) = FCGI_KEEP_CONN;
    FCGI_PARAMS :       begin
                        if AFCGIRecord^.contentLength=0 then
                          Result := False
                        else
                          begin
                          if not assigned(FCGIParams) then
                            FCGIParams := TStringList.Create;
                          GetNameValuePairsFromContentRecord(PFCGI_ContentRecord(AFCGIRecord),FCGIParams);
                          end;
                        end;
    FCGI_STDIN :        begin
                        if AFCGIRecord^.contentLength=0 then
                          begin
                          Result := True;
                          InitRequestVars;
                          end
                        else
                          begin
                          cl := length(FContent);
                          rcl := BetoN(PFCGI_ContentRecord(AFCGIRecord)^.header.contentLength);
                          SetLength(FContent, rcl+cl);
                          move(PFCGI_ContentRecord(AFCGIRecord)^.ContentData[0],FContent[cl+1],rcl);
                          FContentRead:=True;
                          end;
                        end;
  end;
end;

procedure TFCGIRequest.GetNameValuePairsFromContentRecord(const ARecord: PFCGI_ContentRecord; NameValueList: TStrings);

var
  i : integer;

  function GetVarLength : Integer;
  begin
    if (ARecord^.ContentData[i] and 128) = 0 then
      Result:=ARecord^.ContentData[i]
    else
      begin
      Result:=BEtoN(PWord(@(ARecord^.ContentData[i]))^);
      // ((ARecord^.ContentData[i] and $7f) shl 24) + (ARecord^.ContentData[i+1] shl 16)
      //             + (ARecord^.ContentData[i+2] shl 8) + (ARecord^.ContentData[i+3]);
      inc(i,3);
      end;
    inc(i);
  end;

  function GetString(ALength : integer) : string;
  begin
    SetLength(Result,ALength);
    move(ARecord^.ContentData[i],Result[1],ALength);
    inc(i,ALength);
  end;

var
  NameLength, ValueLength : Integer;
  RecordLength : Integer;
  Name,Value : String;

begin
  i := 0;
  RecordLength:=BetoN(ARecord^.Header.contentLength);
  while i < RecordLength do
    begin
    NameLength:=GetVarLength;
    ValueLength:=GetVarLength;

    Name:=GetString(NameLength);
    Value:=GetString(ValueLength);
    NameValueList.Add(Name+'='+Value);
    end;
end;

Function TFCGIRequest.GetFieldValue(Index : Integer) : String;

Type THttpToCGI = array[1..CGIVarCount] of byte;

const HttpToCGI : THttpToCGI =
   (
     18,  //  1 'HTTP_ACCEPT'           - fieldAccept
     19,  //  2 'HTTP_ACCEPT_CHARSET'   - fieldAcceptCharset
     20,  //  3 'HTTP_ACCEPT_ENCODING'  - fieldAcceptEncoding
      0,  //  4
      0,  //  5
      0,  //  6
      0,  //  7
      0,  //  8
      2,  //  9 'CONTENT_LENGTH'
      3,  // 10 'CONTENT_TYPE'          - fieldAcceptEncoding
     24,  // 11 'HTTP_COOKIE'           - fieldCookie
      0,  // 12
      0,  // 13
      0,  // 14
     21,  // 15 'HTTP_IF_MODIFIED_SINCE'- fieldIfModifiedSince
      0,  // 16
      0,  // 17
      0,  // 18
     22,  // 19 'HTTP_REFERER'          - fieldReferer
      0,  // 20
      0,  // 21
      0,  // 22
     23,  // 23 'HTTP_USER_AGENT'       - fieldUserAgent
      1,  // 24 'AUTH_TYPE'             - fieldWWWAuthenticate
      5,  // 25 'PATH_INFO'
      6,  // 26 'PATH_TRANSLATED'
      8,  // 27 'REMOTE_ADDR'
      9,  // 28 'REMOTE_HOST'
     13,  // 29 'SCRIPT_NAME'
     15,  // 30 'SERVER_PORT'
     12,  // 31 'REQUEST_METHOD'
      0,  // 32
      7,  // 33 'QUERY_STRING'
     27   // 34 'HTTP_HOST'
    );

var ACgiVarNr : Integer;

begin
  Result := '';
  if assigned(FCGIParams) and (index < high(HttpToCGI)) and (index > 0) then
    begin
    ACgiVarNr:=HttpToCGI[Index];
    if ACgiVarNr>0 then
      Result:=FCGIParams.Values[CgiVarNames[ACgiVarNr]]
    else
      Result := '';
    end
  else
    Result:=inherited GetFieldValue(Index);
end;

{ TCGIResponse }
procedure TFCGIResponse.Write_FCGIRecord(ARecord : PFCGI_Header);
var BytesToWrite : word;
    BytesWritten  : Integer;
begin
  BytesToWrite := BEtoN(ARecord^.contentLength) + ARecord^.paddingLength+sizeof(FCGI_Header);
  BytesWritten := sockets.fpsend(TFCGIRequest(Request).Handle, ARecord, BytesToWrite, MSG_NOSIGNAL);
  Assert(BytesWritten=BytesToWrite);
end;

procedure TFCGIResponse.DoSendHeaders(Headers : TStrings);
var
  cl : word;
  pl : byte;
  str : String;
  ARespRecord : PFCGI_ContentRecord;

begin
  str := Headers.Text;
  cl := length(str);
  pl := (cl mod 8);

  ARespRecord:=nil;
  Getmem(ARespRecord,8+cl+pl);
  ARespRecord^.header.version:=FCGI_VERSION_1;
  ARespRecord^.header.reqtype:=FCGI_STDOUT;
  ARespRecord^.header.paddingLength:=pl;
  ARespRecord^.header.contentLength:=NtoBE(cl);
  ARespRecord^.header.requestId:=NToBE(TFCGIRequest(Request).RequestID);
  move(str[1],ARespRecord^.ContentData,cl);
  Write_FCGIRecord(PFCGI_Header(ARespRecord));
  Freemem(ARespRecord);
end;

procedure TFCGIResponse.DoSendContent;
var
  cl : word;
  pl : byte;
  str : String;
  ARespRecord : PFCGI_ContentRecord;
  EndRequest : FCGI_EndRequestRecord;

begin
  If Assigned(ContentStream) then
    begin
    setlength(str,ContentStream.Size);
    ContentStream.Position:=0;
    ContentStream.Read(str[1],ContentStream.Size);
    end
  else
    str := Contents.Text;

  cl := length(str);
  pl := (cl mod 8);

  ARespRecord:=Nil;
  Getmem(ARespRecord,8+cl+pl);
  ARespRecord^.header.version:=FCGI_VERSION_1;
  ARespRecord^.header.reqtype:=FCGI_STDOUT;
  ARespRecord^.header.paddingLength:=pl;
  ARespRecord^.header.contentLength:=NtoBE(cl);
  ARespRecord^.header.requestId:=NToBE(TFCGIRequest(Request).RequestID);
  move(str[1],ARespRecord^.ContentData,cl);
  Write_FCGIRecord(PFCGI_Header(ARespRecord));
  Freemem(ARespRecord);

  EndRequest.header.version:=FCGI_VERSION_1;
  EndRequest.header.reqtype:=FCGI_END_REQUEST;
  EndRequest.header.contentLength:=NtoBE(8);
  EndRequest.header.paddingLength:=0;
  EndRequest.header.requestId:=NToBE(TFCGIRequest(Request).RequestID);
  Write_FCGIRecord(PFCGI_Header(@EndRequest));
end;

{ TCustomFCgiApplication }

constructor TCustomFCgiApplication.Create(AOwner: TComponent);
begin
  FRequestsAvail:=5;
  SetLength(FRequestsArray,FRequestsAvail);
  FHandle := -1;
end;

destructor TCustomFCgiApplication.Destroy;
begin
  SetLength(FRequestsArray,0);
  inherited Destroy;
end;

procedure TCustomFCgiApplication.EndRequest(ARequest: TRequest; AResponse: TResponse);
begin
  with FRequestsArray[TFCGIRequest(ARequest).RequestID] do
    begin
    Assert(ARequest=Request);
    Assert(AResponse=Response);
    if not TFCGIRequest(ARequest).KeepConnectionAfterRequest then
      begin
      fpshutdown(FHandle,SHUT_RDWR);
      FpClose(FHandle);
      FHandle := -1;
      end;
    Request := Nil;
    Response := Nil;
    end;
  Inherited;
end;

function TCustomFCgiApplication.Read_FCGIRecord : PFCGI_Header;

var Header : FCGI_Header;
    BytesRead : integer;
    ContentLength : word;
    PaddingLength : byte;
    ResRecord : pointer;
    ReadBuf : pointer;

  function ReadBytes(ByteAmount : Word) : boolean;
  begin
   result := False;
    if ByteAmount>0 then
      begin
      BytesRead := sockets.fpRecv(FHandle, ReadBuf, ByteAmount, MSG_NOSIGNAL);
      if BytesRead<>ByteAmount then
        begin
//        SendDebug('FCGIRecord incomplete');
//        SendDebug('BytesRead: '+inttostr(BytesRead)+', expected: '+inttostr(ByteAmount));
        exit;
        end
      else
        Result := True;
      end;
  end;

begin
  Result := Nil;
  ResRecord:=Nil;
  ReadBuf:=@Header;
  if not ReadBytes(Sizeof(Header)) then exit;
  ContentLength:=BetoN(Header.contentLength);
  PaddingLength:=Header.paddingLength;
  Getmem(ResRecord,BytesRead+ContentLength+PaddingLength);
  PFCGI_Header(ResRecord)^:=Header;
  ReadBuf:=ResRecord+BytesRead;
  ReadBytes(ContentLength);
  ReadBuf:=ReadBuf+BytesRead;
  ReadBytes(PaddingLength);
  Result := ResRecord;
end;

function TCustomFCgiApplication.WaitForRequest(var ARequest: TRequest; var AResponse: TResponse): boolean;
var
  Address       : TInetSockAddr;
  AddressLength : tsocklen;
  ARequestID    : word;
  AFCGI_Record  : PFCGI_Header;
  ATempRequest  : TFCGIRequest;
begin
  Result := False;
  AddressLength:=Sizeof(Address);
  if FHandle=-1 then
    begin
    FHandle:=fpaccept(StdInputHandle,psockaddr(@Address),@AddressLength);
    if FHandle=-1 then
      raise Exception.CreateFmt(SNoInputHandle,[socketerror]);
    end;

  repeat
  AFCGI_Record:=Read_FCGIRecord;
  if assigned(AFCGI_Record) then
    begin
    ARequestID:=BEtoN(AFCGI_Record^.requestID);
    if AFCGI_Record^.reqtype = FCGI_BEGIN_REQUEST then
      begin
      if ARequestID>FRequestsAvail then
        begin
        inc(FRequestsAvail,10);
        SetLength(FRequestsArray,FRequestsAvail);
        end;
      assert(not assigned(FRequestsArray[ARequestID].Request));
      assert(not assigned(FRequestsArray[ARequestID].Response));

      ATempRequest:=TFCGIRequest.Create;
      ATempRequest.RequestID:=ARequestID;
      ATempRequest.Handle:=FHandle;
      FRequestsArray[ARequestID].Request := ATempRequest;
      end;
    if FRequestsArray[ARequestID].Request.ProcessFCGIRecord(AFCGI_Record) then
      begin
      ARequest:=FRequestsArray[ARequestID].Request;
      FRequestsArray[ARequestID].Response := TFCGIResponse.Create(ARequest);
      AResponse:=FRequestsArray[ARequestID].Response;
      Result := True;
      Break;
      end;
    end;
  until Terminated;
end;

end.
