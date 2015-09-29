{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Ondrej Pokorny

    Unit implementing two-way (request/response) IPC between 1 server and more
    clients, based on files.
    The order of message processing is not deterministic (if there are more
    pending messages, the server won't process them in the order they have
    been sent to the server.
    SendRequest and PostRequest+PeekResponse sequences from 1 client are
    blocking and processed in correct order.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit advancedipc;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF UNIX}
  baseunix,
  {$endif}
  sysutils, Classes;

const
  HEADER_VERSION = 2;

type
  TMessageType = LongInt;
  TMessageHeader = packed record
    HeaderVersion: Byte;
    FileLock: Byte;//0 = unlocked, 1 = locked
    MsgType: TMessageType;
    MsgLen: Integer;
    MsgVersion: Integer;
  end;

  TFileHandle = Classes.THandle;

  TReleaseHandleStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

  TIPCBase = class(TComponent)
  private
    FGlobal: Boolean;
    FFileName: string;
    FServerID: string;
    FMessageVersion: Integer;
  protected
    class function ServerIDToFileName(const aServerID: string; const aGlobal: Boolean): string;
    function GetResponseFileName(const aMsgID: Integer): string;
    function GetResponseFileName(const aRequestFileName: string): string;
    function GetPeekedRequestFileName(const aMsgID: Integer): string;
    function GetPeekedRequestFileName(const aRequestFileName: string): string;
    function GetRequestPrefix: string;
    function GetRequestFileName(const aMsgID: Integer): string;
    function RequestFileNameToMsgID(const aFileName: string): Integer;

    function GetUniqueRequest(out outFileName: string): Integer;
    procedure SetServerID(const aServerID: string); virtual;
    procedure SetGlobal(const aGlobal: Boolean); virtual;

    function CanReadMessage(const aFileName: string; out outStream: TStream; out outMsgType: TMessageType; out outMsgLen: Integer): Boolean;
    procedure DoPostMessage(const aFileName: string; const aMsgType: TMessageType; const aStream: TStream);

    property FileName: string read FFileName;
  public
    class procedure FindRunningServers(const aServerIDPrefix: string;
      const outServerIDs: TStrings; const aGlobal: Boolean = False);
    class function ServerRunning(const aServerID: string; const aGlobal: Boolean = False): Boolean; overload;
  public
    //ServerID: name/ID of the server. Use only ['a'..'z', 'A'..'Z', '_'] characters
    property ServerID: string read FServerID write SetServerID;
    //Global: if true, processes from different users can communicate; false, processes only from current users can communicate
    property Global: Boolean read FGlobal write SetGlobal;
    //MessageVersion: only messages with the same MessageVersion can be delivered between server/client
    property MessageVersion: Integer read FMessageVersion write FMessageVersion;
  end;

  TIPCClient = class(TIPCBase)
  private
    FLastMsgFileName: string;
  public
    //post request to server, do not wait until request is peeked; returns request ID
    function PostRequest(const aMsgType: TMessageType; const aStream: TStream): Integer;
    //send request to server, wait until request is peeked; returns True if request was peeked within the aTimeOut limit
    function SendRequest(const aMsgType: TMessageType; const aStream: TStream; const aTimeOut: Integer): Boolean;
    function SendRequest(const aMsgType: TMessageType; const aStream: TStream; const aTimeOut: Integer; out outRequestID: Integer): Boolean;
    //peek a response from last request from this client
    function PeekResponse(const aStream: TStream; out outMsgType: TMessageType; const aTimeOut: Integer): Boolean;
    //delete last request from this client
    procedure DeleteRequest;
    //check if server is running
    function ServerRunning: Boolean; overload;
  end;

  TIPCServer = class(TIPCBase)
  private
    FFileHandle: TFileHandle;
    FActive: Boolean;

    function FindFirstRequest(out outFileName: string; out outStream: TStream; out outMsgType: TMessageType; out outMsgLen: Integer): Integer;

  protected
    procedure SetServerID(const aServerID: string); override;
    procedure SetGlobal(const aGlobal: Boolean); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    //peek request and read the message into a stream
    function PeekRequest(const aStream: TStream; out outMsgType: TMessageType): Boolean; overload;
    function PeekRequest(const aStream: TStream; out outMsgID: Integer; out outMsgType: TMessageType): Boolean; overload;
    function PeekRequest(const aStream: TStream; out outMsgID: Integer; out outMsgType: TMessageType; const aTimeOut: Integer): Boolean; overload;
    //only peek request, you have to read/delete the request manually with ReadRequest/DeleteRequest
    function PeekRequest(out outMsgType: TMessageType): Boolean; overload;
    function PeekRequest(out outMsgID: Integer; out outMsgType: TMessageType): Boolean; overload;
    function PeekRequest(out outMsgID: Integer; out outMsgType: TMessageType; const aTimeOut: Integer): Boolean; overload;
    //read a peeked request (that hasn't been read yet)
    function ReadRequest(const aMsgID: Integer; const aStream: TStream): Boolean;
    //delete a peeked request (that hasn't been read yet)
    procedure DeleteRequest(const aMsgID: Integer);

    //post response to a request
    procedure PostResponse(const aMsgID: Integer; const aMsgType: TMessageType; const aStream: TStream);

    //find the highest request ID from all pending requests
    function FindHighestPendingRequestId: Integer;
    //get the pending request count
    function GetPendingRequestCount: Integer;

    //start server: returns true if unique and started
    function StartServer(const aDeletePendingRequests: Boolean = True): Boolean;
    //stop server: returns true if stopped
    function StopServer(const aDeletePendingRequests: Boolean = True): Boolean;

    //delete all pending requests and responses
    procedure DeletePendingRequests;
  public
    //true if server runs (was started)
    property Active: Boolean read FActive;
  end;

  EICPException = class(Exception);

resourcestring
  SErrInvalidServerID = 'Invalid server ID "%s". Please use only alphanumerical characters and underlines.';
  SErrSetGlobalActive = 'You cannot change the global property when the server is active.';
  SErrSetServerIDActive = 'You cannot change the server ID when the server is active.';

implementation

const
  {$IFDEF UNIX}
  GLOBAL_RIGHTS = S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  {$ELSE}
  GLOBAL_RIGHTS = 0;
  {$ENDIF}

{ TIPCBase }

function TIPCBase.CanReadMessage(const aFileName: string; out
  outStream: TStream; out outMsgType: TMessageType; out outMsgLen: Integer
  ): Boolean;
var
  xFileHandle: TFileHandle;
  xHeader: TMessageHeader;
begin
  outStream := nil;
  outMsgType := -1;
  outMsgLen := 0;
  Result := FileExists(aFileName);
  if not Result then
    Exit;

  xFileHandle := FileOpen(aFileName, fmOpenRead or fmShareExclusive);
  Result := xFileHandle <> feInvalidHandle;
  if not Result then
    Exit;

  outStream := TReleaseHandleStream.Create(xFileHandle);

  Result := (outStream.Size >= SizeOf(xHeader));
  if not Result then
  begin
    FreeAndNil(outStream);
    Exit;
  end;

  outStream.ReadBuffer(xHeader{%H-}, SizeOf(xHeader));
  Result :=
    (xHeader.HeaderVersion = HEADER_VERSION) and (xHeader.FileLock = 0) and
    (xHeader.MsgVersion = MessageVersion) and
    (outStream.Size = Int64(SizeOf(xHeader))+Int64(xHeader.MsgLen));
  if not Result then
  begin
    FreeAndNil(outStream);
    Exit;
  end;
  outMsgType := xHeader.MsgType;
  outMsgLen := xHeader.MsgLen;
end;

function TIPCBase.GetUniqueRequest(out outFileName: string): Integer;
begin
  Randomize;
  repeat
    Result := Random(High(Integer));
    outFileName := GetRequestFileName(Result);
  until not FileExists(outFileName);
end;

class function TIPCBase.ServerRunning(const aServerID: string;
  const aGlobal: Boolean): Boolean;
var
  xServerFileHandle: TFileHandle;
  xFileName: String;
begin
  xFileName := ServerIDToFileName(aServerID, aGlobal);
  Result := FileExists(xFileName);
  if Result then
  begin//+ check -> we should not be able to access the file
    xServerFileHandle := FileCreate(xFileName, fmOpenReadWrite or fmShareExclusive, GLOBAL_RIGHTS);
    Result := (xServerFileHandle=feInvalidHandle);
    if not Result then
      FileClose(xServerFileHandle);
  end;
end;

class function TIPCBase.ServerIDToFileName(const aServerID: string;
  const aGlobal: Boolean): string;
begin
  Result := GetTempDir(aGlobal)+aServerID;
end;

procedure TIPCBase.SetGlobal(const aGlobal: Boolean);
begin
  if FGlobal = aGlobal then Exit;

  FGlobal := aGlobal;
  FFileName := ServerIDToFileName(FServerID, FGlobal);
end;

procedure TIPCBase.DoPostMessage(const aFileName: string;
  const aMsgType: TMessageType; const aStream: TStream);
var
  xHeader: TMessageHeader;
  xStream: TFileStream;
begin
  xHeader.HeaderVersion := HEADER_VERSION;
  xHeader.FileLock := 1;//locking
  xHeader.MsgType := aMsgType;
  if Assigned(aStream) then
    xHeader.MsgLen := aStream.Size-aStream.Position
  else
    xHeader.MsgLen := 0;
  xHeader.MsgVersion := MessageVersion;

  xStream := TFileStream.Create(aFileName, fmCreate or fmShareExclusive, GLOBAL_RIGHTS);
  try
    xStream.WriteBuffer(xHeader, SizeOf(xHeader));
    if Assigned(aStream) then
      xStream.CopyFrom(aStream, 0);

    xStream.Position := 0;//unlocking
    xHeader.FileLock := 0;
    xStream.WriteBuffer(xHeader, SizeOf(xHeader));
  finally
    xStream.Free;
  end;
end;

function TIPCBase.RequestFileNameToMsgID(const aFileName: string): Integer;
begin
  //the function prevents all responses/temp files to be handled
  //only valid response files are returned
  if (Length(aFileName) > 9) and (aFileName[Length(aFileName)-8] = '-') then
    Result := StrToIntDef('$'+Copy(aFileName, Length(aFileName)-7, 8), -1)
  else
    Result := -1;
end;

class procedure TIPCBase.FindRunningServers(const aServerIDPrefix: string;
  const outServerIDs: TStrings; const aGlobal: Boolean);
var
  xRec: TRawByteSearchRec;
begin
  if FindFirst(ServerIDToFileName(aServerIDPrefix+'*', aGlobal), faAnyFile, xRec) = 0 then
  begin
    repeat
      if (Pos('-', xRec.Name) = 0) and//file that we found is a pending message
         ServerRunning(xRec.Name, aGlobal)
      then
        outServerIDs.Add(xRec.Name);
    until FindNext(xRec) <> 0;
  end;
  FindClose(xRec);
end;

function TIPCBase.GetPeekedRequestFileName(const aMsgID: Integer): string;
begin
  Result := GetPeekedRequestFileName(GetRequestFileName(aMsgID));
end;

function TIPCBase.GetPeekedRequestFileName(const aRequestFileName: string
  ): string;
begin
  Result := aRequestFileName+'-t';
end;

function TIPCBase.GetRequestFileName(const aMsgID: Integer): string;
begin
  Result := GetRequestPrefix+IntToHex(aMsgID, 8);
end;

function TIPCBase.GetRequestPrefix: string;
begin
  Result := FFileName+'-';
end;

function TIPCBase.GetResponseFileName(const aMsgID: Integer): string;
begin
  Result := GetResponseFileName(GetRequestFileName(aMsgID));
end;

function TIPCBase.GetResponseFileName(const aRequestFileName: string): string;
begin
  Result := aRequestFileName+'-r';
end;

procedure TIPCBase.SetServerID(const aServerID: string);
var
  I: Integer;
begin
  if FServerID = aServerID then Exit;

  for I := 1 to Length(aServerID) do
  if not (aServerID[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
    raise EICPException.CreateFmt(SErrInvalidServerID , [aServerID]);

  FServerID := aServerID;

  FFileName := ServerIDToFileName(FServerID, FGlobal);
end;

{ TIPCClient }

procedure TIPCClient.DeleteRequest;
begin
  if DeleteFile(FLastMsgFileName) then
    FLastMsgFileName := '';
end;

function TIPCClient.PeekResponse(const aStream: TStream; out
  outMsgType: TMessageType; const aTimeOut: Integer): Boolean;
var
  xStart: QWord;
  xStream: TStream;
  xMsgLen: Integer;
  xFileResponse: string;
begin
  aStream.Size := 0;
  Result := False;
  xStart := GetTickCount64;
  repeat
    xFileResponse := GetResponseFileName(FLastMsgFileName);
    if CanReadMessage(xFileResponse, xStream, outMsgType, xMsgLen) then
    begin
      aStream.CopyFrom(xStream, xMsgLen);
      xStream.Free;
      aStream.Position := 0;
      DeleteFile(xFileResponse);
      Exit(True);
    end
    else if aTimeOut > 20 then
      Sleep(10);
  until (GetTickCount64-xStart > aTimeOut);
end;

function TIPCClient.PostRequest(const aMsgType: TMessageType;
  const aStream: TStream): Integer;
begin
  Result := GetUniqueRequest(FLastMsgFileName);
  DeleteFile(GetResponseFileName(FLastMsgFileName));//delete old response, if there is any
  DoPostMessage(FLastMsgFileName, aMsgType, aStream);
end;

function TIPCClient.SendRequest(const aMsgType: TMessageType;
  const aStream: TStream; const aTimeOut: Integer): Boolean;
var
  xRequestID: Integer;
begin
  Result := SendRequest(aMsgType, aStream, aTimeOut, xRequestID);
end;

function TIPCClient.SendRequest(const aMsgType: TMessageType;
  const aStream: TStream; const aTimeOut: Integer; out outRequestID: Integer
  ): Boolean;
var
  xStart: QWord;
  xRequestFileName: string;
begin
  outRequestID := PostRequest(aMsgType, aStream);
  Result := False;

  xRequestFileName := GetRequestFileName(outRequestID);
  xStart := GetTickCount64;
  repeat
    if not FileExists(xRequestFileName) then
      Exit(True)
    else if aTimeOut > 20 then
      Sleep(10);
  until (GetTickCount64-xStart > aTimeOut);
end;

function TIPCClient.ServerRunning: Boolean;
begin
  Result := ServerRunning(ServerID, Global);
end;

{ TReleaseHandleStream }

destructor TReleaseHandleStream.Destroy;
begin
  FileClose(Handle);

  inherited Destroy;
end;

{ TIPCServer }

procedure TIPCServer.DeletePendingRequests;
var
  xRec: TRawByteSearchRec;
  xDir: string;
begin
  xDir := ExtractFilePath(FFileName);
  if FindFirst(GetRequestPrefix+'*', faAnyFile, xRec) = 0 then
  begin
    repeat
      DeleteFile(xDir+xRec.Name);
    until FindNext(xRec) <> 0;
  end;
  FindClose(xRec);
end;

procedure TIPCServer.DeleteRequest(const aMsgID: Integer);
begin
  DeleteFile(GetPeekedRequestFileName(aMsgID));
end;

constructor TIPCServer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFileHandle := feInvalidHandle;
end;

destructor TIPCServer.Destroy;
begin
  if FActive then
    StopServer;

  inherited Destroy;
end;

function TIPCServer.FindFirstRequest(out outFileName: string; out
  outStream: TStream; out outMsgType: TMessageType; out outMsgLen: Integer
  ): Integer;
var
  xRec: TRawByteSearchRec;
begin
  outFileName := '';
  outStream := nil;
  outMsgType := -1;
  outMsgLen := 0;
  Result := -1;
  if FindFirst(GetRequestPrefix+'*', faAnyFile, xRec) = 0 then
  begin
    repeat
      Result := RequestFileNameToMsgID(xRec.Name);
      if Result >= 0 then
      begin
        outFileName := GetRequestFileName(Result);
        if not CanReadMessage(outFileName, outStream, outMsgType, outMsgLen) then
          Result := -1;
      end;
    until (Result >= 0) or (FindNext(xRec) <> 0);
  end;
  FindClose(xRec);
end;

function TIPCServer.FindHighestPendingRequestId: Integer;
var
  xRec: TRawByteSearchRec;
  xMsgID, xHighestId: LongInt;
begin
  xHighestId := -1;
  Result := -1;
  if FindFirst(GetRequestPrefix+'*', faAnyFile, xRec) = 0 then
  begin
    repeat
      xMsgID := RequestFileNameToMsgID(xRec.Name);
      if xMsgID > xHighestId then
      begin
        xHighestId := xMsgID;
        Result := xMsgID;
      end;
    until FindNext(xRec) <> 0;
  end;
  FindClose(xRec);
end;

function TIPCServer.GetPendingRequestCount: Integer;
var
  xRec: TRawByteSearchRec;
begin
  Result := 0;
  if FindFirst(GetRequestPrefix+'*', faAnyFile, xRec) = 0 then
  begin
    repeat
      if RequestFileNameToMsgID(xRec.Name) >= 0 then
        Inc(Result);
    until FindNext(xRec) <> 0;
  end;
  FindClose(xRec);
end;

function TIPCServer.PeekRequest(out outMsgID: Integer; out
  outMsgType: TMessageType): Boolean;
var
  xStream: TStream;
  xMsgLen: Integer;
  xMsgFileName: string;
begin
  outMsgType := -1;
  xMsgFileName := '';
  outMsgID := FindFirstRequest(xMsgFileName, xStream, outMsgType, xMsgLen);
  Result := outMsgID >= 0;
  if Result then
  begin
    xStream.Free;
    RenameFile(xMsgFileName, GetPeekedRequestFileName(xMsgFileName));
  end;
end;

function TIPCServer.PeekRequest(out outMsgID: Integer; out
  outMsgType: TMessageType; const aTimeOut: Integer): Boolean;
var
  xStart: QWord;
begin
  Result := False;
  xStart := GetTickCount64;
  repeat
    if PeekRequest(outMsgID, outMsgType) then
      Exit(True)
    else if aTimeOut > 20 then
      Sleep(10);
  until (GetTickCount64-xStart > aTimeOut);
end;

function TIPCServer.PeekRequest(out outMsgType: TMessageType): Boolean;
var
  xMsgID: Integer;
begin
  Result := PeekRequest(xMsgID, outMsgType);
end;

function TIPCServer.PeekRequest(const aStream: TStream; out outMsgID: Integer;
  out outMsgType: TMessageType): Boolean;
begin
  Result := PeekRequest(outMsgID, outMsgType);
  if Result then
    Result := ReadRequest(outMsgID, aStream);
end;

function TIPCServer.PeekRequest(const aStream: TStream; out outMsgID: Integer;
  out outMsgType: TMessageType; const aTimeOut: Integer): Boolean;
begin
  Result := PeekRequest(outMsgID, outMsgType, aTimeOut);
  if Result then
    Result := ReadRequest(outMsgID, aStream);
end;

function TIPCServer.PeekRequest(const aStream: TStream; out
  outMsgType: TMessageType): Boolean;
var
  xMsgID: Integer;
begin
  Result := PeekRequest(aStream, xMsgID, outMsgType);
end;

procedure TIPCServer.PostResponse(const aMsgID: Integer;
  const aMsgType: TMessageType; const aStream: TStream);
begin
  DoPostMessage(GetResponseFileName(aMsgID), aMsgType, aStream);
end;

function TIPCServer.ReadRequest(const aMsgID: Integer; const aStream: TStream
  ): Boolean;
var
  xStream: TStream;
  xMsgLen: Integer;
  xMsgType: TMessageType;
  xFileRequest: string;
begin
  aStream.Size := 0;
  xFileRequest := GetPeekedRequestFileName(aMsgID);
  Result := CanReadMessage(xFileRequest, xStream, xMsgType, xMsgLen);
  if Result then
  begin
    aStream.CopyFrom(xStream, xMsgLen);
    xStream.Free;
    aStream.Position := 0;
    DeleteFile(xFileRequest);
    Exit(True);
  end;
end;

procedure TIPCServer.SetGlobal(const aGlobal: Boolean);
begin
  if Active then
    raise EICPException.Create(SErrSetGlobalActive);

  inherited SetGlobal(aGlobal);
end;

procedure TIPCServer.SetServerID(const aServerID: string);
begin
  if Active then
    raise EICPException.Create(SErrSetServerIDActive);

  inherited SetServerID(aServerID);
end;

function TIPCServer.StartServer(const aDeletePendingRequests: Boolean): Boolean;
begin
  FFileHandle := FileCreate(FFileName, fmCreate or fmShareExclusive, GLOBAL_RIGHTS);
  Result := (FFileHandle<>feInvalidHandle);
  FActive := Result;
  if Result and aDeletePendingRequests then
    DeletePendingRequests;
end;

function TIPCServer.StopServer(const aDeletePendingRequests: Boolean): Boolean;
begin
  if not FActive then
    Exit(True);

  if FFileHandle<>feInvalidHandle then
    FileClose(FFileHandle);
  Result := DeleteFile(FFileName);
  FFileName := '';

  if aDeletePendingRequests then
    DeletePendingRequests;

  FActive := False;
end;

end.

