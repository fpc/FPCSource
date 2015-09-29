{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Ondrej Pokorny

    Unit implementing two-way (request/response) IPC between 1 server and more clients, based on files.

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
  HEADER_VERSION = 1;

type
  TMessageHeader = packed record
    HeaderVersion: Integer;
    FileLock: Byte;//0 = unlocked, 1 = locked
    MsgType: Integer;
    MsgLen: Integer;
    MsgVersion: Integer;
  end;

  TFileHandle = Classes.THandle;

  TReleaseHandleStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

  TIPCBase = class
  private
    FGlobal: Boolean;
    FFileName: string;
    FServerName: string;
    FMessageVersion: Integer;
  protected
    class function ServerNameToFileName(const aServerName: string; const aGlobal: Boolean): string;
    function GetResponseFileName(const aMsgID: Integer): string;
    function GetResponseFileName(const aRequestFileName: string): string;
    function GetRequestPrefix: string;
    function GetRequestFileName(const aMsgID: Integer): string;
    function RequestFileNameToMsgID(const aFileName: string): Integer;

    function GetUniqueRequest(out outFileName: string): Integer;
    procedure SetServerName(const aServerName: string); virtual;
    procedure SetGlobal(const aGlobal: Boolean); virtual;

    function CanReadMessage(const aFileName: string; out outStream: TStream; out outMsgType, outMsgLen: Integer): Boolean;
    procedure DoPostMessage(const aFileName: string; const aMsgType: Integer; const aStream: TStream);

    property FileName: string read FFileName;
  public
    constructor Create; virtual;
  public
    class procedure FindRunningServers(const aServerNamePrefix: string;
      const outServerNames: TStrings; const aGlobal: Boolean = False);
    class function ServerIsRunning(const aServerName: string; const aGlobal: Boolean = False): Boolean;
    property ServerName: string read FServerName write SetServerName;
    property Global: Boolean read FGlobal write SetGlobal;
    property MessageVersion: Integer read FMessageVersion write FMessageVersion;
  end;

  TIPCClient = class(TIPCBase)
  var
    FLastMsgFileName: string;
  public
    function PostRequest(const aMsgType: Integer; const aStream: TStream): Integer;//returns ID
    function PeekResponse(const aStream: TStream; var outMsgType: Integer; const aTimeOut: Integer): Boolean;
    procedure DeleteRequest;
    function ServerRunning: Boolean;
  end;

  TIPCServer = class(TIPCBase)
  private
    FFileHandle: TFileHandle;
    FActive: Boolean;

    function FindFirstRequest(out outFileName: string; out outStream: TStream; out outMsgType, outMsgLen: Integer): Integer;

  protected
    procedure SetServerName(const aServerName: string); override;
    procedure SetGlobal(const aGlobal: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function PeekRequest(const aStream: TStream; var outMsgType: Integer): Boolean; overload;
    function PeekRequest(const aStream: TStream; var outMsgID, outMsgType: Integer): Boolean; overload;
    function PeekRequest(const aStream: TStream; var outMsgID, outMsgType: Integer; const aTimeOut: Integer): Boolean; overload;
    procedure PostResponse(const aMsgID: Integer; const aMsgType: Integer; const aStream: TStream);

    function FindHighestPendingRequestId: Integer;
    function GetPendingRequestCount: Integer;

    function StartServer(const aDeletePendingRequests: Boolean = True): Boolean;//returns true if unique and started
    function StopServer(const aDeletePendingRequests: Boolean = True): Boolean;//returns true if stopped

    procedure DeletePendingRequests;

    property Active: Boolean read FActive;//true if started
  end;

  EICPException = class(Exception);

implementation

const
  {$IFDEF UNIX}
  GLOBAL_RIGHTS = S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  {$ELSE}
  GLOBAL_RIGHTS = 0;
  {$ENDIF}

{ TIPCBase }

function TIPCBase.CanReadMessage(const aFileName: string; out
  outStream: TStream; out outMsgType, outMsgLen: Integer): Boolean;
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

constructor TIPCBase.Create;
begin
  inherited Create;
end;

function TIPCBase.GetUniqueRequest(out outFileName: string): Integer;
begin
  Randomize;
  repeat
    Result := Random(High(Integer));
    outFileName := GetRequestFileName(Result);
  until not FileExists(outFileName);
end;

class function TIPCBase.ServerIsRunning(const aServerName: string;
  const aGlobal: Boolean): Boolean;
var
  xServerFileHandle: TFileHandle;
  xFileName: String;
begin
  xFileName := ServerNameToFileName(aServerName, aGlobal);
  Result := FileExists(xFileName);
  if Result then
  begin//+ check -> we should not be able to access the file
    xServerFileHandle := FileCreate(xFileName, fmOpenReadWrite or fmShareExclusive, GLOBAL_RIGHTS);
    Result := (xServerFileHandle=feInvalidHandle);
    if not Result then
      FileClose(xServerFileHandle);
  end;
end;

class function TIPCBase.ServerNameToFileName(const aServerName: string;
  const aGlobal: Boolean): string;
begin
  Result := GetTempDir(aGlobal)+aServerName;
end;

procedure TIPCBase.SetGlobal(const aGlobal: Boolean);
begin
  if FGlobal = aGlobal then Exit;

  FGlobal := aGlobal;
  FFileName := ServerNameToFileName(FServerName, FGlobal);
end;

procedure TIPCBase.DoPostMessage(const aFileName: string;
  const aMsgType: Integer; const aStream: TStream);
var
  xHeader: TMessageHeader;
  xStream: TFileStream;
begin
  xHeader.HeaderVersion := HEADER_VERSION;
  xHeader.FileLock := 1;//locking
  xHeader.MsgType := aMsgType;
  xHeader.MsgLen := aStream.Size-aStream.Position;
  xHeader.MsgVersion := MessageVersion;

  xStream := TFileStream.Create(aFileName, fmCreate or fmShareExclusive, GLOBAL_RIGHTS);
  try
    xStream.WriteBuffer(xHeader, SizeOf(xHeader));
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
  if Length(aFileName) > 8 then
    Result := StrToIntDef('$'+Copy(aFileName, Length(aFileName)-7, 8), -1)
  else
    Result := -1;
end;

class procedure TIPCBase.FindRunningServers(const aServerNamePrefix: string;
  const outServerNames: TStrings; const aGlobal: Boolean);
var
  xRec: TRawByteSearchRec;
begin
  if FindFirst(ServerNameToFileName(aServerNamePrefix+'*', aGlobal), faAnyFile, xRec) = 0 then
  begin
    repeat
      if (Pos('_', xRec.Name) = 0) and//file that we found is not pending a message
         ServerIsRunning(xRec.Name)
      then
        outServerNames.Add(xRec.Name);
    until FindNext(xRec) <> 0;
  end;
  FindClose(xRec);
end;

function TIPCBase.GetRequestFileName(const aMsgID: Integer): string;
begin
  Result := GetRequestPrefix+IntToHex(aMsgID, 8);
end;

function TIPCBase.GetRequestPrefix: string;
begin
  Result := FFileName+'_';
end;

function TIPCBase.GetResponseFileName(const aMsgID: Integer): string;
begin
  Result := GetResponseFileName(GetRequestFileName(aMsgID));
end;

function TIPCBase.GetResponseFileName(const aRequestFileName: string): string;
begin
  Result := aRequestFileName+'_r';
end;

procedure TIPCBase.SetServerName(const aServerName: string);
var
  I: Integer;
begin
  if FServerName = aServerName then Exit;

  for I := 1 to Length(aServerName) do
  if not (aServerName[I] in ['a'..'z', 'A'..'Z', '0'..'9']) then
    raise EICPException.Create('You cannot use the "_" character in server name.');

  FServerName := aServerName;

  FFileName := ServerNameToFileName(FServerName, FGlobal);
end;

{ TIPCClient }

procedure TIPCClient.DeleteRequest;
begin
  if DeleteFile(FLastMsgFileName) then
    FLastMsgFileName := '';
end;

function TIPCClient.PeekResponse(const aStream: TStream;
  var outMsgType: Integer; const aTimeOut: Integer): Boolean;
var
  xStart: QWord;
  xStream: TStream;
  xMsgLen: Integer;
  xFileResponse: string;
begin
  aStream.Size := 0;
  outMsgType := -1;
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

function TIPCClient.PostRequest(const aMsgType: Integer; const aStream: TStream
  ): Integer;
begin
  Result := GetUniqueRequest(FLastMsgFileName);
  DeleteFile(GetResponseFileName(FLastMsgFileName));//delete old response, if there is any
  DoPostMessage(FLastMsgFileName, aMsgType, aStream);
end;

function TIPCClient.ServerRunning: Boolean;
begin
  Result := ServerIsRunning(ServerName);
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

constructor TIPCServer.Create;
begin
  inherited Create;

  FFileHandle := feInvalidHandle;
end;

destructor TIPCServer.Destroy;
begin
  if FActive then
    StopServer;

  inherited Destroy;
end;

function TIPCServer.FindFirstRequest(out outFileName: string; out
  outStream: TStream; out outMsgType, outMsgLen: Integer): Integer;
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

function TIPCServer.PeekRequest(const aStream: TStream; var outMsgID,
  outMsgType: Integer): Boolean;
var
  xStream: TStream;
  xMsgLen: Integer;
  xMsgFileName: string;
begin
  aStream.Size := 0;
  outMsgType := -1;
  xMsgFileName := '';
  outMsgID := FindFirstRequest(xMsgFileName, xStream, outMsgType, xMsgLen);
  Result := outMsgID >= 0;
  if Result then
  begin
    aStream.CopyFrom(xStream, xMsgLen);
    aStream.Position := 0;
    xStream.Free;
    DeleteFile(xMsgFileName);
  end;
end;

function TIPCServer.PeekRequest(const aStream: TStream; var outMsgID,
  outMsgType: Integer; const aTimeOut: Integer): Boolean;
var
  xStart: QWord;
begin
  Result := False;
  xStart := GetTickCount64;
  repeat
    if PeekRequest(aStream, outMsgID, outMsgType) then
      Exit(True)
    else if aTimeOut > 20 then
      Sleep(10);
  until (GetTickCount64-xStart > aTimeOut);
end;

function TIPCServer.PeekRequest(const aStream: TStream; var outMsgType: Integer
  ): Boolean;
var
  xMsgID: Integer;
begin
  Result := PeekRequest(aStream, xMsgID{%H-}, outMsgType);
end;

procedure TIPCServer.PostResponse(const aMsgID: Integer;
  const aMsgType: Integer; const aStream: TStream);
begin
  DoPostMessage(GetResponseFileName(aMsgID), aMsgType, aStream);
end;

procedure TIPCServer.SetGlobal(const aGlobal: Boolean);
begin
  if Active then
    raise EICPException.Create('You cannot change the global property when the server is active.');

  inherited SetGlobal(aGlobal);
end;

procedure TIPCServer.SetServerName(const aServerName: string);
begin
  if Active then
    raise EICPException.Create('You cannot change the server name when the server is active.');

  inherited SetServerName(aServerName);
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
  DeleteFile(FFileName);
  FFileName := '';

  if aDeletePendingRequests then
    DeletePendingRequests;

  FActive := False;
end;

end.

