{
    This file is part of the Free Component library.
    Copyright (c) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team

    Unix implementation of one-way IPC between 2 processes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef ipcunit}
unit pipesipc;

interface

uses sysutils, classes, simpleipc, baseunix;

{$else}

uses baseunix;
{$endif}


ResourceString
  SErrFailedToCreatePipe = 'Failed to create named pipe: %s';
  SErrFailedToRemovePipe = 'Failed to remove named pipe: %s';

{ ---------------------------------------------------------------------
    TPipeClientComm
  ---------------------------------------------------------------------}

Type
  TPipeClientComm = Class(TIPCClientComm)
  Private
    FFileName: String;
    FStream: TFileStream;
  Public
    Constructor Create(AOWner : TSimpleIPCClient); override;
    Procedure Connect; override;
    Procedure Disconnect; override;
    Procedure SendMessage(MsgType : TMessageType; AStream : TStream); override;
    Function  ServerRunning : Boolean; override;
    Property FileName : String Read FFileName;
    Property Stream : TFileStream Read FStream;
  end;

{$ifdef ipcunit}
implementation
{$endif}

constructor TPipeClientComm.Create(AOWner: TSimpleIPCClient);
begin
  inherited Create(AOWner);
  FFileName:=Owner.ServerID;
  If (Owner.ServerInstance<>'') then
    FFileName:=FFileName+'-'+Owner.ServerInstance;
  if FFileName[1]<>'/' then
    FFileName:=GetTempDir(true)+FFileName;
end;


procedure TPipeClientComm.Connect;
begin
  If Not ServerRunning then
    DoError(SErrServerNotActive,[Owner.ServerID]);
  // Use the sharedenynone line to allow more then one client 
  // communicating with one server at the same time
  // see also mantis 15219
  FStream:=TFileStream.Create(FFileName,fmOpenWrite+fmShareDenyNone);
  // FStream:=TFileStream.Create(FFileName,fmOpenWrite);
end;

procedure TPipeClientComm.Disconnect;
begin
  FreeAndNil(FStream);
end;

procedure TPipeClientComm.SendMessage(MsgType : TMessagetype; AStream: TStream);

Var
  Hdr : TMsgHeader;

begin
  Hdr.Version:=MsgVersion;
  Hdr.msgType:=MsgType;
  Hdr.MsgLen:=AStream.Size;
  FStream.WriteBuffer(hdr,SizeOf(hdr));
  FStream.CopyFrom(AStream,0);
end;

function TPipeClientComm.ServerRunning: Boolean;
var
  fd: cint;
begin
  Result:=FileExists(FFileName);
  // it's possible to have a stale file that is not open for reading which will
  // cause fpOpen to hang/block later when .Active is set to true while it
  // wait's for the pipe to be opened on the other end
  if Result then
  begin
    // O_WRONLY | O_NONBLOCK causes fpOpen to return -1 if the file is not open for reading
    // so in fact the 'server' is not running
    fd := FpOpen(FFileName, O_WRONLY or O_NONBLOCK);
    if fd = -1 then
    begin
      Result := False;
      // delete the named pipe since it's orphaned
      FpUnlink(FFileName);
    end
    else
      FpClose(fd);
  end;
end;


{ ---------------------------------------------------------------------
    TPipeServerComm
  ---------------------------------------------------------------------}

Type

  { TPipeServerComm }

  TPipeServerComm = Class(TIPCServerComm)
  Private
    FFileName: String;
    FStream: TFileStream;
  Protected
    Procedure DoReadMessage; virtual;
  Public
    Constructor Create(AOWner : TSimpleIPCServer); override;
    Procedure StartServer; override;
    Procedure StopServer; override;
    Function  PeekMessage(TimeOut : Integer) : Boolean; override;
    Procedure ReadMessage ; override;
    Function GetInstanceID : String;override;
    Property FileName : String Read FFileName;
    Property Stream : TFileStream Read FStream;
  end;

procedure TPipeServerComm.DoReadMessage;

Var
  Hdr : TMsgHeader;

begin
  FStream.ReadBuffer(Hdr,SizeOf(Hdr));
  PushMessage(Hdr,FStream);
end;

constructor TPipeServerComm.Create(AOWner: TSimpleIPCServer);
begin
  inherited Create(AOWner);
  FFileName:=Owner.ServerID;
  If Not Owner.Global then
    FFileName:=FFileName+'-'+IntToStr(fpGetPID);
  if FFileName[1]<>'/' then
    FFileName:=GetTempDir(Owner.Global)+FFileName;
end;


procedure TPipeServerComm.StartServer;

const
  PrivateRights = S_IRUSR or S_IWUSR;
  GlobalRights  = PrivateRights or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Rights : Array [Boolean] of Integer = (PrivateRights,GlobalRights);  
    
begin
  If not FileExists(FFileName) then
    If (fpmkFifo(FFileName,438)<>0) then
      DoError(SErrFailedToCreatePipe,[FFileName]);
  FStream:=TFileStream.Create(FFileName,fmOpenReadWrite+fmShareDenyNone,Rights[Owner.Global]);
end;

procedure TPipeServerComm.StopServer;
begin
  FreeAndNil(FStream);
  if Not DeleteFile(FFileName) then
    DoError(SErrFailedtoRemovePipe,[FFileName]);
end;

function TPipeServerComm.PeekMessage(TimeOut: Integer): Boolean;

Var
  FDS : TFDSet;

begin
  fpfd_zero(FDS);
  fpfd_set(FStream.Handle,FDS);
  Result:=False;
  While fpSelect(FStream.Handle+1,@FDS,Nil,Nil,TimeOut)>0 do
    begin
    DoReadMessage;
    Result:=True;
    end;
end;

procedure TPipeServerComm.ReadMessage;

begin
  DoReadMessage;
end;


function TPipeServerComm.GetInstanceID: String;
begin
  Result:=IntToStr(fpGetPID);
end;


{ ---------------------------------------------------------------------
    Set TSimpleIPCClient / TSimpleIPCServer defaults.
  ---------------------------------------------------------------------}

{$ifndef ipcunit}
function TSimpleIPCServer.CommClass: TIPCServerCommClass;

begin
  if (DefaultIPCServerClass<>Nil) then
    Result:=DefaultIPCServerClass
  else
    Result:=TPipeServerComm;
end;

function TSimpleIPCClient.CommClass: TIPCClientCommClass;
begin
  if (DefaultIPCClientClass<>Nil) then
    Result:=DefaultIPCClientClass
  else
    Result:=TPipeClientComm;
end;

{$else ipcunit}

end.
{$endif}
