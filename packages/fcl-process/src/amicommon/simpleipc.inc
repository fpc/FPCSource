{
  Amiga style simpleipc.inc
}

uses
  Exec, dos;

const
  PORTNAMESTART = 'fpc_';

Type
  TAmigaClientComm = Class(TIPCClientComm)
  Private
    FMsgPort: PMsgPort;
    FPortName: String;
  Public
    Constructor Create(AOwner: TSimpleIPCClient); override;
    Procedure Connect; override;
    Procedure Disconnect; override;
    Procedure SendMessage(MsgType : TMessageType; AStream : TStream); override;
    //Function  ServerRunning : Boolean; override;
  end;

  TAmigaServerComm = Class(TIPCServerComm)
  Private
    FMsgPort: PMsgPort;
    FPortName: String;
    MsgBody: PMessage;
  Public
    Constructor Create(AOwner: TSimpleIPCServer); override;
    destructor Destroy; override;
    Procedure StartServer; override;
    Procedure StopServer; override;
    Function  PeekMessage(TimeOut : Integer) : Boolean; override;
    Procedure ReadMessage ; override;
    Function GetInstanceID : String;override;
  end;
  
// ####### CLIENT  
  
function SafePutToPort(Msg: PMessage; Portname: string): Integer;
 var
   Port: PMsgPort;
   PName: PChar;
 begin
   Result := -1;
   PName := PChar(Portname + #0);
   Forbid();
   Port := FindPort(PName);
   if Assigned(Port) then
   begin
     PutMsg(Port, Msg);
     Result := 0;
   end;
   Permit();
 end;

Constructor TAmigaClientComm.Create(AOwner: TSimpleIPCClient);
begin
  inherited;
end;

Procedure TAmigaClientComm.Connect;
begin
 
end;

Procedure TAmigaClientComm.Disconnect;
begin
  
end;

Procedure TAmigaClientComm.SendMessage(MsgType : TMessageType; AStream : TStream);
var
  Size: Integer;
  FullSize: Integer;
  Memory: Pointer;
  Temp: PByte;
  MsgHead: Exec.PMessage;
  MP: PMsgPort;
  PortName: string;
begin
  Size := AStream.Size - AStream.Position;
  FullSize := Size + Sizeof(Exec.TMessage);
  PortName := PORTNAMESTART + Owner.ServerID;
  Memory := System.AllocMem(FullSize);
  MP := CreateMsgPort;
  try
    MsgHead := Memory;
    MsgHead^.mn_ReplyPort := MP;
    MsgHead^.mn_Length := Size;
    Temp := Memory;
    Inc(Temp, SizeOf(Exec.TMessage));
    AStream.Read(Temp^, Size);
    if SafePutToPort(MsgHead, PortName) = 0 then
      WaitPort(MP);
  finally
    System.FreeMem(Memory);
    DeleteMsgPort(MP);
  end;
end;

// ###### SERVER

Constructor TAmigaServerComm.Create(AOwner: TSimpleIPCServer);
begin
  inherited;
  FMsgPort := CreateMsgPort;
  MsgBody := nil;
end;

destructor TAmigaServerComm.Destroy;
begin
  if Assigned(MsgBody) then
    System.FreeMem(MsgBody);
  DeleteMsgPort(FMsgPort);
  inherited;
end;

Procedure TAmigaServerComm.StartServer;
begin
  FPortName := PORTNAMESTART + Owner.ServerID + #0;
  FMsgPort^.mp_Node.ln_Name := PChar(FPortName);
  FMsgPort^.mp_Node.ln_Pri := 0;
  AddPort(FMsgPort);
  if Assigned(MsgBody) then
    System.FreeMem(MsgBody); 
  MsgBody := nil;
end;

Procedure TAmigaServerComm.StopServer;
begin
  RemPort(FMsgPort);
  if Assigned(MsgBody) then
    System.FreeMem(MsgBody);
  MsgBody := nil;
end;

Function TAmigaServerComm.PeekMessage(TimeOut : Integer) : Boolean;
var
  Msg: PMessage;
  Temp: PByte;
  StartTime: Int64;
begin
  StartTime := GetMsCount;
  Result := False;
  if TimeOut < 0 then
    TimeOut := MaxInt;
  repeat
    Msg := GetMsg(FMsgPort);
    if Assigned(Msg) then
    begin
      Result := True;
      Temp := PByte(Msg);
      Inc(Temp, SizeOf(Exec.TMessage));
      if Assigned(MsgBody) then
        System.FreeMem(MsgBody);
      MsgBody := System.AllocMem(SizeOf(Exec.TMessage) + Msg^.mn_Length);
      Move(Msg^, MsgBody^, SizeOf(Exec.TMessage) + Msg^.mn_Length);
      ReplyMsg(Msg);
      break;
    end;
    Sleep(25);
  until GetMsCount - StartTime >= TimeOut;
end;

Procedure TAmigaServerComm.ReadMessage;
var
  Temp: PByte;
begin
  if Assigned(MsgBody) then
  begin
    Temp := Pointer(MsgBody);
    Inc(Temp, SizeOf(Exec.TMessage));
    Owner.FMsgType := mtString;
    Owner.FMsgData.Size := 0;
    Owner.FMsgData.Seek(0, soFrombeginning);
    Owner.FMsgData.WriteBuffer(temp^, MsgBody^.mn_Length);
    System.FreeMem(MsgBody);
    MsgBody := nil;
  end;
end;

Function TAmigaServerComm.GetInstanceID: String;
begin
  Result := HexStr(FindTask(nil));
end;

// ###### Register

Function TSimpleIPCServer.CommClass : TIPCServerCommClass;
begin
  Result:=TAmigaServerComm;
end;

function TSimpleIPCClient.CommClass: TIPCClientCommClass;
begin
  Result:=TAmigaClientComm;
end;

