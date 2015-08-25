{
  Amiga style simpleipc.inc
}

{$DEFINE OSNEEDIPCINITDONE}

uses
  Exec, dos;

ResourceString
  SErrMsgPortExists = 'MsgPort already exists: %s';

const
  PORTNAMESTART = 'fpc_';

Var
  MsgPorts: Classes.TList;

procedure AddMsgPort(AMsgPort: PMsgPort);
begin
  if Assigned(MsgPorts) then
  begin
    MsgPorts.Add(AMsgPort);
  end;
end;

procedure RemoveMsgPort(AMsgPort: PMsgPort);
var
  Idx: Integer;
begin
  if Assigned(MsgPorts) then
  begin
    Idx := MsgPorts.IndexOf(AMsgPort);
    if Idx >= 0 then
    begin
      MsgPorts.Delete(Idx);
      if Assigned(AMsgPort^.mp_Node.ln_Name) and (string(AMsgPort^.mp_Node.ln_Name) <> '') and Assigned(FindPort(AMsgPort^.mp_Node.ln_Name)) then
        RemPort(AMsgPort);
      DeleteMsgPort(AMsgPort);
    end;  
  end;
end;


procedure IPCInit;
begin
  MsgPorts := Classes.TList.Create;
end;

procedure IPCDone;
var
  I: integer;
begin
  try
    for i := 0 to MsgPorts.Count - 1 do
        RemoveMsgPort(PMsgPort(MsgPorts[i]));
    finally  
      FreeAndNil(MsgPorts);  
    end;  
end;


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
    Function  ServerRunning : Boolean; override;
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
  
function SafePutToPort(Msg: PMessage; Portname: string): Boolean;
 var
   Port: PMsgPort;
   PName: PChar;
 begin
   Result := False;
   PName := PChar(Portname + #0);
   Forbid();
   Port := FindPort(PName);
   if Assigned(Port) then
   begin
     if Assigned(Msg) then
       PutMsg(Port, Msg);
     Result := True;
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
  FullSize := Size + SizeOf(TMessageType) + Sizeof(Exec.TMessage);
  PortName := PORTNAMESTART + Owner.ServerID;
  Memory := System.AllocMem(FullSize);
  MP := CreateMsgPort;
  try
    MsgHead := Memory;
    MsgHead^.mn_ReplyPort := MP;
    MsgHead^.mn_Length := Size;
    Temp := Memory;
    Inc(Temp, SizeOf(Exec.TMessage));
    Move(MsgType, Temp^, SizeOf(TMessageType));
    Inc(Temp, SizeOf(TMessageType));
    AStream.Read(Temp^, Size);
    if SafePutToPort(MsgHead, PortName) then
      WaitPort(MP);
  finally
    System.FreeMem(Memory);
    DeleteMsgPort(MP);
  end;
end;

Function TAmigaClientComm.ServerRunning : Boolean;
begin
  Result := SafePutToPort(nil, PORTNAMESTART + Owner.ServerID); 
end;

// ###### SERVER

Constructor TAmigaServerComm.Create(AOwner: TSimpleIPCServer);
begin
  inherited;
  FMsgPort := CreateMsgPort;
  AddMsgPort(FMsgPort);
  MsgBody := nil;
end;

destructor TAmigaServerComm.Destroy;
begin
  if Assigned(MsgBody) then
    System.FreeMem(MsgBody);
  RemoveMsgPort(FMsgPort);
  inherited;
end;

Procedure TAmigaServerComm.StartServer;
begin
  FPortName := PORTNAMESTART + Owner.ServerID + #0;
  if Assigned(FindPort(PChar(FPortName))) then
  begin
    DoError(SErrMsgPortExists,[FPortName]);
    Exit;
  end;
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
      MsgBody := System.AllocMem(SizeOf(Exec.TMessage) + SizeOf(TMessageType) + Msg^.mn_Length);
      Move(Msg^, MsgBody^, SizeOf(Exec.TMessage) + SizeOf(TMessageType) + Msg^.mn_Length);
      ReplyMsg(Msg);
      break;
    end;
    Sleep(25);
  until GetMsCount - StartTime >= TimeOut;
end;

Procedure TAmigaServerComm.ReadMessage;
var
  Temp: PByte;
  MsgType: TMessageType;
begin
  if Assigned(MsgBody) then
  begin
    Temp := Pointer(MsgBody);
    Inc(Temp, SizeOf(Exec.TMessage));
    MsgType := 0;
    Move(Temp^, MsgType, SizeOf(TMessageType));
    Inc(Temp, SizeOf(TMessageType));    
    Owner.FMsgType := MsgType;
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

