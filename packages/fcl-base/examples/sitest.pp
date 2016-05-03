program SITest;

{$mode objfpc}
{$h+}

uses
  Classes,
  CustApp, advancedipc, singleinstance;

type
  TMyCustomApplication = class(TCustomApplication)
  private
    procedure ServerReceivedParams(Sender: TBaseSingleInstance; aParams: TStringList);
    procedure ServerReceivedCustomRequest(Sender: TBaseSingleInstance; {%H-}MsgID: Integer; aMsgType: TMessageType; MsgData: TStream);
  end;

const
  MsgType_Request_No_Response = 1;
  MsgType_Request_With_Response = 2;
  MsgType_Response = 3;

{ TMyCustomApplication }

procedure TMyCustomApplication.ServerReceivedCustomRequest(
  Sender: TBaseSingleInstance; MsgID: Integer; aMsgType: TMessageType;
  MsgData: TStream);
var
  xData: string;
  xStringStream: TStringStream;
begin
  MsgData.Position := 0;
  SetLength(xData, MsgData.Size div SizeOf(Char));
  if MsgData.Size > 0 then
    MsgData.ReadBuffer(xData[1], MsgData.Size);

  WriteLn('Request: ', xData);

  if aMsgType = MsgType_Request_With_Response then
  begin
    WriteLn('Sending response to client.');
    xStringStream := TStringStream.Create('my response');
    try
      Sender.ServerPostCustomResponse(MsgID, MsgType_Response, xStringStream);
    finally
      xStringStream.Free;
    end;
  end;
end;

procedure TMyCustomApplication.ServerReceivedParams(Sender: TBaseSingleInstance;
  aParams: TStringList);
var
  I: Integer;
begin
  Writeln('-----');
  Writeln('Params:');
  for I := 0 to aParams.Count-1 do
    Writeln(aParams[I]);
  Writeln('-----');
end;

var
  xApp: TMyCustomApplication;
  xStream: TStringStream;
  xMsgType: TMessageType;
begin
  xApp := TMyCustomApplication.Create(nil);
  try
    xApp.SingleInstance.Enabled := True;
    xApp.SingleInstance.OnServerReceivedParams := @xApp.ServerReceivedParams;
    xApp.SingleInstance.OnServerReceivedCustomRequest := @xApp.ServerReceivedCustomRequest;
    xApp.Initialize;
    Writeln(xApp.SingleInstance.StartResult);
    xApp.Run;

    case xApp.SingleInstance.StartResult of
      siNotResponding: ReadLn;
      siClient:
      begin
        xStream := TStringStream.Create('hello');
        try
          xApp.SingleInstance.ClientSendCustomRequest(MsgType_Request_No_Response, xStream);
        finally
          xStream.Free;
        end;
        xStream := TStringStream.Create('I want a response');
        try
          xApp.SingleInstance.ClientSendCustomRequest(MsgType_Request_With_Response, xStream);
          xStream.Size := 0;
          if xApp.SingleInstance.ClientPeekCustomResponse(xStream, xMsgType) then
            WriteLn('Response: ', xStream.DataString)
          else
            WriteLn('Error: no response');
        finally
          xStream.Free;
        end;
        ReadLn;
      end;
    end;
  finally
    xApp.Free;
  end;
end.

