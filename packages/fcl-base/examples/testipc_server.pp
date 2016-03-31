program testipc_server;

{$MODE ObjFPC}
{$H+}

uses
  Classes, SysUtils, AdvancedIPC;

const
  STRINGMESSAGE_WANTS_RESPONSE = 3;
  STRINGMESSAGE_NO_RESPONSE = 2;
  MESSAGE_STOP = 4;

var
  xServer: TIPCServer;
  xStream, xResponseStream: TStringStream;
  xMsgID: Integer;
  xMsgType: TMessageType;
  xNotRunningMessagesCount: Integer;
begin
  xServer := nil;
  xStream := nil;
  xResponseStream := nil;
  try
    xStream := TStringStream.Create('');
    xResponseStream := TStringStream.Create('OK');

    //first get all messages from the hello server
    xServer := TIPCServer.Create(nil);
    xServer.ServerID := 'hello';
    xServer.StartServer;

    WriteLn('Server ', xServer.ServerID, ' started.');
    WriteLn('-----');

    while True do
    begin
      if xServer.PeekRequest(xMsgID{%H-}, xMsgType{%H-}) then
      begin
        case xMsgType of
          STRINGMESSAGE_WANTS_RESPONSE, STRINGMESSAGE_NO_RESPONSE:
          begin
            xServer.ReadRequest(xMsgID, xStream);
            WriteLn('Received string message:');
            WriteLn(xStream.DataString);
            if xMsgType = STRINGMESSAGE_WANTS_RESPONSE then
            begin
              xResponseStream.Position := 0;
              xServer.PostResponse(xMsgID, STRINGMESSAGE_NO_RESPONSE, xResponseStream);
              WriteLn('Posting response.');
            end;
            WriteLn('-----');
          end;
          MESSAGE_STOP:
          begin
            WriteLn('Stopping '+xServer.ServerID+' server.');
            WriteLn('-----');
            Break;
          end;
        end;
      end else
        Sleep(50);
    end;

    FreeAndNil(xServer);

    //now try to get all unhandled messages from the not_running server
    //please see that the messages are not peeked in the order they have been posted (this is correct/designed behavior).
    xServer := TIPCServer.Create(nil);
    xServer.ServerID := 'not_running';
    xServer.StartServer(False);

    WriteLn('');
    WriteLn('Server ', xServer.ServerID, ' started.');
    WriteLn('-----');

    xNotRunningMessagesCount := 0;
    while xServer.PeekRequest(xStream, xMsgID, xMsgType) do
    begin
      if xMsgType = STRINGMESSAGE_NO_RESPONSE then
      begin
        WriteLn('Received message: ', xStream.DataString);
        Inc(xNotRunningMessagesCount);
      end else
        WriteLn('ERROR: Wrong message type: ', xMsgType);

      WriteLn('-----');
    end;

    if xNotRunningMessagesCount <> 10 then
    begin
      WriteLn('ERROR: Wrong message count: ', xNotRunningMessagesCount);
      WriteLn('-----');
    end;

    WriteLn('Stopping '+xServer.ServerID+' server.');
    WriteLn('-----');
    FreeAndNil(xServer);
  finally
    xServer.Free;
    xStream.Free;
    xResponseStream.Free;
  end;
end.

