program testipc_client;

{$MODE ObjFPC}
{$H+}

uses
  Classes, SysUtils, AdvancedIPC;

const
  STRINGMESSAGE_WANTS_RESPONSE = 3;
  STRINGMESSAGE_NO_RESPONSE = 2;
  MESSAGE_STOP = 4;

var
  xClient, xClientNotRunning: TIPCClient;
  xStream, xResponseStream: TStringStream;
  xRequestID: Integer;
  xMsgType: TMessageType;
  I: Integer;
begin
  xClient := nil;
  xClientNotRunning := nil;
  xStream := nil;
  xResponseStream := nil;
  try
    xResponseStream := TStringStream.Create('OK');

    //check connection to to the "hello" server (that has to run)

    xClient := TIPCClient.Create(nil);
    xClient.ServerID := 'hello';

    if not xClient.ServerRunning then
    begin
      Writeln('ERROR: Server '+xClient.ServerID+' is not running.');
      Writeln('Closing');
      Exit;
    end;

    //first send some messages to server that is not running
    xClientNotRunning := TIPCClient.Create(nil);
    xClientNotRunning.ServerID := 'not_running';

    if xClientNotRunning.ServerRunning then
    begin
      Writeln('ERROR: Server '+xClientNotRunning.ServerID+' is running. This test needs that the server doesn''t run.');
      Writeln('Closing');
      Exit;
    end;

    for I := 1 to 10 do
    begin
      FreeAndNil(xStream);
      xStream := TStringStream.Create('Message '+IntToStr(I));
      xStream.Position := 0;
      xClientNotRunning.PostRequest(STRINGMESSAGE_NO_RESPONSE, xStream);
    end;

    FreeAndNil(xClientNotRunning);

    //now send messages to the "hello" server
    FreeAndNil(xStream);
    xStream := TStringStream.Create('I want some response.');
    xStream.Position := 0;
    if xClient.SendRequest(STRINGMESSAGE_WANTS_RESPONSE, xStream, 100, xRequestID) and
       xClient.PeekResponse(xResponseStream, xMsgType, 100)
    then
      Writeln('Request-response test OK.')
    else
      Writeln('ERROR: Request-response test failed.');

    FreeAndNil(xStream);
    xStream := TStringStream.Create('I do not want any response.');
    xStream.Position := 0;
    if xClient.SendRequest(STRINGMESSAGE_NO_RESPONSE, xStream, 100, xRequestID) then
    begin
      if xClient.PeekResponse(xResponseStream, xMsgType, 100) then
        Writeln('ERROR: I received a response even that I didn''t want any. What happened?')
      else
        Writeln('Request test OK.');
    end else
      Writeln('ERROR: Request test failed.');

    if xClient.SendRequest(MESSAGE_STOP, nil, 100) and
       not xClient.ServerRunning
    then
      Writeln('Server was sucessfully stopped.')
    else
      Writeln('ERROR: I could not stop the server.')
  finally
    xClient.Free;
    xClientNotRunning.Free;
    xStream.Free;
    xResponseStream.Free;
  end;
end.

