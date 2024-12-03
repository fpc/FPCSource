library wasmwebsocketdemo;

uses fpjson, jsonparser, basenenc, sysutils, wasm.logger.api, wasm.websocket.api, wasm.websocket.shared, wasm.websocket.objects;

Type

  { TApplication }

  TApplication = class(TObject)
  Private
    FWS : TWasmWebsocket;
    procedure HandleError(Sender: TObject);
    procedure HandleMessage(Sender: TObject; const IsString: Boolean; aPayload: TBytes);
    procedure HandleOpen(Sender: TObject);
    procedure HandleClose(Sender: TObject; aCode : Integer; const aReason : String; aIsClean : Boolean);
    procedure HandleWebsocketLog(Level: TWasmWebSocketLogLevel; const Msg: string);
  Public
    Procedure Run;
    Property WS : TWasmWebSocket Read FWS;
  end;

var
  Application : TApplication;

procedure sendmessage(buf : PByte; Len : Longint);

var
  Msg : UTF8String;

begin
  SetLength(Msg,Len);
  Move(Buf^,Msg[1],Len);
  Application.FWS.SendMessage(Msg);
end;

exports sendmessage;

procedure TApplication.HandleOpen(Sender: TObject);
begin
  Writeln('Websocket is opened');
end;

procedure TApplication.HandleClose(Sender: TObject; aCode : Integer; const aReason : String; aIsClean : Boolean);

const
  SClean : Array[Boolean] of string = ('not ','');

begin
  Writeln('Websocket closed ',SClean[aIsClean],'cleanly with code ',aCode,', reason: "',aReason,'"');
end;

procedure TApplication.HandleWebsocketLog(Level: TWasmWebSocketLogLevel; const Msg: string);
begin
  Writeln('(Websocket Log) [', Level,']: ',Msg);
end;

procedure TApplication.HandleError(Sender: TObject);
begin
  Writeln('Error detected on websocket.');
end;

procedure TApplication.HandleMessage(Sender: TObject; const IsString: Boolean; aPayload: TBytes);

var
  Msg,lfrom,lRecip : String;
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  if IsString then
    begin
    Msg:=TEncoding.UTF8.GetAnsiString(aPayLoad);
    D:=Nil;
    try
      D:=GetJSON(Msg,True);
    except
      on E : Exception do
        Writeln('Received non-JSON message: '+Msg);
    end;
    if D is TJSONObject then
      begin
      lFrom:=O.get('from','(unknown)');
      lRecip:=O.get('recip','');
      msg:=O.get('msg','');
      if lRecip<>'' then
        lFrom:=lFrom+' [PM]';
      Writeln(lFrom,' > ',Msg);
      end
    else
      Writeln('Received invalid JSON message: '+Msg);
    end
  else
    begin
    Msg:=Base64.Encode(aPayload);
    Writeln('Received binary message : ',Msg);
    end;
end;

Procedure TApplication.Run;

begin
  FWS:=TWasmWebsocket.Create(Nil);
  OnWasmLog:=@HandleWebsocketLog;
  WS.OnOpen:=@HandleOpen;
  WS.OnError:=@HandleError;
  WS.OnClose:=@HandleClose;
  WS.OnMessage:=@HandleMessage;
  WS.Open('ws://localhost:6060/','');
  Writeln('Websocket opened, waiting for messages');
end;

begin
  Application:=TApplication.Create;
  Application.Run;
end.

