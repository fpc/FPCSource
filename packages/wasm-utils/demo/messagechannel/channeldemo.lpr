library channeldemo;

uses sysutils, wasm.messagechannel.objects;

Type

  { TApp }

  TApp = Class(TObject)
    FChannel : TWasmMessageChannel;
    FCounter : Integer;
    procedure SendMessage;
    constructor create;
  end;

var
  App : TApp;

procedure SendMessage;

begin
  App.SendMessage;
end;

exports SendMessage;

{ TApp }

procedure HandleMessage(Sender: TObject; const aMessage: string);
begin
  Writeln('WASM received on "some_channel" a message: ',aMessage);
end;

procedure TApp.SendMessage;
var
  S : string;
begin
  inc(FCounter);
  S:=Format('This is message #%d.',[FCounter]);
  FChannel.SendMessage(S,False);
end;

constructor TApp.create;
begin
  FChannel:=TWasmBroadcastMessageChannel.Create('some_channel');
  FChannel.OnMessage:=@HandleMessage;
end;

begin
  App:=TApp.Create;
end.

