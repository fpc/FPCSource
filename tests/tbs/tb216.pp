{ Old file: tbs0249.pp }
{ procedure of object cannot be assigned to property.  OK 0.99.11 (PFV) }

program TestEvent;

{$mode objfpc}
{$M+}

type
  TNotifyEvent = procedure( Sender: TObject ) of object;

  THost = class
  protected
    FOnEvent: TNotifyEvent;
    procedure SetOnEvent( Value: TNotifyEvent );
  public
    constructor Create;
    procedure Trigger;
    procedure SayHello;
  published
    property OnEvent: TNotifyEvent read FOnEvent write SetOnEvent;
  end;

  TDummy = class
    procedure HandleEvent( Sender: TObject );
  end;

constructor THost.Create;
begin
  FOnEvent := nil;
end;

procedure THost.Trigger;
begin
  if @FOnEvent <> nil then
    FOnEvent( Self )
end;

procedure THost.SetOnEvent( Value: TNotifyEvent );
begin
  FOnEvent := Value
end;

procedure THost.SayHello;
begin
  Writeln( 'Hello event' )
end;

procedure TDummy.HandleEvent( Sender: TObject );
begin
  THost( Sender ).SayHello
end;


var
  Host: THost;
  Dummy: TDummy;
begin
  Dummy := TDummy.Create;
  Host := THost.Create;
  with Host,Dummy do
    OnEvent := @HandleEvent; // this is 57, 27 is ";"
  Host.Trigger;
end.
