unit ltimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLTimer }

  TLTimer = class(TObject)
  protected
    FOnTimer: TNotifyEvent;
    FInterval: TDateTime;
    FStarted: TDateTime;
    FOneShot: Boolean;
    FEnabled: Boolean;

    function  GetInterval: Integer;
    procedure SetInterval(const aValue: Integer);
  public
    procedure CallAction;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read GetInterval write SetInterval;
    property OneShot: Boolean read FOneShot write FOneShot;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

{ TLTimer }

function TLTimer.GetInterval: Integer;
begin
  Result := Round(FInterval * MSecsPerDay);
end;

procedure TLTimer.SetInterval(const aValue: Integer);
begin
  FInterval := AValue / MSecsPerDay;
  FStarted := Now;
  FEnabled := true;
end;

procedure TLTimer.CallAction;
begin
  if FEnabled and Assigned(FOnTimer) and (Now - FStarted >= FInterval) then 
  begin
    FOnTimer(Self);
    if not FOneShot then
      FStarted := Now
    else
      FEnabled := false;
  end;
end;

end.

