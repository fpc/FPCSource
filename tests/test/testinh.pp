{$ifdef fpc}
   {$mode delphi}
{$endif}
type
  TMyComponent = class
  public
    constructor Create(k1:longint;k2: shortstring);
    destructor Destroy;override;
  end;

  TMyComponent1 = class(TMyComponent)
  public
    constructor Create(l1:longint;l2:shortstring);
  end;

constructor TMyComponent.Create(k1:longint;k2:shortstring);
begin
end;

destructor TMyComponent.Destroy;
begin
end;

constructor TMyComponent1.Create(l1:longint;l2:shortstring);
begin
  inherited;
end;

begin
end.
