program tanonfunc31;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test capturing a reference counted interface }

type
  TProc = reference to procedure;

  IGetInt = interface
    function GetInt: Integer;
  end;

  TTestObj = class(TInterfacedObject, IGetInt)
    constructor Create;
    destructor Destroy; override;
    function GetInt: Integer;
  end;

var
  IntfAlive: Boolean;

constructor TTestObj.Create;
begin
  inherited;
  IntfAlive := True;
end;

destructor TTestObj.Destroy;
begin
  IntfAlive := False;
  inherited
end;

function TTestObj.GetInt: Integer;
begin
  Result := 123;
end;

function CaptureIntf(Intf: IGetInt): TProc;
begin
  Result := procedure
  begin
    if Intf.GetInt <> 123 then
      Halt(1);
  end;
end;

procedure Test;
var
  Intf: IGetInt;
  P: TProc;
begin
  Intf := TTestObj.Create;
  if not IntfAlive then
    Halt(2);

  P := CaptureIntf(Intf);
  Intf := nil;
  if not IntfAlive then
    Halt(3);

  P();
  P := nil;
  if IntfAlive then
    Halt(4);
end;

begin
  Test;
end.

