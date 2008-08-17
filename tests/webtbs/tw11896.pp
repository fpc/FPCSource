program destroytest;

{$mode delphi}

type
  TTest = class(TObject)
    a: array[0..32767] of Integer;
    procedure x;
    procedure y;
    procedure beforedestruction;override;
  end;

var
  testobj: TTest;
  destroyed: boolean;

procedure TTest.beforedestruction;
begin
  destroyed:=true;
  inherited beforedestruction;
end;

procedure TTest.x;
begin
  Destroy;
end;

procedure TTest.y;
begin
  Self.Destroy;
end;

function GetUsedMemory: Integer;
begin
  Result := GetHeapStatus.TotalAllocated;
end;

begin
  testobj := TTest.create;
  destroyed:=false;
  testobj.x;
  if not destroyed then
    halt(1);

  destroyed:=false;
  testobj := TTest.create;
  testobj.y;
  if not destroyed then
    halt(2);
end.

