{ %norun }
unit terecs_u1;

{$mode delphi}

interface
type
  HWND = integer;
  TFoo = record
    hWnd : HWND;
  private
    F1: Integer;
    F2: Byte;
  public
    type
      TBar = Integer;
    const
      C: TBar = 1;
    var
      F3: TBar;
      F4: Byte;
    class var
      F5: TBar;
    function Test(n: TBar): TBar;
    class function Test1(n: TBar): TBar; static;

    procedure Set3(const Value: TBar);
    class procedure Set5(const Value: TBar); static;

    property P3: TBar read F3 write Set3;
    class property P5: TBar read F5 write Set5;

    class constructor Create;
    class destructor Destroy;

    procedure Test2;
    procedure Test3;
  end;

procedure Test4(AFoo: TFoo);

implementation

function TFoo.Test(n: TBar): TBar;
begin
  Result := F3 + F4 + n;
end;

class function TFoo.Test1(n: TBar): TBar;
begin
  Result := C + n;
end;

class constructor TFoo.Create;
begin
  F5 := 6;
end;

class destructor TFoo.Destroy;
begin
  WriteLn('TFoo.Destroy');
end;

procedure TFoo.Set3(const Value: TBar);
begin
  F3 := Value;
end;

class procedure TFoo.Set5(const Value: TBar); static;
begin
  F5 := Value;
end;

procedure TFoo.Test2;
begin
  if Self.C <> 1 then
    halt(50);
  if Self.F3 <> 7 then
    halt(51);
end;

procedure TFoo.Test3;
begin
  Test4(Self);
end;

procedure Test4(AFoo: TFoo);
begin
  if AFoo.C <> 1 then
    halt(100);
  if AFoo.P3 <> 7 then
    halt(101);
end;

end.