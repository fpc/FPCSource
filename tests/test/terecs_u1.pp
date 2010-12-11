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
    class function Test1(n: TBar): TBar;
    class constructor Create;
    class destructor Destroy;
  end;

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

end.