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
    function Test(n: TBar): TBar;
    class function Test1(n: TBar): TBar;
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

end.

