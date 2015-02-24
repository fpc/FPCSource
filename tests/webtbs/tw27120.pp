{ %NORUN }

program tw27120;

{$mode objfpc}

type
  TFoo = class
  end;

  TBar = class helper for TFoo
  private class var
    FFoo: TFoo;
  end;

begin

end.
