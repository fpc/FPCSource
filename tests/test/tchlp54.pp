{ tests whether the methods of a parent helper are usable in a derived helper }
program tchlp54;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TFooHelper = class helper for TFoo
    procedure Test;
  end;

  TFooBarHelper = class helper(TFooHelper) for TFoo
    procedure AccessTest;
  end;

procedure TFooHelper.Test;
begin

end;

procedure TFooBarHelper.AccessTest;
begin
  Test;
end;

begin
end.
