{ %NORUN }

{ class helpers of a parent are available in a subclass as well }
program tchlp87;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TBar = class(TFoo)

  end;

  TFooHelper = class helper for TFoo
    procedure TestFoo;
  end;

procedure TFooHelper.TestFoo;
begin

end;

var
  b: TBar;
begin
  b.TestFoo;
end.
