{ %NORUN }

{ a class helper can already be accessed when implementing a class' methods }
program tchlp57;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class
    procedure Test;
  end;

  TFooHelper = class helper for TFoo
    procedure Bar;
  end;

procedure TFoo.Test;
begin
  Bar;
end;

procedure TFooHelper.Bar;
begin

end;

begin
end.
