{ %FAIL }

{$MODE DELPHI}

type
  TFoo = class
    class procedure F; static; virtual;
  end;

class procedure TFoo.F;
begin
end;

begin
end.