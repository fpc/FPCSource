{ %FAIL }

{$MODE DELPHI}

type
  TFoo = class
    class procedure F; virtual; static;
  end;

class procedure TFoo.F;
begin
end;

begin
end.