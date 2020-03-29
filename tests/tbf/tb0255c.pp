{ %FAIL }

{$MODE DELPHI}

type
  TFoo = class
    class procedure F(V: byte); virtual;
    class property P: byte write F;
  end;

class procedure TFoo.F(V: byte);
begin
end;

begin
end.