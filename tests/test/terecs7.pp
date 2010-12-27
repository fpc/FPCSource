{ %fail }
program terecs7;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

type
  TBar = record
  end;

  TFoo = record
    class operator GreaterThan(i, j: TBar): Boolean;
  end;

{ TFoo }

class operator TFoo.GreaterThan(i, j: TBar): Boolean;
begin

end;

begin
end.

