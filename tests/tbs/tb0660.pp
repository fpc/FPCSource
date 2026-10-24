{$mode objfpc}
{$inline on}
{$R-}
{$Q-}

function f : longint;inline;
  begin
    result:=result*result;
  end;

begin
  writeln(f);
end.
