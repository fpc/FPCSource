{$mode delphi}
{$q+}

function F(N: integer) : integer;
var NN : integer;
begin
    NN := N;
    if NN < 0 then NN := 0 - NN;
    result := NN;
end;

procedure Crash; cdecl;
var
    N, M : integer;
begin
    N := -10;
    M := F(N) div 4;
end;

begin
  Crash;
end.

