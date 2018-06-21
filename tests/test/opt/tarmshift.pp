{ %opt=-O2 }

{ Explicitly disable overflow and range checks }
{$Q-}
{$R-}

var
  i : longint;

begin
  i:=1234;
  i:=i shl 23;
  i:=i shl 23;
  if i<>0 then
    halt(1);
end.
