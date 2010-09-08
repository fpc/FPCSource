var
  P1, P2: PCardinal;
  I: Integer;
begin

  // random pointer
  P1 := @I;
  P2 := P1;

  Inc(P1, 97);
  Inc(P1, -97);

  if (P1 <> P2) then
    halt(1);
end.
