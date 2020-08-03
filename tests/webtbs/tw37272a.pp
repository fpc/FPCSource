program tw37272a;

{ note: there is a tw37272b in webtbf }

{$mode objfpc}

type
  TA1 = array of integer;

procedure Test(A: integer; const B: TA1 = []);
begin end;

begin
  Test(1, []);
  Test(1);
end.
