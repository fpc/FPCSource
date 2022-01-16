{ %FAIL }

program tw37272b;

{ note: there is a tw37272a in webtbs }

{$mode objfpc}

type
  TA1 = array of integer;

procedure Test(A: integer; const B: TA1 = [1]);
begin end;

begin
  Test(1, []);
  Test(1);
end.
