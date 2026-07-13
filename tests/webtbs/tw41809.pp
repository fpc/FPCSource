{ %OPT=-O2 }

{ With a short circuit and/or chain the cse pass treats expressions of
  a right side as available in later links of the chain. That only
  holds while the chain stays short circuit: once the outer and/or is
  converted to full boolean evaluation (here after inlining foo, whose
  body stays short circuit), its right side also runs when the inner
  right side was skipped, and reusing the shared temp dereferenced a
  register that was only set on the skipped path. }

program tw41809;

{$mode objfpc}

var
  gate: integer;
  pair: record
    a, b: integer;
  end;

function foo: boolean; inline;
begin
  result := (gate <> 0) and (pair.b = 0);
end;

var
  d, a, b: integer;
  got, want: boolean;
begin
  { runtime zeros the compiler cannot fold; with the bug this crashed
    dereferencing a wild pointer on the gate = 0 path }
  gate := ParamCount;
  pair.a := ParamCount;
  pair.b := ParamCount;
  if foo and (pair.a <> 0) then
    Halt(1);

  for d := 0 to 1 do
    for a := 0 to 1 do
      for b := 0 to 1 do
        begin
          gate := d;
          pair.a := a;
          pair.b := b;
          got := foo and (pair.a <> 0);
          want := (d <> 0) and (b = 0) and (a <> 0);
          if got <> want then
            Halt(2);
        end;
  writeln('ok');
end.
