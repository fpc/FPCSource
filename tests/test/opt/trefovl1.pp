{ %OPT=-O2 }
{ tests that pairs of memory moves are not merged by the x86 peephole optimizer
  if a pointer source might overlap with a global destination }
program trefovl1;

type
  PInt64Arr = ^TInt64Arr;
  TInt64Arr = array[0..1] of int64;

var
  G: array[0..3] of int64;

procedure CopyUp(src: PInt64Arr);
  begin
    G[1]:=src^[0];
    G[2]:=src^[1];
  end;

procedure CopyDown(src: PInt64Arr);
  begin
    G[2]:=src^[1];
    G[1]:=src^[0];
  end;

procedure Init;
  begin
    G[0]:=1;
    G[1]:=2;
    G[2]:=3;
    G[3]:=4;
  end;

begin
  Init;
  { src^[1] is G[1], which is overwritten by the first assignment }
  CopyUp(PInt64Arr(@G[0]));
  if (G[0]<>1) or (G[1]<>1) or (G[2]<>1) or (G[3]<>4) then
    halt(1);

  Init;
  { src^[0] is G[2], which is overwritten by the first assignment }
  CopyDown(PInt64Arr(@G[2]));
  if (G[0]<>1) or (G[1]<>4) or (G[2]<>4) or (G[3]<>4) then
    halt(2);

  writeln('ok');
end.
