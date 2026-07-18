program tw41270;
{$mode objfpc}

{$OPTIMIZATION OFF}
{$OPTIMIZATION REGVAR}

var
  arr: array [0..3] of Double;

procedure mdtgen(n: Integer; var alu: Double);
var
  nsr, ind, i, j: Integer;
  sumrowi, h, s: Double;
  palu, t, pp: PDouble;
  singular : Boolean;
begin
  palu := @alu;
  pp := @alu;
  nsr := n*sizeof(Double);
  getmem(t, nsr);
  for i:=1 to n do
  begin
    for j:=1 to n do;
    ind:=1;
    h := random;
    singular := false;
    sumrowi := palu[ind];
  end;

  s:=0.0;
  i:=0;
  h:=s+palu[i+i+0]; // <- ACCESS VIOLATION:  addsd xmm0,[rax*8+rax]
  freemem(t, nsr)
end;

begin
  mdtgen(2, arr[0]);
  WriteLn('ok');
end.
