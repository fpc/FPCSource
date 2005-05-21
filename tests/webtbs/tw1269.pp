{ Source provided for Free Pascal Bug Report 1269 }
{ Submitted by "Rob Kolstad" on  2000-11-28 }
{ e-mail: kolstad@ace.delos.com }

{$ifndef cpui386}
  {$define COMP_IS_INT64}
{$endif cpui386}

var
  A : array [0..25, 0..100] of comp;
  V : array [1..25] of longint;
  vt, nn : longint;
  i, j : longint;

function calc(m : longint; n : longint) : comp;
var i : longint;
begin
   writeln(m,' ',n, ' ', a[m,n]);
   if A[m, n] <> -1 then begin
       calc := A[m, n]
   end else begin
      if n = 0 then begin
         A[m, n] := 1;
         calc := 1;
         exit;
      end;
      A[m, n] := 0;
      for i := m downto 1 do
         if n - V[i] >= 0 then
            A[m, n] := A[m, n] + calc(i, n - V[i]);
      calc := A[m, n];
   end;
end;

begin
  vt := 10;
  nn := 100;
  v[1] := 1; v[2] := 2; v[3] := 3;
  v[4] := 4; v[5] := 5; v[6] := 6;
  v[7] := 7; v[8] := 8; v[9] := 9;
  v[10] := 10;
  for i := 0 to 25 do
    for j := 0 to nn do A[i, j] := -1;
{$ifdef COMP_IS_INT64}
  writeln( calc(vt, nn));
{$else not COMP_IS_INT64}
  writeln( calc(vt, nn) :0:0);
{$endif COMP_IS_INT64}
end.
