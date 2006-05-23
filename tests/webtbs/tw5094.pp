{ %OPT=-O-1 -Ooasmcse -Oonoregvar }

function B(n, k: Integer):Integer;
var
  i: Integer;
begin
  if k > n-k then
    k := n-k;
  B := 1;
  for i := n-k+1 to n do
    B := B * i;
  for i := 2 to k do
    B := B div i;
end;
begin
  if B(0,1) <> 1 then
    halt(1); { Should write 1; fpc -O1 binom.pas writes 0 }
end.


