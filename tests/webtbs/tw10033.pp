{$apptype console}
type Txxx = set of (one,two,three);

var x : txxx;

begin
  writeln(low(x));
  writeln(high(x));
end.
