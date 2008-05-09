unit uw11182;

{$inline on}

interface

procedure test(k: longint); inline;

implementation

function f(a: longint): longint; inline;
begin
  f:=a+a;
end;

procedure test(k: longint);
var
  i: longint;
begin
  for i := f(k) to f(k+5) do
    writeln(i);
end;


end.
