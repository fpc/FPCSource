{ %OPT=-OG2p3}

procedure t;
var
  a: array[1..10,1..10] of string[31];
  i, j: longint;
  c: char;

begin
  i := 5;
  j := 7;
  a[i,j] := '123456789';
  c := '0';
{ clear the optimizer state }
  asm
  end;
  a[i,j] := a[i,j] + c;
  if a[i,j] <> '1234567890' then
    begin
      writeln('error!');
      halt(1)
    end;
end;

begin
  t;
end.
