{ %version=1.1}
{$mode objfpc}
program testa;

Type
  TA = array of Integer;

var
  A,B : TA;
  I,J : Integer;
begin
  Setlength(A,10);
  For I:=0 to 9 do
      A[I]:=I;
  B:=Copy(A,3,6);
  if High(B)<>5 then
    begin
      writeln('Error 1');
      halt(1);
    end;
  For I:=0 to High(B) do
    if b[i]<>i+3 then
      begin
        writeln('Error 2');
        halt(1);
      end;
end.
