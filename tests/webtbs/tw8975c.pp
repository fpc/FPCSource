{ %opt=-CRriot -O-2 -Ooregvar }
{ %result=201 }

{Internal FPC2.1.4 error, compile with fpc -B -dDebug -O3}
procedure bug(var b: array of longint);
var
  l: longint;

  procedure intern;
  begin
    if (b[l] <> 1) then {Fatal: Internal error 200409241}
      halt(1);
  end;
begin
  l:=-1;
  intern;
end;

const
  a: array[1..3] of longint = (1,2,3);
begin
  bug(a);
end.


