{ %opt=-CRriot -O-2 -Ooregvar }
{ %fail }

{Internal FPC2.1.4 error, compile with fpc -B -dDebug -O3}
procedure bug(var b: array of longint);

  procedure intern;
  begin
    if (b[low(b)-1] <> 1) then {Fatal: Internal error 200409241}
      halt(1);
  end;
begin
  intern;
end;

const
  a: array[1..3] of longint = (1,2,3);
begin
  bug(a);
end.


