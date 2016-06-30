{ %opt=-CRriot -O-2 -Ooregvar }

{Internal FPC2.1.4 error, compile with fpc -B -dDebug -O3}
procedure bug(var b: array of longint);
var
  l: longint;

  procedure intern;
  begin
    if (b[l] <> 1) then {Fatal: Internal error 200409241}
      halt(1);
    inc(b[l]);
    if (b[l] <> 2) then {Fatal: Internal error 200409241}
      halt(2);

    if (b[l+1] <> 2) then {Fatal: Internal error 200409241}
      halt(3);
    if (b[l+2] <> 3) then {Fatal: Internal error 200409241}
      halt(4);
    if (b[low(b)] <> 2) then {Fatal: Internal error 200409241}
      halt(5);
    if (b[low(b)+1] <> 2) then {Fatal: Internal error 200409241}
      halt(6);
    if (b[low(b)+2] <> 3) then {Fatal: Internal error 200409241}
      halt(7);
  end;
begin
  l:=0;
  intern;
end;

const
  a: array[1..3] of longint = (1,2,3);
begin
  bug(a);
end.


