program qwordbooltest;

{$mode objfpc}{$H+}

var
  A, B : Boolean64;
begin
  A := True;

  // here it fails: qwordbooltest.pas(12,3) Fatal: Internal error 200109227
  B := not A;

  if B then
    halt(1);

  writeln('ok');
end.
