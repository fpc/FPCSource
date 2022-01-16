{$mode delphi}
{$modeswitch nestedprocvars}

type
  tfunc = function (a1,a2,a3,a4,a5,a6,a7,a8,a9: longint): longint;

procedure proc;

  function nested(a1,a2,a3,a4,a5,a6,a7,a8,a9: longint): longint;
  begin
    result:=a1+a2+a8+a9;
  end;

var
  n: tfunc;
  i: longint;
begin
  i:=nested(1,2,3,4,5,6,7,8,9);
  writeln(i);
  if i<>20 then
    begin
      writeln('Invalid result.');
      halt(1);
    end;

  n:=@nested;
  i:=n(1,2,3,4,5,6,7,8,9);
  writeln(i);
  if i<>20 then
    begin
      writeln('Invalid result.');
      halt(2);
    end;
end;

begin
  proc;
  writeln('OK');
end.
