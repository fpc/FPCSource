{$mode objfpc}
{$modeswitch nestedprocvars}

type
  tnestedfunc = function (i: longint): longint is nested;

function test: longint;
var
  i: longint;

  function func3(aa: longint): longint;
  begin
    result:=i+aa;
  end;

  function func(aa: integer): integer;
  var
    nf: tnestedfunc;
  begin
    nf:=@func3;
    result:=nf(aa);
  end;

begin
  i:=100;
  result:=func(10);
end;

begin
  if test <> 110 then
    halt(1);
  writeln('OK');
end.
