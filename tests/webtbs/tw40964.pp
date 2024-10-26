{$goto on}
program ie200211262;

function func(v: pointer): string; inline;
begin
  func:='';
end;

label lab;

begin
  lab:
  func(@lab); // app.lpr(11,3) Error: Internal error 200211262
end.
