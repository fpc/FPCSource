{ Source provided for Free Pascal Bug Report 3572 }
{ Submitted by "Lukas Gebauer" on  2005-01-18 }
{ e-mail: gebauerl@mlp.cz }
{$MODE DELPHI}
program rtest;

function x(value: string): string;
begin
  result := Value;
  result := result;
end;

var
  s: string;
begin
  s := 'ahoy!';
  writeln(x(s));
  if x(s)<>s then
    halt(1);
end.
