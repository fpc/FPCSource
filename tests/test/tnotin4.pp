{ modeswitch delphislang, and thus "not in", is enabled by mode delphi }
program tnotin4;

{$mode delphi}

var
  B: byte;
begin
  B:=1;
  if B not in [0..5] then
    halt(1);
  if not (B not in [2,3]) then
    halt(2);
  writeln('ok');
end.
