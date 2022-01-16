{ %OPT=-FNudots.moredots -FNudots }

program tudots5;

uses
  unit8; // picks up UDots.MoreDots.Unit8 in udots.moredots.unit8.pp (instead of also existing udots.unit8.pp)

begin
  if Unit8Str <> 'UDots.MoreDots.Unit8' then
    Halt(1);
  if Unit8.Unit8Str <> 'UDots.MoreDots.Unit8' then
    Halt(2);
  if MoreDots.Unit8.Unit8Str <> 'UDots.MoreDots.Unit8' then
    Halt(3);
  if UDots.MoreDots.Unit8.Unit8Str <> 'UDots.MoreDots.Unit8' then
    Halt(4);

  Writeln('ok');
end.
