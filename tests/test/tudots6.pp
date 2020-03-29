{ %OPT="-FNudots;udots.udots" }

program tudots6;

uses
  Unit1, { finds UDots.Unit1 in udots.unit.pp }
  Unit5; { finds UDots.UDots.Unit5 in udots.udots.unit5.pp }

begin
  if Unit1Str <> 'UDots.Unit1' then
    Halt(1);
  if Unit5Str <> 'UDots.UDots.Unit5' then
    Halt(2);

  Writeln('ok');
end.

