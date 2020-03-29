{ %OPT=-FNudots -FNudots.moredots -Un }

{$UnitPath ./dots}

program tudots2;

uses
  unit1, // picks up UDots.Unit1 in udots.unit1.pp
  unit2, // picks up Alpha.Beta in udots.unit2.pp
  unit3, // picks up Unit3 in unit3.pp (instead of also existing udots.unit3.pp)
  unit4, // picks up UDots.Unit4 in udots.unit4.pp (instead of also existing udots.udots.unit4.pp)
  udots.unit5, // picks up UDots.UDots.Unit5 in udots.udots.unit5.pp
  unit6, // picks up Unit6 in udots/unit6.pp (instead of also existing udots.unit6.pp)
  unit7, // picks up UDots.MoreDots.Unit7 in udots.moredots.unit7.pp
  unit8; // picks up UDots.Unit8 in udots.unit8.pp (instead of also existing udots.moredots.unit8.pp)

begin
  if Unit1Str <> 'UDots.Unit1' then
    Halt(1);
  if Unit2Str <> 'Alpha.Beta' then
    Halt(2);
  if Unit3Str <> 'Unit3' then
    Halt(3);
  if Unit4Str <> 'UDots.Unit4' then
    Halt(4);
  if Unit5Str <> 'UDots.UDots.Unit5' then
    Halt(5);
  if Unit6Str <> 'Unit6' then
    Halt(6);
  if Unit7Str <> 'UDots.MoreDots.Unit7' then
    Halt(7);
  if Unit8Str <> 'UDots.Unit8' then
    Halt(8);

  { check whether the sub symbols are available as well }
  if unit1.Unit1Str <> 'UDots.Unit1' then
    Halt(20);
  if udots.unit1.Unit1Str <> 'UDots.Unit1' then
    Halt(21);
  if unit2.Unit2Str <> 'Alpha.Beta' then
    Halt(22);
  if udots.unit2.Unit2Str <> 'Alpha.Beta' then
    Halt(23);
  if unit3.Unit3Str <> 'Unit3' then
    Halt(24);
  if unit4.Unit4Str <> 'UDots.Unit4' then
    Halt(25);
  if udots.unit4.Unit4Str <> 'UDots.Unit4' then
    Halt(26);
  { Note: unit5.Unit5Str would be illegal }
  if udots.unit5.Unit5Str <> 'UDots.UDots.Unit5' then
    Halt(27);
  if udots.udots.unit5.Unit5Str <> 'UDots.UDots.Unit5' then
    Halt(28);
  if unit6.Unit6Str <> 'Unit6' then
    Halt(29);
  if unit7.Unit7Str <> 'UDots.MoreDots.Unit7' then
    Halt(30);
  if moredots.unit7.Unit7Str <> 'UDots.MoreDots.Unit7' then
    Halt(31);
  if udots.moredots.unit7.Unit7Str <> 'UDots.MoreDots.Unit7' then
    Halt(32);
  if unit8.Unit8Str <> 'UDots.Unit8' then
    Halt(33);
  if udots.unit8.Unit8Str <> 'UDots.Unit8' then
    Halt(34);

  Writeln('ok');
end.
