{ %NORUN }

{ test that "type helper" is parsed as "type alias" + "type name" if modeswitch typehelpers is not set }

program tthlp20;

type
  helper = LongInt;

  TTest = type helper;

begin

end.
