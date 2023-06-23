{ %FAIL }

program tthlp31;

{$mode objfpc}
{$modeswitch typehelpers}

type
  Test = type LongInt;

  TTestHelper = type helper for Test
  end;

  TLongIntHelper = type helper(TTestHelper) for LongInt
  end;

begin

end.
