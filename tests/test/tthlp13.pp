{ %FAIL }

{ this tests that the same type must be extended by the child helper }

program tthlp13;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TLongIntHelper = type helper for LongInt
  end;

  TByteHelper = type helper(TLongIntHelper) for Byte
  end;

begin

end.
