{ %NORUN }
program tb0709;

{$mode objfpc}
{ ensure that function reference are disabled }
{$modeswitch functionreferences-}

type
  Reference = LongInt;

  TTest = type Reference;

begin

end.
