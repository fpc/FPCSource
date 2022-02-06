{ %FAIL }
{ %TARGET=darwin,iphonesim,ios }

{ a C-block may not reference itself }

program tblock7;

{$mode objfpc}
{$modeswitch cblocks}
{$modeswitch functionreferences}

type
  TBlock = reference to function(l: LongInt): specialize TArray<TBlock>; cdecl; cblock;

begin
end.
