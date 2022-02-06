{ %FAIL }
{ %TARGET=darwin,iphonesim,ios }

{ a C-block may not reference itself }

program tblock5;

{$mode objfpc}
{$modeswitch cblocks}
{$modeswitch functionreferences}

type
  TBlock = reference to function(l: TBlock): LongInt; cdecl; cblock;

begin
end.
