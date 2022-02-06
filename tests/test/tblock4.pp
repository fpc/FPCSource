{ %FAIL }
{ %TARGET=darwin,iphonesim,ios }

{ a C-block may not reference itself }

program tblock4;

{$mode objfpc}
{$modeswitch cblocks}

type
  TBlock = reference to function(l: longint): TBlock; cdecl; cblock;

begin
end.
