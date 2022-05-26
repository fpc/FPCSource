{ %FAIL }
{ %TARGET=darwin,iphonesim,ios}

program tblock8;

{$mode objfpc}
{$modeswitch cblocks}
{$modeswitch functionreferences-}

type
  TBlock = reference to procedure(aArg: LongInt); cdecl;

begin
end.
