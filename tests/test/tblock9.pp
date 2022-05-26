{ %FAIL }
{ %TARGET=darwin,iphonesim,ios}

program tblock9;

{$mode objfpc}
{$modeswitch cblocks}
{$modeswitch functionreferences-}

var
  block: reference to procedure(aArg: LongInt); cdecl;

begin
end.
