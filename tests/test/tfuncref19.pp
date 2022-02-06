{ %FAIL }

{ it's not allowed to call IUnknown methods on a function reference directly }
program tfuncref19;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TProc = reference to procedure(aArg: LongInt);

var
  p: TProc;
begin
  p._AddRef;
end.
