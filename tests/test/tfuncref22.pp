{ %FAIL }

{ a function reference can't be cast to IUnknown in mode Delphi to call its
  management functions }
program tfuncref22;

{$mode delphi}
{$modeswitch functionreferences}

type
  TProc = reference to procedure(aArg: LongInt);

var
  p: TProc;
begin
  IUnknown(p)._AddRef;
end.
