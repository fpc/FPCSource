{ %NORUN }

{ a function reference can be cast to IUnknown in non-Delphi modes to call its
  management functions }
program tfuncref21;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TProc = reference to procedure(aArg: LongInt);
  TProc2 = reference to procedure;

var
  p: TProc;
  p2: TProc2;
begin
  IUnknown(p)._AddRef;
  IUnknown(p2)._AddRef;
end.
