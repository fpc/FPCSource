{ %FAIL }

program tb0297;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TTestProc = procedure;
  TTestProcRef = reference to TTestProc;

begin

end.
