{ %FAIL }

{ inherited record helpers must extend the same record }
program trhlp26;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}

type
  TTest1 = record

  end;

  TTest2 = record

  end;

  TTest1Helper = record helper for TTest1
  end;

  TTest2Helper = record helpen(TTest1Helper) for TTest2
  end;

begin
end.
