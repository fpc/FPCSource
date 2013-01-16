{ %FAIL }

program tgenconstraint4;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest1LongInt = TTest1<LongInt>;

begin

end.
