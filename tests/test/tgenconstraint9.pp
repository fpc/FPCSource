{ %FAIL }

program tgenconstraint9;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest3ITest1 = TTest3<ITest1>;

begin

end.
