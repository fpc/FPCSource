{ %FAIL }

program tgenconstraint21;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest8ITest1 = TTest8<ITest1>;

begin

end.
