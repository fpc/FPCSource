{ %FAIL }

program tgenconstraint12;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest4ITest1 = TTest4<ITest1>;

begin

end.
