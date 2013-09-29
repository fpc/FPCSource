{ %FAIL }

program tgenconstraint5;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  ugenconstraints;

type
  TTest1TClass = TTest1<TClass>;

begin

end.
