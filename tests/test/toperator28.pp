{ %FAIL }
program toperator28;

type
  TTest = (One, Two, Three);
  TTests = set of TTest;

operator >< (left: TTests; right: TTests) res : TTests;
begin

end;

begin

end.
