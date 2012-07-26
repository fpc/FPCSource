{ %FAIL }
program toperator35;

type
  TTest = (One, Two, Three);
  TTests = set of TTest;

operator in (left: TTest; right: TTests) res : Boolean;
begin

end;

begin

end.
