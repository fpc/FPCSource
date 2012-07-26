{ %FAIL }
program toperator29;

type
  TTest = (One, Two, Three);
  TTests = set of TTest;

operator < (left: TTests; right: TTests) res : Boolean;
begin

end;

begin

end.
