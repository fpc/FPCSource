{ %FAIL }

{ don't allow helper methods for constant declarations }

program tthlp19;

uses
  uthlp;

const
  TestLongInt: LongInt = 42.Test;

begin

end.
