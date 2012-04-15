{ OPT=-gw }

unit uvmt_a;

{$mode objfpc}

interface

{$ifndef VAR_ONLY}
type
  tclass = class (tobject)
    x : integer;
  end;
{$endif ndef VAR_ONLY}
var
  int : longint;

implementation

end.
