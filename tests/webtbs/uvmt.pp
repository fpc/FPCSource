{ OPT=-gw }

unit uvmt;

{$mode objfpc}

interface

{$ifndef VAR_ONLY}
type
  a_tclass = class (tobject)
    x : integer;
  end;
{$endif ndef VAR_ONLY}

var
  a_int : longint;

implementation

end.
