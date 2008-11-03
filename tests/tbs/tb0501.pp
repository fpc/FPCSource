{ %OPT=-Sen -vn }

{$mode objfpc}

type
  tc = class
    procedure t;
    private
      fleft: tc;
  end;

procedure tc.t;
var
  oldroot: tc;
begin
  fleft := nil;
  oldroot := nil;
  oldroot.fleft := nil;
end;

begin
end.
