{ %OPT= -Un }

{ Old file: tbs0176.pp }
{ unit.symbol not allowed for implementation vars         OK 0.99.9 (PM) }

{ no unit name checking !! }
unit tb150_wrong;
interface

var
  l1 : longint;

implementation

var
  l2 : longint;

begin
  tb150_wrong.l1:=1;
  tb150_wrong.l2:=1;
end.
