{ %FAIL }
{ Old file: tbf0298.pp }
{ l1+l2:=l1+l2 gives no error                          OK 0.99.13 (PFV) }

program test_loc_mem;

var l1,l2 : longint;
begin
  l1+l2:=l1+l2;
end.
