{ %FAIL }
{ Old file: tbf0298.pp }
{ l1+l2:=l1+l2 gives no error                          OK 0.99.13 (PFV) }

program test_loc_mem;

{$ifdef go32v2}
  uses
    dpmiexcp;
{$endif go32v2}

var l1,l2 : longint;
begin
  l1+l2:=l1+l2;
end.
