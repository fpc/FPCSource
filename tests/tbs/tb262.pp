{ Old file: tbs0297.pp }
{ calling of interrupt procedure allowed but wrong code generated OK 0.99.13 (PM) }

program test_int;

{$ifdef go32v2}
  uses
    dpmiexcp;
{$endif go32v2}

procedure int;interrupt;
begin
end;

begin
  int;
end.
