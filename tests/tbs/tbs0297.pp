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
