{ Old file: tbs0154.pp }
{ Subrange types give type mismatch when assigning to   OK 0.99.7 (PFV) }

type
  week=(mon,tue,wed);
Var
    w : week;
    w1 : mon..tue;
begin
  w1:=w;
end.
