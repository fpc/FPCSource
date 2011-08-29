{ Test for bug report 20093
  Reported 2011-08-29
  a_tclass in unit uvmt
  and tclass in unit uvmt_a
  both generate the same symbol name for the VMT
  }

uses
  uvmt, uvmt_a;

var
  a1 : tclass;
  a2 : a_tclass;
begin
  a1 := tclass.create;
  a2 := a_tclass.create;
  a1.destroy;
  a2.destroy;
end.
