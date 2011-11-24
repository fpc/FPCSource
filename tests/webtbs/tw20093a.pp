{ Test for bug report 20093
  Reported 2011-08-29
  a_tclass in unit uvmt
  and tclass in unit uvmt_a
  both generate the same symbol name for the VMT
  }

{ Use same name as unit to test also
  possible confusion there }


{$mode objfpc}

program vmt_uvmt;

uses
  u_uvmta, uvmta, uvmta_a;

var
  t : longint;
begin
  t:=6;
  inc(t);
  uvmta.a_int:=t;
  inc(t);

  uvmta_a.int:=t;

  if (uvmta_a.int - uvmta.a_int <> 1) then
    begin
      Writeln('Error in generated executable');
      if (@int = @a_int) then
        Writeln('Both variables are at same address');
      halt(1);
    end;
  test;
  a_test;
  u_uvmta.a_int;
  if (test_count <> 1) or
     (u_test_count <> 1) or
     (a_test_count <> 1) then
    begin
      Writeln('Wrong code generated');
      halt(2);
    end;
end.
