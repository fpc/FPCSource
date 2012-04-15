{ Test for bug report 20093
  Reported 2011-08-29
  a_tclass in unit uvmt
  and tclass in unit uvmt_a
  both generate the same symbol name for the VMT
  }

{ Use same name as unit to test also
  possible confusion there }

{ Test should not fail anymore after this change
  dated 2011-08-31 PM }

{$mode objfpc}

program vmt_uvmt;

uses
  uvmt, uvmt_a;

{$ifndef VAR_ONLY}
type
  tclass = class(tobject)
  end;
  a_tclass = class(tobject)
  end;

var
  a1 : uvmt_a.tclass;
  a2 : uvmt.a_tclass;
  a3 : tclass;
  a4 : a_tclass;
{$endif ndef VAR_ONLY}
var
  t : longint;
begin
  t:=6;
  inc(t);
  uvmt.a_int:=t;
  inc(t);
  uvmt_a.int:=t;
  if (uvmt_a.int - uvmt.a_int <> 1) then
    begin
      Writeln('Error in generated executable');
      if (@int = @a_int) then
        Writeln('Both variables are at same address');
      halt(1);
    end;
{$ifndef VAR_ONLY}
  a1 := uvmt_a.tclass.create;
  a2 := uvmt.a_tclass.create;
  a3 := tclass.create;
  a4 := a_tclass.create;
  a1.destroy;
  a2.destroy;
  a3.destroy;
  a4.destroy;
{$endif ndef VAR_ONLY}
end.
