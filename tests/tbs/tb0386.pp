{ %version=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}
uses ub0386;
type
  tobj2 = class (tobj1)
      { this will try to override tobj.proc1, it should not
        see tobj1.proc1 }
      procedure proc1 (a: integer);override;
  end;

procedure tobj2.proc1 (a: integer);
begin
end;

begin
end.
