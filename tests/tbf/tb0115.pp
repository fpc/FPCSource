{ %FAIL }

{$ifdef fpc}{$mode objfpc}{$endif}
uses ub0115;
type
  tobj2 = class (tobj)
      { this will try to override tobj.proc1 which is private
        and can not be seen. This needs an error }
      procedure proc1 (a: integer);override;
  end;

procedure tobj2.proc1 (a: integer);
begin
end;

begin
end.
