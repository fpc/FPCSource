{$ifdef fpc}{$mode objfpc}{$endif}
type
  tobj = class
      procedure proc1 (a: integer);virtual;
  end;

  tobj1 = class (tobj)
      procedure proc1 (a: char);overload;
  end;

  tobj2 = class (tobj1)
      { this will try to override tobj1.proc1 which is not
        allowed and therefor needs an error }
      procedure proc1 (a: integer);override;
  end;

procedure tobj.proc1 (a: integer);
begin
end;

procedure tobj1.proc1 (a: char);
begin
end;

procedure tobj2.proc1 (a: integer);
begin
end;

begin
end.
