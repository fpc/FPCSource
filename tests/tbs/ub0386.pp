{$ifdef fpc}{$mode objfpc}{$endif}
unit ub0386;
interface

type
  tobj = class
      procedure proc1 (a: integer);overload; virtual;
  end;

  tobj1 = class(tobj)
  { this proc1 definition should not been seen by tobj2 }
  private
      procedure proc1 (a: char);
  end;

implementation

procedure tobj.proc1 (a: integer);
begin
end;

procedure tobj1.proc1 (a: char);
begin
end;

end.
