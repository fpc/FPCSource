{$ifdef fpc}{$mode objfpc}{$endif}
unit ub0115;
interface

type
  tobj = class
  private
      procedure proc1 (a: integer);overload; virtual;
  end;

implementation

procedure tobj.proc1 (a: integer);
begin
end;

end.
