{$ifdef fpc}{$mode objfpc}{$endif}
unit ub0391;
interface
type
  tc2 = class
  protected
    procedure p1(s:string);
  end;


implementation

procedure tc2.p1(s:string);
begin
  writeln('string: ',s);
end;


end.
