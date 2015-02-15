unit
  uw27294;

interface

procedure global;

implementation

var
  p : procedure;

procedure test;

begin
  p:=@test;
  writeln('OK');
end;

procedure global;
begin
  p:=nil;
  test;
  p();
end;

end.

