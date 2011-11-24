unit uvmta_a;

interface

var
  int : longint;

const
  test_count : longint = 0;


procedure test;

implementation

procedure test;
begin
  Writeln('Procedure test in uvmt_a unit');
  inc(test_count);
end;

end.
