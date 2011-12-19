unit uvmta;

interface

var
  a_int : longint;

const
  a_test_count : longint = 0;

procedure a_test;

implementation

procedure a_test;
begin
  Writeln('Procedure a_test in uvmt unit');
  inc(a_test_count);
end;


end.
