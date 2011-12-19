unit u_uvmta;

interface

const
  u_test_count : longint = 0;

procedure a_int;

implementation

procedure a_int;
begin
  Writeln('Procedure a_int in u_uvmta unit');
  inc(u_test_count);
end;


end.
