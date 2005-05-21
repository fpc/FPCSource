unit uprocext2;
interface
uses uprocext1;

procedure proc2;

implementation

procedure proc3;[public,alias:'ExternalProc3'];
begin
  writeln('proc3');
  err:=false;
end;

procedure proc2;
begin
  { call proc1 so it already needs the mangledname }
  proc1;
end;

end.
