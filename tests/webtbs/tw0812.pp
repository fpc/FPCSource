program TestVm2;

procedure Test;
var
  P: Pointer;
begin
  P:=nil;
  ReAllocMem(P, 8);
  ReAllocMem(P, 0);
end;

var MemBefore : longint;
begin
  writeln(MemAvail);
  MemBefore:=MemAvail;
  Test;
  writeln(MemAvail);
  if MemBefore<>MemAvail then
    begin
      Writeln('ReAllocMem creates emory leaks');
      Writeln('Bug 812 is not yet fixed');
      Halt(1);
    end;
end.
