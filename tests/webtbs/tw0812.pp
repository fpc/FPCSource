uses
  erroru;

procedure Test;
var
  P: Pointer;
begin
  P:=nil;
  ReAllocMem(P, 8);
  ReAllocMem(P, 0);
end;

var Mem : sizeint;
begin
  domem(mem);
  Test;
  if domem(mem)<>0 then
    begin
      Writeln('ReAllocMem creates emory leaks');
      Writeln('Bug 812 is not yet fixed');
      Halt(1);
    end;
end.
