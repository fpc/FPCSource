var
  a : array[0..3] of pointer;
  i,j : longint;
  HeapStatus : THeapStatus;
  oldmemsize: ptruint;
begin
  randomize;  
{$if not(defined(CPU8)) and not(defined(CPU16))}
  for i:=1 to 12 do
    begin
      j:=random(length(a));
      if not(assigned(a[j])) then
        getmem(a[j],1024*1024)
      else
        begin
          oldmemsize:=MemSize(a[j]);
          reallocmem(a[j],oldmemsize*11 div 10);
          if pbyte(a[j])^<>123 then
            halt(1);
          if (pbyte(a[j])+oldmemsize-1)^<>231 then
            halt(2);
        end;
      pbyte(a[j])^:=123;
      pbyte(a[j]+memsize(a[j])-1)^:=231;
    end;
  for i:=0 to high(a) do
    freemem(a[i]);
  HeapStatus:=GetHeapStatus;
  with HeapStatus do
    begin
      writeln('TotalAllocated: ',TotalAllocated);
      writeln('TotalFree: ',TotalFree);
    end;      
{$endif not(defined(CPU8)) and not(defined(CPU16))}
end.
