var
  a : array[0..3] of pointer;
  i,j : longint;
  HeapStatus : THeapStatus;
begin
  randomize;  
{$if not(defined(CPU8)) and not(defined(CPU16))}
  for i:=1 to 12 do
    begin
      j:=random(length(a));
      if not(assigned(a[j])) then
        getmem(a[j],1024*1024)
      else
        reallocmem(a[j],MemSize(a[j])*11 div 10);
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
