{ %result=204 }
{ FreeMemSize with a size larger than the block raises runtime error 204 with the cmem memory manager }
program tcmemsize2;

uses
  cmem;

var
  lP: Pointer;

begin
  GetMem(lP,100);
  FreeMem(lP,MemSize(lP)+1);
  Writeln('FreeMemSize with a size larger than the block did not fail');
end.
