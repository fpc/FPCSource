function getheapsize:longint;assembler;
var
  heapsize : longint;external name 'HEAPSIZE';
  sbrk : longint;external name '___sbrk';
asm
        movl    HEAPSIZE,%eax
end ['EAX'];

begin
  writeln(getheapsize);
end.

