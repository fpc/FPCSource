function getheapsize:longint;assembler;
var
  heapsize : longint;external name 'HEAPSIZE';
asm
        movl    HEAPSIZE,%eax
end ['EAX'];

begin
  writeln(getheapsize);
end.

