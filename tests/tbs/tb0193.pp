{ Old file: tbs0227.pp }
{ external var does strange things when declared in localsymtable OK 0.99.11 (PFV) }

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

