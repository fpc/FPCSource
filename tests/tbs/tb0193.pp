{ Old file: tbs0227.pp }
{ external var does strange things when declared in localsymtable OK 0.99.11 (PFV) }

function getheapsize:longint;assembler;
var
  heapsize : longint;external name 'HEAPSIZE';
//  sbrk : longint;external name '___sbrk';
asm
{$ifdef CPUI386}
        movl    HEAPSIZE,%eax
end ['EAX'];
{$endif CPUI386}
{$ifdef CPU68K}
        move.l    HEAPSIZE,d0
end ['D0'];
{$endif CPU68K}


begin
  writeln(getheapsize);
end.

