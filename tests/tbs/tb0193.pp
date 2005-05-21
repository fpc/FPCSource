{ Old file: tbs0227.pp }
{ external var does strange things when declared in localsymtable OK 0.99.11 (PFV) }

var
  stacksize : ptrint;external name '__stklen';

function getstacksize:longint;assembler;
asm
{$ifdef CPUI386}
        movl    stacksize,%eax
end ['EAX'];
{$endif CPUI386}
{$ifdef CPUX86_64}
        movl    stacksize,%eax
end ['EAX'];
{$endif CPUX86_64}
{$ifdef CPU68K}
        move.l    stacksize,d0
end ['D0'];
{$endif CPU68K}
{$ifdef cpupowerpc}
{$ifndef macos}
       lis r3, stacksize@ha
       lwz r3, stacksize@l(r3)
{$else macos}
       lwz r3, stacksize(r2)
       lwz r3, 0(r3)
{$endif macos}
end;
{$endif cpupowerpc}
{$ifdef cpusparc}
       sethi   %hi(stacksize),%i0
       or      %i0,%lo(stacksize),%i0
end;
{$endif cpusparc}
begin
  writeln(getstacksize);
end.
