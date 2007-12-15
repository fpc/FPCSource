{ %OPT=-Cg- }
{ Old file: tbs0227.pp }
{ external var does strange things when declared in localsymtable OK 0.99.11 (PFV) }

var
  stacksize : ptrint;external name '__stklen';

function getstacksize:ptrint;assembler;
asm
{$ifdef CPUI386}
        movl    stacksize,%eax
end ['EAX'];
{$endif CPUI386}
{$ifdef CPUX86_64}
        movq    stacksize@GOTPCREL(%rip),%rax
        movq    (%rax),%rax
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
{$ifdef cpuarm}
       ldr r0,.Lpstacksize
       ldr r0,[r0]
       b .Lend
.Lpstacksize:
       .long stacksize
.Lend:
end;
{$endif cpuarm}

begin
  writeln(getstacksize);
end.
