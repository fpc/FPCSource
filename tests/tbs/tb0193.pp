{ %skiptarget=win32,win64,android }
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
{$define implemented}
{$endif CPUI386}
{$ifdef CPUX86_64}
        movq    stacksize@GOTPCREL(%rip),%rax
        movq    (%rax),%rax
end ['EAX'];
{$define implemented}
{$endif CPUX86_64}
{$ifdef CPU68K}
        move.l    stacksize,d0
end ['D0'];
{$define implemented}
{$endif CPU68K}
{$ifdef cpupowerpc}
{$if not defined(macos) and not defined(aix)}
       lis r3, stacksize@ha
       lwz r3, stacksize@l(r3)
{$else macos}
       lwz r3, stacksize(r2)
       lwz r3, 0(r3)
{$endif macos}
end;
{$define implemented}
{$endif cpupowerpc}
{$ifdef cpusparc}
       sethi   %hi(stacksize),%i0
       or      %i0,%lo(stacksize),%i0
end;
{$define implemented}
{$endif cpusparc}
{$ifdef cpuarm}
       ldr r0,.Lpstacksize
       ldr r0,[r0]
       b .Lend
.Lpstacksize:
       .long stacksize
.Lend:
end;
{$define implemented}
{$endif cpuarm}
{$ifdef cpumips}
  la $v0,stacksize
  lw $v0,($v0)
 end;
{$define implemented}
{$endif cpumips}
{$ifdef cpuaarch64}
  adrp x0,stacksize@PAGE
  ldr  x0,[x0,stacksize@PAGEOFF]
end;
{$define implemented}   
{$endif cpuaarch64}
{$ifndef implemented}
 {$error This test does not supported this CPU}
end;
{$endif}

begin
  writeln(getstacksize);
end.
