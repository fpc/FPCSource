{ %opt=-dtest\$FPCCPU -dtest\$FPCOS -dtest\$FPCCPUtest -dtest\$FPCOStest }

begin
{ test the most frequent combinations }
{$ifdef CPUX86_64}
{$ifdef testx86_64}
  writeln('ok 1');
{$else testx86_64}
  halt(1);
{$endif testx86_64}
{$ifdef testx86_64test}
  writeln('ok 2');
{$else testx86_64test}
  halt(1);
{$endif testx86_64test}
{$endif CPUX86_64}

{$ifdef CPUAARCH64}
{$ifdef testaarch64}
  writeln('ok 1');
{$else testaarch64}
  halt(1);
{$endif testaarch64}
{$ifdef testaarch64test}
  writeln('ok 2');
{$else testaarch64test}
  halt(1);
{$endif testaarch64test}
{$endif CPUAARCH64}

{$ifdef LINUX}
{$ifdef testlinux}
  writeln('ok 1');
{$else testlinux}
  halt(1);
{$endif testlinux}
{$ifdef testlinuxtest}
  writeln('ok 2');
{$else testlinuxtest}
  halt(1);
{$endif testlinuxtest}
{$endif LINUX}
end.
