{ %opt=-dtest\$FPCCPU -dtest\$FPCOS }

begin
{ test the most frequent combinations }
{$ifdef CPUX86_64}
{$ifdef testx86_64}
  writeln('ok');
{$else testx86_64}
  halt(1);
{$endif testx86_64}
{$endif CPUX86_64}

{$ifdef CPUAARCH64}
{$ifdef testaarch64}
  writeln('ok');
{$else testaarch64}
  halt(1);
{$endif testaarch64}
{$endif CPUAARCH64}

{$ifdef LINUX}
{$ifdef testlinux}
  writeln('ok');
{$else testlinux}
  halt(1);
{$endif testlinux}
{$endif LINUX}
end.
