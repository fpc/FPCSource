{ %target=linux}
{ %result=216 }

program ExecStack;
  procedure DoIt;
  type
    proc = procedure;
  var
{$if defined(cpupowerpc) or defined(cpupowerpc64)}
    ret: longint;
{$endif}
{$if defined(cpui386) or defined(cpux86_64)}
    ret: Byte;
{$endif}
{$ifdef cpuarm}
    'add arm code to test stack execution'
{$endif}
    DoNothing: proc;

  begin
{$if defined(cpupowerpc) or defined(cpupowerpc64)}
    ret := ($4e shl 24) or ($80 shl 16) or ($00 shl 8) or $20;
{$if defined(cpupowerpc64)}
    { can't use proc(@ret) because linux/ppc64 always expects some kind of
      trampoline
    }
    asm
      la r0, ret
      mtctr r0
      bctrl
    end;
{$else}
    DoNothing := proc(@ret);
    DoNothing;
{$endif}
{$endif}
{$if defined(cpui386) or defined(cpux86_64)}
    ret := $C3;
    DoNothing := proc(@ret);
    DoNothing;
{$endif}
  end;
begin
  DoIt;
end.
