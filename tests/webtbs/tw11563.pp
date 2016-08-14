{ %target=linux,haiku}
{ %result=216 }

program ExecStack;
  procedure DoIt;
  type
    proc = procedure;
  var
{$if defined(cpupowerpc) or defined(cpupowerpc64)}
    ret: longint;
{$endif}
{$if defined(cpuaarch64)}
    ret: longint;
{$endif}
{$if defined(cpui386) or defined(cpux86_64)}
    ret: Byte;
{$endif}
{$ifdef cpuarm}
    'add arm code to test stack execution'
{$endif}
{$ifdef cpumips}
    ret: array[0..1] of longword;
{$endif}
{$ifdef cpum68k}
    ret: word;
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
{$if defined(cpuaarch64)}
    ret := $d65f03c0;
    DoNothing := proc(@ret);
    DoNothing;
{$endif}
{$if defined(cpui386) or defined(cpux86_64)}
    ret := $C3;
    DoNothing := proc(@ret);
    DoNothing;
{$endif}
{$ifdef cpumips}
{$ifdef ENDIAN_BIG}
    ret[0]:=$03e00008;
{$else ENDIAN_BIG}
    ret[0]:=$0800e003;
{$endif ENDIAN_BIG}    
    ret[1]:=0;                   { delay slot }
    DoNothing:=proc(@ret);
    DoNothing;
{$endif cpumips}
{$ifdef cpum68k}
    ret:=$4E75;
    DoNothing:=proc(@ret);
    DoNothing;
{$endif cpum68k}

  end;
begin
  DoIt;
end.
