{ %CPU=m68k }

{ This tests the $E+ compiler
  switch. It verifies if the
  switch is correctly enabled
  depending on the target OS
  for m68k.
}
program tb0414;
{$ifdef amiga}
{ Emulation is off by default }
{$ifopt E-}
{$error Emulation is disabled by default for amiga!!}
{$endif}
{$endif}

{$ifdef atari}
{ Emulation is off by default }
{$ifopt E-}
{$error Emulation is disabled by default for amiga!!}
{$endif}
{$endif}


{$ifdef netbsd}
{ Emulation is on by default }
{$ifopt E+}
{$error Emulation is enabled by default for unix!!}
{$endif}
{$endif}

{$ifdef linux}
{ Emulation is on by default }
{$ifopt E+}
{$error Emulation is enabled by default for unix!!}
{$endif}
{$endif}




Begin
End.
