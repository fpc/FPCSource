{ %NORUN }
{ %CPU=i8086,i386,x86_64 }
program tuglylabels1;

{ This test is TP7 compatible }

{$ifdef FPC}
  {$asmmode intel}
{$endif FPC}

{ allowed characters in a local label:
  @$&_?abcdefghijklmnopqrstuvwxyz0123456789 }

begin
  asm
@:
    jmp @
@@:
    jmp @@
@$&_?@9:
    jmp @$&_?@9
  end;
end.
