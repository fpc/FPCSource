Program Mouse1;

{example for InitMouse and MouseFound}

Uses MsMouse;

Begin
  If MouseFound Then
    Begin
     {go into graphics mode 13h}
      Asm
        movl $0x013, %eax
        pushl %ebp
        int $0x010
        popl %ebp
      End;
      InitMouse;
      ShowMouse; {otherwise it stays invisible}
      Writeln('Mouse Found! (press enter to quit)');
      Readln;
     {back to text mode}
      Asm
        movl $3, %eax
        pushl %ebp
        int $0x010
        popl %ebp
      End
    End
End.

