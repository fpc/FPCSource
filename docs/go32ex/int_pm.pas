{ example for :
          set_pm_interrupt()
          get_pm_interrupt()
          Interrupt redirection
          Software interrupt
          tseginfo record
          get_cs()
}

{ This example shows how to redirect a software interrupt by changing the
  protected mode handler of the DPMI host.

  In more detail it hooks interrupt 1Ch which is called every time the timer
  interrupt (int 08) is executed. This is the preferred way to hook the timer,
  because int 1Ch is a software interrupt which doesn't need so much
  initialization stuff compared to hooking a hardware interrupt.
}

uses crt, { wherey(), keypressed() }
     go32;

const int1c = $1c; { interrupt number we want to hook }

var oldint1c : tseginfo; { 48 bit pointer to old interrupt handler }
    newint1c : tseginfo; { 48 bit pointer to new interrupt handler }

    int1c_counter : Longint; { increased every time the interrupt is called  }

{$ASMMODE DIRECT}
{ the actual handler code }
procedure int1c_handler; assembler;
asm
   cli
{ save all registers }
   pushw %ds
   pushw %ax
{ prepare segment registers for FPC procedure }
   movw %cs:INT1C_DS, %ax
   movw %ax, %ds
{ simply increase the counter by one }
   incl _INT1C_COUNTER
{ restore registers }
   popw %ax
   popw %ds
   sti
   iret
INT1C_DS: .word 0
end;

var i : Longint;

begin
     { insert right handler data into new handler variable }
     newint1c.offset := @int1c_handler;
     newint1c.segment := get_cs;
     { get the old handler }
     get_pm_interrupt(int1c, oldint1c);
     { store necessary data into the handler }
     asm
        movw %ds, %ax
        movw %ax, INT1C_DS
     end;
     Writeln('-- Press any key to exit --');
     { set new handler }
     set_pm_interrupt(int1c, newint1c);
     { write the number of interrupts occured }
     while (not keypressed) do begin
           gotoxy(1, wherey); write('Number of interrupts occured : ', int1c_counter);
     end;
     { restore old handler }
     set_pm_interrupt(int1c, oldint1c);
end.