{ example for : Interrupt redirection (Hardware interrupts)
                set_pm_interrupt()
                get_pm_interrupt()
                lock_code()
                lock_data()
                unlock_code()
                unlock_data()
                tseginfo record
}
{ This example demonstrates how to chain to a hardware interrupt.

  In more detail, it hooks the keyboard interrupt, calls a user procedure
  which in this case simply turns the PC speaker on and off. Then the old
  interrupt is called.
}

uses crt, { readkey() }
     go32;

const kbdint = $9; { keyboard is IRQ 1 -> interrupt 9 }

var oldint9_handler : tseginfo; { holds old PM interrupt handler address }
    newint9_handler : tseginfo; { new PM interrupt handler }

    clickproc : pointer; { pointer to interrupt handler }

{$ASMMODE DIRECT}
{ interrupt handler }
procedure int9_handler; assembler;
asm
   cli
   { save all registers, because we don't know which the compiler uses for
     the called procedure }
   pushal
   { set up to call a FPC procedure }
   movw %cs:INT9_DS, %ax
   movw %ax, %ds
   movw %ax, %es
   movw U_GO32_DOSMEMSELECTOR, %ax
   movw %ax, %fs
   { call user procedure }
   call *_CLICKPROC
   { restore all registers }
   popal

   ljmp %cs:OLDHANDLER { call old handler }
   { we don't need to do anything more, because the old interrupt handler
     does this for us (send EOI command, iret, sti...) }

INT9_DS: .word 0
OLDHANDLER:
         .long 0
         .word 0
end;
{ dummy procedure to retrieve exact length of handler, for locking and
  unlocking functions  }
procedure int9_dummy; begin end;

{ demo user procedure, simply clicks on every keypress }
procedure clicker;
begin
     sound(500); delay(10); nosound;
end;
{ dummy procedure to retrieve exact length of user procedure for locking and
  unlocking functions }
procedure clicker_dummy; begin end;

{ installs our new handler }
procedure install_click;
begin
     clickproc := @clicker;
     { lock used code and data }
     lock_data(clickproc, sizeof(clickproc));
     lock_data(dosmemselector, sizeof(dosmemselector));

     lock_code(@clicker, longint(@clicker_dummy)-longint(@clicker));
     lock_code(@int9_handler, longint(@int9_dummy)-longint(@int9_handler));
     { fill in new handler's 48 bit pointer }
     newint9_handler.offset := @int9_handler;
     newint9_handler.segment := get_cs;
     { get old PM interrupt handler }
     get_pm_interrupt(kbdint, oldint9_handler);
     { store old PM interrupt handlers address in interrupt handler }
     asm
        movw %ds, %ax
        movw %ax, INT9_DS
        movl _OLDINT9_HANDLER, %eax
        movl %eax, OLDHANDLER
        movw 4+_OLDINT9_HANDLER, %ax
        movw %ax, 4+OLDHANDLER
     end;
     { set the new interrupt handler }
     set_pm_interrupt(kbdint, newint9_handler);
end;

{ deinstalls our interrupt handler }
procedure remove_click;
begin
     { set old handler }
     set_pm_interrupt(kbdint, oldint9_handler);
     { unlock used code & data }
     unlock_data(dosmemselector, sizeof(dosmemselector));
     unlock_data(clickproc, sizeof(clickproc));

     unlock_code(@clicker, longint(@clicker_dummy)-longint(@clicker));
     unlock_code(@int9_handler, longint(@int9_dummy)-longint(@int9_handler));
end;

var ch : char;

begin
     install_click;
     Writeln('Enter any message. Press return when finished');
     while (ch <> #13) do begin
           ch := readkey; write(ch);
     end;
     remove_click;
end.