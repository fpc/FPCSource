{ This example demonstrates how to chain to a hardware interrupt.

In more detail, it hooks the keyboard interrupt, calls a user
procedure which in this case simply turns the PC speaker on and off.
Then the old interrupt is called.
}

{$ASMMODE ATT}
{$MODE FPC}

uses
        crt,
        go32;

const
        { keyboard is IRQ 1 -> interrupt 9 }
        kbdint = $9;

var
        { holds old PM interrupt handler address }
        oldint9_handler : tseginfo;
        { new PM interrupt handler }
        newint9_handler : tseginfo;

        { pointer to interrupt handler }
        clickproc : pointer;
        { the data segment selector }
        backupDS : Word; external name '___v2prt0_ds_alias';

{ interrupt handler }
procedure int9_handler; assembler;
asm
        cli
        { save all registers, because we don't know which the compiler
        uses for the called procedure }
        pushl %ds
        pushl %es
        pushl %fs
        pushl %gs
        pushal
        { set up to call a FPC procedure }
        movw %cs:backupDS, %ax
        movw %ax, %ds
        movw %ax, %es
        movw dosmemselector, %ax
        movw %ax, %fs
        { call user procedure }
        call *clickproc
        { restore all registers }
        popal
        popl %gs
        popl %fs
        popl %es
        popl %ds
        { note: in go32v2 mode %cs=%ds=%es !!!}
        ljmp %cs:oldint9_handler { call old handler }
        { we don't need to do anything more, because the old interrupt
        handler does this for us (send EOI command, iret, sti...) }
end;
{ dummy procedure to retrieve exact length of handler, for locking
and unlocking functions  }
procedure int9_dummy; begin end;

{ demo user procedure, simply clicks on every keypress }
procedure clicker;
begin
        sound(500); delay(10); nosound;
end;
{ dummy procedure to retrieve exact length of user procedure for
locking and unlocking functions }
procedure clicker_dummy; begin end;

{ installs our new handler }
procedure install_click;
begin
        clickproc := @clicker;
        { lock used code and data }
        lock_data(clickproc, sizeof(clickproc));
        lock_data(dosmemselector, sizeof(dosmemselector));

        lock_code(@clicker,
                longint(@clicker_dummy) - longint(@clicker));
        lock_code(@int9_handler,
                longint(@int9_dummy)-longint(@int9_handler));
        { fill in new handler's 48 bit pointer }
        newint9_handler.offset := @int9_handler;
        newint9_handler.segment := get_cs;
        { get old PM interrupt handler }
        get_pm_interrupt(kbdint, oldint9_handler);
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

        unlock_code(@clicker,
                longint(@clicker_dummy)-longint(@clicker));
        unlock_code(@int9_handler,
                longint(@int9_dummy)-longint(@int9_handler));
end;

var
        ch : char;

begin
        install_click;
        Writeln('Enter any message. Press return when finished');
        while (ch <> #13) do begin
                ch := readkey; write(ch);
        end;
        remove_click;
end.