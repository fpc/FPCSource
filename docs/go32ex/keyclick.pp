{$ASMMODE ATT}
{$MODE FPC}

uses
        crt,
        go32;

const
        kbdint = $9;

var
        oldint9_handler : tseginfo;
        newint9_handler : tseginfo;

        clickproc : pointer;
        backupDS : Word; external name '___v2prt0_ds_alias';

procedure int9_handler; assembler;
asm
        cli
        pushl %ds
        pushl %es
        pushl %fs
        pushl %gs
        pushal
        movw %cs:backupDS, %ax
        movw %ax, %ds
        movw %ax, %es
        movw dosmemselector, %ax
        movw %ax, %fs
        call *clickproc
        popal
        popl %gs
        popl %fs
        popl %es
        popl %ds
        ljmp %cs:oldint9_handler
end;
procedure int9_dummy; begin end;

procedure clicker;
begin
        sound(500); delay(10); nosound;
end;
procedure clicker_dummy; begin end;

procedure install_click;
begin
        clickproc := @clicker;
        lock_data(clickproc, sizeof(clickproc));
        lock_data(dosmemselector, sizeof(dosmemselector));

        lock_code(@clicker,
                longint(@clicker_dummy) - longint(@clicker));
        lock_code(@int9_handler,
                longint(@int9_dummy)-longint(@int9_handler));
        newint9_handler.offset := @int9_handler;
        newint9_handler.segment := get_cs;
        get_pm_interrupt(kbdint, oldint9_handler);
        set_pm_interrupt(kbdint, newint9_handler);
end;

procedure remove_click;
begin
        set_pm_interrupt(kbdint, oldint9_handler);
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