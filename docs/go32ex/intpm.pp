uses
        crt,
        go32;

const
        int1c = $1c;

var
        oldint1c : tseginfo;
        newint1c : tseginfo;

        int1c_counter : Longint;

        int1c_ds : Word; external name '___v2prt0_ds_alias';

procedure int1c_handler; assembler;
asm
   cli
   pushw %ds
   pushw %ax
   movw %cs:int1c_ds, %ax
   movw %ax, %ds
   incl int1c_counter
   popw %ax
   popw %ds
   sti
   iret
end;

var i : Longint;

begin
     newint1c.offset := @int1c_handler;
     newint1c.segment := get_cs;
     get_pm_interrupt(int1c, oldint1c);
     Writeln('-- Press any key to exit --');
     set_pm_interrupt(int1c, newint1c);
     while (not keypressed) do begin
           gotoxy(1, wherey);
           write('Number of interrupts occured : ', int1c_counter);
     end;
     set_pm_interrupt(int1c, oldint1c);
end.