Program int_pm;

uses crt, go32;

const int1c = $1c; 

var oldint1c : tseginfo;
    newint1c : tseginfo;
    int1c_counter : Longint;

{$ASMMODE DIRECT}
procedure int1c_handler; assembler;
asm
   cli
   pushw %ds
   pushw %ax
   movw %cs:INT1C_DS, %ax
   movw %ax, %ds
   incl _INT1C_COUNTER
   popw %ax
   popw %ds
   sti
   iret
INT1C_DS: .word 0
end;

var i : Longint;

begin
     newint1c.offset := @int1c_handler;
     newint1c.segment := get_cs;
     get_pm_interrupt(int1c, oldint1c);
     asm
        movw %ds, %ax
        movw %ax, INT1C_DS
     end;
     Writeln('-- Press any key to exit --');
     set_pm_interrupt(int1c, newint1c);
     while (not keypressed) do begin
           gotoxy(1, wherey); 
           write('Number of interrupts occured : ', 
                 int1c_counter);
     end;
     set_pm_interrupt(int1c, oldint1c);
end.