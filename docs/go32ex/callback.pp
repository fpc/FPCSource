{$ASMMODE ATT}
{$MODE FPC}

uses
        crt,
        go32;

const
        mouseint = $33;

var
        mouse_regs    : trealregs; external name '___v2prt0_rmcb_regs';
        mouse_seginfo : tseginfo;

var
        mouse_numbuttons : longint;

        mouse_action : word;
        mouse_x, mouse_y : Word;
        mouse_b : Word;

        userproc_installed : Longbool;
        userproc_length : Longint;
        userproc_proc : pointer;

procedure callback_handler; assembler;
asm
   pushw %ds
   pushl %eax
   movw %es, %ax
   movw %ax, %ds

   cmpl $1, USERPROC_INSTALLED
   jne .LNoCallback
   pushal
   movw DOSmemSELECTOR, %ax
   movw %ax, %fs
   call *USERPROC_PROC
   popal
.LNoCallback:

   popl %eax
   popw %ds

   pushl %eax
   movl (%esi), %eax
   movl %eax, %es: 42(%edi)
   addw $4, %es:46(%edi)
   popl %eax
   iret
end;
procedure mouse_dummy; begin end;

procedure textuserproc;
begin
        mouse_b := mouse_regs.bx;
        mouse_x := (mouse_regs.cx shr 3) + 1;
        mouse_y := (mouse_regs.dx shr 3) + 1;
end;

procedure install_mouse(userproc : pointer; userproclen : longint);
var r : trealregs;
begin
        r.eax := $0; realintr(mouseint, r);
        if (r.eax <> $FFFF) then begin
                Writeln('No Microsoft compatible mouse found');
                Writeln('A Microsoft compatible mouse driver is necessary ',
                        'to run this example');
                halt;
        end;
        if (r.bx = $ffff) then mouse_numbuttons := 2
        else mouse_numbuttons := r.bx;
        Writeln(mouse_numbuttons, ' button Microsoft compatible mouse ',
                ' found.');
        if (userproc <> nil) then begin
                userproc_proc := userproc;
                userproc_installed := true;
                userproc_length := userproclen;
                lock_code(userproc_proc, userproc_length);
        end else begin
                userproc_proc := nil;
                userproc_length := 0;
                userproc_installed := false;
        end;
        lock_data(mouse_x, sizeof(mouse_x));
        lock_data(mouse_y, sizeof(mouse_y));
        lock_data(mouse_b, sizeof(mouse_b));
        lock_data(mouse_action, sizeof(mouse_action));

        lock_data(userproc_installed, sizeof(userproc_installed));
        lock_data(userproc_proc, sizeof(userproc_proc));

        lock_data(mouse_regs, sizeof(mouse_regs));
        lock_data(mouse_seginfo, sizeof(mouse_seginfo));
        lock_code(@callback_handler,
                longint(@mouse_dummy)-longint(@callback_handler));
        get_rm_callback(@callback_handler, mouse_regs, mouse_seginfo);
        r.eax := $0c; r.ecx := $7f;
        r.edx := longint(mouse_seginfo.offset);
        r.es := mouse_seginfo.segment;
        realintr(mouseint, r);
        r.eax := $01;
        realintr(mouseint, r);
end;

procedure remove_mouse;
var
        r : trealregs;
begin
        r.eax := $02; realintr(mouseint, r);
        r.eax := $0c; r.ecx := 0; r.edx := 0; r.es := 0;
        realintr(mouseint, r);
        free_rm_callback(mouse_seginfo);
        if (userproc_installed) then begin
                unlock_code(userproc_proc, userproc_length);
                userproc_proc := nil;
                userproc_length := 0;
                userproc_installed := false;
        end;
        unlock_data(mouse_x, sizeof(mouse_x));
        unlock_data(mouse_y, sizeof(mouse_y));
        unlock_data(mouse_b, sizeof(mouse_b));
        unlock_data(mouse_action, sizeof(mouse_action));

        unlock_data(userproc_proc, sizeof(userproc_proc));
        unlock_data(userproc_installed, sizeof(userproc_installed));

        unlock_data(mouse_regs, sizeof(mouse_regs));
        unlock_data(mouse_seginfo, sizeof(mouse_seginfo));
        unlock_code(@callback_handler,
                longint(@mouse_dummy)-longint(@callback_handler));
        fillchar(mouse_seginfo, sizeof(mouse_seginfo), 0);
end;


begin
        install_mouse(@textuserproc, 400);
        Writeln('Press any key to exit...');
        while (not keypressed) do begin
                gotoxy(1, wherey);
                write('MouseX : ', mouse_x:2, ' MouseY : ', mouse_y:2,
                        ' Buttons : ', mouse_b:2);
        end;
        remove_mouse;
end.