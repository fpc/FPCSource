{ This program tries to give an example how to install a callback
procedure with the help of the GO32 unit.

It installs a callback which is supplied by any Microsoft compatible
mouse driver; at a specified mouse action this routine is called.
This callback must provide the services explained in the docs. The
main callback has to be in assembly, because it isn't possible to do
these services with pascal alone. But is written as general as
possible to provide maximum re-usability for other applications and
hence it simply calls a normal pascal user procedure in addition to
some initialization and callback service code, so you don't need to
hassle around with it too much.

Notes to this user procedure :
*) it should not last too long to execute it
*) ALL data and code touched in this proc MUST be locked BEFORE it is
called the first time


Used software interrupt calls (rough descriptions, only what's used):

Int 33h 0000h - Microsoft Mouse driver : Reset mouse
Input : AX = 0000h
Return : AX = FFFFh if successful
         BX = number of buttons (if FFFFh then mouse has 2 buttons)

Int 33h 0001h - Microsoft Mouse driver : Show mouse cursor
Input : AX = 0001h
Return : Mouse cursor shown on screen

Int 33h 0002h - Microsoft mouse driver : Hide mouse cursor
Input : AX = 0002h
Return : Hides mouse cursor again

 Int 33h 000Ch - Microsoft mouse driver : Install user callback
Input : AX = 000Ch
        CX = bit mask which tells the mouse driver at which actions
        the callback should be called, i.e. if button pressed, mouse
        moved etc.
        (In this example it's set to 7Fh so that the callback is
        called on every action)
        ES:EDX = pointer to callback procedure to call
 Note : The registers structure supplied to the callback contains
        valid mouse data when the handler is called.
  BX = button state information
  CX = mouse X coordinates
  DX = mouse Y coordinates
For more detailed information consult any mouse reference or
interrupt list.
}
{$ASMMODE ATT}
{$MODE FPC}

uses
        crt,
        go32;

const
        { the mouse interrupt number }
        mouseint = $33;

var
        { supplied register structure to the callback }
        mouse_regs    : trealregs; external name '___v2prt0_rmcb_regs';
        { real mode 48 bit pointer to the callback }
        mouse_seginfo : tseginfo;

var
        { number of mouse buttons }
        mouse_numbuttons : longint;

        { bit mask for the action which triggered the callback }
        mouse_action : word;
        { current mouse x and y coordinates }
        mouse_x, mouse_y : Word;
        { button state }
        mouse_b : Word;

        { is an additional user procedure installed }
        userproc_installed : Longbool;
        { length of additional user procedure }
        userproc_length : Longint;
        { pointer to user proc }
        userproc_proc : pointer;

{ callback control handler, calls a user procedure if installed }

{ callback control handler, calls a user procedure if installed }
procedure callback_handler; assembler;
asm
   pushw %ds
   pushl %eax
   movw %es, %ax
   movw %ax, %ds

   { give control to user procedure if installed }
   cmpl $1, USERPROC_INSTALLED
   jne .LNoCallback
   pushal
   movw DOSmemSELECTOR, %ax
   movw %ax, %fs  { set fs for FPC }
   call *USERPROC_PROC
   popal
.LNoCallback:

   popl %eax
   popw %ds

   pushl %eax
   movl (%esi), %eax
   movl %eax, %es: 42(%edi) { adjust stack }
   addw $4, %es:46(%edi)
   popl %eax
   iret
end;
{ This dummy is used to obtain the length of the callback control
function. It has to be right after the callback_handler() function.
}
procedure mouse_dummy; begin end;

{ This is the supplied user procedure. In this case we simply
transform the virtual 640x200 mouse coordinate system to a 80x25
text mode coordinate system }
procedure textuserproc;
begin
        { the mouse_regs record contains the real mode registers now }
        mouse_b := mouse_regs.bx;
        mouse_x := (mouse_regs.cx shr 3) + 1;
        mouse_y := (mouse_regs.dx shr 3) + 1;
end;

{ Description : Installs the mouse callback control handler and
handles all necessary mouse related initialization.
  Input : userproc - pointer to a user procedure, nil if none
          userproclen - length of user procedure
}
procedure install_mouse(userproc : pointer; userproclen : longint);
var r : trealregs;
begin
        { mouse driver reset }
        r.eax := $0; realintr(mouseint, r);
        if (r.eax <> $FFFF) then begin
                Writeln('No Microsoft compatible mouse found');
                Writeln('A Microsoft compatible mouse driver is necessary ',
                        'to run this example');
                halt;
        end;
        { obtain number of mouse buttons }
        if (r.bx = $ffff) then mouse_numbuttons := 2
        else mouse_numbuttons := r.bx;
        Writeln(mouse_numbuttons, ' button Microsoft compatible mouse ',
                ' found.');
        { check for additional user procedure, and install it if
        available }
        if (userproc <> nil) then begin
                userproc_proc := userproc;
                userproc_installed := true;
                userproc_length := userproclen;
                { lock code for user procedure }
                lock_code(userproc_proc, userproc_length);
        end else begin
                { clear variables }
                userproc_proc := nil;
                userproc_length := 0;
                userproc_installed := false;
        end;
        { lock code & data which is touched in the callback handler }
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
        { allocate callback (supply registers structure) }
        get_rm_callback(@callback_handler, mouse_regs, mouse_seginfo);
        { install callback }
        r.eax := $0c; r.ecx := $7f;
        r.edx := longint(mouse_seginfo.offset);
        r.es := mouse_seginfo.segment;
        realintr(mouseint, r);
        { show mouse cursor }
        r.eax := $01;
        realintr(mouseint, r);
end;

procedure remove_mouse;
var
        r : trealregs;
begin
        { hide mouse cursor }
        r.eax := $02; realintr(mouseint, r);
        { remove callback handler }
        r.eax := $0c; r.ecx := 0; r.edx := 0; r.es := 0;
        realintr(mouseint, r);
        { free callback }
        free_rm_callback(mouse_seginfo);
        { check if additional userproc is installed, and clean up if
        needed }
        if (userproc_installed) then begin
                unlock_code(userproc_proc, userproc_length);
                userproc_proc := nil;
                userproc_length := 0;
                userproc_installed := false;
        end;
        { unlock used code & data }
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
                { write mouse state info }
                gotoxy(1, wherey);
                write('MouseX : ', mouse_x:2, ' MouseY : ', mouse_y:2,
                        ' Buttons : ', mouse_b:2);
        end;
        remove_mouse;
end.