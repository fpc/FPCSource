{
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   New linux unit. Linux only calls only. Will be renamed to linux.pp
   when 1.0.x support is killed off.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit Linux;

{$packrecords c}

interface

uses
  ctypes;

Type
  TSysinfo = packed record
    uptime    : longint;
    loads     : array[1..3] of longint;
    totalram,
    freeram,
    sharedram,
    bufferram,
    totalswap,
    freeswap  : longint;
    procs     : integer;
    s         : string[18];
  end;
  PSysInfo = ^TSysInfo;

Function Sysinfo(var Info:TSysinfo):Boolean; {$ifdef FPC_USE_LIBC} cdecl; external name 'sysinfo'; {$endif}

Const
  CSIGNAL       = $000000ff; // signal mask to be sent at exit
  CLONE_VM      = $00000100; // set if VM shared between processes
  CLONE_FS      = $00000200; // set if fs info shared between processes
  CLONE_FILES   = $00000400; // set if open files shared between processes
  CLONE_SIGHAND = $00000800; // set if signal handlers shared
  CLONE_PID     = $00001000; // set if pid shared

  EPOLLIN  = $01; { The associated file is available for read(2) operations. }
  EPOLLOUT = $02; { The associated file is available for write(2) operations. }
  EPOLLPRI = $04; { There is urgent data available for read(2) operations. }
  EPOLLERR = $08; { Error condition happened on the associated file descriptor. }
  EPOLLHUP = $10; { Hang up happened on the associated file descriptor. }
  EPOLLET  = $80000000; { Sets  the  Edge  Triggered  behaviour  for  the  associated file descriptor. }

  { Valid opcodes ( "op" parameter ) to issue to epoll_ctl }
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_MOD = 2;
  EPOLL_CTL_DEL = 3;

  {Some console iotcl's.}
  GIO_FONT        = $4B60;  {gets font in expanded form}
  PIO_FONT        = $4B61;  {use font in expanded form}
  GIO_FONTX       = $4B6B;  {get font using struct consolefontdesc}
  PIO_FONTX       = $4B6C;  {set font using struct consolefontdesc}
  PIO_FONTRESET   = $4B6D;  {reset to default font}
  GIO_CMAP        = $4B70;  {gets colour palette on VGA+}
  PIO_CMAP        = $4B71;  {sets colour palette on VGA+}
  KIOCSOUND       = $4B2F;  {start sound generation (0 for off)}
  KDMKTONE        = $4B30;  {generate tone}
  KDGETLED        = $4B31;  {return current led state}
  KDSETLED        = $4B32;  {set led state [lights, not flags]}
  KDGKBTYPE       = $4B33;  {get keyboard type}
  KDADDIO         = $4B34;  {add i/o port as valid}
  KDDELIO         = $4B35;  {del i/o port as valid}
  KDENABIO        = $4B36;  {enable i/o to video board}
  KDDISABIO       = $4B37;  {disable i/o to video board}
  KDSETMODE       = $4B3A;  {set text/graphics mode}
  KDGETMODE       = $4B3B;  {get current mode}
  KDMAPDISP       = $4B3C;  {map display into address space}
  KDUNMAPDISP     = $4B3D;  {unmap display from address space}
  GIO_SCRNMAP     = $4B40;  {get screen mapping from kernel}
  PIO_SCRNMAP     = $4B41;  {put screen mapping table in kernel}
  GIO_UNISCRNMAP  = $4B69;  {get full Unicode screen mapping}
  PIO_UNISCRNMAP  = $4B6A;  {set full Unicode screen mapping}
  GIO_UNIMAP      = $4B66;  {get unicode-to-font mapping from kernel}
  PIO_UNIMAP      = $4B67;  {put unicode-to-font mapping in kernel}
  PIO_UNIMAPCLR   = $4B68;  {clear table, possibly advise hash algorithm}
  KDGKBDIACR      = $4B4A;  {read kernel accent table}
  KDSKBDIACR      = $4B4B;  {write kernel accent table}
  KDGETKEYCODE    = $4B4C;  {read kernel keycode table entry}
  KDSETKEYCODE    = $4B4D;  {write kernel keycode table entry}
  KDSIGACCEPT     = $4B4E;  {accept kbd generated signals}
  KDFONTOP        = $4B72;  {font operations}

  {Keyboard types (for KDGKBTYPE)}
  KB_84           = 1;
  KB_101          = 2;    {Normal PC keyboard.}
  KB_OTHER        = 3;

  {Keyboard LED constants.}
  LED_SCR         = 1;    {scroll lock led}
  LED_NUM         = 2;    {num lock led}
  LED_CAP         = 4;    {caps lock led}

  {Tty modes. (for KDSETMODE)}
  KD_TEXT         = 0;
  KD_GRAPHICS     = 1;
  KD_TEXT0        = 2;    {obsolete}
  KD_TEXT1        = 3;    {obsolete}



type
  TCloneFunc=function(args:pointer):longint;cdecl;

  EPoll_Data = Record
    case integer of
      0: (ptr: pointer);
      1: (fd: cint);
      2: (u32: cuint);
      3: (u64: cuint64);
  end;
  TEPoll_Data =  Epoll_Data;
  PEPoll_Data = ^Epoll_Data;

  EPoll_Event = Record
    Events: cuint32;
    Data  : TEpoll_Data;
  end;
  TEPoll_Event =  Epoll_Event;
  PEpoll_Event = ^Epoll_Event;

function Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint; {$ifdef FPC_USE_LIBC} cdecl; external name 'clone'; {$endif}

{ open an epoll file descriptor }
function epoll_create(size: cint): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_create'; {$endif}
{ control interface for an epoll descriptor }
function epoll_ctl(epfd, op, fd: cint; event: pepoll_event): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_ctl'; {$endif}
{ wait for an I/O event on an epoll file descriptor }
function epoll_wait(epfd: cint; events: pepoll_event; maxevents, timeout: cint): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_wait'; {$endif}

implementation

{$ifndef FPC_USE_LIBC}
Uses Syscall;

Function Sysinfo(var Info:TSysinfo):Boolean;
{
  Get system info
}
Begin
  Sysinfo:=do_SysCall(SysCall_nr_Sysinfo,TSysParam(@info))=0;
End;

function Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;

begin
  if (pointer(func)=nil) or (sp=nil) then
   exit(-1); // give an error result
{$ifdef cpui386}
{$ASMMODE ATT}
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        movl    flags,%ebx
        movl    SysCall_nr_clone,%eax
        int     $0x80
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
{$endif cpui386}
{$ifdef cpum68k}
  { No yet translated, my m68k assembler is too weak for such things PM }
(*
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        movl    flags,%ebx
        movl    SysCall_nr_clone,%eax
        int     $0x80
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
  *)
{$endif cpum68k}
end;

function epoll_create(size: cint): cint;
begin
  epoll_create := do_syscall(syscall_nr_epoll_create,tsysparam(size));
end;

function epoll_ctl(epfd, op, fd: cint; event: pepoll_event): cint;
begin
  epoll_ctl := do_syscall(syscall_nr_epoll_ctl, tsysparam(epfd),
    tsysparam(op), tsysparam(fd), tsysparam(event));
end;

function epoll_wait(epfd: cint; events: pepoll_event; maxevents, timeout: cint): cint;
begin
  epoll_wait := do_syscall(syscall_nr_epoll_wait, tsysparam(epfd),
    tsysparam(events), tsysparam(maxevents), tsysparam(timeout));
end;
{$endif}

end.
