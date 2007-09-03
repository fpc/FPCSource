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
  BaseUnix;//, ctypes;

type
  TSysInfo = record
    uptime: clong;                     //* Seconds since boot */
    loads: array[0..2] of culong;      //* 1, 5, and 15 minute load averages */
    totalram: culong;                  //* Total usable main memory size */
    freeram: culong;                   //* Available memory size */
    sharedram: culong;                 //* Amount of shared memory */
    bufferram: culong;                 //* Memory used by buffers */
    totalswap: culong;                 //* Total swap space size */
    freeswap: culong;                  //* swap space still available */
    procs: cushort;                    //* Number of current processes */
    pad: cushort;                      //* explicit padding for m68k */
    totalhigh: culong;                 //* Total high memory size */
    freehigh: culong;                  //* Available high memory size */
    mem_unit: cuint;                   //* Memory unit size in bytes */
{$ifndef cpu64}
    { the upper bound of the array below is negative for 64 bit cpus }
    _f: array[0..19-2*sizeof(clong)-sizeof(cint)] of cChar;  //* Padding: libc5 uses this.. */
{$endif cpu64}
  end;
  PSysInfo = ^TSysInfo;

function Sysinfo(Info: PSysinfo): cInt; {$ifdef FPC_USE_LIBC} cdecl; external name 'sysinfo'; {$endif}

const
  CSIGNAL              = $000000ff; // signal mask to be sent at exit
  CLONE_VM             = $00000100; // set if VM shared between processes
  CLONE_FS             = $00000200; // set if fs info shared between processes
  CLONE_FILES          = $00000400; // set if open files shared between processes
  CLONE_SIGHAND        = $00000800; // set if signal handlers shared
  CLONE_PID            = $00001000; // set if pid shared
  CLONE_PTRACE         = $00002000; // Set if tracing continues on the child.
  CLONE_VFORK          = $00004000; // Set if the parent wants the child to wake it up on mm_release.
  CLONE_PARENT         = $00008000; // Set if we want to have the same parent as the cloner.
  CLONE_THREAD         = $00010000; // Set to add to same thread group.
  CLONE_NEWNS          = $00020000; // Set to create new namespace.
  CLONE_SYSVSEM        = $00040000; // Set to shared SVID SEM_UNDO semantics.
  CLONE_SETTLS         = $00080000; // Set TLS info.
  CLONE_PARENT_SETTID  = $00100000; // Store TID in userlevel buffer before MM copy.
  CLONE_CHILD_CLEARTID = $00200000; // Register exit futex and memory location to clear.
  CLONE_DETACHED       = $00400000; // Create clone detached.
  CLONE_UNTRACED       = $00800000; // Set if the tracing process can't force CLONE_PTRACE on this clone.
  CLONE_CHILD_SETTID   = $01000000; // Store TID in userlevel buffer in the child.
  CLONE_STOPPED        = $02000000; // Start in stopped state.


  FUTEX_WAIT            = 0;
  FUTEX_WAKE            = 1;
  FUTEX_FD              = 2;
  FUTEX_REQUEUE         = 3;
  FUTEX_CMP_REQUEUE     = 4;
  FUTEX_WAKE_OP         = 5;
  FUTEX_LOCK_PI         = 6;
  FUTEX_UNLOCK_PI       = 7;
  FUTEX_TRYLOCK_PI      = 8;

  FUTEX_OP_SET          = 0;   // *(int *)UADDR2 = OPARG;
  FUTEX_OP_ADD          = 1;   // *(int *)UADDR2 += OPARG;
  FUTEX_OP_OR           = 2;   // *(int *)UADDR2 |= OPARG;
  FUTEX_OP_ANDN         = 3;   // *(int *)UADDR2 &= ~OPARG;
  FUTEX_OP_XOR          = 4;   // *(int *)UADDR2 ^= OPARG;

  FUTEX_OP_OPARG_SHIFT  = 8;   // Use (1 << OPARG) instead of OPARG.

  FUTEX_OP_CMP_EQ       = 0;   // if (oldval == CMPARG) wake
  FUTEX_OP_CMP_NE       = 1;   // if (oldval != CMPARG) wake
  FUTEX_OP_CMP_LT       = 2;   // if (oldval < CMPARG) wake
  FUTEX_OP_CMP_LE       = 3;   // if (oldval <= CMPARG) wake
  FUTEX_OP_CMP_GT       = 4;   // if (oldval > CMPARG) wake
  FUTEX_OP_CMP_GE       = 5;   // if (oldval >= CMPARG) wake

{ FUTEX_WAKE_OP will perform atomically
   int oldval = *(int *)UADDR2;
   *(int *)UADDR2 = oldval OP OPARG;
   if (oldval CMP CMPARG)
     wake UADDR2; }

{$ifndef FPC_USE_LIBC}
function futex_op(op, oparg, cmp, cmparg: cint): cint; {$ifdef SYSTEMINLINE}inline;{$endif}
{$endif}

const
  EPOLLIN  = $01; { The associated file is available for read(2) operations. }
  EPOLLPRI = $02; { There is urgent data available for read(2) operations. }
  EPOLLOUT = $04; { The associated file is available for write(2) operations. }
  EPOLLERR = $08; { Error condition happened on the associated file descriptor. }
  EPOLLHUP = $10; { Hang up happened on the associated file descriptor. }
  EPOLLET  = $80000000; { Sets  the  Edge  Triggered  behaviour  for  the  associated file descriptor. }

  { Valid opcodes ( "op" parameter ) to issue to epoll_ctl }
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

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

  MAP_GROWSDOWN  = $100;       { stack-like segment }
  MAP_DENYWRITE  = $800;       { ETXTBSY }
  MAP_EXECUTABLE = $1000;      { mark it as an executable }
  MAP_LOCKED     = $2000;      { pages are locked }
  MAP_NORESERVE  = $4000;      { don't check for reservations }

type
  TCloneFunc = function(args:pointer):longint;cdecl;

{$ifdef cpui386}
  {$define clone_implemented}
{$endif}
{$ifdef cpum68k}
  {$define clone_implemented}
{$endif}

{$ifdef clone_implemented}
function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint; {$ifdef FPC_USE_LIBC} cdecl; external name 'clone'; {$endif}
{$endif}

const
  MODIFY_LDT_CONTENTS_DATA       = 0;
  MODIFY_LDT_CONTENTS_STACK      = 1;
  MODIFY_LDT_CONTENTS_CODE       = 2;

{ Flags for user_desc.flags }
  UD_SEG_32BIT            = $01;
  UD_CONTENTS_DATA        = MODIFY_LDT_CONTENTS_DATA  shl 1;
  UD_CONTENTS_STACK       = MODIFY_LDT_CONTENTS_STACK shl 1;
  UD_CONTENTS_CODE        = MODIFY_LDT_CONTENTS_CODE  shl 1;
  UD_READ_EXEC_ONLY       = $08;
  UD_LIMIT_IN_PAGES       = $10;
  UD_SEG_NOT_PRESENT      = $20;
  UD_USEABLE              = $40;
  UD_LM                   = $80;

type
  user_desc = packed record
    entry_number  : cint;
    base_addr     : cuint;
    limit         : cuint;
    flags         : cuint;
  end;

  TUser_Desc = user_desc;
  PUser_Desc = ^user_desc;


type
  EPoll_Data = record
    case integer of
      0: (ptr: pointer);
      1: (fd: cint);
      2: (u32: cuint);
      3: (u64: cuint64);
  end;
  TEPoll_Data =  Epoll_Data;
  PEPoll_Data = ^Epoll_Data;

  EPoll_Event = {$ifdef cpu64} packed {$endif} record
    Events: cuint32;
    Data  : TEpoll_Data;
  end;

  TEPoll_Event =  Epoll_Event;
  PEpoll_Event = ^Epoll_Event;


{ open an epoll file descriptor }
function epoll_create(size: cint): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_create'; {$endif}
{ control interface for an epoll descriptor }
function epoll_ctl(epfd, op, fd: cint; event: pepoll_event): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_ctl'; {$endif}
{ wait for an I/O event on an epoll file descriptor }
function epoll_wait(epfd: cint; events: pepoll_event; maxevents, timeout: cint): cint; {$ifdef FPC_USE_LIBC} cdecl; external name 'epoll_wait'; {$endif}

type Puser_cap_header=^user_cap_header;
     user_cap_header=packed record
       version,pid:cardinal;
     end;
     
     Puser_cap_data=^user_cap_data;
     user_cap_data=packed record
        effective,permitted,inheritable:cardinal;
     end;

{Get a capability.}
function capget(header:Puser_cap_header;data:Puser_cap_data):cint;{$ifdef FPC_USE_LIBC} cdecl; external name 'capget'; {$endif}
{Set a capability.}
function capset(header:Puser_cap_header;data:Puser_cap_data):cint;{$ifdef FPC_USE_LIBC} cdecl; external name 'capset'; {$endif}

     
const CAP_CHOWN            = 0;
      CAP_DAC_OVERRIDE     = 1;
      CAP_DAC_READ_SEARCH  = 2;
      CAP_FOWNER           = 3;
      CAP_FSETID           = 4;
      CAP_FS_MASK          = $1f;
      CAP_KILL             = 5;
      CAP_SETGID           = 6;
      CAP_SETUID           = 7;
      CAP_SETPCAP          = 8;
      CAP_LINUX_IMMUTABLE  = 9;
      CAP_NET_BIND_SERVICE = 10;
      CAP_NET_BROADCAST    = 11;
      CAP_NET_ADMIN        = 12;
      CAP_NET_RAW          = 13;
      CAP_IPC_LOCK         = 14;
      CAP_IPC_OWNER        = 15;
      CAP_SYS_MODULE       = 16;
      CAP_SYS_RAWIO        = 17;
      CAP_SYS_CHROOT       = 18;
      CAP_SYS_PTRACE       = 19;
      CAP_SYS_PACCT        = 20;
      CAP_SYS_ADMIN        = 21;
      CAP_SYS_BOOT         = 22;
      CAP_SYS_NICE         = 23;
      CAP_SYS_RESOURCE     = 24;
      CAP_SYS_TIME         = 25;
      CAP_SYS_TTY_CONFIG   = 26;
      CAP_MKNOD            = 27;
      CAP_LEASE            = 28;
      CAP_AUDIT_WRITE      = 29;
      CAP_AUDIT_CONTROL    = 30;

      LINUX_CAPABILITY_VERSION = $19980330;


//***********************************************SPLICE from kernel 2.6.17+****************************************

const
{* Flags for SPLICE and VMSPLICE.  *}
  SPLICE_F_MOVE		= 1;   { Move pages instead of copying.  }
  SPLICE_F_NONBLOCK	= 2;   {* Don't block on the pipe splicing
                            (but we may still block on the fd
                                        we splice from/to).  *}
  SPLICE_F_MORE	    = 4;   {* Expect more data.  *}
  SPLICE_F_GIFT	    = 8;   {* Pages passed in are a gift.  *}

{$ifdef cpu86}
{* Splice address range into a pipe.  *}
function vmsplice (fdout: cInt; iov: PIOVec; count: size_t; flags: cuInt): cInt; {$ifdef FPC_USE_LIBC} cdecl; external name 'vmsplice'; {$ENDIF}

{* Splice two files together.  *}
// NOTE: offin and offout should be "off64_t" but we don't have that type. It's an "always 64 bit offset" so I use cint64
function splice (fdin: cInt; offin: cInt64; fdout: cInt;
                             offout: cInt64; len: size_t; flags: cuInt): cInt; {$ifdef FPC_USE_LIBC} cdecl; external name 'splice'; {$ENDIF}
                             
function tee(fd_in: cInt; fd_out: cInt; len: size_t; flags: cuInt): cInt; {$ifdef FPC_USE_LIBC} cdecl; external name 'tee'; {$ENDIF}

{$endif} // x86

implementation


{$ifndef FPC_USE_LIBC}
Uses Syscall;

function Sysinfo(Info: PSysinfo): cInt;
begin
  Sysinfo := do_SysCall(SysCall_nr_Sysinfo, TSysParam(info));
end;

{$ifdef clone_implemented}
function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;

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
{$endif}

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

function capget(header:Puser_cap_header;data:Puser_cap_data):cint;

begin
  capget:=do_syscall(syscall_nr_capget,Tsysparam(header),Tsysparam(data));
end;

function capset(header:Puser_cap_header;data:Puser_cap_data):cint;

begin
  capset:=do_syscall(syscall_nr_capset,Tsysparam(header),Tsysparam(data));
end;

// TODO: update also on non x86!
{$ifdef cpu86} // didn't update syscall_nr on others yet

function vmsplice (fdout: cInt; iov: PIOVec; count: size_t; flags: cuInt): cInt;
begin
  vmsplice := do_syscall(syscall_nr_vmsplice, TSysParam(fdout), TSysParam(iov), TSysParam(count), TSysParam(flags));
end;

function splice (fdin: cInt; offin: cint64; fdout: cInt; offout: cint64; len: size_t; flags: cuInt): cInt; 
begin
  splice := do_syscall(syscall_nr_splice, TSysParam(fdin), TSysParam(offin), TSysParam(fdout), TSysParam(offout), 
                       TSysParam(len), TSysParam(flags));
end;

function tee(fd_in: cInt; fd_out: cInt; len: size_t; flags: cuInt): cInt;
begin
  tee := do_syscall(syscall_nr_tee, TSysParam(fd_in), TSysParam(fd_out),
                    TSysParam(len), TSysParam(flags));
end;

{$endif} // x86

{$endif} // non-libc

{ FUTEX_OP is a macro, doesn't exist in libC as function}
function FUTEX_OP(op, oparg, cmp, cmparg: cint): cint; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  FUTEX_OP := ((op and $F) shl 28) or ((cmp and $F) shl 24) or ((oparg and $FFF) shl 12) or (cmparg and $FFF);
end;

end.
