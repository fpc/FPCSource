{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ Translated to FPK pascal by Pierre Muller,
without changing the exceptn.s file }
Unit DPMIEXCP;

{$I os.inc}

{ Real mode control-C check removed
because I got problems with the RMCB
can be used by setting this conditionnal }
{$define UseRMcbrk}

interface

uses go32{,sysutils};

{$S- no stack check !!! }
{$packrecords 2 }
type   tjmprec = record
          eax,ebx,ecx,edx,esi,edi,ebp,esp,eip,flags : longint;
          cs,ds,es,fs,gs,ss : word;
          { we should also save the FPU state, if we use this for excpections }
          { and the compiler supports FPU register variables }
       end;
    type pjmprec = ^tjmprec;

type texception_state = record
  __eax, __ebx, __ecx, __edx, __esi : longint;
  __edi, __ebp, __esp, __eip, __eflags : longint;
  __cs, __ds, __es, __fs, __gs, __ss : word;
  __sigmask : longint; {  for POSIX signals only  }
  __signum : longint; {  for expansion  }
  __exception_ptr : longint; {  pointer to previous exception  }
  __fpu_state : array [0..108-1] of byte; {  for future use  }
  end;
    pexception_state = ^texception_state;

{ /* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */ }
{#define __djgpp_exception_state (*__djgpp_exception_state_ptr) }
const SIGABRT	= 288;
const SIGFPE	= 289;
const SIGILL	= 290;
const SIGSEGV	= 291;
const SIGTERM	= 292;
const SIGINT   = 295;

{const SIG_DFL  = 0;}
function SIG_DFL( x: longint) : longint;
function SIG_ERR( x: longint) : longint;
function SIG_IGN( x: longint) : longint;
{const SIG_ERR	= -1;
const SIG_IGN	= -1;}

{ __DJ_pid_t
#undef __DJ_pid_t
const __DJ_pid_t

typedef int sig_atomic_t;

int	raise(int _sig);
void	(*signal(int _sig, void (*_func)(int)))(int); }
  
{ #ifndef __STRICT_ANSI__

const SA_NOCLDSTOP	1

const SIGALRM	293
const SIGHUP	294
/* SIGINT is ansi */}
const SIGKILL	= 296;
const SIGPIPE	= 297;
const SIGQUIT	= 298;
const SIGUSR1	= 299;
const SIGUSR2	= 300;
{
const SIG_BLOCK	1
const SIG_SETMASK	2
const SIG_UNBLOCK	3 }

const SIGNOFP = 301;
const SIGTRAP = 302;
const SIGTIMR = 303;	{/* Internal for setitimer (SIGALRM, SIGPROF) */ }
const SIGPROF = 304;
const SIGMAX  = 320;



{ extern unsigned short __djgpp_our_DS;
extern unsigned short __djgpp_app_DS;	/* Data selector invalidated by HW ints */
extern unsigned short __djgpp_ds_alias;	/* Data selector always valid */
extern unsigned short __djgpp_dos_sel;	/* Linear mem selector copy in locked mem */
extern unsigned short __djgpp_hwint_flags; /* 1 = Disable Ctrl-C; 2 = Count Ctrl-Break (don't kill) */
extern unsigned __djgpp_cbrk_count;	/* Count of CTRL-BREAK hits */
extern int __djgpp_exception_inprog;	/* Nested exception count */ }

type SignalHandler = function (v : longint) : longint;

function signal(sig : longint;func : SignalHandler) : SignalHandler;

function _raise(sig : longint) : longint;

procedure djgpp_exception_toggle;

function  djgpp_set_ctrl_c(enable : boolean) : boolean; {	/* On by default */}

procedure djgpp_exception_setup;

function djgpp_exception_state : pexception_state;

function do_faulting_finish_message : integer;

function setjmp(var rec : tjmprec) : longint;

function dpmi_set_coprocessor_emulation(flag : longint) : longint;

procedure longjmp({const}var rec : tjmprec;return_value : longint);

implementation

{$L exceptn.o}

const exceptions_on : boolean = false;

var starttext, endtext : pointer;

function SIG_ERR( x: longint) : longint;
begin
   SIG_ERR:=-1;
end;

function SIG_IGN( x: longint) : longint;
begin
   SIG_IGN:=-1;
end;

function SIG_DFL( x: longint) : longint;
begin
   SIG_DFL:=0;
end;

{ #include <libc/stubs.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <io.h>
#include <libc/farptrgs.h>
#include <dpmi.h>
#include <go32.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <crt0.h>
#include <pc.h>
#include <sys/exceptn.h>
#include <sys/nearptr.h>		/* For DS base/limit info */
#include <libc/internal.h> }

const newline = #13#10;

procedure err(x : string);
begin
   write(stderr, x);
   flush(stderr);
end;

{ extern unsigned end __asm__ ('end'); }
const cbrk_vect : byte = $1b;
{	/* May be $06 for PC98 */ }

{ /* These are all defined in exceptn.S and only used here */
extern int __djgpp_exception_table;
extern int __djgpp_npx_hdlr;
extern int __djgpp_kbd_hdlr;
extern int __djgpp_kbd_hdlr_pc98;
extern int __djgpp_iret, __djgpp_i24;
extern void __djgpp_cbrk_hdlr(void);
extern int __djgpp_hw_lock_start, __djgpp_hw_lock_end;
extern tseginfo __djgpp_old_kbd; }

procedure itox(v,len : longint);
  var st : string;
  begin
     st:=hexstr(v,len);
     write(stderr,st);
     flush(stderr);
  end;

function except_to_sig(excep : longint) : longint;
  begin
     case excep of
        5,8,9,11,12,13,14 : exit(SIGSEGV);
        0,4,16            : exit(SIGFPE);
        1,3               : exit(SIGTRAP);
        7                 : exit(SIGNOFP);
        else
           begin
              if(excep = $75)	then	{/* HW int to fake exception values hardcoded in exceptn.S */}
                exit(SIGFPE)
              else if (excep = $78) then
                exit(SIGTIMR)
              else if ((excep = $79) or (excep = $1b)) then
                exit(SIGINT)
              else
                exit(SIGILL);
           end;
        end;
  end;

  function djgpp_exception_state : pexception_state;
    begin
       asm
          movl ___djgpp_exception_state_ptr,%eax
          movl %eax,__RESULT
       end;
    end;

procedure show_call_frame;

  begin
     err('Call frame traceback EIPs:'+newline);
     err('  0x'+hexstr(djgpp_exception_state^.__eip, 8)+newline);
     dump_stack(djgpp_exception_state^.__ebp);
  end;

const EXCEPTIONCOUNT = 18;
const exception_names : array[0..EXCEPTIONCOUNT-1] of pchar = (
  'Division by Zero',
  'Debug',
  'NMI',
  'Breakpoint',
  'Overflow',
  'Bounds Check',
  'Invalid Opcode',
  'Coprocessor not available',
  'Double Fault',
  'Coprocessor overrun',
  'Invalid TSS',
  'Segment Not Present',
  'Stack Fault',
  'General Protection Fault',
  'Page fault',
  ' ',
  'Coprocessor Error',
  'Alignment Check');

const has_error : array [0..EXCEPTIONCOUNT-1] of byte =
   (0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,1);

const
    cbrk_hooked : boolean = false;


procedure dump_selector(const name : string; sel : word);
  var base,limit : longint;
  begin
     write(stderr, name);
     err(': sel=');
     itox(sel, 4);
  if (sel<>0) then
    begin
       base:=get_segment_base_address(sel);
   
       {
         err('  invalid');
       }
       { else }
   
       err('  base='); itox(base, 8);
       limit:=get_segment_limit(sel);
       err('  limit='); itox(limit, 8);
    end;
  err(newline);
  end;

function farpeekb(sel : word;offset : longint) : byte;
  var b : byte;
  begin
     seg_move(sel,offset,get_ds,longint(@b),1);
     farpeekb:=b;
  end;

  const old_video_mode : byte = 3;

function do_faulting_finish_message : integer;
  var en : pchar;
      signum,i : longint;
      old_vid : byte;
  begin
     do_faulting_finish_message:=0;
     signum:=djgpp_exception_state^.__signum;
     {/* check video mode for original here and reset (not if PC98) */ }
     if ((go32_info_block.linear_address_of_primary_screen <> $a0000) and
        (farpeekb(dosmemselector, $449) <> old_video_mode)) then
       begin
          old_vid:=old_video_mode;
          asm
             pusha
             movzbl old_vid,%eax
             int $0x10
             popa
             nop
          end;
       end;

     if (signum >= EXCEPTIONCOUNT) then
       en:=nil
     else
       en:=exception_names[signum];
     if (signum = $75) then
       en:='Floating Point exception';
     if (signum = $1b) then
       en:='Control-Break Pressed';
     if (signum = $79) then
       en:='Control-C Pressed';
     if (en = nil) then
       begin
          err('Exception ');
          itox(signum, 2);
          err(' at eip=');
          itox(djgpp_exception_state^.__eip, 8);
       end
     else
       begin
          write(stderr, 'FPK ',en);
          err(' at eip=');
          itox(djgpp_exception_state^.__eip, 8);
       end;
     { Control-C should stop the program also !}
     {if (signum = $79) then
       begin
          err(newline);
          exit(-1);
       end;}
     if ((signum < EXCEPTIONCOUNT) and (has_error[signum]=1)) then
       begin
          errorcode := djgpp_exception_state^.__sigmask and $ffff;
          if(errorcode<>0) then
            begin
               err(', error=');
               itox(errorcode, 4);
            end;
       end;
     err(newline);
     err('eax=');
     itox(djgpp_exception_state^.__eax, 8);
     err(' ebx='); itox(djgpp_exception_state^.__ebx, 8);
     err(' ecx='); itox(djgpp_exception_state^.__ecx, 8);
     err(' edx='); itox(djgpp_exception_state^.__edx, 8);
     err(' esi='); itox(djgpp_exception_state^.__esi, 8);
     err(' edi='); itox(djgpp_exception_state^.__edi, 8);
     err(newline);
     err('ebp='); itox(djgpp_exception_state^.__ebp, 8);
     err(' esp='); itox(djgpp_exception_state^.__esp, 8);
     err(' program=');
     err(paramstr(0)+newline);
     dump_selector('cs', djgpp_exception_state^.__cs);
     dump_selector('ds', djgpp_exception_state^.__ds);
     dump_selector('es', djgpp_exception_state^.__es);
     dump_selector('fs', djgpp_exception_state^.__fs);
     dump_selector('gs', djgpp_exception_state^.__gs);
     dump_selector('ss', djgpp_exception_state^.__ss);
     err(newline);
     if (djgpp_exception_state^.__cs = get_cs) then
       show_call_frame;
     { must not return !! }
     if exceptions_on then
       djgpp_exception_toggle;
     asm
        pushw $1
        call  ___exit
     end;
end;

var  signal_list : Array[0..SIGMAX] of SignalHandler;
 {	/* SIG_DFL = 0 */ }

function signal(sig : longint;func : SignalHandler) : SignalHandler;
  var temp : SignalHandler;

  begin
     if ((sig <= 0) or (sig > SIGMAX) or (sig = SIGKILL)) then
       begin
          signal:=@SIG_ERR;
          runerror(201);
       end;
     temp := signal_list[sig - 1];
     signal_list[sig - 1] := func;
     signal:=temp;
  end;


const signames : array [0..14] of string[4] = (
   'ABRT',
   'FPE ',
   'ILL ',
   'SEGV',
   'TERM',
   'ALRM',
   'HUP ',
   'INT ',
   'KILL',
   'PIPE',
   'QUIT',
   'USR1',
   'USR2',
   'NOFP',
   'TRAP');


function _raise(sig : longint) : longint;
  var temp : SignalHandler;
  label traceback_exit;
  begin
     if(sig <= 0) then
       exit(-1);
  if (sig > SIGMAX) then
    exit(-1);
  temp:=signal_list[sig - 1];
  if (temp = SignalHandler(@SIG_IGN)) then
    exit(0); {			/* Ignore it */ }
  if (temp = SignalHandler(@SIG_DFL)) then
    begin
      traceback_exit:
      if ((sig >= SIGABRT) and (sig <= SIGTRAP)) then
        begin
           err('Exiting due to signal SIG');
           err(signames[sig-sigabrt]);
        end
      else
        begin
           err('Exiting due to signal $');
           itox(sig, 4);
        end;
      err(newline);
      { if(djgpp_exception_state<>nil) then }
        do_faulting_finish_message();	{/* Exits, does not return */ }
      exit(-1);
    end;
  if ((longint(temp) < longint(starttext)) or (longint(temp) > longint(endtext))) then
    begin
       err('Bad signal handler, ');
       goto traceback_exit;
    end;
  temp(sig);
  exit(0);
  end;

{ /* This routine must call exit() or jump changing stacks.  This routine is
   the basis for traceback generation, core creation, signal handling. */ }

{ taken from sysutils.pas }
    function setjmp(var rec : tjmprec) : longint;

      begin
         asm
            pushl %edi
            movl rec,%edi
            movl %eax,(%edi)
            movl %ebx,4(%edi)
            movl %ecx,8(%edi)
            movl %edx,12(%edi)
            movl %esi,16(%edi)

            { load edi }
            movl -4(%ebp),%eax

            { ... and store it }
            movl %eax,20(%edi)

            { ebp ... }
            movl (%ebp),%eax
            movl %eax,24(%edi)

            { esp ... }
            movl %esp,%eax
            addl $12,%eax
            movl %eax,28(%edi)

            { the return address }
            movl 4(%ebp),%eax
            movl %eax,32(%edi)

            { flags ... }
            pushfl
            popl 36(%edi)

            { !!!!! the segment registers, not yet needed }
            { you need them if the exception comes from
            an interrupt or a seg_move }
            movw %cs,40(%edi)
            movw %ds,42(%edi)
            movw %es,44(%edi)
            movw %fs,46(%edi)
            movw %gs,48(%edi)
            movw %ss,50(%edi)

	    movl ___djgpp_exception_state_ptr, %eax
	    movl %eax, 60(%edi)

            { restore EDI }
            pop %edi

            { we come from the initial call }
            xorl %eax,%eax

            leave
            ret $4
         end;
      end;

const exception_level : longint = 0;

    procedure longjmp({const}var  rec : tjmprec;return_value : longint);

      begin
         if (@rec=pjmprec(djgpp_exception_state)) and
            (exception_level>0) then
           dec(exception_level);
         asm
            { restore compiler shit }
            popl %ebp
{/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */}
{/* This is file LONGJMP.S */}
	movl	4(%esp),%edi	{/* get jmp_buf */}
	movl	8(%esp),%eax	{/* store retval in j->eax */}
	movl	%eax,0(%edi)

	movw	46(%edi),%fs
	movw	48(%edi),%gs
	movl	4(%edi),%ebx
	movl	8(%edi),%ecx
	movl	12(%edi),%edx
	movl	24(%edi),%ebp

	{/* Now for some uglyness.  The jmp_buf structure may be ABOVE the
	   point on the new SS:ESP we are moving to.  We don't allow overlap,
	   but do force that it always be valid.  We will use ES:ESI for
	   our new stack before swapping to it.  */}

	movw	50(%edi),%es
	movl	28(%edi),%esi
	subl	$28,%esi	{/* We need 7 working longwords on stack */}

	movl	60(%edi),%eax
	es
	movl	%eax,(%esi)	{/* Exception pointer */}

	movzwl	42(%edi),%eax
	es
	movl	%eax,4(%esi)	{/* DS */}

	movl	20(%edi),%eax
	es
	movl	%eax,8(%esi)	{/* EDI */}

	movl	16(%edi),%eax
	es
	movl	%eax,12(%esi)	{/* ESI */}

	movl	32(%edi),%eax
	es
	movl	%eax,16(%esi)	{/* EIP - start of IRET frame */}

	movl	40(%edi),%eax
	es
	movl	%eax,20(%esi)	{/* CS */}

	movl	36(%edi),%eax
	es
	movl	%eax,24(%esi)	{/* EFLAGS */}

	movl	0(%edi),%eax
	movw	44(%edi),%es

	movw	50(%edi),%ss
	movl	%esi,%esp

	popl	___djgpp_exception_state_ptr
	popl	%ds
	popl	%edi
	popl	%esi
	iret			{/* actually jump to new cs:eip loading flags */}
         end;
      end;


      procedure djgpp_exception_processor;[public,alias : '___djgpp_exception_processor'];
    var sig : longint;

    begin
       inc(exception_level);
       sig:=djgpp_exception_state^.__signum;
       if (exception_level=1) or (sig=$78) then
         begin
            sig := except_to_sig(sig);
            _raise(sig);
            if (djgpp_exception_state^.__signum >= EXCEPTIONCOUNT) then
            { /* Not exception so continue OK */ }
              longjmp(pjmprec(djgpp_exception_state)^, djgpp_exception_state^.__eax);
            {/* User handler did not exit or longjmp, we must exit */}
            err('FPK cannot continue from exception, exiting due to signal ');
            itox(sig, 4);
            err(newline);
         end
       else
         begin
            if exception_level>2 then
              begin
                 err('FPK triple exception, exiting !!! ');
                 if (exceptions_on) then
                   djgpp_exception_toggle;
                 asm
                    pushw $1
                    call  ___exit
                 end;
              end;
            err('FPK double exception, exiting due to signal ');
            itox(sig, 4);
            err(newline);
         end;
       do_faulting_finish_message;
    end;

type trealseginfo = tseginfo;
     pseginfo = ^tseginfo;

var except_ori : array [0..EXCEPTIONCOUNT-1] of tseginfo;
    kbd_ori : tseginfo;
    npx_ori : tseginfo;
    cbrk_ori,cbrk_rmcb : trealseginfo;
    cbrk_regs : registers;
{/* Routine toggles ALL the exceptions.  Used around system calls, at exit. */}

function djgpp_cbrk_hdlr : pointer;
  begin
     asm
        movl ___djgpp_cbrk_hdlr,%eax
        movl %eax,__RESULT
     end;
  end;

function djgpp_old_kbd : pseginfo;
  begin
     asm
        movl ___djgpp_old_kbd,%eax
        movl %eax,__RESULT
     end;
  end;

procedure djgpp_exception_toggle;
  var _except : tseginfo;
      i : longint;
      local_ex : boolean;

  begin
{$ifdef DEBUG}
     if exceptions_on then
       begin
          err('Disabling FPK exceptions');
          err(newline);
       end
     else
       begin
          err('Enabling FPK exceptions');
          err(newline);
       end;
{$endif DEBUG}
     { toggle here to avoid infinite recursion }
     { if a subfunction calls runerror !!      }
     exceptions_on:= not exceptions_on;
     local_ex:=exceptions_on;
     asm
        movzbl local_ex,%eax
        movl   %eax,_v2prt0_exceptions_on
     end;
     for i:=0 to  EXCEPTIONCOUNT-1 do
       begin
          if get_pm_exception_handler(i,_except) then
            begin
               if (i <> 2) {or (_crt0_startup_flags & _CRT0_FLAG_NMI_SIGNAL))} then
               if not set_pm_exception_handler(i,except_ori[i]) then
                 err('error setting exception nø'+hexstr(i,2));
               except_ori[i] := _except;
            end
          else
            begin
               if get_exception_handler(i,_except) then
                 begin
                    if (i <> 2) {or (_crt0_startup_flags & _CRT0_FLAG_NMI_SIGNAL))} then
                    if not set_exception_handler(i,except_ori[i]) then
                      err('error setting exception nø'+hexstr(i,2));
                    except_ori[i] := _except;
                 end
            end;
       end;
     get_pm_interrupt($75, _except);
     set_pm_interrupt($75, npx_ori);
     npx_ori:=_except;
     get_pm_interrupt(9, _except);
     set_pm_interrupt(9, kbd_ori);
     kbd_ori := _except;
{$ifdef UseRMcbrk}
     if (cbrk_hooked) then
       begin
          set_rm_interrupt(cbrk_vect,cbrk_ori);
          free_rm_callback(cbrk_rmcb);
          cbrk_hooked := false;
{$ifdef DEBUG}
       err('back to ori rm cbrk  '+hexstr(cbrk_ori.segment,4)+':'+hexstr(longint(cbrk_ori.offset),4));

{$endif DEBUG}
       end
     else
       begin
          get_rm_interrupt(cbrk_vect, cbrk_ori);
{$ifdef DEBUG}
       err('ori rm cbrk  '+hexstr(cbrk_ori.segment,4)+':'+hexstr(longint(cbrk_ori.offset),4));
{$endif DEBUG}
          get_rm_callback(djgpp_cbrk_hdlr, cbrk_regs, cbrk_rmcb);
          set_rm_interrupt(cbrk_vect, cbrk_rmcb);
{$ifdef DEBUG}
       err('now rm cbrk  '+hexstr(cbrk_rmcb.segment,4)+':'+hexstr(longint(cbrk_rmcb.offset),4));
{$endif DEBUG}
          cbrk_hooked := true;
       end;
{$endif UseRMcbrk}
  end;

  function dpmi_set_coprocessor_emulation(flag : longint) : longint;

    var
       res : longint;

    begin
       asm
          movl flag,%ebx
          movl $0xe01,%eax
          int $0x31
          jc .L_coproc_error
          xorl %eax,%eax
       .L_coproc_error:
          movl %eax,res
       end;
       dpmi_set_coprocessor_emulation:=res;
    end;


procedure dpmiexcp_exit{(status : longint)};[alias : 'excep_exit'];
{
  /* We need to restore hardware interrupt handlers even if somebody calls
     `_exit' directly, or else we crash the machine in nested programs.
     We only toggle the handlers if the original keyboard handler is intact
     (otherwise, they might have already toggled them).  */       }
  begin
     if (exceptions_on) then
       djgpp_exception_toggle;
     asm
        xorl %eax,%eax
        movl %eax,_exception_exit
        movl %eax,_swap_in
        movl %eax,_swap_out
     end;
     { restore the FPU state }
     dpmi_set_coprocessor_emulation(1);
  end;

{ used by dos.pp for swap vectors }
procedure dpmi_swap_in;[alias : 'swap_in'];
  begin
     if not (exceptions_on) then
       djgpp_exception_toggle;
  end;

procedure dpmi_swap_out;[alias : 'swap_out'];
  begin
     if (exceptions_on) then
       djgpp_exception_toggle;
  end;

procedure djgpp_exception_setup;

  var _except,old_kbd : tseginfo;
      locksize : longint;
      hw_lock_start, hw_lock_end : longint;
      i : longint;
      dossel :word;
  begin
     asm
        movl _exception_exit,%eax
        xorl %eax,%eax
        jne  .L_already
        leal excep_exit,%eax
        movl %eax,_exception_exit
        leal swap_in,%eax
        movl %eax,_swap_in
        leal swap_out,%eax
        movl %eax,_swap_out
     end;

     for i := 0 to  SIGMAX-1 do
        signal_list[i] := SignalHandler(@SIG_DFL);

     { /* app_DS only used when converting HW interrupts to exceptions */ }
     asm
        movw %ds,___djgpp_app_DS
        movw %ds,___djgpp_our_DS
        movl $___djgpp_hw_lock_start,%eax
        movl %eax,hw_lock_start
        movl $___djgpp_hw_lock_end,%eax
        movl %eax,hw_lock_end
     end;
     dossel := dosmemselector;
     asm
        movw dossel,%ax
        movw %ax,___djgpp_dos_sel
     end;
     {/* lock addresses which may see HW interrupts */}
     { lockmem.address = __djgpp_base_address + (unsigned) &__djgpp_hw_lock_start;}
     locksize := hw_lock_end - hw_lock_start;
     lock_code(pointer(hw_lock_start),locksize);
     _except.segment:=get_cs;
{        _except.offset:= (unsigned) &__djgpp_exception_table;}
      asm
         leal _except,%eax
         movl $___djgpp_exception_table,(%eax)
      end;

      for i:=0 to EXCEPTIONCOUNT-1 do
        begin
           except_ori[i] := _except;	{/* New value to set */}
           _except.offset:=_except.offset + 4;	{/* This is the size of push n, jmp */}
        end;

      kbd_ori.segment := _except.segment;
      npx_ori.segment := _except.segment;
           asm
              leal _NPX_ORI,%eax
              movl $___djgpp_npx_hdlr,(%eax)
           end;
      {npx_ori.offset32:= (unsigned) &__djgpp_npx_hdlr;}
      if (go32_info_block.linear_address_of_primary_screen <> $a0000) then
        begin
           asm
              leal _KBD_ORI,%eax
              movl $___djgpp_kbd_hdlr,(%eax)
           end;
        {kbd_ori.offset32 = (unsigned) &__djgpp_kbd_hdlr;}
        end
      else
        begin
           asm
              leal _KBD_ORI,%eax
              movl $___djgpp_kbd_hdlr_pc98,(%eax)
           end;
           {kbd_ori.offset32 = (unsigned) &__djgpp_kbd_hdlr_pc98;}
           cbrk_vect := $06;
           asm
              leal _except,%eax
              movl $___djgpp_iret,(%eax)
           end;
           {_except.offset32 = (unsigned) &__djgpp_iret;		/* TDPMI98 bug */}
           set_pm_interrupt($23,_except);
        end;
     asm
        leal _except,%eax
        movl $___djgpp_i24,(%eax)
     end;
     {except.offset32 = (unsigned) &__djgpp_i24;}
     set_pm_interrupt($24, _except);
     get_pm_interrupt(9,old_kbd);
     asm
        movl $___djgpp_old_kbd,%edi
        leal old_kbd,%esi
        movl $6,%ecx { sier of tseginfo }
        rep
        movsb
     end;
     djgpp_exception_toggle;	{/* Set new values & save old values */}

     {/* get original video mode and save */}
     old_video_mode := farpeekb(dosmemselector, $449);
     asm
        .L_already:
     end;
  end;


function djgpp_set_ctrl_c(enable : boolean) : boolean;
  var oldenable : boolean;
begin
  asm
     movb ___djgpp_hwint_flags,%al
     andb $1,%al
     movb %al,oldenable
  end;
  if (enable) then
       asm
         movl ___djgpp_hwint_flags,%eax
         andl $0xfffe,%eax
         movl %eax,___djgpp_hwint_flags
       end
  else
       asm
         movl ___djgpp_hwint_flags,%eax
         orl $1,%eax
         movl %eax,___djgpp_hwint_flags
       end;
    {__djgpp_hwint_flags |= 1;}
  djgpp_set_ctrl_c:=oldenable;
end;

begin
   asm
      movl $_etext,_ENDTEXT
      movl $start,_STARTTEXT
      movl ___v2prt0_ds_alias,%eax
      movl %eax,___djgpp_ds_alias
   end;
djgpp_exception_setup;
end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.9  1998/03/18 15:34:46  pierre
    + fpu state is restaured in excep_exit
      less risk of problems

  Revision 1.8  1998/03/01 18:18:53  carl
    * bugfix of wrong vector initialization because of incorrect
      error indexes (were starting at 1 instead of zero in some places).

  Revision 1.7  1998/02/05 17:04:58  pierre
    * emulation is working with wmemu387.dxe

  Revision 1.6  1998/02/03 15:52:49  pierre
    * swapvectors really disable exception handling
      and interrupt redirection with go32v2
    * in dos.pp bug if arg path from fsearch had a directory part fixed

  Revision 1.5  1998/01/26 11:57:25  michael
  + Added log at the end

  Revision 1.4  1998/01/16 16:49:12  pierre
    * Crtl-C did not break the program

}


{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.9  1998/03/18 15:34:46  pierre
    + fpu state is restaured in excep_exit
      less risk of problems

  Revision 1.8  1998/03/01 18:18:53  carl
    * bugfix of wrong vector initialization because of incorrect
      error indexes (were starting at 1 instead of zero in some places).

  Revision 1.7  1998/02/05 17:04:58  pierre
    * emulation is working with wmemu387.dxe

  Revision 1.6  1998/02/03 15:52:49  pierre
    * swapvectors really disable exception handling
      and interrupt redirection with go32v2
    * in dos.pp bug if arg path from fsearch had a directory part fixed

  Revision 1.5  1998/01/26 11:57:25  michael
  + Added log at the end


  
  Working file: rtl/dos/go32v2/dpmiexcp.pp
  description:
  ----------------------------
  revision 1.4
  date: 1998/01/16 16:49:12;  author: pierre;  state: Exp;  lines: +8 -3
    * Crtl-C did not break the program
  ----------------------------
  revision 1.3
  date: 1997/12/12 13:14:38;  author: pierre;  state: Exp;  lines: +40 -4
     + added handling of swap_vectors if under exceptions
       i.e. swapvector is not dummy under go32v2
     * bug in output, exceptions where not allways reset correctly
       now the code in dpmiexcp is called from v2prt0.as exit routine
     * in crt.pp corrected init_delay calibration loop
       and added it for go32v2 also (was disabled before due to crashes !!)
       the previous code did a wrong assumption on the time need to call
       get_ticks compared to an internal loop without call
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:26:08;  author: michael;  state: Exp;  lines: +14 -3
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
