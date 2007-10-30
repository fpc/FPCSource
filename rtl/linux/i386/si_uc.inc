{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Michael Van Canneyt, Peter Vreman,
    & Daniel Mantione, members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 Linux/uClibc startup code for Free Pascal
    taken from uClibc/sysdeps/linux/i386/crt1.S

 %edx  Contains a function pointer to be registered with `atexit'.
       This is how the dynamic linker arranges to have DT_FINI
       functions called for shared libraries that have been loaded
       before this code runs.
   

 Stack layout at program start:

         nil
         envn
         ....
         ....           ENVIRONMENT VARIABLES
         env1
         env0
         nil
         argn
         ....
         ....           COMMAND LINE OPTIONS
         arg1
         arg0
         argc <--- esp
}


procedure libc_init; external name '__uClibc_init';
procedure libc_fini; external name '__uClibc_fini';
procedure libc_exit; external name '_exit';
procedure libc_main; external name '__uClibc_main';

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                          C library start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
asm
  xorl    %ebp,%ebp               { clear outer most frame for backtraces }
  popl    %esi                    { Get argc in ecx }
  movl    %esp,%ecx               { Esp now points to the arguments }
  leal    4(%esp,%esi,4),%eax     { The start of the environment is: esp+4*eax+8 }
  andl    $0xfffffff0,%esp        { Align stack }
  pushl   %eax                    { push garbage, so we push 32 bytes in total }

  movl    %eax,operatingsystem_parameter_envp    { save the environment pointer }
  movl    %esi,operatingsystem_parameter_argc    { save the argument counter    }
  movl    %ecx,operatingsystem_parameter_argv    { save the argument pointer    }
  movl    %esp,initialstkptr                     { save initial stack pointer   }

  pushl   %esp            { provide highest stack address to C library }
  pushl   %edx            { push address of shared library finalization }

{$ifdef PIC}
  call    .L0
.L0:
  pop     %ebx
  addl    $_GLOBAL_OFFSET_TABLE_+[.-.L0],%ebx

  pushl   _fini@GOT(%ebx) { push address of entry points }
  pushl   _init@GOT(%ebx)

  pushl   %ecx            { push argv }                                 
  pushl   %esi            { push argc }
                                                                       
  pushl   $PASCALMAIN     { push fpc main procedure }
  call    libc_main       { let fpc main be called from libc startup }
{$else}
  pushl   $libc_fini      { push address of entry points }
  pushl   $libc_init

  pushl   %ecx            { push argv }
  pushl   %esi            { push argc }

  pushl   $PASCALMAIN     { push fpc main procedure }
  call    libc_main       { let fpc main be called from libc startup }
{$endif}
end;

procedure _FPC_libc_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhaltproc:
{$if sizeof(ExitCode)=2}
  movzwl  ExitCode,%ebx
{$else}
  mov     ExitCode,%ebx
{$endif}
  pushl   %ebx
  call    libc_exit
  xorl    %eax,%eax
  incl    %eax                    { eax=1, exit call }
  popl    %ebx
  int     $0x80
  jmp     .Lhaltproc
end;

