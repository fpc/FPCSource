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
 Linux ELF startup code for Free Pascal


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

{$asmmode att}

procedure ini_dummy;
  begin
  end;

procedure libc_start_main; external name '__libc_start_main';
procedure libc_exit(code: longint); cdecl; external name 'exit';

{******************************************************************************
                       glibc 2.1 lib + profiling start/halt
 ******************************************************************************}

procedure _FPC_libc21_start; assembler; nostackframe; public name '_start';
asm
  xorl    %ebp,%ebp
  { First locate the start of the environment variables }

  popl    %ecx                    { Get argc in ecx }

  movl    %esp,%esi               { Esp now points to the arguments }
  leal    4(%esp,%ecx,4),%eax     { The start of the environment is: esp+4*eax+4 }
  andl    $0xfffffff0,%esp        { Align stack to 16 bytes }

  {$ifdef FPC_PIC}
	call .Lpiclab
.Lpiclab:
        popl  %ebx
        addl  $_GLOBAL_OFFSET_TABLE_+1,%ebx

	movl  operatingsystem_parameter_envp@GOT(%ebx),%edi
	movl  %eax,(%edi)

	movl  operatingsystem_parameter_argc@GOT(%ebx),%edi
	movl  %ecx,(%edi)

	movl  operatingsystem_parameter_argv@GOT(%ebx),%edi
	movl  %esi,(%edi)

        movl  initialstkptr@GOT(%ebx),%edi
        movl  %esp,(%edi)
  {$else FPC_PIC}
  	movl  %eax,operatingsystem_parameter_envp
  	movl  %ecx,operatingsystem_parameter_argc
  	movl  %esi,operatingsystem_parameter_argv
        movl  %esp,initialstkptr
  {$endif FPC_PIC}

        { int __libc_start_main(
		int *(main) (int, char * *, char * *), 
		int argc, 
		char * * ubp_av, 
		void (*init) (void), 
		void (*fini) (void), 
		void (*rtld_fini) (void), 
		void (* stack_end)); } 

        pushl %ebp   			{ padding }
        pushl %esp   			{ stack_end }
        pushl %edx   			{ function to be registered with
                      			  atexit(), passed by loader }
        pushl $ini_dummy
        pushl $ini_dummy
        pushl %esi                      { Push second argument: argv.  }
        pushl %ecx                      { Push first argument: argc.  }

        pushl $PASCALMAIN
        call  libc_start_main
	hlt
end;

procedure _FPC_libc21_haltproc(e: longint); cdecl; public name '_haltproc';
  begin
    libc_exit(e);
    { we should never return from libc_exit }
    asm
      hlt
    end;
  end;
