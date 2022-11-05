{
    This file is part of the Free Pascal run time library.
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

{
The entry point's job is to call __libc_start_main.  Per the ABI,
a0 contains the address of a function to be passed to atexit.
__libc_start_main wants this in a5.

int __libc_start_main (
  int (*main) (int, char **, char **),
  int argc,
  char **argv,
  __typeof (main) init,
  void (*fini) (void),
  void (*rtld_fini) (void),
  void *stack_end);
}

var
  libc_environ: pchar; external name '__environ';
  libc_fpu_control: word; external name '__fpu_control';
  libc_init_proc: procedure; external name '_init';
  libc_fini_proc: procedure; external name '_fini';
  _etext: pointer; external name '_etext';

  fpc_ret_ra,fpc_ret_sp : pointer;

procedure libc_atexit; external name 'atexit';
procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

procedure gmon_monstartup; external name 'monstartup';
procedure gmon_mcleanup; external name '_mcleanup';

procedure _FPC_libc_start; forward;
procedure _FPC_libc_haltproc(e:longint); forward;

procedure main_stub; assembler; nostackframe;
  asm
    { let fp = sp in case of no fp value }
    ori $fp, $sp, 0

    { save ra and sp }
    la.pcrel $t0, fpc_ret_ra
    st.d $ra, $t0, 0
    la.pcrel $t1, fpc_ret_sp
    st.d $sp, $t1, 0

    { call gmon_monstartup(&_FPC_libc_start, &_etext) }
    la.got $a0, _FPC_libc_start
    la.got $a1, _etext
    la.got $ra, gmon_monstartup
    jirl $ra, $ra, 0

    { call libc_atexit(&gmon_mcleanup) }
    la.got $a0, gmon_mcleanup
    la.got $ra, libc_atexit
    jirl $ra, $ra, 0

{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
    la.got, $t0, SysInitEntryInformation
    st.d $sp, $t0, TEntryInformation.OS.stkptr
    la.got $t1, StackLength
    st.d $t1, $t0, TEntryInformation.OS.stklen
    la.got $t2, _FPC_libc_haltproc
    st.d $t2, $t0, TEntryInformation.OS.haltproc
    move $a0, $t0
    bl %plt(SysEntry)
{$else}
    { save stack pointer }
    la.got $t1, initialstkptr
    st.d $sp, $t1, 0

    { call PascalMain }
    bl %plt(PASCALMAIN)
{$endif}
    break 1
  end;

procedure ini_dummy;
  begin
  end;

{******************************************************************************
                          C library start/halt
 ******************************************************************************}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
  asm
    { clear frame pointer }
    ori $fp, $zero, 0

{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
    la.got $t1, SysInitEntryInformation

    { argc = *(int *)sp }
    ld.w $a1, $sp, 0
    st.w $a1, $t1, TEntryInformation.OS.argc

    { argv = (char **)(sp + 8) }
    addi.d $a2, $sp, 8
    st.d $a2, $t1, TEntryInformation.OS.argv

    { save envp }
    alsl.d $t0, $a1, $a2, 3
    addi.d $t0, $t0, 8
    st.d $t0, $t1, TEntryInformation.OS.envp
{$else}
    { argc = *(int *)sp }
    ld.w $a1, $sp, 0

    { save operatingsystem parameter argc }
    la.got $t0, operatingsystem_parameter_argc
    st.w $a1, $t0, 0

    { argv = (char **)(sp + 8) }
    addi.d $a2, $sp, 8

    { save operatingsystem parameter argv }
    la.got $t0, operatingsystem_parameter_argv
    st.d $a2, $t0, 0

    { save operatingsystem parameter envp }
    la.got $t0, operatingsystem_parameter_envp
    alsl.d $t1, $a1, $a2, 3
    addi.d $t1, $t1, 8
    st.d $t1, $t0, 0
{$endif}

    { adjust $sp for 16-aligned }
    bstrins.d $sp, $zero, 3, 0

    { call libc_start_main (&main_stub, argc, argv, dummy, dummy, rtld_fini, stkend ) }
    la.got $a3, ini_dummy
    la.got $a4, ini_dummy
    ori $a5, $a0, 0
    ori $a6, $sp, 0
    la.pcrel $a0, main_stub
    bl %plt(libc_start_main)
    break 1
  end;

procedure _FPC_libc_haltproc(e:longint); assembler; nostackframe; public name '_haltproc';
  asm
    la.pcrel $t0, fpc_ret_sp
    ld.d $sp, $t0, 0
    la.pcrel $t1, fpc_ret_ra
    ld.d $ra, $t1, 0
    jr $ra
  end;
