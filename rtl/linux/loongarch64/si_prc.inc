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

{$ifndef FPC_USE_LIBC}
procedure InitTLS; [external name 'FPC_INITTLS'];
{$endif}

var
  dlexitproc : pointer;

procedure _FPC_proc_haltproc(e: longint); cdecl; forward;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
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

    { adjust $sp for 16-aligned }
    bstrins.d $sp, $zero, 3, 0

    st.d $sp, $t1, TEntryInformation.OS.stkptr
    la.got $t0, StackLength
    st.d $t0, $t1, TEntryInformation.OS.stklen
    la.got $t2, _FPC_proc_haltproc
    st.d $t2, $t1, TEntryInformation.OS.haltproc
    move $a0, $t1

{$if defined(FPC_USE_LIBC)}
    { call PascalMain }
    bl SysEntry
{$else}
    bl SysEntry_InitTLS
{$endif}
{$else}
    { save operatingsystem parameter argc }
    ld.w $a1, $sp, 0
    la.got $t0, operatingsystem_parameter_argc
    st.w $a1, $t0, 0

    { save operatingsystem parameter argv }
    addi.d $a2, $sp, 8
    la.got $t0, operatingsystem_parameter_argv
    st.d $a2, $t0, 0

    { save operatingsystem parameter envp }
    la.got $t0, operatingsystem_parameter_envp
    alsl.d $t1, $a1, $a2, 3
    addi.d $t1, $t1, 8
    st.d $t1, $t0, 0

    { adjust $sp for 16-aligned }
    bstrins.d $sp, $zero, 3, 0

    { save stack pointer }
    la.got $t0, initialstkptr
    st.d $sp, $t0, 0

{$if defined(FPC_USE_LIBC)}
    { call PascalMain }
    bl PASCALMAIN
{$else}
    bl InitTLS
{$endif}
{$endif}
    break 1
  end;


procedure _FPC_dynamic_proc_start; assembler; nostackframe; public name '_dynamic_start';
  asm
    la.got $t0, dlexitproc
    st.d $a0, $t0, 0
    b _FPC_proc_start
  end;


procedure _FPC_loongarch_exit(e:longint); assembler; nostackframe;
  asm
    ori $a7, $zero, 94
    syscall 0
    b _FPC_loongarch_exit
  end;


procedure _FPC_proc_haltproc(e:longint); cdecl; public name '_haltproc';
  begin
    if assigned(dlexitproc) then
      TProcedure(dlexitproc);
    _FPC_loongarch_exit(e);
  end;
