{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Jeppe Johansen.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

var
  dlexitproc : pointer;

var
  BSS_START: record end; external name '__bss_start';
  STACK_PTR: record end; external name '__stkptr';

  //uclibc_init : TProcedure; external name '_init';
  //uclibc_fini : TProcedure; external name '_fini';
  procedure uclibc_exit(e : longint); weakexternal name 'exit';
  procedure uclibc_main(main : TProcedure; argc : longint; argv : ppchar; init : TProcedure;  fini : TProcedure; sp : pointer); external name '__uClibc_main';


procedure uclibc_init; public name '_init'; assembler; nostackframe;
  asm
    entry a1, 48
  end;

procedure uclibc_fini; public name '_fini'; assembler; nostackframe;
  asm
    entry a1, 48
  end;

procedure _FPC_xtensa_enter(at_exit: TProcedure; sp: pptruint);
  var
    argc: ptruint;
    argv: ppchar;
  begin
    argc:=sp[0];
    argv:=@sp[1];

    initialstkptr:=sp;
    operatingsystem_parameter_argc:=argc;
    operatingsystem_parameter_argv:=argv;
    operatingsystem_parameter_envp:=@sp[argc+2];
 {$ifdef FPC_ABI_WINDOWED }
    { Windowed ABI }
    asm
      movi a0,0
      movi a6,PascalMain
      addi a8,argv
      addi a7,a8,-4
      movi a9,uclibc_init
      movi a10,uclibc_fini
      addi a11,at_exit
      movi a4,uclibc_main
      s32i a1,a1,0
      callx4 a4
      ill
    end;
 {$endif}
 {$ifdef FPC_ABI_CALL0 }
    { Call0 ABI}
    asm
      movi a7,at_exit
      movi a2,PascalMain
      addi a4,argv
      addi a3,a4,-4
      movi a5,uclibc_init
      movi a6,uclibc_fini
      s32i a1,a1,0
      movi a0,uclibc_main
      callx0 a0
      ill
    end;
 {$endif}
  end;


procedure _FPC_proc_start; assembler; public name '_start';
  asm
    ill
  end;


procedure _FPC_xtensa_exit(e:longint); assembler;
  asm
    mov a6,a3
    movi a2,119
    syscall
  end;


procedure _FPC_proc_haltproc(e:longint); cdecl; public name '_haltproc';
  begin
    while true do
      begin
 {$ifdef FPC_ABI_WINDOWED }
    { Windowed ABI }
        asm
	  l32i a5,e
	  movi a4,uclibc_exit
	  callx4 a4
	end;
 {$endif}
 {$ifdef FPC_ABI_CALL0 }
    { Call0 ABI}
        asm
	  l32i a2,e
	  movi a0,uclibc_exit
	  callx0 a0
	end;
 {$endif}
        _FPC_xtensa_exit(e);
      end;
  end;
