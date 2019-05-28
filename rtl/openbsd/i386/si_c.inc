{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Free Pascal development team

    This file implements parts of the startup code for OpenBSD
    programs that link to the C library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode att}

procedure __init; cdecl; external name '__init';
procedure c_exit(exit_code: cint); cdecl; noreturn; external name 'exit';
function _csu_finish(_argv: PPChar; _envp: PPChar; _cleanup: TCdeclProcedure): PPPChar; cdecl; external name '_csu_finish';

procedure _FPC_proc___start(argc: LongInt; argv: PPChar; envp: Pointer; cleanup: TCdeclProcedure); cdecl; forward;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start'; public name '__start';
  asm
    movl    %esp,%ebp
    andl    $0xFFFFFFF0,%esp
    pushl   %edx
    movl    0(%ebp),%eax
    leal    8(%ebp,%eax,4),%ecx
    leal    4(%ebp),%edx
    pushl   %ecx
    pushl   %edx
    pushl   %eax
    xorl    %ebp,%ebp
    call    _FPC_proc___start
  end;

function _strrchr(str: PChar; character: LongInt): PChar; forward;

procedure _FPC_proc___start(argc: LongInt; argv: PPChar; envp: Pointer; cleanup: TCdeclProcedure); cdecl;
  var
    I: SizeUInt;
    environp: PPPChar;
  begin
    environp:=_csu_finish(argv, envp, cleanup);
    environ:=environp^;
    operatingsystem_parameter_envp:=environ;
    operatingsystem_parameter_argc:=argc;
    operatingsystem_parameter_argv:=argv;
    if argv[0]<>nil then
      begin
        __progname:=_strrchr(argv[0], Ord('/'));
        if __progname<>nil then
          Inc(__progname)
        else
          __progname:=argv[0];
        I:=Low(__progname_storage);
        while (I<High(__progname_storage)) and (__progname[I]<>#0) do
          begin
            __progname_storage[I]:=__progname[I-Low(__progname_storage)];
            Inc(I);
          end;
        __progname_storage[I]:=#0;
        __progname:=@__progname_storage;
      end;
    __init;
    PascalMain;
    c_exit(operatingsystem_result);
  end;

procedure _FPC_proc_haltproc; cdecl; noreturn; public name '_haltproc';
  begin
    c_exit(operatingsystem_result);
  end;

function _strrchr(str: PChar; character: LongInt): PChar; public name '_strrchr';
  begin
    _strrchr:=nil;
    repeat
      if str^=Chr(character) then
        _strrchr:=str;
      if str^<>#0 then
        Inc(str);
    until str^=#0;
  end;

