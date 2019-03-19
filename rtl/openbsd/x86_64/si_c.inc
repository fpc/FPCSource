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

{$asmmode gas}

type
  cint = LongInt;
  u_long = QWord;
  TCdeclProcedure = procedure; cdecl;

var
  _etext: Byte; external name '_etext';
  _eprol: Byte; external name '_eprol';

{procedure _mcleanup; cdecl; external name '_mcleanup';
function atexit(proc: TCdeclProcedure): cint; cdecl; external name 'atexit';
procedure monstartup(lowpc, highpc: u_long); cdecl; external name 'monstartup';}
procedure __init; cdecl; external name '__init';
procedure c_exit(exit_code: cint); cdecl; noreturn; external name 'exit';

procedure _FPC_proc___start(argc: LongInt; argv: PPChar; envp: Pointer; para1, para2, para3: QWord); forward;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start'; public name '__start';
  asm
    movq    %rbx,%r9
    movq    %rcx,%r8
    movq    %rdx,%rcx
    movq    (%rsp),%rdi
    leaq    16(%rsp,%rdi,8),%rdx
    leaq    8(%rsp),%rsi
    subq    $8,%rsp
    andq    $0xFFFFFFFFFFFFFFF0,%rsp
    addq    $8,%rsp
    jmp     _FPC_proc___start
  end;

procedure _FPC_proc_haltproc; noreturn; forward;
function _strrchr(str: PChar; character: LongInt): PChar; forward;

procedure _FPC_proc___start(argc: LongInt; argv: PPChar; envp: Pointer; para1, para2, para3: QWord);
  var
    I: SizeUInt;
  begin
    environ:=envp;
    operatingsystem_parameter_envp:=envp;
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
{    atexit(@_mcleanup);
    monstartup(u_long(@_eprol),u_long(@_etext));}
    __init;
    PascalMain;
    asm
      jmp     _FPC_proc_haltproc
    end;
  end;

procedure _FPC_proc_haltproc; noreturn; public name '_haltproc';
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

procedure MD_EPROL_LABEL; assembler; nostackframe; public name '_eprol';
  asm
  end;
