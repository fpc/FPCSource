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
                        Shared library start/halt
 ******************************************************************************}

procedure _FPC_shared_lib_haltproc(e:longint); cdecl; forward;

procedure __FPC_shared_lib_start(argc : dword;argv,envp : pointer); cdecl;
  begin
{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
    SysInitEntryInformation.OS.argc:=argc;
    SysInitEntryInformation.OS.argv:=argv;
    SysInitEntryInformation.OS.envp:=envp;
    SysInitEntryInformation.OS.stkptr:=get_frame;
    SysInitEntryInformation.OS.stklen:=StackLength;
    SysInitEntryInformation.OS.haltproc:=@_FPC_shared_lib_haltproc;
    SysEntry(SysInitEntryInformation);
{$else}
    operatingsystem_parameter_argc:=argc;    { Copy the argument count      }
    operatingsystem_parameter_argv:=argv;    { Copy the argument pointer    }
    operatingsystem_parameter_envp:=envp;    { Copy the environment pointer }
    initialstkptr:=get_frame;
    PASCALMAIN;
{$endif}
  end;

procedure _FPC_shared_lib_start; assembler; nostackframe; public name 'FPC_SHARED_LIB_START'; public name '_start';
  asm
    .L_FPC_shared_lib_start:
    .section ".init_array","a"
    .dc.a .L_FPC_shared_lib_start
    .text
    b __FPC_shared_lib_start
  end;

{ this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called }
procedure _FPC_shared_lib_haltproc(e:longint); cdecl; assembler; nostackframe; public name '_haltproc';
  asm
    ori $a7, $zero, 94
    syscall 0
    b _FPC_shared_lib_haltproc
  end;
