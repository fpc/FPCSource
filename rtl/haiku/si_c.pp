{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    System Entry point for Haiku, linked-against-libc version

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_c;

interface

implementation

{ Bindings to RTL }
var
  initialstkptr: pointer; public name '__stkptr';
  argc: longint; public name 'operatingsystem_parameter_argc';
  argv: pointer; public name 'operatingsystem_parameter_argv';
  envp: pointer; public name 'operatingsystem_parameter_envp';

procedure PascalMain; external name 'PASCALMAIN';

{ Bindings to libroot/libc }
const
  libc = 'root';

var
  argv_save: pointer; external name 'argv_save';
  main_thread_id: ptruint; external name '__main_thread_id';

function find_thread(name: pchar): ptruint; cdecl; external libc name 'find_thread';
procedure _init_c_library_(argc: longint; argv: ppchar; env: ppchar); cdecl; external libc name '_init_c_library_';
procedure _call_init_routines_; cdecl; external libc name '_call_init_routines_';
procedure __exit(status: longint); cdecl; external libc name 'exit';


function _FPC_proc_start(_argc: longint; _argv: pointer; _envp: pointer): longint; cdecl; public name '_start';
begin
  initialstkptr:=get_frame;
  argc:=_argc;
  argv:=_argv;
  envp:=_envp;

  argv_save:=_argv;
  main_thread_id:=find_thread(nil);

  { This is actually only needed for BeOS R5 compatibility,
    they're empty stubs in Haiku, according to the C code (KB) }
  _init_c_library_(_argc,_argv,_envp);
  _call_init_routines_;

  PascalMain;
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  { call C exit code }
  __exit(_ExitCode);
end;


end.
