{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    System Entry point for Haiku shared libraries,
    linked-against-libc version

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_dllc;

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
  __libc_argc: longint; external libc name '__libc_argc';
  __libc_argv: pointer; external libc name '__libc_argv';
  environ: pointer; external libc name 'environ';

procedure __exit(status: longint); cdecl; external libc name 'exit';

procedure _FPC_shared_lib_start; cdecl; public name 'initialize_after';
begin
  initialstkptr:=get_frame;
  argc:=__libc_argc;
  argv:=__libc_argv;
  envp:=environ;

  PascalMain;
end;

procedure _FPC_shared_lib_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  { call C exit code }
  __exit(_ExitCode);
end;


end.
