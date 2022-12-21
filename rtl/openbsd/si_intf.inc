{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Free Pascal development team

    This file implements the public interface parts of the startup
    code for OpenBSD programs or shared object (.so) libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$SMARTLINK OFF}
{$GOTO ON}

type
  cint = LongInt;
{$if defined(cpu64)}
  culong = QWord;
{$else}
  culong = LongWord;
{$endif}
  u_long = culong;
  TCdeclProcedure = procedure; cdecl;

var
  operatingsystem_parameter_envp: PPAnsiChar; public name 'operatingsystem_parameter_envp';
  operatingsystem_parameter_argc: longint; public name 'operatingsystem_parameter_argc';
  operatingsystem_parameter_argv: PPAnsiChar; public name 'operatingsystem_parameter_argv';
  environ: PPAnsiChar; public name 'environ';
  __progname: PAnsiChar = ''; public name '__progname';
  __progname_storage: array [0..255] of AnsiChar; public name '__progname_storage';
