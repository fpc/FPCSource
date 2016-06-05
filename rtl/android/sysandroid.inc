{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Yury Sidorov,
    member of the Free Pascal development team.

    Android-specific part of the System unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

procedure atexit(p: pointer); cdecl; external;

var
  _SaveStdOut: THandle;
  _SaveStdErr: THandle;

procedure SysAndroidLibExit; cdecl;
var
  ioclosed: boolean;
begin
  // Check if stdio is closed now
  ioclosed:=do_syscall(syscall_nr_fcntl, TSysParam(1), 1 {F_GETFD}) = -1;
  // If stdio is closed, restore stdout and stderr
  if ioclosed then
    begin
      FpDup2(_SaveStdOut, 1);
      FpDup2(_SaveStdErr, 2);
    end;
  // Close saved handles
  FpClose(_SaveStdOut);
  FpClose(_SaveStdErr);
  // Finalize the library
  lib_exit;
  // Close stdout and stderr if stdio has been closed
  if ioclosed then
    begin
      FpClose(1);
      FpClose(2);
    end;
end;

procedure SysInitAndroidLib; [public, alias:'FPC_LIB_INIT_ANDROID'];
begin
  { Starting from Android 4.4 stdio handles are closed by libc prior to calling
    finalization routines of shared libraries. This causes a error while trying to
    writeln during library finalization and finally a crash because the error can
    not be printer too.
    It is needed to save stdout and stderr handles by duplicating them and restore
    them before library finalization.
  }
  _SaveStdOut:=FpDup(1);
  _SaveStdErr:=FpDup(2);
  // Register the finalization routine
  atexit(@SysAndroidLibExit);
end;
