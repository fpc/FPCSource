{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020,2021 by the Free Pascal development team.

    API functions for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function __wasi_fd_write(fd: __wasi_fd_t;
                         iovs: P__wasi_ciovec_t;
                         iovs_len: size_t;
                         nwritten: P__wasi_size_t): __wasi_errno_t; external 'wasi_snapshot_preview1' name 'fd_write';
function __wasi_fd_read(fd: __wasi_fd_t;
                        iovs: P__wasi_iovec_t;
                        iovs_len: size_t;
                        nread: P__wasi_size_t): __wasi_errno_t; external 'wasi_snapshot_preview1' name 'fd_read';
procedure __wasi_proc_exit(rval: __wasi_exitcode_t); noreturn; external 'wasi_snapshot_preview1' name 'proc_exit';
function __wasi_fd_fdstat_get(fd: __wasi_fd_t;
                              stat: P__wasi_fdstat_t): __wasi_errno_t; external 'wasi_snapshot_preview1' name 'fd_fdstat_get';
function __wasi_path_open(fd: __wasi_fd_t;
                          dirflags: __wasi_lookupflags_t;
                          path: PChar;
                          path_len: size_t;
                          oflags: __wasi_oflags_t;
                          fs_rights_base,
                          fs_rights_inherting: __wasi_rights_t;
                          fdflags: __wasi_fdflags_t;
                          opened_fd: P__wasi_fd_t): __wasi_errno_t; external 'wasi_snapshot_preview1' name 'path_open';
function __wasi_fd_close(fd: __wasi_fd_t): __wasi_errno_t; external 'wasi_snapshot_preview1' name 'fd_close';

