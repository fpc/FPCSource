{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by the Free Pascal development team.

    Helper RTL functions for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasiutil;

{$mode objfpc}

interface

uses
  wasiapi;

function ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word; external name 'FPC_WASI_CONVERTTOFDRELATIVEPATH';
function fpc_wasi_path_readlink_ansistring(fd: __wasi_fd_t; const path: PChar; path_len: size_t; out link: rawbytestring): __wasi_errno_t; external name 'FPC_WASI_PATH_READLINK_ANSISTRING';

implementation

end.
