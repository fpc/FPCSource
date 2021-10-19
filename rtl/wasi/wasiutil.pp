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

function ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word; external name 'FPC_WASI_CONVERTTOFDRELATIVEPATH';

implementation

end.
